#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FEATURE IMPORTANCE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, feature importance plots are created for all machine learning
# models and outcomes in both the binary and multivalued treatment setting. 
# However, this is only done for the main model (no robustness checks).
# Two strategies are examined. First, aggregated feature importance measures
# across the folds, repetitions, and imputed data sets. Second, training new
# models on the full data set using the "best" hyperparameter combinations.
#+++
# Sources:
# -> https://medium.com/analytics-vidhya/feature-importance-explained-bfc8d874bcf
# -> https://medium.com/the-artificial-impostor/feature-importance-measures-for-tree-models-part-i-47f187c1a2c3
# -> https://becominghuman.ai/feature-importance-measures-for-tree-models-part-ii-20c9ff4329b
#+++

# save variable names for descriptives
var_names_descr <- c()

# user selection
if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}

if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### USE COEFFICIENTS FROM CROSS-FITTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Binary Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Lasso ####
#+++++++++++++#

# load all estimation results
lasso_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_lasso_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across folds, repetitions and MICE data frames
lasso_binary_coef <- data.frame()
for (i in 1:5) {
  lasso_binary_coef <- rbind(
    lasso_binary_coef, lasso_binary_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
lasso_binary_coef <- lasso_binary_coef %>% filter(!term %in% c("(Intercept)", "treatment_period"))

# summarize them by taking the mean
lasso_binary_coef <- lasso_binary_coef %>%
  group_by(model, term) %>%
  summarize(estimate = mean(estimate)) %>%
  ungroup() %>%
  rename(
    Pred_Type = model, Variable = term, Importance = estimate
  ) %>%
  mutate(Pred_Type = case_when(
    Pred_Type == "g0" ~ "Outcome 0 Prediction",
    Pred_Type == "g1" ~ "Outcome 1 Prediction",
    TRUE ~ "Treatment Prediction"
  )) %>%
  filter(!Variable %in% c("social_integr_1", "uni_achievement_comp_much_worse", 
                          "uni_courses_num_much_less_lag", "uni_degree_importance_well_rather",
                          "uni_degree_importance_well_rather_not")) %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features) %>%
  ungroup() 

# add them to variable description vector (used later for descriptive statistics)
var_names_descr <- c(var_names_descr, lasso_binary_coef$Variable)

# generate feature importance plots
lasso_feature_imp_plot <- func_feature_importance_plot("binary", lasso_binary_coef, "LASSO", "separate")

# save plots
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_m.png", 
       lasso_feature_imp_plot$m + ggtitle("LASSO") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_g0.png", 
       lasso_feature_imp_plot$g0 + ggtitle("LASSO") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_g1.png", 
       lasso_feature_imp_plot$g1 + ggtitle("LASSO") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')



#%%%%%%%%%%%%%%%%%%#
#### Post-Lasso ####
#%%%%%%%%%%%%%%%%%%#

postlasso_feature_imp_plot <- list()
postlasso_binary_coef_all <- data.frame()
#"neuroticism", 
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "openness", "conscientiousness", "neuroticism")) { 
  print(paste("OUTCOME:", str_to_title(outcome_var_sel)))
  
  # load coefficient estimates
  if (str_detect(outcome_var_sel, "grade")) {
    load_path <- "Grades"
  } else {
    load_path <- "Personality"
  }
  postlasso_binary_results_all <- 
    readRDS(paste0(
      "Output/DML/Estimation/", load_path, "/binary_", outcome_var_sel, "_postlasso_", model_type, "_",
      str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
      extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
      "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
      "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))
  
  # extract coefficients across MICE data frames
  postlasso_binary_coef <- data.frame()
  for (i in 1:5) {
    postlasso_binary_coef <- rbind(
      postlasso_binary_coef, postlasso_binary_results_all[[i]]$coef %>% mutate(MICE = i)
    )
  }
  postlasso_binary_coef <- postlasso_binary_coef %>% filter(term != "(Intercept)")
  
  # aggregate
  postlasso_binary_coef <- postlasso_binary_coef %>%
    group_by(model, term) %>%
    summarize(estimate = mean(estimate)) %>%
    ungroup() %>%
    rename(
      Pred_Type = model, Variable = term, Importance = estimate
    ) 
  
  if (str_detect(outcome_var_sel, "grade")) {
    postlasso_binary_coef <- postlasso_binary_coef %>%
      filter(!Variable %in% c("treatment_period", "uni_achievement_comp_much_worse", 
                              "uni_achievement_expect_rather", "uni_chances_job_rather_good",
                              "uni_degree_importance_well_rather"))
  } else if (outcome_var_sel == "conscientiousness") {
    postlasso_binary_coef <- postlasso_binary_coef %>%
      filter(!Variable %in% c("motivation_degree_4"))
  } else if (outcome_var_sel == "openness") {
    postlasso_binary_coef <- postlasso_binary_coef %>%
      filter(!Variable %in% c("interest_art_musuem_once_lag", "interest_reading_num_books_11_to_25_books_lag",
                              "interest_reading_num_books_11_to_25_books", "interest_art_musuem_once"))
  } else if (outcome_var_sel == "neuroticism") {
    postlasso_binary_coef <- postlasso_binary_coef %>%
      filter(!Variable %in% c("uni_fear_1_not"))
  }
  
  if (str_detect(outcome_var_sel, "grade")) {
    postlasso_binary_coef <- postlasso_binary_coef %>% 
      mutate(Pred_Type = case_when(
        Pred_Type == "g0" ~ "Outcome 0 Prediction", 
        Pred_Type == "g1" ~ "Outcome 1 Prediction",
        TRUE ~ "Treatment Prediction"
      )) %>%
      group_by(Pred_Type) %>%
      slice_max(order_by = Importance, n = n_features) %>%
      ungroup() %>%
      mutate(Outcome = outcome_var_sel)
  } else {
    postlasso_binary_coef <- postlasso_binary_coef %>% 
      mutate(Pred_Type = case_when(
        Pred_Type == "g0" ~ "Outcome Prediction", 
        Pred_Type == "g1" ~ "Outcome Prediction",
        TRUE ~ "Treatment Prediction"
      )) %>%
      group_by(Variable, Pred_Type) %>%
      summarize(Importance = mean(Importance)) %>%
      group_by(Pred_Type) %>%
      slice_max(order_by = Importance, n = n_features_pers) %>%
      ungroup() %>%
      mutate(Outcome = outcome_var_sel)
  }
  
  
  if (str_detect(outcome_var_sel, "grade")) {
    # create feature importance and save in list
    postlasso_feature_imp_plot[[outcome_var_sel]] <- func_feature_importance_plot("binary", postlasso_binary_coef, "POST-LASSO", "separate")
    postlasso_binary_coef_all <- rbind(postlasso_binary_coef_all, postlasso_binary_coef)
  } else {
    # store estimates in data frame
    postlasso_binary_coef_all <- rbind(postlasso_binary_coef_all, postlasso_binary_coef)
  }
} # close iteration over outcome variables

# feature importance for big five personality traits only for aggregated outcome
for (outcome_var_sel in c("neuroticism", "openness", "conscientiousness", "extraversion", "agreeableness")) {
  postlasso_feature_imp_plot[[outcome_var_sel]] <- func_feature_importance_plot(
    "binary",
    postlasso_binary_coef_all %>% filter(Outcome == outcome_var_sel),
    "POST-LASSO", "separate")
}

var_names_descr <- c(var_names_descr, postlasso_binary_coef_all$Variable)

# save grades
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_grades_m.png", 
       postlasso_feature_imp_plot$grades$m + ggtitle("POST-LASSO") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_grades_g0.png", 
       postlasso_feature_imp_plot$grades$g0 + ggtitle("POST-LASSO") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_grades_g1.png", 
       postlasso_feature_imp_plot$grades$g1 + ggtitle("POST-LASSO") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

# save personality
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_agreeableness_m.png", 
       postlasso_feature_imp_plot$agreeableness$m + ggtitle("Agreeableness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_extraversion_m.png", 
       postlasso_feature_imp_plot$extraversion$m + ggtitle("Extroversion") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_conscientiousness_m.png", 
       postlasso_feature_imp_plot$conscientiousness$m + ggtitle("Conscientiousness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_openness_m.png", 
       postlasso_feature_imp_plot$openness$m + ggtitle("Openness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_neuroticism_m.png", 
       postlasso_feature_imp_plot$neuroticism$m + ggtitle("Neuroticism") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')


ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_agreeableness_g.png", 
       postlasso_feature_imp_plot$agreeableness$g + xlim(0, 0.11) + ggtitle("Agreeableness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_extraversion_g.png", 
       postlasso_feature_imp_plot$extraversion$g + ggtitle("Extroversion") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_conscientiousness_g.png", 
       postlasso_feature_imp_plot$conscientiousness$g + ggtitle("Conscientiousness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_openness_g.png", 
       postlasso_feature_imp_plot$openness$g + ggtitle("Openness") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')

ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_neuroticism_g.png", 
       postlasso_feature_imp_plot$neuroticism$g + ggtitle("Neuroticism") +
         theme(plot.title = element_text(size = 28), axis.text.x = element_text(size = 26), axis.title = element_text(size = 26)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')


#%%%%%%%%%%%%%%%%%%%%%%#
#### Random Forests ####
#%%%%%%%%%%%%%%%%%%%%%%#

# load all estimation results
rf_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_randomforests_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", "1", "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across folds, repetitions and MICE data frames
rf_binary_imp <- data.frame()
for (i in 1:5) {
  rf_binary_imp <- rbind(
    rf_binary_imp, rf_binary_results_all[[i]]$imp %>% mutate(MICE = i)
  )
}
rf_binary_imp <- rf_binary_imp %>% filter(!Variable %in% c("treatment_period"))

# add variable names
rf_binary_imp <- rf_binary_imp %>% 
  filter(!Variable %in% c("uni_achievement_comp_much_better", "uni_achievement_comp_slightly_better",
                          "uni_commitment_1", "uni_prep_2", "satisfaction_study_2", "satisfaction_study_3",
                          "social_integr_1", "social_integr_1_lag", "personality_selfesteem_1",
                          "motivation_degree_2", "motivation_degree_1")) %>%
  group_by(Pred_Type, Variable) %>%
  summarize(Importance = mean(Importance)) %>%
  slice_max(order_by = Importance, n = n_features) %>%
  ungroup()

# add to variable vector
var_names_descr <- c(var_names_descr, rf_binary_imp$Variable)

# generate feature importance plots
rf_feature_imp_plot <- func_feature_importance_plot("binary", rf_binary_imp, "Random Forests", "separate")

# save
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_m.png", 
       rf_feature_imp_plot$m + ggtitle("RANDOM FORESTS") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g0.png", 
       rf_feature_imp_plot$g0 + ggtitle("RANDOM FORESTS") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g1.png", 
       rf_feature_imp_plot$g1 + ggtitle("RANDOM FORESTS") + theme(
         plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)
       ),
       width = 10, height = 8, dpi = 300, units = "in", device='png')



#%%%%%%%%%%%%%%%#
#### XGBoost ####
#%%%%%%%%%%%%%%%#

# load all estimation results
xgb_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_xgboost_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across folds and repetitions (XGBoost has only one MICE iteration)
xgb_binary_imp <- xgb_binary_results_all[[1]]$imp %>% mutate(MICE = 1)
xgb_binary_imp <- xgb_binary_imp %>% filter(!Variable %in% c("treatment_period"))

# add variable names
xgb_binary_imp <- xgb_binary_imp %>% 
  filter(!Variable %in% c("social_integr_1", "uni_achievement_comp_much_better", "uni_achievement_comp_slightly_better")) %>%
  group_by(Pred_Type, Variable) %>%
  summarize(Importance = mean(Importance)) %>%
  slice_max(order_by = Importance, n = n_features) %>%
  ungroup()


var_names_descr <- c(var_names_descr, xgb_binary_imp$Variable)
xgb_feature_imp_plot <- func_feature_importance_plot("binary", xgb_binary_imp, "XGBoost", "separate")
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_m.png", 
       xgb_feature_imp_plot$m + ggtitle("XGBOOST") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g0.png", 
       xgb_feature_imp_plot$g0 + ggtitle("XGBOOST") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g1.png", 
       xgb_feature_imp_plot$g1 + xlim(0, 0.26) + ggtitle("XGBOOST") + theme(plot.title = element_text(size = 24), axis.text.x = element_text(size = 22), axis.title = element_text(size = 22)),
       width = 10, height = 8, dpi = 300, units = "in", device='png')



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Lasso ####
#+++++++++++++#

# load all estimation results
lasso_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_lasso_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across MICE data frames
lasso_multi_coef <- data.frame()
for (i in 1:5) {
  lasso_multi_coef <- rbind(
    lasso_multi_coef, lasso_multi_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
lasso_multi_coef <- lasso_multi_coef %>% filter(term != "(Intercept)")

lasso_multi_coef <- lasso_multi_coef %>%
  group_by(model, term) %>%
  summarize(estimate = mean(estimate)) %>%
  ungroup() %>%
  rename(Pred_Type = model, Variable = term, Importance = estimate) %>%
  mutate(Pred_Type = case_when(
    Pred_Type == "m1" ~ "Treatment 1 Prediction",
    Pred_Type == "m2" ~ "Treatment 2 Prediction", 
    Pred_Type == "m3" ~ "Treatment 3 Prediction", 
    Pred_Type == "g1" ~ "Outcome 1 Prediction",
    Pred_Type == "g2" ~ "Outcome 2 Prediction", 
    Pred_Type == "g3" ~ "Outcome 3 Prediction", 
    TRUE ~ "Treatment Prediction"
  )) %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features_multi)


# var_names_descr <- c(var_names_descr, lasso_multi_coef$Variable)


lasso_multi_feature_imp_plot <- func_feature_importance_plot("multi", lasso_multi_coef, "LASSO", "separate")
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance.png",
  ggarrange(
    lasso_multi_feature_imp_plot$m1 + ggtitle("Weekly Sport Participation") + xlab("") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    lasso_multi_feature_imp_plot$m2 + ggtitle("Monthly Sport Participation") + xlab("") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    lasso_multi_feature_imp_plot$m3 + ggtitle("No Sport Participation") + xlab("") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    lasso_multi_feature_imp_plot$g1 + ggtitle("Outcome for Weekly Sport Participation") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    lasso_multi_feature_imp_plot$g2 + ggtitle("Outcome for Monthly Sport Participation") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    lasso_multi_feature_imp_plot$g3 + ggtitle("Outcome for No Sport Participation") + theme(
      plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
    ),
    ncol = 3, nrow = 2
  ), width = 20, height = 12, dpi = 300, units = "in", device='png'
)


#### Post-Lasso ####
#++++++++++++++++++#

# load all estimation results
postlasso_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_postlasso_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across MICE data frames
postlasso_multi_coef <- data.frame()
for (i in 1:5) {
  postlasso_multi_coef <- rbind(
    postlasso_multi_coef, postlasso_multi_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
postlasso_multi_coef <- postlasso_multi_coef %>% 
  filter(!term %in% c("(Intercept)", "uni_achievement_comp_much_worse", 
                      "interest_music_play", "uni_achievement_expect_rather"))

postlasso_multi_coef <- postlasso_multi_coef %>%
  group_by(model, term) %>%
  summarize(estimate = mean(estimate)) %>%
  ungroup() %>%
  rename(
    Pred_Type = model, Variable = term, Importance = estimate
  ) %>%
  mutate(Pred_Type = case_when(
    Pred_Type == "m1" ~ "Treatment 1 Prediction",
    Pred_Type == "m2" ~ "Treatment 2 Prediction", 
    Pred_Type == "m3" ~ "Treatment 3 Prediction", 
    Pred_Type == "g1" ~ "Outcome 1 Prediction",
    Pred_Type == "g2" ~ "Outcome 2 Prediction", 
    Pred_Type == "g3" ~ "Outcome 3 Prediction", 
    TRUE ~ "Treatment Prediction"
  )) %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features_multi) %>%
  ungroup()

var_names_descr <- c(var_names_descr, postlasso_multi_coef$Variable)

postlasso_multi_feature_imp_plot <- func_feature_importance_plot(
  "multi", postlasso_multi_coef, "POST-LASSO", "separate")
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_m1.png", 
       postlasso_multi_feature_imp_plot$m1 + ggtitle("Weekly Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
         ), width = 10, height = 8, dpi = 300)
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_m2.png", 
       postlasso_multi_feature_imp_plot$m2 + ggtitle("Monthly Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
       ), width = 10, height = 8, dpi = 300)
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_m3.png", 
       postlasso_multi_feature_imp_plot$m3 + ggtitle("No Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
       ), width = 10, height = 8, dpi = 300)
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_g1.png", 
       postlasso_multi_feature_imp_plot$g1 + ggtitle("Outcome for Weekly Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
       ), width = 10, height = 8, dpi = 300)
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_g2.png", 
       postlasso_multi_feature_imp_plot$g2 + ggtitle("Outcome for Monthly Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
       ), width = 10, height = 8, dpi = 300)
ggsave("Output/DML/Feature_Importance/postlasso_multi_feature_importance_g3.png", 
       postlasso_multi_feature_imp_plot$g3 + ggtitle("Outcome for No Sport Participation") + theme(
         plot.title = element_text(size = 22), axis.text.x = element_text(size = 20), 
         axis.text.y = element_text(size = 16), axis.title = element_text(size = 20)
       ), width = 10, height = 8, dpi = 300)



#### XGBoost ####
#+++++++++++++++#

# load all estimation results
xgb_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_xgboost_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))


# extract coefficients across MICE data frames
xgb_multi_imp <- xgb_multi_results_all[[1]]$imp %>% mutate(MICE = 1)
xgb_multi_imp <- xgb_multi_imp %>% filter(Variable != "(Intercept)")

# aggregate
xgb_multi_imp <- xgb_multi_imp %>%
  group_by(Variable, Pred_Type) %>%
  summarize(Importance = mean(Importance)) %>%
  ungroup() %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features_multi)


# generate plot
xgb_multi_feature_imp_plot <- func_feature_importance_plot("multi", xgb_multi_imp, "XGBoost", "separate")
ggsave("Output/DML/Feature_Importance/xgboost_multi_feature_importance.png",
       ggarrange(
         xgb_multi_feature_imp_plot$m1 + ggtitle("Weekly Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         xgb_multi_feature_imp_plot$m2 + ggtitle("Monthly Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         xgb_multi_feature_imp_plot$m3 + ggtitle("No Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         xgb_multi_feature_imp_plot$g1 + ggtitle("Outcome for Weekly Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         xgb_multi_feature_imp_plot$g2 + ggtitle("Outcome for Monthly Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         xgb_multi_feature_imp_plot$g3 + ggtitle("Outcome for No Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         ncol = 3, nrow = 2
       ), width = 20, height = 12, dpi = 300, units = "in", device='png'
)


#### Random Forests ####
#++++++++++++++++++++++#


# load all estimation results
rf_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_randomforests_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", 1, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract coefficients across MICE data frames
rf_multi_imp <- data.frame()
for (i in 1:5) {
  rf_multi_imp <- rbind(
    rf_multi_imp, rf_multi_results_all[[i]]$imp %>% mutate(MICE = i)
  )
}
rf_multi_imp <- rf_multi_imp %>% filter(Variable != "(Intercept)")

# aggregate
rf_multi_imp <- rf_multi_imp %>%
  group_by(Variable, Pred_Type) %>%
  summarize(Importance = mean(Importance)) %>%
  ungroup() %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features_multi)


rf_multi_feature_imp_plot <- func_feature_importance_plot("multi", rf_multi_imp, "Random Forests", "separate")
ggsave("Output/DML/Feature_Importance/rf_multi_feature_importance.png",
       ggarrange(
         rf_multi_feature_imp_plot$m1 + ggtitle("Weekly Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         rf_multi_feature_imp_plot$m2 + ggtitle("Monthly Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         rf_multi_feature_imp_plot$m3 + ggtitle("No Sport Participation") + xlab("") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         rf_multi_feature_imp_plot$g1 + ggtitle("Outcome for Weekly Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         rf_multi_feature_imp_plot$g2 + ggtitle("Outcome for Monthly Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         rf_multi_feature_imp_plot$g3 + ggtitle("Outcome for No Sport Participation") + theme(
           plot.title = element_text(size = 12), axis.text.x = element_text(size = 12), 
           axis.text.y = element_text(size = 10), axis.title = element_text(size = 12)
         ),
         ncol = 3, nrow = 2
       ), width = 20, height = 12, dpi = 300, units = "in", device='png'
)


#%%%%%%%%%%%%%%%%%%%%%%%%#
#### TRAIN NEW MODELS ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# only done as robustness check for GPA sample in the binary treatment setting


#### BINARY ####
#++++++++++++++#

#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#

# load data (for random forests and xgboost feature importance)
outcome_var <- "outcome_grade"

for (mice_data_sel in 1:5) { # iterate over mice data sets
  # load data
  if (str_detect(outcome_var, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
  }
  
  data_dml <- readRDS(load_data)
  data_dml <- data_dml %>% mutate(treatment_sport = as.factor(treatment_sport))
  
  # drop lags if desired by user
  if (model_controls_lag == "no_lags") {
    # drop all lags
    data_dml <- data_dml %>% 
      dplyr::select(-c(contains("_lag"))) %>% 
      as.data.frame()
  } else if (model_controls_lag == "no_treatment_outcome_lags") {
    # drop only treatment and outcome lags
    # here differentiate between GPA and personality outcome
    if (str_detect(outcome_var, "grade")) {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with("outcome") & contains("_lag"))) %>%
        as.data.frame()
    } else {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with(str_remove(outcome_var, "outcome_")) & contains("_lag"))) %>%
        as.data.frame()
    }
  } else {
    # keep all lags
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # change name
  if (str_detect(outcome_var, "grade")) {
    assign(as.character(paste0("data_dml_", str_remove(outcome_var, "outcome_"), "_mice", mice_data_sel)), data_dml)
  } else {
    assign(as.character(paste0("data_dml_", str_remove(outcome_var, "outcome_bigfive_"), "_mice", mice_data_sel)), data_dml)
  }
  
}


#%%%%%%%%%#
## LASSO ##
#%%%%%%%%%#


# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
lasso_binary_param <- data.frame()
for (i in 1:5) {
  lasso_binary_param <- rbind(
    lasso_binary_param, 
    left_join(
      # parameters
      lasso_binary_results_all[[i]]$param,
      # error metrics
      lasso_binary_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, AUC_m, starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
## for classification: highest AUC
lasso_binary_best_param_m <- 
  lasso_binary_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m == max(AUC_m)) %>% 
  dplyr::select(MICE, starts_with("m"))
## for regression: smallest RMSE
lasso_binary_best_param_g0 <- 
  lasso_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g0 == min(RMSE_g0)) %>% 
  dplyr::select(MICE, starts_with("g0"))
lasso_binary_best_param_g1 <- 
  lasso_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))

# retrain lasso model
set.seed(1234)
lasso_binary_coef_check <- data.frame()
for (mice_data_sel in 1:5) {
  
  # generate name of data frame used for training
  df_sel_name <- paste0("data_dml_grade_mice", mice_data_sel)
  
  # extract control variables
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", "treatment_sport", "group")) %>% colnames()
  
  # retrain ML models with best penalty term across folds and repetitions
  penalty_m <- lasso_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m)
  penalty_g0 <- lasso_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0)
  penalty_g1 <- lasso_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1)
  
  # specify model with chosen hyperparameter
  lasso_spec_m <- 
    logistic_reg(penalty = {{penalty_m}}, mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("classification")  
  
  lasso_spec_g0 <- 
    linear_reg(penalty = {{penalty_g0}}, mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("regression")  
  
  lasso_spec_g1 <- 
    linear_reg(penalty = {{penalty_g1}}, mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("regression")  
  
  # generate recipes
  lasso_recipe_m <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    update_role("treatment_sport", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  lasso_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport == 1) %>%
    recipe(.) %>%
    update_role("outcome_grade", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  lasso_recipe_g0 <- 
    get(df_sel_name) %>% filter(treatment_sport == 0) %>%
    recipe(.) %>%
    update_role("outcome_grade", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  
  # generate workflows
  lasso_workflow_m <- 
    workflow() %>%
    add_model(lasso_spec_m) %>%
    add_recipe(lasso_recipe_m)
  
  lasso_workflow_g1 <- 
    workflow() %>%
    add_model(lasso_spec_g1) %>%
    add_recipe(lasso_recipe_g1)
  
  lasso_workflow_g0 <- 
    workflow() %>%
    add_model(lasso_spec_g0) %>%
    add_recipe(lasso_recipe_g0)
  
  # fit models
  lasso_fit_m <- 
    lasso_workflow_m %>%
    fit(get(df_sel_name))
  
  lasso_fit_g0 <- 
    lasso_workflow_g0 %>%
    fit(get(df_sel_name))
  
  lasso_fit_g1 <- 
    lasso_workflow_g1 %>%
    fit(get(df_sel_name))
  
  
  # extract non-zero coefficients
  lasso_coef_m <- tidy(lasso_fit_m) %>% as.data.frame()
  lasso_coef_m <- lasso_coef_m %>% filter(estimate > 0) %>% mutate(model = "m") %>% dplyr::select(-penalty)
  
  lasso_coef_g0 <- tidy(lasso_fit_g0) %>% as.data.frame()
  lasso_coef_g0 <- lasso_coef_g0 %>% filter(estimate > 0) %>% mutate(model = "g0") %>% dplyr::select(-penalty)
  
  lasso_coef_g1 <- tidy(lasso_fit_g1) %>% as.data.frame()
  lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% dplyr::select(-penalty)
  
  lasso_binary_coef_check <- rbind(lasso_coef_m, lasso_coef_g0) %>% rbind(lasso_coef_g1)
}

lasso_binary_coef_check <- lasso_binary_coef_check %>% filter(!term %in% c("(Intercept)", "treatment_period"))

lasso_binary_coef_check <- lasso_binary_coef_check %>%
  group_by(model, term) %>%
  summarize(estimate = mean(estimate)) %>%
  ungroup() %>%
  rename(
    Pred_Type = model, Variable = term, Importance = estimate
  ) %>%
  mutate(Pred_Type = case_when(
    Pred_Type == "g0" ~ "Outcome 0 Prediction",
    Pred_Type == "g1" ~ "Outcome 1 Prediction",
    TRUE ~ "Treatment Prediction"
  )) %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features) %>%
  ungroup()


lasso_feature_imp_plot_check <- func_feature_importance_plot("binary", lasso_binary_coef_check, "LASSO", "separate")
ggarrange(lasso_feature_imp_plot$m,lasso_feature_imp_plot_check$m, ncol = 2)
ggarrange(lasso_feature_imp_plot$g0,lasso_feature_imp_plot_check$g0, ncol = 2)
ggarrange(lasso_feature_imp_plot$g1,lasso_feature_imp_plot_check$g1, ncol = 2)



## XGBoost ##
#+++++++++++#

# load all estimation results
xgb_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_xgboost_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
xgb_binary_param <- data.frame()
for (i in 1:length(xgb_binary_results_all)) {
  xgb_binary_param <- rbind(
    xgb_binary_param, 
    left_join(
      # parameters
      xgb_binary_results_all[[i]]$param,
      # error metrics
      xgb_binary_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, AUC_m, starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
## for classification: highest AUC
xgb_binary_best_param_m <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m == max(AUC_m)) %>% 
  dplyr::select(MICE, starts_with("m"))
## for regression: smallest RMSE
xgb_binary_best_param_g0 <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g0 == min(RMSE_g0)) %>% 
  dplyr::select(MICE, starts_with("g0"))
xgb_binary_best_param_g1 <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))


# retrain models and calculate importance score across MICE data frames
set.seed(1234)
xgb_scores_all <- data.frame()
for (mice_data_sel in 1:length(xgb_binary_results_all)) {
  
  # retrain ML models
  tree_depth_m <- xgb_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_tree_depth)
  trees_m <- xgb_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_trees)
  learn_rate_m <- xgb_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_learn_rate)
  mtry_m <- xgb_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_mtry)
  min_n_m <- xgb_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_min_n)
  
  xgb_spec_m <- 
    boost_tree(tree_depth = {{tree_depth_m}}, trees = {{trees_m}}, 
               learn_rate = {{learn_rate_m}}, mtry = {{mtry_m}}, min_n = {{min_n_m}}
    ) %>%
    set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
    set_mode("classification")
  
  
  tree_depth_g0 <- xgb_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_tree_depth)
  trees_g0 <- xgb_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_trees)
  learn_rate_g0 <- xgb_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_learn_rate)
  mtry_g0 <- xgb_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_mtry)
  min_n_g0 <- xgb_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_min_n)
  
  xgb_spec_g0 <- 
    boost_tree(tree_depth = {{tree_depth_g0}}, trees = {{trees_g0}}, 
               learn_rate = {{learn_rate_g0}}, mtry = {{mtry_g0}}, min_n = {{min_n_g0}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  
  tree_depth_g1 <- xgb_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_tree_depth)
  trees_g1 <- xgb_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  learn_rate_g1 <- xgb_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_learn_rate)
  mtry_g1 <- xgb_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g1 <- xgb_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  xgb_spec_g1 <- 
    boost_tree(tree_depth = {{tree_depth_g1}}, trees = {{trees_g1}}, 
               learn_rate = {{learn_rate_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_grade_mice", mice_data_sel)
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", "treatment_sport", "group")) %>% colnames()
  
  xgb_recipe_m <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("treatment_sport", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_m <- 
    workflow() %>%
    add_model(xgb_spec_m) %>%
    add_recipe(xgb_recipe_m)
  
  xgb_fit_m <- 
    xgb_workflow_m %>%
    fit(get(df_sel_name))
  
  
  xgb_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport == 1) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g1 <- 
    workflow() %>%
    add_model(xgb_spec_g1) %>%
    add_recipe(xgb_recipe_g1)
  
  xgb_fit_g1 <- 
    xgb_workflow_g1 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 1))
  
  xgb_recipe_g0 <- 
    get(df_sel_name) %>% filter(treatment_sport == 0) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g0 <- 
    workflow() %>%
    add_model(xgb_spec_g0) %>%
    add_recipe(xgb_recipe_g0)
  
  xgb_fit_g0 <- 
    xgb_workflow_g0 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 0))
  
  
  # calculate feature importance score
  xgb_scores <- rbind(
    func_feature_importance_score(xgb_fit_m, n_features) %>% mutate("Pred_Type" = "Treatment Prediction"),
    func_feature_importance_score(xgb_fit_g0, n_features) %>% mutate("Pred_Type" = "Outcome 0 Prediction"),
    func_feature_importance_score(xgb_fit_g1, n_features) %>% mutate("Pred_Type" = "Outcome 1 Prediction")
  ) %>% mutate(MICE = mice_data_sel)
  
  xgb_scores_all <- rbind(xgb_scores_all, xgb_scores)
}

xgb_scores_all <- 
  xgb_scores_all %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE) %>%
  arrange(desc(Importance)) %>%
  group_by(Pred_Type) %>%
  top_n(n_features, Importance) %>%
  ungroup()


xgb_feature_imp_plot <- func_feature_importance_plot("binary", xgb_scores_all, "XGBoost", "separate")


## Random Forests ##
#++++++++++++++++++#

# load all estimation results
rf_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_randomforests_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", 1, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
rf_binary_param <- data.frame()
for (i in 1:5) {
  rf_binary_param <- rbind(
    rf_binary_param, 
    left_join(
      # parameters
      rf_binary_results_all[[i]]$param,
      # error metrics
      rf_binary_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, AUC_m, starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
## for classification: highest AUC
rf_binary_best_param_m <- 
  rf_binary_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m == max(AUC_m)) %>% 
  dplyr::select(MICE, starts_with("m"))
## for regression: smallest RMSE
rf_binary_best_param_g0 <- 
  rf_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g0 == min(RMSE_g0)) %>% 
  dplyr::select(MICE, starts_with("g0"))
rf_binary_best_param_g1 <- 
  rf_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))


# retrain models and calculate importance score across MICE data frames
set.seed(1234)
rf_scores_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  # retrain ML models
  trees_m <- rf_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_trees)
  mtry_m <- rf_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_mtry)
  min_n_m <- rf_binary_best_param_m %>% filter(MICE == mice_data_sel) %>% pull(m_min_n)
  
  rf_spec_m <- 
    rand_forest(trees = {{trees_m}}, mtry = {{mtry_m}}, min_n = {{min_n_m}}) %>% 
    set_engine("randomForest") %>% 
    set_mode("classification")
  
  trees_g0 <- rf_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_trees)
  mtry_g0 <- rf_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_mtry)
  min_n_g0 <- rf_binary_best_param_g0 %>% filter(MICE == mice_data_sel) %>% pull(g0_min_n)
  
  rf_spec_g0 <- 
    rand_forest(trees = {{trees_g0}}, mtry = {{mtry_g0}}, min_n = {{min_n_g0}}) %>% 
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  
  trees_g1 <- rf_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  mtry_g1 <- rf_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g1 <- rf_binary_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  rf_spec_g1 <- 
    rand_forest(trees = {{trees_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}) %>% 
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_grade_mice", mice_data_sel)
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", "treatment_sport", "group")) %>% colnames()
  
  rf_recipe_m <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("treatment_sport", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_m <- 
    workflow() %>%
    add_model(rf_spec_m) %>%
    add_recipe(rf_recipe_m)
  
  rf_fit_m <- 
    rf_workflow_m %>%
    fit(get(df_sel_name))
  
  
  rf_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport == 1) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_g1 <- 
    workflow() %>%
    add_model(rf_spec_g1) %>%
    add_recipe(rf_recipe_g1)
  
  rf_fit_g1 <- 
    rf_workflow_g1 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 1))
  
  rf_recipe_g0 <- 
    get(df_sel_name) %>% filter(treatment_sport == 0) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_g0 <- 
    workflow() %>%
    add_model(rf_spec_g0) %>%
    add_recipe(rf_recipe_g0)
  
  rf_fit_g0 <- 
    rf_workflow_g0 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 0))
  
  
  # calculate feature importance score
  rf_scores <- rbind(
    func_feature_importance_score(rf_fit_m, n_features) %>% mutate("Pred_Type" = "Treatment Prediction"),
    func_feature_importance_score(rf_fit_g0, n_features) %>% mutate("Pred_Type" = "Outcome 0 Prediction"),
    func_feature_importance_score(rf_fit_g1, n_features) %>% mutate("Pred_Type" = "Outcome 1 Prediction")
  ) %>% mutate(MICE = mice_data_sel)
  
  rf_scores_all <- rbind(rf_scores_all, rf_scores)
}

# saveRDS(rf_scores_all, file = "Output/DML/Feature_Importance/rf_scores.rds")
# rf_scores_all <- readRDS("Output/DML/Feature_Importance/rf_scores.rds")

rf_scores_all_final <- 
  rf_scores_all %>%
  mutate(Variable = str_replace_all(Variable, "_[0-9]", "")) %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE) %>%
  arrange(desc(Importance)) %>%
  group_by(Pred_Type) %>%
  top_n(n_features, Importance) %>%
  ungroup()


rf_feature_imp_plot <- func_feature_importance_plot("binary", rf_scores_all_final, "Random Forests", "separate")



#### MULTI ####
#+++++++++++++#


#%%%%%%%%%%%#
## XGBoost ##
#%%%%%%%%%%%#

# load all estimation results
xgb_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_xgboost_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, cov_balance_save, ".rds"))

# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
xgb_multi_param <- data.frame()
for (i in 1:5) {
  xgb_multi_param <- rbind(
    xgb_multi_param, 
    left_join(
      # parameters
      xgb_multi_results_all[[i]]$param,
      # error metrics
      xgb_multi_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, starts_with("AUC"), starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
## for classification: highest AUC
xgb_multi_best_param_m1 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m1 == max(AUC_m1)) %>% 
  dplyr::select(MICE, starts_with("m1"))
xgb_multi_best_param_m2 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m2 == max(AUC_m2)) %>% 
  dplyr::select(MICE, starts_with("m2"))
xgb_multi_best_param_m3 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m3 == max(AUC_m3)) %>% 
  dplyr::select(MICE, starts_with("m3"))
## for regression: smallest RMSE
xgb_multi_best_param_g1 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))
xgb_multi_best_param_g2 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g2 == min(RMSE_g2)) %>% 
  dplyr::select(MICE, starts_with("g2"))
xgb_multi_best_param_g3 <- 
  xgb_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g3 == min(RMSE_g3)) %>% 
  dplyr::select(MICE, starts_with("g3"))


# retrain models and clculate importance score across data frames
xgb_scores_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_multi_grade_mice", mice_data_sel)
  
  # train models
  tree_depth_m1 <- xgb_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_tree_depth)
  trees_m1 <- xgb_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_trees)
  learn_rate_m1 <- xgb_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_learn_rate)
  mtry_m1 <- xgb_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_mtry)
  min_n_m1 <- xgb_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_min_n)
  
  xgb_spec_m1 <- 
    boost_tree(tree_depth = {{tree_depth_m1}}, trees = {{trees_m1}}, 
               learn_rate = {{learn_rate_m1}}, mtry = {{mtry_m1}}, min_n = {{min_n_m1}}
    ) %>%
    set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
    set_mode("classification")
  
  tree_depth_m2 <- xgb_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_tree_depth)
  trees_m2 <- xgb_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_trees)
  learn_rate_m2 <- xgb_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_learn_rate)
  mtry_m2 <- xgb_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_mtry)
  min_n_m2 <- xgb_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_min_n)
  
  xgb_spec_m2 <- 
    boost_tree(tree_depth = {{tree_depth_m2}}, trees = {{trees_m2}}, 
               learn_rate = {{learn_rate_m2}}, mtry = {{mtry_m2}}, min_n = {{min_n_m2}}
    ) %>%
    set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
    set_mode("classification")
  
  tree_depth_m3 <- xgb_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_tree_depth)
  trees_m3 <- xgb_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_trees)
  learn_rate_m3 <- xgb_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_learn_rate)
  mtry_m3 <- xgb_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_mtry)
  min_n_m3 <- xgb_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_min_n)
  
  xgb_spec_m3 <- 
    boost_tree(tree_depth = {{tree_depth_m3}}, trees = {{trees_m3}}, 
               learn_rate = {{learn_rate_m3}}, mtry = {{mtry_m3}}, min_n = {{min_n_m3}}
    ) %>%
    set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
    set_mode("classification")
  
  
  tree_depth_g1 <- xgb_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_tree_depth)
  trees_g1 <- xgb_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  learn_rate_g1 <- xgb_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_learn_rate)
  mtry_g1 <- xgb_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g1 <- xgb_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  xgb_spec_g1 <- 
    boost_tree(tree_depth = {{tree_depth_g1}}, trees = {{trees_g1}}, 
               learn_rate = {{learn_rate_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  tree_depth_g2 <- xgb_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_tree_depth)
  trees_g2 <- xgb_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_trees)
  learn_rate_g2 <- xgb_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_learn_rate)
  mtry_g2 <- xgb_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_mtry)
  min_n_g2 <- xgb_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_min_n)
  
  xgb_spec_g2 <- 
    boost_tree(tree_depth = {{tree_depth_g2}}, trees = {{trees_g2}}, 
               learn_rate = {{learn_rate_g2}}, mtry = {{mtry_g2}}, min_n = {{min_n_g2}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  tree_depth_g3 <- xgb_multi_best_param_g3 %>% filter(MICE == mice_data_sel) %>% pull(g3_tree_depth)
  trees_g3 <- xgb_multi_best_param_g3 %>% filter(MICE == mice_data_sel) %>% pull(g3_trees)
  learn_rate_g3 <- xgb_multi_best_param_g3 %>% filter(MICE == mice_data_sel) %>% pull(g3_learn_rate)
  mtry_g3 <- xgb_multi_best_param_g3 %>% filter(MICE == mice_data_sel) %>% pull(g3_mtry)
  min_n_g3 <- xgb_multi_best_param_g3 %>% filter(MICE == mice_data_sel) %>% pull(g3_min_n)
  
  xgb_spec_g3 <- 
    boost_tree(tree_depth = {{tree_depth_g3}}, trees = {{trees_g3}}, 
               learn_rate = {{learn_rate_g3}}, mtry = {{mtry_g3}}, min_n = {{min_n_g3}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", starts_with("treatment_sport_freq"), "group")) %>% colnames()
  
  xgb_recipe_m1 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual participates at least weekly in sports
    update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor") # controls
  xgb_recipe_m2 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual participates monthly or less frequently in sports
    update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  xgb_recipe_m3 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual does not participate in sports
    update_role("treatment_sport_freq_never", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_m1 <- 
    workflow() %>%
    add_model(xgb_spec_m1) %>%
    add_recipe(xgb_recipe_m1)
  xgb_workflow_m2 <- 
    workflow() %>%
    add_model(xgb_spec_m2) %>%
    add_recipe(xgb_recipe_m2)
  xgb_workflow_m3 <- 
    workflow() %>%
    add_model(xgb_spec_m3) %>%
    add_recipe(xgb_recipe_m3)
  
  xgb_fit_m1 <- 
    xgb_workflow_m1 %>%
    fit(get(df_sel_name))
  xgb_fit_m2 <- 
    xgb_workflow_m2 %>%
    fit(get(df_sel_name))
  xgb_fit_m3 <- 
    xgb_workflow_m3 %>%
    fit(get(df_sel_name))
  
  xgb_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 1) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g1 <- 
    workflow() %>%
    add_model(xgb_spec_g1) %>%
    add_recipe(xgb_recipe_g1)
  
  xgb_fit_g1 <- 
    xgb_workflow_g1 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 1))
  
  
  xgb_recipe_g2 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 2) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g2 <- 
    workflow() %>%
    add_model(xgb_spec_g2) %>%
    add_recipe(xgb_recipe_g2)
  
  xgb_fit_g2 <- 
    xgb_workflow_g2 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 2))
  
  
  xgb_recipe_g3 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 3) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g3 <- 
    workflow() %>%
    add_model(xgb_spec_g3) %>%
    add_recipe(xgb_recipe_g3)
  
  xgb_fit_g3 <- 
    xgb_workflow_g3 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 3))
  
  
  # calculate feature importance score
  xgb_scores <- rbind(
    func_feature_importance_score(xgb_fit_m1, n_features) %>% mutate("Pred_Type" = "Treatment 1 Prediction"),
    func_feature_importance_score(xgb_fit_m2, n_features) %>% mutate("Pred_Type" = "Treatment 2 Prediction"),
    func_feature_importance_score(xgb_fit_m3, n_features) %>% mutate("Pred_Type" = "Treatment 3 Prediction"),
    func_feature_importance_score(xgb_fit_g1, n_features) %>% mutate("Pred_Type" = "Outcome 1 Prediction"),
    func_feature_importance_score(xgb_fit_g2, n_features) %>% mutate("Pred_Type" = "Outcome 2 Prediction"),
    func_feature_importance_score(xgb_fit_g3, n_features) %>% mutate("Pred_Type" = "Outcome 3 Prediction")
  ) %>% mutate(MICE = mice_data_sel)
  
  xgb_scores_all <- rbind(xgb_scores_all, xgb_scores)
}

xgb_scores_all <- 
  xgb_scores_all %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE)


# generate plot
xgb_multi_feature_imp_plot <- func_feature_importance_plot("multi", xgb_scores_all, "XGBoost", "separate")




#%%%%%%%%%%%%%%%%%%#
## Random Forests ##
#%%%%%%%%%%%%%%%%%%#

set.seed(1234)

# load all estimation results
rf_multi_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/multi_grades_randomforests_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", 1, "_Rep", model_s_rep, cov_balance_save, ".rds"))


# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
rf_multi_param <- data.frame()
for (i in 1:5) {
  rf_multi_param <- rbind(
    rf_multi_param, 
    left_join(
      # parameters
      rf_multi_results_all[[i]]$param,
      # error metrics
      rf_multi_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, starts_with("AUC"), starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
## for classification: highest AUC
rf_multi_best_param_m1 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m1 == max(AUC_m1)) %>% 
  dplyr::select(MICE, starts_with("m1"))
rf_multi_best_param_m2 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m2 == max(AUC_m2)) %>% 
  dplyr::select(MICE, starts_with("m2"))
rf_multi_best_param_m3 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m3 == max(AUC_m3)) %>% 
  dplyr::select(MICE, starts_with("m3"))
## for regression: smallest RMSE
rf_multi_best_param_g1 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))
rf_multi_best_param_g2 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g2 == min(RMSE_g2)) %>% 
  dplyr::select(MICE, starts_with("g2"))
rf_multi_best_param_g3 <- 
  rf_multi_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g3 == min(RMSE_g3)) %>% 
  dplyr::select(MICE, starts_with("g3"))


# retrain models and clculate importance score across data frames
rf_scores_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  print(paste("Data Set:", mice_data_sel))
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_multi_grade_mice", mice_data_sel)
  
  # train models
  trees_m1 <- rf_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_trees)
  mtry_m1 <- rf_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_mtry)
  min_n_m1 <- rf_multi_best_param_m1 %>% filter(MICE == mice_data_sel) %>% pull(m1_min_n)
  
  rf_spec_m1 <- 
    rand_forest(trees = {{trees_m1}}, mtry = {{mtry_m1}}, min_n = {{min_n_m1}}) %>%
    set_engine("randomForest") %>% 
    set_mode("classification")
  
  trees_m2 <- rf_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_trees)
  mtry_m2 <- rf_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_mtry)
  min_n_m2 <- rf_multi_best_param_m2 %>% filter(MICE == mice_data_sel) %>% pull(m2_min_n)
  
  rf_spec_m2 <- 
    rand_forest(trees = {{trees_m2}}, mtry = {{mtry_m2}}, min_n = {{min_n_m2}}) %>%
    set_engine("randomForest") %>% 
    set_mode("classification")
  
  trees_m3 <- rf_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_trees)
  mtry_m3 <- rf_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_mtry)
  min_n_m3 <- rf_multi_best_param_m3 %>% filter(MICE == mice_data_sel) %>% pull(m3_min_n)
  
  rf_spec_m3 <- 
    rand_forest(trees = {{trees_m3}}, mtry = {{mtry_m3}}, min_n = {{min_n_m3}}) %>%
    set_engine("randomForest") %>% 
    set_mode("classification")
  
  
  trees_g1 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  mtry_g1 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g1 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  rf_spec_g1 <- 
    rand_forest(trees = {{trees_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  trees_g2 <- rf_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_trees)
  mtry_g2 <- rf_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_mtry)
  min_n_g2 <- rf_multi_best_param_g2 %>% filter(MICE == mice_data_sel) %>% pull(g2_min_n)
  
  rf_spec_g2 <- 
    rand_forest(trees = {{trees_g2}}, mtry = {{mtry_g2}}, min_n = {{min_n_g2}}) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  trees_g3 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  mtry_g3 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g3 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  rf_spec_g3 <- 
    rand_forest(trees = {{trees_g3}}, mtry = {{mtry_g3}}, min_n = {{min_n_g3}}) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", starts_with("treatment_sport_freq"), "group")) %>% colnames()
  
  rf_recipe_m1 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual participates at least weekly in sports
    update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor") # controls
  rf_recipe_m2 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual participates monthly or less frequently in sports
    update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  rf_recipe_m3 <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # outcome: indicator if individual does not participate in sports
    update_role("treatment_sport_freq_never", new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_m1 <- 
    workflow() %>%
    add_model(rf_spec_m1) %>%
    add_recipe(rf_recipe_m1)
  rf_workflow_m2 <- 
    workflow() %>%
    add_model(rf_spec_m2) %>%
    add_recipe(rf_recipe_m2)
  rf_workflow_m3 <- 
    workflow() %>%
    add_model(rf_spec_m3) %>%
    add_recipe(rf_recipe_m3)
  
  rf_fit_m1 <- 
    rf_workflow_m1 %>%
    fit(get(df_sel_name))
  rf_fit_m2 <- 
    rf_workflow_m2 %>%
    fit(get(df_sel_name))
  rf_fit_m3 <- 
    rf_workflow_m3 %>%
    fit(get(df_sel_name))
  
  rf_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 1) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_g1 <- 
    workflow() %>%
    add_model(rf_spec_g1) %>%
    add_recipe(rf_recipe_g1)
  
  rf_fit_g1 <- 
    rf_workflow_g1 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 1))
  
  
  rf_recipe_g2 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 2) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_g2 <- 
    workflow() %>%
    add_model(rf_spec_g2) %>%
    add_recipe(rf_recipe_g2)
  
  rf_fit_g2 <- 
    rf_workflow_g2 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 2))
  
  
  rf_recipe_g3 <- 
    get(df_sel_name) %>% filter(treatment_sport_freq == 3) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  rf_workflow_g3 <- 
    workflow() %>%
    add_model(rf_spec_g3) %>%
    add_recipe(rf_recipe_g3)
  
  rf_fit_g3 <- 
    rf_workflow_g3 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport_freq == 3))
  
  
  # calculate feature importance score
  rf_scores <- rbind(
    func_feature_importance_score(rf_fit_m1, n_features) %>% mutate("Pred_Type" = "Treatment 1 Prediction"),
    func_feature_importance_score(rf_fit_m2, n_features) %>% mutate("Pred_Type" = "Treatment 2 Prediction"),
    func_feature_importance_score(rf_fit_m3, n_features) %>% mutate("Pred_Type" = "Treatment 3 Prediction"),
    func_feature_importance_score(rf_fit_g1, n_features) %>% mutate("Pred_Type" = "Outcome 1 Prediction"),
    func_feature_importance_score(rf_fit_g2, n_features) %>% mutate("Pred_Type" = "Outcome 2 Prediction"),
    func_feature_importance_score(rf_fit_g3, n_features) %>% mutate("Pred_Type" = "Outcome 3 Prediction")
  ) %>% mutate(MICE = mice_data_sel)
  
  rf_scores_all <- rbind(rf_scores_all, rf_scores)
}

# saveRDS(rf_scores_all, "Output/DML/Feature_Importance/rf_scores_multi.rds")
# rf_scores_all <- readRDS("Output/DML/Feature_Importance/rf_scores_multi.rds")

rf_scores_multi_final <- 
  rf_scores_all %>%
  mutate(Variable = str_replace_all(Variable, "_[0-9]", "")) %>%
  group_by(Pred_Type, Variable) %>%
  summarize(Importance = mean(Importance)) %>%
  ungroup() %>%
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features_multi)


# generate plot
rf_multi_feature_imp_plot <- func_feature_importance_plot("multi", rf_scores_multi_final, "Random Forests", "separate")