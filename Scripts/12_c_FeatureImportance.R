#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FEATURE IMPORTANCE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, feature importance plots are created for all machine learning
# models and grades as outcome in both the binary and multivalued treatment setting. 
# However, this is only done for the main model (no robustness checks).
# For (post-)LASSO the coefficients were already saved during the DML estimation.
# For xgboost and random forests, final models are trained using the "best" 
# hyperparameter combinations and the full data set. This is done across all five 
# mice data sets. The feature importance scores are then again aggregated by taking the mean.
#+++
# Sources:
# -> https://medium.com/analytics-vidhya/feature-importance-explained-bfc8d874bcf
#+++



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### GRADES: BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

outcome_var <- "outcome_grade"

# load data
for (mice_data_sel in 1:5) {
  # load data
  ## extract extracurricular activity ending
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  ## extract outcome
  if (str_detect(outcome_var, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
  ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
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
  assign(as.character(paste0("data_dml_mice", mice_data_sel)), data_dml)
}


#### LASSO ####
#+++++++++++++#

# load all estimation results
lasso_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_lasso_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, ".rds"))

# extract coefficients across MICE data frames
lasso_binary_coef <- data.frame()
for (i in 1:5) {
  lasso_binary_coef <- rbind(
    lasso_binary_coef, lasso_binary_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
lasso_binary_coef <- lasso_binary_coef %>% filter(term != "(Intercept)")

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
  group_by(Pred_Type) %>%
  slice_max(order_by = Importance, n = n_features) %>%
  ungroup()
  

lasso_feature_imp_plot <- func_feature_importance_plot("binary", lasso_binary_coef, "LASSO", "separate")
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_m.png", lasso_feature_imp_plot$m + ggtitle("LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_g0.png", lasso_feature_imp_plot$g0 + ggtitle("LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/lasso_binary_feature_importance_g1.png", lasso_feature_imp_plot$g1 + ggtitle("LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')


# new training - almost identical result
# lasso_binary_lambda <- data.frame()
# for (i in 1:5) {
#   lasso_binary_lambda <- rbind(
#     lasso_binary_lambda, left_join(
#       lasso_binary_results_all[[i]]$param, lasso_binary_results_all[[i]]$error,
#       by = c("Repetition", "Fold")
#   ) %>% mutate(MICE = i)
#   )
# }
# 
# lasso_best_param_m <- lasso_binary_lambda %>% 
#   group_by(MICE) %>% 
#   filter(AUC_m == max(AUC_m)) %>% 
#   dplyr::select(MICE, m)
# 
# lasso_coef_m_all <- data.frame()
# for (mice_data_sel in 1:5) {
#   # generate name of data frame
#   df_sel_name <- paste0("data_dml_mice", mice_data_sel)
#   
#   X_controls <- get(df_sel_name) %>% 
#     dplyr::select(-c("outcome_grade", "treatment_sport", "group")) %>% colnames()
#   
#   
#   lasso_recipe_m <- 
#     get(df_sel_name) %>%
#     recipe(.) %>%
#     # define outcome
#     update_role("treatment_sport", new_role = "outcome") %>%
#     # define predictors
#     update_role(all_of(X_controls), new_role = "predictor")
#   
#   lasso_best_param_m_mice <- lasso_best_param_m %>%
#     filter(MICE == mice_data_sel) %>% pull(m)
#   
#   lasso_spec_m <- 
#     logistic_reg(penalty = {{lasso_best_param_m_mice}}, mixture = 1) %>%  
#     set_engine("glmnet") %>%  
#     set_mode("classification") 
#   
#   lasso_workflow_m <- 
#     workflow() %>%
#     add_model(lasso_spec_m) %>%
#     add_recipe(lasso_recipe_m)
#   
#   lasso_fit_m <- 
#     lasso_workflow_m %>%
#     fit(get(df_sel_name))
#   
#   lasso_coef_m <- tidy(lasso_fit_m) %>% as.data.frame()
#   lasso_coef_m <- lasso_coef_m %>% 
#     filter(estimate > 0) %>% 
#     mutate(model = "m", MICE = mice_data_sel) %>% 
#     dplyr::select(-penalty) %>%
#     filter(term != "(Intercept)")
#   lasso_coef_m_all <- rbind(lasso_coef_m_all, lasso_coef_m)
# }
# 
# lasso_coef_m_all <- lasso_coef_m_all %>%
#   group_by(model, term) %>%
#   summarize(estimate = mean(estimate)) %>%
#   ungroup() %>%
#   rename(
#     Pred_Type = model, Variable = term, Importance = estimate
#   ) %>%
#   mutate(Pred_Type = case_when(
#     Pred_Type == "g0" ~ "Outcome 0 Prediction", 
#     Pred_Type == "g1" ~ "Outcome 1 Prediction",
#     TRUE ~ "Treatment Prediction"
#   )) %>%
#   group_by(Pred_Type) %>%
#   slice_max(order_by = Importance, n = n_features)
# 
# 
# lasso_check <- func_feature_importance_plot("binary", lasso_coef_m_all, "LASSO", "separate")
# lasso_check$m
# lasso_feature_imp_plot$m


#### POST-LASSO ####
#++++++++++++++++++#

# for post-lasso same predictors are used in treatment and outcome predictions
# load all estimation results
postlasso_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_postlasso_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, ".rds"))

# extract coefficients across MICE data frames
postlasso_binary_coef <- data.frame()
for (i in 1:5) {
  postlasso_binary_coef <- rbind(
    postlasso_binary_coef, postlasso_binary_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
postlasso_binary_coef <- postlasso_binary_coef %>% filter(term != "(Intercept)")

postlasso_binary_coef <- postlasso_binary_coef %>%
  # group_by(term) %>%
  # summarize(estimate = mean(estimate)) %>%
  # ungroup() %>%
  # rename(
  #   Variable = term, Importance = estimate
  # ) %>%
  # slice_max(order_by = Importance, n = n_features)
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


postlasso_feature_imp_plot <- func_feature_importance_plot("binary", postlasso_binary_coef, "POST-LASSO", "separate")
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_m.png", postlasso_feature_imp_plot$m + ggtitle("POST-LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_g0.png", postlasso_feature_imp_plot$g0 + ggtitle("POST-LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/postlasso_binary_feature_importance_g1.png", postlasso_feature_imp_plot$g1 + ggtitle("POST-LASSO"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')


#### XGBoost ####
#+++++++++++++++#

# load all estimation results
xgb_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_xgboost_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", model_k_tuning, "_Rep", model_s_rep, ".rds"))

# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
xgb_binary_param <- data.frame()
for (i in 1:5) {
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
xgb_scores_all <- data.frame()
for (mice_data_sel in 1:5) {
  
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
  df_sel_name <- paste0("data_dml_mice", mice_data_sel)
  
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

# generate plot
xgb_feature_imp_plot <- func_feature_importance_plot("binary", xgb_scores_all, "XGBoost", "separate")
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_m.png", xgb_feature_imp_plot$m + ggtitle("XGBoost"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g0.png", xgb_feature_imp_plot$g0 + ggtitle("XGBoost"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g1.png", xgb_feature_imp_plot$g1 + ggtitle("XGBoost"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')



#### Random Forests ####
#++++++++++++++++++++++#

# load all estimation results
rf_binary_results_all <- 
  readRDS(paste0(
    "Output/DML/Estimation/Grades/binary_grades_randomforests_", model_type, "_",
    str_replace_all(cohort_prep, "_", ""), "_", treatment_def, "_", treatment_repl, 
    extra_act_save, "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""),
    "_endog", model_controls_endog, "_trimming", model_trimming, "_K", model_k,
    "-", 1, "_Rep", model_s_rep, ".rds"))

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
  df_sel_name <- paste0("data_dml_mice", mice_data_sel)
  
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


cbind(rownames(randomForest::importance(rf_fit_g1$fit, type=1, scale=F)) %>% as_tibble(), randomForest::importance(rf_model$fit, type=1, scale=F) %>% as_tibble())%>% arrange(desc(MeanDecreaseAccuracy)) %>% head()




rf_scores_all_final <- 
  rf_scores_all %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE) %>%
  arrange(desc(Importance)) %>%
  group_by(Pred_Type) %>%
  top_n(n_features, Importance) %>%
  ungroup()

# generate plot
rf_feature_imp_plot <- func_feature_importance_plot("binary", rf_scores_all_final, "Random Forests", "separate")
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_m.png", rf_feature_imp_plot$m + ggtitle("RANDOM FORESTS"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g0.png", rf_feature_imp_plot$g0 + ggtitle("RANDOM FORESTS"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g1.png", rf_feature_imp_plot$g1 + ggtitle("RANDOM FORESTS"),
       width = 10, height = 8, dpi = 150, units = "in", device='png')


#### FINAL PLOTS ####
#+++++++++++++++++++#

feature_imp_binary_m <- grid.arrange(
  lasso_feature_imp_plot$m + ggtitle("LASSO") + rremove("xlab"),
  postlasso_feature_imp_plot$m + ggtitle("POST-LASSO") + rremove("xlab"),
  rf_feature_imp_plot$m + ggtitle("RANDOM FORESTS") + rremove("xlab"),
  xgb_feature_imp_plot$m + ggtitle("XGBOOST") + rremove("xlab"),
  ncol = 2, 
  widths = c(1/2, 1/2)) 

ggsave("Output/DML/Feature_Importance/binary_feature_importance_m.png", feature_imp_binary_m,
       width = 10, height = 8, dpi = 150, units = "in", device='png')
ggsave("Output/DML/Feature_Importance/binary_feature_importance_g0.png", feature_imp_binary_g0)
ggsave("Output/DML/Feature_Importance/binary_feature_importance_g1.png", feature_imp_binary_g1)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# load data
for (mice_data_sel in 1:5) {
  # load data
  ## extract extracurricular activity ending
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  ## extract outcome
  if (str_detect(outcome_var, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
  ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
  }
  
  data_dml <- readRDS(load_data)
  data_dml <- data_dml %>% mutate(
    treatment_sport_freq_weekly_atleast = as.factor(treatment_sport_freq_weekly_atleast),
    treatment_sport_freq_monthly_less = as.factor(treatment_sport_freq_monthly_less),
    treatment_sport_freq_never = as.factor(treatment_sport_freq_never)
  )
  if (model_controls == "no_lags") {
    data_dml <- data_dml %>% dplyr::select(-ends_with("lag"))
  }
  assign(as.character(paste0("data_dml_multi_mice", mice_data_sel)), data_dml)
}


#### LASSO ####
#+++++++++++++#

# load all estimation results
lasso_multi_results_all <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_lasso_all_controlssameoutcome_weekly_down_extradrop.rds")

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
  slice_max(order_by = Importance, n = n_features)


lasso_multi_feature_imp_plot <- func_feature_importance_plot("multi", lasso_multi_coef, "LASSO", "separate")
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_m1.png", lasso_multi_feature_imp_plot$m1)
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_m2.png", lasso_multi_feature_imp_plot$m2)
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_m3.png", lasso_multi_feature_imp_plot$m3)
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_g1.png", lasso_multi_feature_imp_plot$g1)
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_g2.png", lasso_multi_feature_imp_plot$g2)
ggsave("Output/DML/Feature_Importance/lasso_multi_feature_importance_g3.png", lasso_multi_feature_imp_plot$g3)



#### POST-LASSO ####
#++++++++++++++++++#

# load all estimation results
postlasso_multi_results_all <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_postlasso_all_controlssameoutcome_weekly_down_extradrop.rds")

# extract coefficients across MICE data frames
postlasso_multi_coef <- data.frame()
for (i in 1:5) {
  postlasso_multi_coef <- rbind(
    postlasso_multi_coef, postlasso_multi_results_all[[i]]$coef %>% mutate(MICE = i)
  )
}
postlasso_multi_coef <- postlasso_multi_coef %>% filter(term != "(Intercept)")

postlasso_multi_coef <- postlasso_multi_coef %>%
  group_by(term) %>%
  summarize(estimate = mean(estimate)) %>%
  ungroup() %>%
  rename(Variable = term, Importance = estimate) %>%
  slice_max(order_by = Importance, n = n_features)

postlasso_multi_feature_imp_plot <- func_feature_importance_plot("multi", postlasso_multi_coef, "POST-LASSO", "separate")


#### XGBoost ####
#+++++++++++++++#

# load all estimation results
xgb_multi_results_all <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_xgboost_all_controlssameoutcome_weekly_down_extradrop.rds")

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
  df_sel_name <- paste0("data_dml_multi_mice", mice_data_sel)
  
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
  X_controls <- c(X_controls, "treatment_sport_freq_na", "treatment_sport_freq_source_leisure", 
                  "treatment_sport_freq_source_uni")
  if ("treatment_sport_freq_lag" %in% ncol(get(df_sel_name))) {
    X_controls <- c(X_controls, "treatment_sport_freq_lag")
  }
  
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
ggsave("Output/DML/Feature_Importance/xgboost_multi_feature_importance_m1.png", xgb_multi_feature_imp_plot$m1)
ggsave("Output/DML/Feature_Importance/xgboost_multi_feature_importance_m2.png", xgb_multi_feature_imp_plot$m2)
ggsave("Output/DML/Feature_Importance/xgboost_multi_feature_importance_m3.png", xgb_multi_feature_imp_plot$m3)
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g1.png", xgb_multi_feature_imp_plot$g1)
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g2.png", xgb_multi_feature_imp_plot$g2)
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g3.png", xgb_multi_feature_imp_plot$g3)



#### Random Forests ####
#++++++++++++++++++++++#

# load all estimation results
rf_multi_results_all <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_randomforests_all_controlssameoutcome_weekly_down_extradrop.rds")

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
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_multi_mice", mice_data_sel)
  
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
  
  trees_g4 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_trees)
  mtry_g4 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_mtry)
  min_n_g1 <- rf_multi_best_param_g1 %>% filter(MICE == mice_data_sel) %>% pull(g1_min_n)
  
  rf_spec_g1 <- 
    rand_forest(trees = {{trees_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", starts_with("treatment_sport_freq"), "group")) %>% colnames()
  X_controls <- c(X_controls, "treatment_sport_freq_na", "treatment_sport_freq_source_leisure", 
                  "treatment_sport_freq_source_uni")
  
  if ("treatment_sport_freq_lag" %in% ncol(get(df_sel_name))) {
    X_controls <- c(X_controls, "treatment_sport_freq_lag")
  }
  
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

rf_scores_all <- 
  rf_scores_all %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE)

# generate plot
rf_multi_feature_imp_plot <- func_feature_importance_plot("multi", rf_scores_all, "Random Forests", "separate")
ggsave("Output/DML/Feature_Importance/rf_multi_feature_importance_m1.png", rf_multi_feature_imp_plot$m1)
ggsave("Output/DML/Feature_Importance/rf_multi_feature_importance_m2.png", rf_multi_feature_imp_plot$m2)
ggsave("Output/DML/Feature_Importance/rf_multi_feature_importance_m3.png", rf_multi_feature_imp_plot$m3)
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g1.png", rf_multi_feature_imp_plot$g1)
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g2.png", rf_multi_feature_imp_plot$g2)
ggsave("Output/DML/Feature_Importance/rf_binary_feature_importance_g3.png", rf_multi_feature_imp_plot$g3)