#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML Analysis Results: Binary and Multivalued Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++ 
# by Lana Kern
#+++
# In this files, the DML results are analyzed and prepared so that they can
# be copied within the final paper. For instance, error metrics are generated,
# common support plots are created etc. 
#+++


#%%%%%%%%%%%%%%%%%%%%#
#### LOAD RESULTS ####
#%%%%%%%%%%%%%%%%%%%%#

if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else if (extra_act == "uni") {
  extra_act_save <- "_extrauni"
} else {
  extra_act_save <- ""
}

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}


## Data Sets ##
#+++++++++++++#

data_grades_binary <- 
  readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_down_extradrop", cov_balance_save, "_mice1.rds"))

data_personality_binary <- 
  readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_", treatment_def, "_down_extradrop", cov_balance_save, "_mice1_personality.rds"))

data_grades_multi <- 
  readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_", treatment_def, "_down_extradrop", cov_balance_save, "_mice1.rds"))

data_personality_multi <- 
  readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_", treatment_def, "_down_extradrop", cov_balance_save, "_mice1_personality.rds"))


## SUMMARY OF RESULTS ##
#++++++++++++++++++++++#

df_dml_main_binary <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")

df_dml_main_multi <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")

df_dml_sep_multi <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")


## LASSO ##
#+++++++++#

# BINARY
lasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI
lasso_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_lasso_all_controlssameoutcome_", treatment_def,
                 "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# MULTI: PERSONALITY
lasso_agree_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_agreeableness_", "lasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

lasso_consc_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_conscientiousness_", "lasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

lasso_extra_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_extraversion_", "lasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

lasso_open_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_openness_", "lasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

lasso_neuro_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_neuroticism_", "lasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


## POST-LASSO ##
#++++++++++++++#

# BINARY: GPA
postlasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI: GPA
postlasso_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_postlasso_all_controlssameoutcome_",
                 treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# BINARY: PERSONALITY
postlasso_agree <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_agreeableness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_consc <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_conscientiousness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_extra <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_extraversion_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_open <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_openness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_neuro <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_neuroticism_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# MULTI: PERSONALITY
postlasso_agree_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_agreeableness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_consc_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_conscientiousness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_extra_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_extraversion_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_open_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_openness_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_neuro_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_neuroticism_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# Robustnesschecks regarding samples
postlasso_grades_rc1_befall <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlsbefall_", treatment_def, 
                 "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc2_noextra <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, 
                 "_down_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc3_endog <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_all_notreatmentoutcomelags_endogno_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc4_lags <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_all_onlylags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc5_polys <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_allpoly_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_allpoly_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))



# Robustness checks regarding hyperparameters
postlasso_grades_1SE <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_1SE", cov_balance_save, ".rds"))

postlasso_grades_1SEplus <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_1SE_plus", cov_balance_save, ".rds"))


## XGBoosst ##
#++++++++++++#

# BINARY
xgb_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "xgboost", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI
xgb_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "xgboost", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))



## Random Forests ##
#++++++++++++++++++#

# BINARY
rf_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "randomforests", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-1_Rep5", cov_balance_save, ".rds"))


# MULTI
rf_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "randomforests", 
                 "_all_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-1_Rep5", cov_balance_save, ".rds"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++TREATMENT EFFECTS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Binary Treatment Setting ####
#++++++++++++++++++++++++++++++++#

# main model
df_treatment_effects_main_binary <- df_dml_main_binary %>% filter(
  cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
  treatment_repl == main_treatment_repl, extra_act == main_extra_act,
  model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
  model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
  model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
  model_covbal == "yes", Type %in% c("ATE", "ATTE")
) %>% 
  mutate(model = "main") 

df_treatment_effects_main_binary_save <- df_treatment_effects_main_binary %>%
  dplyr::select(model, outcome, model_algo, Type, theta_median, se_median, pvalue_median, time_stamp) 

cat("Treatment Effects in the Binary Treatment Setting:")
df_treatment_effects_main_binary_save



## PLOT ##

# GPA: plot to show how variable estimates are across MICE data sets and repetitions
lasso_grades_boxplot <- data.frame()
postlasso_grades_boxplot <- data.frame()
rf_grades_boxplot <- data.frame()
xgb_grades_boxplot <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_boxplot <- rbind(
    lasso_grades_boxplot,
    lasso_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATET", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
  postlasso_grades_boxplot <- rbind(
    postlasso_grades_boxplot,
    postlasso_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATET", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
  rf_grades_boxplot <- rbind(
    rf_grades_boxplot,
    rf_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATET", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
}

xgb_grades_boxplot <- rbind(
  xgb_grades_boxplot,
  xgb_grades[[1]]$detail %>%
    dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
    filter(Type %in% c("ATE", "ATET", "ATTE")) %>%
    mutate(MICE = mice_sel)
)

df_binary_boxplot <- rbind(lasso_grades_boxplot, postlasso_grades_boxplot)
df_binary_boxplot <- rbind(df_binary_boxplot, rf_grades_boxplot)
df_binary_boxplot <- rbind(df_binary_boxplot, xgb_grades_boxplot)

df_binary_boxplot <- df_binary_boxplot %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "LASSO", ML_algo == "postlasso" ~ "Post-LASSO",
      ML_algo == "randomforests" ~ "Random forests",
      ML_algo == "xgboost" ~ "XGBoost"
    ),
    Type = case_when(Type == "ATTE" ~ "ATET", TRUE ~ Type)
  )

dml_boxplot_binary <- 
  ggplot(df_binary_boxplot, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey", outlier.size = 5, width = 0.8, linewidth = 0.8) +
  stat_summary(fun.y=mean, geom = "point", shape = 15, size = 5, color = "darkblue",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.1, 0) +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 40), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 40), # rotate x-axis ticks
    axis.title = element_text(size = 40), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 45) # size of facet text
    ) 
dml_boxplot_binary

pdf("Output/DML/Treatment_Effects/plot_binary_main_grades_treatment_effects_variability.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(dml_boxplot_binary)
dev.off()


# PERSONALITY
postlasso_personality_boxplot <- data.frame()
for (personality_sel in c("agree", "extra", "consc", "neuro", "open")) {
  postlasso_perso_sel <- paste0("postlasso_", personality_sel)
  for (mice_sel in 1:5) {
    postlasso_personality_boxplot <- rbind(
      postlasso_personality_boxplot,
      get(postlasso_perso_sel)[[mice_sel]]$detail %>%
        mutate(Outcome = personality_sel) %>%
        dplyr::select(Outcome, Type, Rep, Treatment_Effect) %>%
        filter(Type %in% c("ATE", "ATTE")) %>%
        mutate(MICE = mice_sel, Type = case_when(Type == "ATTE" ~ "ATET", TRUE ~ Type))
    )
  } # close iteration over mice_sel
} # close iteration over personality

postlasso_personality_boxplot <- postlasso_personality_boxplot %>%
  mutate(
    Outcome = case_when(
      Outcome == "agree" ~ "Agreeableness", Outcome == "extra" ~ "Extroversion",
      Outcome == "consc" ~ "Conscientiousness", Outcome == "open" ~ "Openness",
      Outcome == "neuro" ~ "Neuroticism", TRUE ~ as.character(NA)
    )
  )

dml_boxplot_binary_personality <- 
  ggplot(postlasso_personality_boxplot, aes(x = Outcome, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey", outlier.size = 5, linewidth = 0.8) +
  stat_summary(fun.y=mean, geom = "point", shape = 15, size = 5, color = "darkblue",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.15, 0.25) +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 40), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 40), # rotate x-axis ticks
    axis.title = element_text(size = 40), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 45) # size of facet text
  ) 
dml_boxplot_binary_personality

pdf("Output/DML/Treatment_Effects/plot_binary_main_personality_treatment_effects_variability.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(dml_boxplot_binary_personality)
dev.off()


#### Multivalued Treatment Setting ####
#+++++++++++++++++++++++++++++++++++++#

# main
df_treatment_effects_main_multi <- df_dml_main_multi %>% filter(
  cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
  treatment_repl == main_treatment_repl, extra_act == main_extra_act,
  model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
  model_k_tuning %in% c(1,2),
  model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
  model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
  model_covbal == "yes", Type %in% c("ATE", "ATTE"), Prob_norm == "yes"
) %>% mutate(model = "main") 

df_treatment_effects_main_multi_save <- df_treatment_effects_main_multi %>%
  dplyr::select(model, outcome, model_algo, Type, Treatment, theta_median, se_median, pvalue_median) 

cat("Treatment Effects in the Multivalued Treatment Setting:")
df_treatment_effects_main_multi_save


# plot to show how variable estimates are across MICE data sets and repetitions
lasso_grades_boxplot_multi <- data.frame()
postlasso_grades_boxplot_multi <- data.frame()
rf_grades_boxplot_multi <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_boxplot_multi <- rbind(
    lasso_grades_boxplot_multi,
    lasso_grades_multi[[mice_sel]]$detail %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      dplyr::select(ML_algo, Treatment, Type, Rep, Treatment_Effect) %>%
      mutate(MICE = mice_sel)
  )
  
  postlasso_grades_boxplot_multi <- rbind(
    postlasso_grades_boxplot_multi,
    postlasso_grades_multi[[mice_sel]]$detail %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      dplyr::select(ML_algo, Treatment, Type, Rep, Treatment_Effect) %>%
      mutate(MICE = mice_sel)
  )
  
 rf_grades_boxplot_multi <- rbind(
   rf_grades_boxplot_multi,
    rf_grades_multi[[mice_sel]]$detail %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      dplyr::select(ML_algo, Treatment, Type, Rep, Treatment_Effect) %>%
      mutate(MICE = mice_sel)
  )
}

xgb_grades_boxplot_multi <- 
  xgb_grades_multi[[1]]$detail %>%
    filter(Type %in% c("ATE", "ATTE")) %>%
    dplyr::select(ML_algo, Treatment, Type, Rep, Treatment_Effect) %>%
    mutate(MICE = 1)

df_multi_boxplot <- rbind(lasso_grades_boxplot_multi, postlasso_grades_boxplot_multi)
df_multi_boxplot <- rbind(df_multi_boxplot, rf_grades_boxplot_multi)
df_multi_boxplot <- rbind(df_multi_boxplot, xgb_grades_boxplot_multi)

df_multi_boxplot <- df_multi_boxplot %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "LASSO", ML_algo == "postlasso" ~ "Post-LASSO",
      ML_algo == "randomforests" ~ "Random forests",
      ML_algo == "xgboost" ~ "XGBoost"
    )
  ) %>%
  mutate(
    Treatment = case_when(
      Treatment == "monthly_weekly" ~ "Weekly - Monthly", 
      Treatment == "no_weekly" ~ "Weekly - Never",
      Treatment == "no_monthly" ~ "Monthly - Never", TRUE ~ as.character(NA))
  ) %>%
  mutate(Type = ifelse(Type == "ATTE", "ATET", Type)) %>%
  mutate(across(Treatment, factor, 
                levels = c("Weekly - Monthly","Weekly - Never","Monthly - Never")))

grades_boxplot_multi_all <- 
  ggplot(df_multi_boxplot, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey", outlier.size = 5, linewidth = 0.8) +
  stat_summary(fun.y=mean, geom = "point", shape = 15, size = 5, color = "darkblue",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.15, 0.25) +
  facet_grid(rows = vars(Type), cols = vars(Treatment),
             scales="free_y", switch = 'y') + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 36), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 36), # rotate x-axis ticks
    axis.title = element_text(size = 36), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 40), strip.text.y = element_text(size = 40) # size of facet text
  ) 
grades_boxplot_multi_all

pdf("Output/DML/Treatment_Effects/plot_multi_main_grades_treatment_effects_variability_all.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(grades_boxplot_multi_all)
dev.off()


# personality
postlasso_personality_boxplot_multi <- data.frame()
for (personality_sel in c("agree", "extra", "consc", "neuro", "open")) {
  postlasso_perso_sel <- paste0("postlasso_", personality_sel, "_multi")
  for (mice_sel in 1:5) {
    postlasso_personality_boxplot_multi <- rbind(
      postlasso_personality_boxplot_multi,
      get(postlasso_perso_sel)[[mice_sel]]$detail %>%
        mutate(Outcome = personality_sel) %>%
        dplyr::select(Outcome,  Treatment, Type, Rep, Treatment_Effect) %>%
        filter(Type %in% c("ATE", "ATTE")) %>%
        mutate(MICE = mice_sel, Type = case_when(Type == "ATTE" ~ "ATET", TRUE ~ Type))
    )
  } # close iteration over mice_sel
} # close iteration over personality

postlasso_personality_boxplot_multi <- postlasso_personality_boxplot_multi %>%
  mutate(
    Outcome = case_when(
      Outcome == "agree" ~ "Agreeableness", Outcome == "extra" ~ "Extroversion",
      Outcome == "consc" ~ "Conscientiousness", Outcome == "open" ~ "Openness",
      Outcome == "neuro" ~ "Neuroticism", TRUE ~ as.character(NA)
    )
  ) %>%
  mutate(
    Treatment = case_when(
      Treatment == "monthly_weekly" ~ "Weekly - Monthly", 
      Treatment == "no_weekly" ~ "Weekly - Never",
      Treatment == "no_monthly" ~ "Monthly - Never", TRUE ~ as.character(NA))
  ) %>%
  mutate(across(Treatment, factor, levels=c("Weekly - Monthly","Weekly - Never","Monthly - Never")))

postlasso_personality_boxplot_multi_all <- 
  ggplot(postlasso_personality_boxplot_multi, aes(x = Outcome, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey", outlier.size = 5, linewidth = 0.8) +
  stat_summary(fun.y=mean, geom = "point", shape = 15, size = 5, color = "darkblue",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.15, 0.25) +
  facet_grid(rows = vars(Type), cols = vars(Treatment),
             scales="free_y", switch = 'y') + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 36), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 36), # rotate x-axis ticks
    axis.title = element_text(size = 36), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 40), strip.text.y = element_text(size = 40) # size of facet text
  ) 
postlasso_personality_boxplot_multi_all

pdf("Output/DML/Treatment_Effects/plot_multi_main_personality_treatment_effects_variability_all.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(postlasso_personality_boxplot_multi_all)
dev.off()


# LASSO
postlasso_personality_boxplot_lasso_multi <- data.frame()
for (personality_sel in c("agree", "extra", "consc", "neuro", "open")) {
  postlasso_perso_sel <- paste0("lasso_", personality_sel, "_multi")
  for (mice_sel in 1:5) {
    postlasso_personality_boxplot_lasso_multi <- rbind(
      postlasso_personality_boxplot_lasso_multi,
      get(postlasso_perso_sel)[[mice_sel]]$detail %>%
        mutate(Outcome = personality_sel) %>%
        dplyr::select(Outcome,  Treatment, Type, Rep, Treatment_Effect) %>%
        filter(Type %in% c("ATE", "ATTE")) %>%
        mutate(MICE = mice_sel, Type = case_when(Type == "ATTE" ~ "ATET", TRUE ~ Type))
    )
  } # close iteration over mice_sel
} # close iteration over personality

postlasso_personality_boxplot_lasso_multi <- postlasso_personality_boxplot_lasso_multi %>%
  mutate(
    Outcome = case_when(
      Outcome == "agree" ~ "Agreeableness", Outcome == "extra" ~ "Extroversion",
      Outcome == "consc" ~ "Conscientiousness", Outcome == "open" ~ "Openness",
      Outcome == "neuro" ~ "Neuroticism", TRUE ~ as.character(NA)
    )
  ) %>%
  mutate(
    Treatment = case_when(
      Treatment == "monthly_weekly" ~ "Weekly - Monthly", 
      Treatment == "no_weekly" ~ "Weekly - Never",
      Treatment == "no_monthly" ~ "Monthly - Never", TRUE ~ as.character(NA))
  ) %>%
  mutate(across(Treatment, factor, levels=c("Weekly - Monthly","Weekly - Never","Monthly - Never")))

postlasso_personality_boxplot_lasso_multi <- 
  ggplot(postlasso_personality_boxplot_lasso_multi, aes(x = Outcome, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey", outlier.size = 5, linewidth = 0.8) +
  stat_summary(fun.y=mean, geom = "point", shape = 15, size = 5, color = "darkblue",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.15, 0.25) +
  facet_grid(rows = vars(Type), cols = vars(Treatment),
             scales="free_y", switch = 'y') + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 36), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 36), # rotate x-axis ticks
    axis.title = element_text(size = 36), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 40), strip.text.y = element_text(size = 40) # size of facet text
  ) 
postlasso_personality_boxplot_lasso_multi


#### Robustness Checks ####
#+++++++++++++++++++++++++#

## MULTI ##
df_treatment_effects_rc_multi <- df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_k_tuning %in% c(1,2) & model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc"
  )) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade") %>%
  dplyr::select(cohort_prep, treatment_repl, extra_act, starts_with("model"), 
                Prob_norm, Treatment, Type, theta_median, se_median, pvalue_median, time_stamp) %>% 
  arrange(desc(time_stamp)) 





#### Variability Around Zero ####
#+++++++++++++++++++++++++++++++#

# this is only relevant for personality sample

## BINARY ##
df_variability_pers_binary <- data.frame()
for (outcome_var_sel in c("agree", "neuro", "consc", "extra", "open")) {
  model_sel <- get(paste0("postlasso_",outcome_var_sel))
  ate_yes_no <- c()
  atte_yes_no <- c()
  for (mice_sel in 1:5) {
    ate_yes_no <- c(
      ate_yes_no, 
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATE") %>% pull(Treatment_Effect)
    )
    
    atte_yes_no <- c(
      atte_yes_no, 
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATTE") %>% pull(Treatment_Effect)
    )
  } # close iteration over mice_sel
  df_variability_pers_binary <- rbind(
    df_variability_pers_binary, 
    data.frame(outcome = outcome_var_sel, 
               "ATE" = any(ate_yes_no > 0) & any(ate_yes_no < 0),
               "ATET" = any(atte_yes_no > 0) & any(atte_yes_no < 0)
    )
  )
} # close iteration over outcome_sel

## MULTI ##
df_variability_pers_multi <- data.frame()
for (outcome_var_sel in c("agree", "neuro", "consc", "extra", "open")) {
  model_sel <- get(paste0("postlasso_",outcome_var_sel, "_multi"))
  ate_monthly_weekly <- c()
  ate_no_weekly <- c()
  ate_no_monthly <- c()
  atte_monthly_weekly <- c()
  atte_no_weekly <- c()
  atte_no_monthly <- c()
  for (mice_sel in 1:5) {
    ate_monthly_weekly <- c(
      ate_monthly_weekly, 
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATE") %>% filter(Treatment == "monthly_weekly") %>% pull(Treatment_Effect)
    )
    ate_no_weekly <- c(
      ate_no_weekly,
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATE") %>% filter(Treatment == "no_weekly") %>% pull(Treatment_Effect)
    )
    ate_no_monthly <- c(
      ate_no_monthly,
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATE") %>% filter(Treatment == "no_monthly") %>% pull(Treatment_Effect)
    )
    
    atte_monthly_weekly <- c(
      atte_monthly_weekly, 
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATTE") %>% filter(Treatment == "monthly_weekly") %>% pull(Treatment_Effect)
    )
    atte_no_weekly <- c(
      atte_no_weekly, 
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATTE") %>% filter(Treatment == "no_weekly") %>% pull(Treatment_Effect)
    )
    atte_no_monthly <- c(
      atte_no_monthly,
      model_sel[[mice_sel]]$detail %>% filter(Type == "ATTE") %>% filter(Treatment == "no_monthly") %>% pull(Treatment_Effect)
    )
  } # close iteration over mice_sel
  df_variability_pers_multi <- rbind(
    df_variability_pers_multi, 
    data.frame(outcome = outcome_var_sel, 
               "ate_monthly_weekly" = any(ate_monthly_weekly > 0) & any(ate_monthly_weekly < 0),
               "ate_no_weekly" = any(ate_no_weekly > 0) & any(ate_no_weekly < 0),
               "ate_no_monthly" = any(ate_no_monthly > 0) & any(ate_no_monthly < 0),
               "atte_monthly_weekly" = any(atte_monthly_weekly > 0) & any(atte_monthly_weekly < 0),
               "atte_no_weekly" = any(atte_no_weekly > 0) & any(atte_no_weekly < 0),
               "atte_no_monthly" = any(atte_no_monthly > 0) & any(atte_no_monthly < 0)
    )
  )
} # close iteration over outcome_sel


#%%%%%%%%%%%%%%%%#
#### ++ APE++ ####
#%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
#++++++++++++++++++++++++++++#

# the APE is identical across the MICE data sets and algorithms
# calculated APE is not for standardized outcome variable
lasso_grades[[1]]$ape # binary

# APE
df_ape_binary <- readRDS("Output/Descriptives/BINARY_MEAN_COMPARISON.rds")
df_ape_binary <- df_ape_binary %>%
  filter(treatment != "all", variable_measure == "stand") %>%
  dplyr::select(variable, treatment, mean, starts_with("p_value")) %>%
  spread(treatment, mean) %>%
  mutate(ape = `1` - `0`)
# SE
data_descr <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
                             "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1.rds"))
data_descr_stand <- data_descr %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame() %>%
  dplyr::select(treatment_sport, outcome_grade)

data_descr_pers <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
                             "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1_personality.rds"))
data_descr_stand_pers <- data_descr_pers %>%
  recipe(.) %>%
  step_normalize(all_of(data_descr_pers %>% dplyr::select(starts_with("bigfive")) %>% colnames())) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame() %>%
  dplyr::select(treatment_sport, all_of(data_descr_pers %>% dplyr::select(starts_with("bigfive")) %>% colnames()))

# ALL
df_ape_binary <- left_join(df_ape_binary,
                           rbind(
                             data.frame(variable = "outcome_grade", "se" = t.test(formula = outcome_grade ~ treatment_sport, data = data_descr_stand)$stderr),
                             data.frame(variable = "bigfive_agreeableness", "se" = t.test(formula = bigfive_agreeableness ~ treatment_sport, data = data_descr_stand_pers)$stderr)
                           ) %>% rbind(
                             data.frame(variable = "bigfive_conscientiousness", "se" = t.test(formula = bigfive_conscientiousness ~ treatment_sport, data = data_descr_stand_pers)$stderr)
                           ) %>% rbind(
                             data.frame(variable = "bigfive_extraversion", "se" = t.test(formula = bigfive_extraversion ~ treatment_sport, data = data_descr_stand_pers)$stderr)
                           ) %>% rbind(
                             data.frame(variable = "bigfive_neuroticism", "se" = t.test(formula = bigfive_neuroticism ~ treatment_sport, data = data_descr_stand_pers)$stderr)
                           ) %>% rbind(
                             data.frame(variable = "bigfive_openness", "se" = t.test(formula = bigfive_openness ~ treatment_sport, data = data_descr_stand_pers)$stderr)
                           ),
                           by = "variable")

cat("APE in binary treatment setting:")
df_ape_binary



## Multivalued Treatment Setting ##
#+++++++++++++++++++++++++++++++++#

# APE
df_ape_multi <- readRDS("Output/Descriptives/MULTI_MEAN_COMPARISON.rds")

# mean
df_mean_multi <- df_ape_multi %>%
  filter(treatment != "all", variable_measure == "stand") %>%
  dplyr::select(variable, treatment, mean) %>%
  spread(treatment, mean) %>%
  group_by(variable) %>%
  fill(all_of(c("monthly_less", "never", "weekly_atleast")), .direction = 'downup') %>%
  mutate(ape_weekly_monthly = `weekly_atleast` - `monthly_less`,
         ape_weekly_never = `weekly_atleast` - `never`,
         ape_monthly_never = `monthly_less` - `never`)

# p-value
df_p_multi <- 
  df_ape_multi %>% 
  filter(!treatment %in% c("weekly_atleast", "all"), variable_measure == "stand") %>%   
  dplyr::select(variable, treatment, p_value_daily, p_value_monthly) %>% 
  pivot_wider(
    names_from = treatment,
    values_from = c(p_value_daily, p_value_monthly)
    ) %>% 
  dplyr::select(-p_value_monthly_monthly_less)

df_ape_multi <- left_join(df_mean_multi, df_p_multi, by = "variable")

# SE
data_descr <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
                             "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1.rds"))
data_descr_stand <- data_descr %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame() %>%
  dplyr::select(treatment_sport_freq, outcome_grade)

data_descr_pers <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
                                  "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1_personality.rds"))
data_descr_stand_pers <- data_descr_pers %>%
  recipe(.) %>%
  step_normalize(all_of(data_descr_pers %>% dplyr::select(starts_with("bigfive")) %>% colnames())) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame() %>%
  dplyr::select(treatment_sport_freq, all_of(data_descr_pers %>% dplyr::select(starts_with("bigfive")) %>% colnames()))
# ALL
data_descr_stand_sub <- data_descr_stand %>% filter(treatment_sport_freq != 3)
data_descr_stand_sub_pers <- data_descr_stand_pers %>% filter(treatment_sport_freq != 3)
df_multi_se_monthly_weekly <-
  rbind(
    data.frame(variable = "outcome_grade", "se_daily_monthly" = t.test(formula = outcome_grade ~ treatment_sport_freq, data = data_descr_stand_sub)$stderr),
    data.frame(variable = "bigfive_agreeableness", "se_daily_monthly" = t.test(formula = bigfive_agreeableness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
    ) %>% rbind(
      data.frame(variable = "bigfive_conscientiousness", "se_daily_monthly" = t.test(formula = bigfive_conscientiousness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
      ) %>% rbind(
        data.frame(variable = "bigfive_extraversion", "se_daily_monthly" = t.test(formula = bigfive_extraversion ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
        ) %>% rbind(
          data.frame(variable = "bigfive_neuroticism", "se_daily_monthly" = t.test(formula = bigfive_neuroticism ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
          ) %>% rbind(
            data.frame(variable = "bigfive_openness", "se_daily_monthly" = t.test(formula = bigfive_openness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
            )


data_descr_stand_sub <- data_descr_stand %>% filter(treatment_sport_freq != 2)
data_descr_stand_sub_pers <- data_descr_stand_pers %>% filter(treatment_sport_freq != 2)
df_multi_se_never_weekly <-
  rbind(
    data.frame(variable = "outcome_grade", "se_daily_never" = t.test(formula = outcome_grade ~ treatment_sport_freq, data = data_descr_stand_sub)$stderr),
    data.frame(variable = "bigfive_agreeableness", "se_daily_never" = t.test(formula = bigfive_agreeableness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_conscientiousness", "se_daily_never" = t.test(formula = bigfive_conscientiousness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_extraversion", "se_daily_never" = t.test(formula = bigfive_extraversion ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_neuroticism", "se_daily_never" = t.test(formula = bigfive_neuroticism ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_openness", "se_daily_never" = t.test(formula = bigfive_openness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  )



data_descr_stand_sub <- data_descr_stand %>% filter(treatment_sport_freq != 1)
data_descr_stand_sub_pers <- data_descr_stand_pers %>% filter(treatment_sport_freq != 1)
df_multi_se_never_monthly <-
  rbind(
    data.frame(variable = "outcome_grade", "se_monthly_never" = t.test(formula = outcome_grade ~ treatment_sport_freq, data = data_descr_stand_sub)$stderr),
    data.frame(variable = "bigfive_agreeableness", "se_monthly_never" = t.test(formula = bigfive_agreeableness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_conscientiousness", "se_monthly_never" = t.test(formula = bigfive_conscientiousness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_extraversion", "se_monthly_never" = t.test(formula = bigfive_extraversion ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_neuroticism", "se_monthly_never" = t.test(formula = bigfive_neuroticism ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  ) %>% rbind(
    data.frame(variable = "bigfive_openness", "se_monthly_never" = t.test(formula = bigfive_openness ~ treatment_sport_freq, data = data_descr_stand_sub_pers)$stderr)
  )


df_ape_multi <- left_join(df_ape_multi, df_multi_se_monthly_weekly, by = "variable") %>%
  left_join(df_multi_se_never_weekly, by = "variable") %>%
  left_join(df_multi_se_never_monthly, by = "variable") %>%
  dplyr::select(variable, starts_with("ape"), starts_with("p_value"), starts_with("se"))


cat("APE in multivalued treatment setting:")
df_ape_multi


## RCs ##
#+++++++#

# APE
df_ape_multi_rc <- readRDS("Output/Descriptives/MULTI_MEAN_COMPARISON_RC.rds")

# mean
df_mean_multi_rc <- df_ape_multi_rc %>%
  filter(treatment_sport_freq != "all") %>%
  dplyr::select(RC, treatment_sport_freq, mean) %>%
  spread(treatment_sport_freq, mean) %>%
  group_by(RC) %>%
  fill(all_of(c("monthly_less", "never", "weekly_atleast")), .direction = 'downup') %>%
  mutate(ape_weekly_monthly = `weekly_atleast` - `monthly_less`,
         ape_weekly_never = `weekly_atleast` - `never`,
         ape_monthly_never = `monthly_less` - `never`)

# p-value
df_p_multi_rc <- 
  df_ape_multi_rc %>% 
  filter(!treatment_sport_freq %in% c("weekly_atleast", "all")) %>%   
  dplyr::select(RC, treatment_sport_freq, p_value_daily, p_value_monthly) %>% 
  pivot_wider(
    names_from = treatment_sport_freq,
    values_from = c(p_value_daily, p_value_monthly)
  ) %>% 
  dplyr::select(-p_value_monthly_monthly_less)

df_ape_multi_rc <- left_join(df_mean_multi_rc, df_p_multi_rc, by = "RC")

# SE
df_ape_multi_se <- readRDS("Output/Descriptives/MULTI_MEAN_COMPARISON_RC_SE.rds")


df_ape_multi_rc <- left_join(df_ape_multi_rc, df_ape_multi_se, by = "RC") %>%
  dplyr::select(RC, starts_with("ape"), starts_with("p_value"), starts_with("se"))

cat("APE for robustness checks:")
df_ape_multi_rc


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++ERROR METRICS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# The error metrics obtained in the Excel files are calculated based on the
# standardized outcome variables. For better interpretability, the standardization
# is reversed.
# https://support.numxl.com/hc/en-us/articles/207841883-Transforming-standardized-values-to-non-standardized-values

# mean and standard deviation used for standardization (same across MICE data sets
# as outcome vars are not replaced as they do not contain any missing values)
data_stand_grades <- cbind(data_grades_binary %>%
  summarize(mean = mean(outcome_grade), sd = sd(outcome_grade)),
  data_grades_binary %>% 
    group_by(treatment_sport) %>% 
    summarize(mean = mean(outcome_grade)) %>% 
    spread(treatment_sport, mean) %>%
    rename(mean_no = `0`, mean_yes = `1`))

data_stand_pers <- cbind(data_personality_binary %>%
  summarize(agree_mean = mean(bigfive_agreeableness), agree_sd = sd(bigfive_agreeableness),
            consc_mean = mean(bigfive_conscientiousness), consc_sd = sd(bigfive_conscientiousness),
            extra_mean = mean(bigfive_extraversion), extra_sd = sd(bigfive_extraversion),
            open_mean = mean(bigfive_openness), open_sd = sd(bigfive_openness),
            neuro_mean = mean(bigfive_neuroticism), neuro_sd = sd(bigfive_neuroticism)), 
  data_personality_binary %>% 
    filter(treatment_sport == 0) %>% 
    summarize(agree_mean_no = mean(bigfive_agreeableness), consc_mean_no = mean(bigfive_conscientiousness),
              extra_mean_no = mean(bigfive_extraversion), open_mean_no = mean(bigfive_openness),
              neuro_mean_no = mean(bigfive_neuroticism))) %>% 
  cbind(
    data_personality_binary %>% 
      filter(treatment_sport == 1) %>% 
      summarize(agree_mean_yes = mean(bigfive_agreeableness), consc_mean_yes = mean(bigfive_conscientiousness),
                extra_mean_yes = mean(bigfive_extraversion), open_mean_yes = mean(bigfive_openness),
                neuro_mean_yes = mean(bigfive_neuroticism))) 
  

#### BINARY TREATMENT SETTING ####
#++++++++++++++++++++++++++++++++#

df_error_main_binary <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # load predictions
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      # re-standardize outcomes
      if (outcome_var_sel == "grades") {
        df_sd_mean <- data_stand_grades 
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
            # original scale
            outcome = outcome_stand*data_stand_grades$sd + data_stand_grades$mean,
            g0 = g0_stand*data_stand_grades$sd + data_stand_grades$mean,
            g1 = g1_stand*data_stand_grades$sd + data_stand_grades$mean
          )
      } else {
        # extract sd and mean
        df_sd_mean <- data_stand_pers %>% 
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean) <- sub(".*_", "", colnames(df_sd_mean))
        df_sd_mean <- df_sd_mean %>% rename(mean_no = "no", mean_yes = "yes")
        
        # convert
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
            # original scale
            outcome = outcome_stand*df_sd_mean$sd + df_sd_mean$mean,
            g0 = g0_stand*df_sd_mean$sd + df_sd_mean$mean,
            g1 = g1_stand*df_sd_mean$sd + df_sd_mean$mean
          )
      }
      
      # calculate error metrics
      df_error_sub <- func_ml_error_metrics("binary", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g0 = (RMSE_g0 / df_sd_mean$mean_no)*100, 
               RRMSE_g1 = (RMSE_g1 / df_sd_mean$mean_yes)*100) 
      
      # bind row
      df_error_main_binary <- rbind(df_error_main_binary, df_error_sub)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

# extract error metrics for paper
df_error_main_binary <- df_error_main_binary %>%
  dplyr::select(outcome, ml_algo, AUC_m, ACC_m, BACC_m, RMSE_g0, RRMSE_g0, MAE_g0, RMSE_g1, RRMSE_g1, MAE_g1)

cat("Error Metrics in the Binary Treatment Setting after Trimming:")
df_error_main_binary

## ERROR METRICS BEFORE TRIMMING ##
df_error_main_binary_bef_trimming <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred_bef_trimming, # BEF_TRIMMING!!!
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean <- data_stand_grades
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
            # original scale
            outcome = outcome_stand*data_stand_grades$sd + data_stand_grades$mean,
            g0 = g0_stand*data_stand_grades$sd + data_stand_grades$mean,
            g1 = g1_stand*data_stand_grades$sd + data_stand_grades$mean
          )
      } else {
        # extract sd and mean
        df_sd_mean <- data_stand_pers %>% 
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean) <- sub(".*_", "", colnames(df_sd_mean))
        
        # convert
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
            # original scale
            outcome = outcome_stand*df_sd_mean$sd + df_sd_mean$mean,
            g0 = g0_stand*df_sd_mean$sd + df_sd_mean$mean,
            g1 = g1_stand*df_sd_mean$sd + df_sd_mean$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("binary", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      
      # calculate RRMSE
      if (outcome_var_sel == "grades") {
        df_error_sub <- df_error_sub %>%
          mutate(RRMSE_g0 = (RMSE_g0 / df_sd_mean$mean_no)*100, 
                 RRMSE_g1 = (RMSE_g1 / df_sd_mean$mean_yes)*100) 
      } else {
        df_error_sub <- df_error_sub %>%
          mutate(RRMSE_g0 = (RMSE_g0 / df_sd_mean$no)*100, 
                 RRMSE_g1 = (RMSE_g1 / df_sd_mean$yes)*100) 
      }

      
      # bind row
      df_error_main_binary_bef_trimming <- rbind(df_error_main_binary_bef_trimming, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel


df_error_main_binary_bef_trimming <- df_error_main_binary_bef_trimming %>%
  dplyr::select(outcome, ml_algo, AUC_m, ACC_m, BACC_m, RMSE_g0, RRMSE_g0, MAE_g0, RMSE_g1, RRMSE_g1, MAE_g1)

cat("Error Metrics in the Binary Treatment Setting before Trimming:")
df_error_main_binary_bef_trimming



#### MULTIVALUED TREATMENT SETTING ####
#+++++++++++++++++++++++++++++++++++++#

data_stand_grades_multi <- cbind(data_grades_multi %>% summarize(mean = mean(outcome_grade), sd = sd(outcome_grade)),
                                 data_grades_multi %>% 
                                   group_by(treatment_sport_freq) %>% 
                                   summarize(mean = mean(outcome_grade)) %>% 
                                   spread(treatment_sport_freq, mean) %>%
                                   rename(mean_weekly = `1`, mean_monthly = `2`, mean_never = `3`))

data_stand_pers_multi <- cbind(data_personality_multi %>%
                           summarize(agree_mean = mean(bigfive_agreeableness), agree_sd = sd(bigfive_agreeableness),
                                     consc_mean = mean(bigfive_conscientiousness), consc_sd = sd(bigfive_conscientiousness),
                                     extra_mean = mean(bigfive_extraversion), extra_sd = sd(bigfive_extraversion),
                                     open_mean = mean(bigfive_openness), open_sd = sd(bigfive_openness),
                                     neuro_mean = mean(bigfive_neuroticism), neuro_sd = sd(bigfive_neuroticism)), 
                           data_personality_multi %>% 
                           filter(treatment_sport_freq == 1) %>% 
                           summarize(agree_mean_weekly = mean(bigfive_agreeableness), consc_mean_weekly = mean(bigfive_conscientiousness),
                                     extra_mean_weekly = mean(bigfive_extraversion), open_mean_weekly = mean(bigfive_openness),
                                     neuro_mean_weekly = mean(bigfive_neuroticism))) %>% 
  cbind(data_personality_multi %>% filter(treatment_sport_freq == 2) %>% 
          summarize(agree_mean_monthly = mean(bigfive_agreeableness), consc_mean_monthly = mean(bigfive_conscientiousness),
                    extra_mean_monthly = mean(bigfive_extraversion), open_mean_monthly = mean(bigfive_openness),
                    neuro_mean_monthly = mean(bigfive_neuroticism))) %>%
  cbind(data_personality_multi %>% filter(treatment_sport_freq == 3) %>% 
          summarize(agree_mean_never = mean(bigfive_agreeableness), consc_mean_never = mean(bigfive_conscientiousness),
                    extra_mean_never = mean(bigfive_extraversion), open_mean_never = mean(bigfive_openness),
                    neuro_mean_never = mean(bigfive_neuroticism))) 

df_error_main_multi <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean_multi <- data_stand_grades_multi
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      } else {
        df_sd_mean_multi <- data_stand_pers_multi %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean_multi) <- 
          str_remove(colnames(df_sd_mean_multi), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g1 = (RMSE_g1 / df_sd_mean_multi$mean_weekly)*100, 
               RRMSE_g2 = (RMSE_g2 / df_sd_mean_multi$mean_monthly)*100,
               RRMSE_g3 = (RMSE_g3 / df_sd_mean_multi$mean_never)*100) 
      
      # bind row
      df_error_main_multi <- rbind(df_error_main_multi, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

# calculate error metrics for paper
df_error_main_multi <- df_error_main_multi %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()

cat("Error Metrics in the Multivalued Treatment Setting after Trimming:")
df_error_main_multi


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++NUMBER PREDICTORS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
df_predictors_all_binary <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      # load predictors
      df_predictors <- 
        rbind(get(load_pred_algo)[[1]]$predictors, get(load_pred_algo)[[2]]$predictors) %>%
        rbind(get(load_pred_algo)[[3]]$predictors) %>%
        rbind(get(load_pred_algo)[[4]]$predictors) %>%
        rbind(get(load_pred_algo)[[5]]$predictors)
      
      # calculate statistics
      df_predictors <- df_predictors %>%
        summarize(min_m = min(num_pred_m), mean_m = mean(num_pred_m), max_m = max(num_pred_m),
                  min_g0 = min(num_pred_g0), mean_g0 = mean(num_pred_g0), max_g0 = max(num_pred_g0),
                  min_g1 = min(num_pred_g1), mean_g1 = mean(num_pred_g1), max_g1 = max(num_pred_g1)) %>%
        mutate(min_g = min(min_g0, min_g1), mean_g = mean(c(mean_g0, mean_g1)), max_g = max(max_g0, max_g1)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      df_predictors_all_binary <- rbind(df_predictors_all_binary, df_predictors)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
      
  }
}

df_predictors_all_binary <- rbind(
  df_predictors_all_binary,
  df_predictors_all_binary %>% 
    filter(outcome != "grades") %>% 
    summarize_all(mean) %>%
    mutate(outcome = "personality", ml_algo = "postlasso")
)
                                  

df_treatment_effects_main_binary %>% filter(str_detect(model_algo, "lasso")) %>% 
  dplyr::select(outcome, model_algo, starts_with("num_pred")) %>% distinct()

# for personality also lasso coefficients are extracted
df_predictors_pers_lasso_binary <- data.frame()
for (outcome_var_sel in c("agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      # load predictors
      df_predictors <- 
        rbind(get(load_pred_algo)[[1]]$coef_lasso, get(load_pred_algo)[[2]]$coef_lasso) %>%
        rbind(get(load_pred_algo)[[3]]$coef_lasso) %>% rbind(get(load_pred_algo)[[4]]$coef_lasso) %>%
        rbind(get(load_pred_algo)[[5]]$coef_lasso)
      
      # calculate statistics
      df_predictors <- df_predictors %>%
        summarize(min_m = min(m), mean_m = mean(m), max_m = max(m),
                  min_g0 = min(g0), mean_g0 = mean(g0), max_g0 = max(g0),
                  min_g1 = min(g1), mean_g1 = mean(g1), max_g1 = max(g1)) %>%
        mutate(min_g = min(min_g0, min_g1), mean_g = mean(c(mean_g0, mean_g1)), max_g = max(max_g0, max_g1)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = "lasso") %>%
        dplyr::select(outcome, ml_algo, everything())
      df_predictors_pers_lasso_binary <- rbind(df_predictors_pers_lasso_binary, df_predictors)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

df_predictors_pers_lasso_binary <- rbind(
  df_predictors_pers_lasso_binary,
  df_predictors_pers_lasso_binary %>% 
    filter(outcome != "grades") %>% 
    summarize_all(mean) %>%
    mutate(outcome = "personality", ml_algo = "lasso")
)

df_predictors_all_binary <- rbind(df_predictors_all_binary, df_predictors_pers_lasso_binary)


## Multivalued Treatment Setting ##
df_predictors_all_multi <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso")) {
    
    # load predictors
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_predictors <- 
        rbind(get(load_pred_algo)[[1]]$predictors, get(load_pred_algo)[[2]]$predictors) %>%
        rbind(get(load_pred_algo)[[3]]$predictors) %>%
        rbind(get(load_pred_algo)[[4]]$predictors) %>%
        rbind(get(load_pred_algo)[[5]]$predictors)
      
      # calculate statistics
      df_predictors <- df_predictors %>%
        summarize(min_m1 = min(num_pred_m1), mean_m1 = mean(num_pred_m1), max_m1 = max(num_pred_m1),
                  min_m2 = min(num_pred_m2), mean_m2 = mean(num_pred_m2), max_m2 = max(num_pred_m2),
                  min_m3 = min(num_pred_m3), mean_m3 = mean(num_pred_m3), max_m3 = max(num_pred_m3),
                  min_g1 = min(num_pred_g1), mean_g1 = mean(num_pred_g1), max_g1 = max(num_pred_g1),
                  min_g2 = min(num_pred_g2), mean_g2 = mean(num_pred_g2), max_g2 = max(num_pred_g2),
                  min_g3 = min(num_pred_g3), mean_g3 = mean(num_pred_g3), max_g3 = max(num_pred_g3)) %>%
        mutate(min_g = min(min_g1, min_g2, min_g3), mean_g = mean(c(mean_g1, mean_g2, mean_g3)), max_g = max(c(max_g1, max_g2, max_g3)),
               min_m = min(min_m1, min_m2, min_m3), mean_m = mean(c(mean_m1, mean_m2, mean_m3)), max_m = max(c(max_m1, max_m2, max_m3))) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      df_predictors_all_multi <- rbind(df_predictors_all_multi, df_predictors)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

df_predictors_all_multi <- rbind(
  df_predictors_all_multi,
  df_predictors_all_multi %>%
    filter(outcome != "grades") %>%
    summarize_all(mean) %>%
    mutate(outcome = "personality", ml_algo = "postlasso")
)

# for personality also lasso coefficients are extracted
df_predictors_pers_lasso_multi <- data.frame()
for (outcome_var_sel in c("agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      # load predictors
      df_predictors <- 
        rbind(get(load_pred_algo)[[1]]$coef_lasso, get(load_pred_algo)[[2]]$coef_lasso) %>%
        rbind(get(load_pred_algo)[[3]]$coef_lasso) %>% rbind(get(load_pred_algo)[[4]]$coef_lasso) %>%
        rbind(get(load_pred_algo)[[5]]$coef_lasso)
      
      # calculate statistics
      df_predictors <- df_predictors %>%
        summarize(min_m1 = min(m1), mean_m1 = mean(m1), max_m1 = max(m1),
                  min_m2 = min(m2), mean_m2 = mean(m2), max_m2 = max(m2),
                  min_m3 = min(m3), mean_m3 = mean(m3), max_m3 = max(m3),
                  min_g1 = min(g1), mean_g1 = mean(g1), max_g1 = max(g1),
                  min_g2 = min(g2), mean_g2 = mean(g2), max_g2 = max(g2),
                  min_g3 = min(g3), mean_g3 = mean(g3), max_g3 = max(g3)) %>%
        mutate(min_m = min(min_m3, min_m2, min_m1), mean_m = mean(c(mean_m3, mean_m2, mean_m1)), 
               max_m = max(max_m3, max_m2, max_m1),
               min_g = min(min_g3, min_g2, min_g1), mean_g = mean(c(mean_g3, mean_g2, mean_g1)), 
               max_g = max(max_g3, max_g2, max_g1)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = "lasso") %>%
        dplyr::select(outcome, ml_algo, everything())
      df_predictors_pers_lasso_multi <- rbind(df_predictors_pers_lasso_multi, df_predictors)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

df_predictors_pers_lasso_multi <- rbind(
  df_predictors_pers_lasso_multi,
  df_predictors_pers_lasso_multi %>% 
    filter(outcome != "grades") %>% 
    summarize_all(mean) %>%
    mutate(outcome = "personality", ml_algo = "lasso")
)

df_predictors_all_multi <- rbind(df_predictors_all_multi, df_predictors_pers_lasso_multi)


#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++COMMON SUPPORT++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Check for dropped observations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
#++++++++++++++++++++++++++++#

# main: min-max trimming -> most extreme values by not considering all data sets as one
df_trimming_drop_main_binary <- df_dml_main_binary %>%
  filter(
    cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
    treatment_repl == main_treatment_repl, extra_act == main_extra_act,
    model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
    model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
    model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
    model_covbal == "yes"
  ) %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)
df_trimming_drop_main_binary


## Multivalued Treatment Setting ##
#+++++++++++++++++++++++++++++++++#

df_trimming_drop_main_multi <- df_dml_main_multi %>%
  filter(
    cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
    treatment_repl == main_treatment_repl, extra_act == main_extra_act,
    model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
    model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
    model_controls_endog == main_model_controls_endog, 
    model_covbal == "yes", model_hyperparam_sel == "best", Prob_norm == "yes",
    model_k_tuning %in% c(1, 2)
  ) %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)


#++++++++++++++++++++#
#### Create Plots ####
#++++++++++++++++++++#

## Binary Treatment Setting ##
#++++++++++++++++++++++++++++#

list_binary_plot_common_support <- list()
trimming <- "min-max"

# common support plot across all mice data frames and all repetitions
# All machine learning models and outcome are considered
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions before trimming
      df_dml_pred_all_binary <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$pred_bef_trimming %>% mutate(MICE = mice_sel) 
        df_dml_pred_all_binary <- rbind(df_dml_pred_all_binary, df_dml_pred_sub)
      }
      
      # extract trimming information
      if (trimming == "min-max") {
        df_dml_trimming_all_binary <- df_dml_pred_all_binary %>%
          group_by(treatment) %>%
          summarize(min_m = min(m), max_m = max(m)) %>%
          mutate(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
          dplyr::select(-c(min_m, max_m))
      } else {
        df_dml_trimming_all_binary <- data.frame()
        for (mice_sel in 1:5) {
          df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$trimming %>% mutate(MICE = mice_sel)
          df_dml_trimming_all_binary <- rbind(df_dml_trimming_all_binary, df_dml_pred_sub)
        }
      }

      
      # append predictions and trimming
      if (trimming == "min-max") {
        df_dml_pred_all_binary_sub <- df_dml_pred_all_binary
        df_dml_pred_all_binary_sub$min_trimming <- unique(df_dml_trimming_all_binary$min_trimming)
        df_dml_pred_all_binary_sub$max_trimming <- unique(df_dml_trimming_all_binary$max_trimming)
      } else {
        df_dml_pred_all_binary_sub <- df_dml_pred_all_binary %>% 
          left_join(df_dml_trimming_all_binary, by = c("Repetition", "MICE"))
      }


      # plot
      if (str_detect(outcome_var_sel, "grades")) {
        dec_places_sel <- 4
      } else {dec_places_sel <- 8}
      
      binary_plot_common_support_sub <- func_dml_common_support(
        "binary", df_dml_pred_all_binary_sub, 
        unique(df_dml_pred_all_binary_sub$min_trimming), unique(df_dml_pred_all_binary_sub$max_trimming), 
        "no", "no", model_algo_sel, dec_places_sel, bar_border = NA)
      list_binary_plot_common_support[[outcome_var_sel]][[model_algo_sel]] <- binary_plot_common_support_sub
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
    
}

# Grades
binary_plot_common_support <- ggarrange(
  list_binary_plot_common_support$grades$lasso + 
    ggtitle("LASSO") +  scale_y_continuous(breaks = c(1, 3), limits = c(0, 4.2), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)),
  list_binary_plot_common_support$grades$postlasso + ylab("") +  
    ggtitle("Post-LASSO") + scale_y_continuous(breaks = c(1, 3), limits = c(0, 4.2), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)),
  list_binary_plot_common_support$grades$rf + ylab("") + 
    ggtitle("Random forests") +  scale_y_continuous(breaks = c(1, 3), limits = c(0, 4.2), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)), 
  list_binary_plot_common_support$grades$xgb + ylab("") + 
    ggtitle("XGBoost") +  scale_y_continuous(breaks = c(1, 3), limits = c(0, 4.2), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)),
  nrow = 1, ncol = 4, common.legend = T, legend = "bottom"
) 

pdf("Output/DML/Common_Support/dml_plot_common_support_binary_grades_allalgos_all.pdf",
    width = 30, height = 8, pointsize = 25, family = "Helvetica")
print(binary_plot_common_support)
dev.off()


# Personality
binary_plot_common_support_personality <- ggarrange(
  list_binary_plot_common_support$agree$postlasso + 
    scale_y_continuous(breaks = c(1, 2),  limits = c(0, 2.5), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
    ggtitle("Agreeableness"), 
  list_binary_plot_common_support$consc$postlasso + ylab("") + ggtitle("Conscientiousness") +
    scale_y_continuous(breaks = c(1, 2),  limits = c(0, 2.5), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)),
  list_binary_plot_common_support$extra$postlasso + ylab("") +
    scale_y_continuous(breaks = c(1, 2),  limits = c(0, 2.5), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
    ggtitle("Extroversion"), 
  list_binary_plot_common_support$neuro$postlasso + ylab("") + 
    scale_y_continuous(breaks = c(1, 2),  limits = c(0, 2.5), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
    ggtitle("Neuroticism"),
  list_binary_plot_common_support$open$postlasso + ylab("") + 
    scale_y_continuous(breaks = c(1, 2),  limits = c(0, 2.5), expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 40), 
          axis.text = element_text(size = 38), axis.title = element_text(size = 38),
          legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
    ggtitle("Openness"),
  nrow = 1, ncol = 5, common.legend = T, legend = "bottom"
) 

pdf("Output/DML/Common_Support/dml_plot_common_support_binary_personality_postlasso_all.pdf",
    width = 30, height = 8, pointsize = 25, family = "Helvetica")
print(binary_plot_common_support_personality)
dev.off()

# For MICE = 1, K = 1, S = 1
list_binary_plot_common_support_fold <- list()
trimming <- "min-max"
mice_sel <- 1
rep_sel <- 1
fold_sel <- 1

for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions before trimming
      df_dml_pred_all_binary <- get(load_pred_algo)[[mice_sel]]$pred_bef_trimming %>% mutate(MICE = mice_sel) 
      df_dml_pred_all_binary <- df_dml_pred_all_binary %>%
        filter(Repetition == rep_sel, Fold == fold_sel)
      
      # extract trimming information
      if (trimming == "min-max") {
        df_dml_trimming_all_binary <- df_dml_pred_all_binary %>%
          group_by(treatment) %>%
          summarize(min_m = min(m), max_m = max(m)) %>%
          mutate(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
          dplyr::select(-c(min_m, max_m))
      } else {
        df_dml_trimming_all_binary <- data.frame()
        for (mice_sel in 1:5) {
          df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$trimming %>% mutate(MICE = mice_sel)
          df_dml_trimming_all_binary <- rbind(df_dml_trimming_all_binary, df_dml_pred_sub)
        }
      }
      
      
      # append predictions and trimming
      if (trimming == "min-max") {
        df_dml_pred_all_binary_sub <- df_dml_pred_all_binary
        df_dml_pred_all_binary_sub$min_trimming <- unique(df_dml_trimming_all_binary$min_trimming)
        df_dml_pred_all_binary_sub$max_trimming <- unique(df_dml_trimming_all_binary$max_trimming)
      } else {
        df_dml_pred_all_binary_sub <- df_dml_pred_all_binary %>% 
          left_join(df_dml_trimming_all_binary, by = c("Repetition", "MICE"))
      }
      
      
      # plot
      dec_places_sel <- 4
      
      binary_plot_common_support_sub <- func_dml_common_support(
        "binary", df_dml_pred_all_binary_sub, 
        unique(df_dml_pred_all_binary_sub$min_trimming), unique(df_dml_pred_all_binary_sub$max_trimming), 
        "yes", "yes", model_algo_sel, dec_places_sel, bar_border = NA)
      list_binary_plot_common_support_fold[[outcome_var_sel]][[model_algo_sel]] <- binary_plot_common_support_sub
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
}

binary_plot_common_support_fold <- ggarrange(
  list_binary_plot_common_support_fold$grades$lasso + xlab("") + ggtitle("LASSO") +
    scale_y_continuous(breaks = c(1, 3, 5),  limits = c(0, 5.2), expand = c(0, 0)), 
  list_binary_plot_common_support_fold$grades$postlasso + xlab("") + ylab("") + ggtitle("Post-LASSO") +
    scale_y_continuous(breaks = c(1, 3, 5),  limits = c(0, 5.2), expand = c(0, 0)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  list_binary_plot_common_support_fold$grades$rf + ggtitle("Random forests") +
    scale_y_continuous(breaks = c(1, 3, 5),  limits = c(0, 5.2), expand = c(0, 0)), 
  list_binary_plot_common_support_fold$grades$xgb + ylab("") + ggtitle("XGBoost") +
    scale_y_continuous(breaks = c(1, 3, 5),  limits = c(0, 5.2), expand = c(0, 0)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 2, ncol = 2, common.legend = T, legend = "bottom"
) 

pdf("Output/DML/Common_Support/dml_plot_common_support_binary_grades_allalgos_fold.pdf",
    width = 25, height = 12, pointsize = 25, family = "Helvetica")
print(binary_plot_common_support_fold)
dev.off()


binary_plot_common_support_fold_pers <- ggarrange(
  list_binary_plot_common_support_fold$agree$postlasso + xlab("") + ggtitle("Agreeableness") +
    scale_y_continuous(breaks = c(1, 3),  limits = c(0, 3.2), expand = c(0, 0)),
  list_binary_plot_common_support_fold$consc$postlasso + xlab("") + ylab("") + ggtitle("Conscientiousness") +
    scale_y_continuous(breaks = c(1, 3),  limits = c(0, 3.2), expand = c(0, 0)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  list_binary_plot_common_support_fold$extra$postlasso + xlab("") + ylab("") + ggtitle("Extroversion") +
    scale_y_continuous(breaks = c(1, 3),  limits = c(0, 3.2), expand = c(0, 0)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  list_binary_plot_common_support_fold$neuro$postlasso + ggtitle("Neuroticism") +
    scale_y_continuous(breaks = c(1, 3),  limits = c(0, 3.2), expand = c(0, 0)),
  list_binary_plot_common_support_fold$open$postlasso + ylab("") + ggtitle("Openness") +
    scale_y_continuous(breaks = c(1, 3),  limits = c(0, 3.2), expand = c(0, 0)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 2, ncol = 3, common.legend = T, legend = "bottom"
) 

pdf("Output/DML/Common_Support/dml_plot_common_support_binary_personality_postlasso_fold.pdf",
    width = 25, height = 12, pointsize = 25, family = "Helvetica")
print(binary_plot_common_support_fold_pers)
dev.off()



## Multivalued Treatment Setting ##
#+++++++++++++++++++++++++++++++++#

# common support plot across all mice data frames and all repetitions
list_multi_plot_common_support <- list()
trimming <- "min-max"

# common support plot across all mice data frames and all repetitions
# All machine learning models and outcome are considered
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions before trimming
      df_dml_pred_all_multi <- data.frame()
      for (mice_sel in 1:5) {
        df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$pred_bef_trimming %>% mutate(MICE = mice_sel) 
        df_dml_pred_all_multi <- rbind(df_dml_pred_all_multi, df_dml_pred_sub)
      }
      
      # extract trimming information
      if (trimming == "min-max") {
        df_dml_trimming_all_multi <-
          # m1
          df_dml_pred_all_multi %>% 
          mutate(treatment_1 = ifelse(treatment == 1, 1, 0)) %>%
          group_by(treatment_1) %>% 
          summarise(min_m = min(m1), max_m = max(m1)) %>%
          summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
          mutate(model = "m1") %>% rbind(
            # m2
            df_dml_pred_all_multi %>% 
              mutate(treatment_2 = ifelse(treatment == 2, 1, 0)) %>%
              group_by(treatment_2) %>% 
              summarise(min_m = min(m2), max_m = max(m2)) %>%
              summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
              mutate(model = "m2")
          ) %>% rbind(
            # m3
            df_dml_pred_all_multi %>% 
              mutate(treatment_3 = ifelse(treatment == 3, 1, 0)) %>%
              group_by(treatment_3) %>% 
              summarise(min_m = min(m3), max_m = max(m3)) %>%
              summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
              mutate(model = "m3")
          )
      } else {
        df_dml_trimming_all_multi <- data.frame()
        for (mice_sel in 1:5) {
          df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$trimming %>% mutate(MICE = mice_sel)
          df_dml_trimming_all_multi <- rbind(df_dml_trimming_all_multi, df_dml_pred_sub)
        }
      }
      

      # plot
      multi_plot_common_support_sub <- func_dml_common_support(
        "multi", df_dml_pred_all_multi, 
        df_dml_trimming_all_multi %>% dplyr::select(model, min_trimming), 
        df_dml_trimming_all_multi %>% dplyr::select(model, max_trimming), 
        "no", "no", model_algo_sel, 4, NA)
      list_multi_plot_common_support[[outcome_var_sel]][[model_algo_sel]] <- multi_plot_common_support_sub
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
}


multi_plot <- ggarrange(
  list_multi_plot_common_support$grades$postlasso$m1 +
    scale_y_continuous(breaks = c(1,3,5), limits = c(0,5), expand = c(0,0)) + ggtitle("Weekly"),
  list_multi_plot_common_support$grades$postlasso$m2 + ylab("") + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    scale_y_continuous(breaks = c(1,3,5), limits = c(0,5), expand = c(0,0)) + ggtitle("Monthly"),
  list_multi_plot_common_support$grades$postlasso$m3 + ylab("") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    scale_y_continuous(breaks = c(1,3,5), limits = c(0,5), expand = c(0,0)) + ggtitle("Never"),
  nrow = 1, common.legend = TRUE, legend = "bottom"
)

pdf("Output/DML/Common_Support/dml_plot_common_support_multi_grades_postlasso_all.pdf",
    width = 20, height = 10, pointsize = 5, family = "Helvetica")
print(multi_plot)
dev.off()


#### Check Means ####
#+++++++++++++++++++#

## Binary Treatment Setting ##
df_quantiles_binary <- data.frame()
num_quantiles <- 5
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_quantiles_aggr <- data.frame()
      for (micel_sel in 1:length(get(load_pred_algo))) {
        df_pred_m <- get(load_pred_algo)[[micel_sel]]$pred_bef %>% dplyr::select(m, treatment) %>% arrange(m)
        df_quantiles <- df_pred_m %>% mutate(quint = ntile(m, num_quantiles))
        df_quantiles <- df_quantiles %>% 
          group_by(quint, treatment) %>% 
          summarize(mean_prob = mean(m)) %>%
          mutate(MICE = micel_sel)
        df_quantiles_aggr <- rbind(df_quantiles_aggr, df_quantiles) 
      }
      df_quantiles_aggr <- df_quantiles_aggr %>%
        group_by(quint, treatment) %>%
        summarize(mean_prob = mean(mean_prob)) %>%
        mutate(model_algo = model_algo_sel, outcome = outcome_var_sel) %>%
        dplyr::select(outcome, model_algo, everything())
      
      df_quantiles_binary <- rbind(df_quantiles_binary, df_quantiles_aggr)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


## Multivalued Treatment Setting ##
df_quantiles_multi <- data.frame()
num_quantiles <- 5
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_quantiles_aggr <- data.frame()
      for (micel_sel in 1:5) {
        # m1, m2 ,m3
        df_pred_m1 <- get(load_pred_algo)[[micel_sel]]$pred_bef_trimming %>% dplyr::select(m1, treatment) %>% arrange(m1)
        df_quantiles_m1 <- df_pred_m1 %>% mutate(quint = ntile(m1, num_quantiles))
        df_quantiles_m1 <- df_quantiles_m1 %>% 
          group_by(quint, treatment) %>% 
          summarize(mean_prob_m1 = mean(m1)) %>%
          mutate(MICE = micel_sel)
        df_pred_m2 <- get(load_pred_algo)[[micel_sel]]$pred_bef_trimming %>% dplyr::select(m2, treatment) %>% arrange(m2)
        df_quantiles_m2 <- df_pred_m2 %>% mutate(quint = ntile(m2, num_quantiles))
        df_quantiles_m2 <- df_quantiles_m2 %>% 
          group_by(quint, treatment) %>% 
          summarize(mean_prob_m2 = mean(m2)) %>%
          mutate(MICE = micel_sel)
        df_pred_m3 <- get(load_pred_algo)[[micel_sel]]$pred_bef_trimming %>% dplyr::select(m3, treatment) %>% arrange(m3)
        df_quantiles_m3 <- df_pred_m3 %>% mutate(quint = ntile(m3, num_quantiles))
        df_quantiles_m3 <- df_quantiles_m3 %>% 
          group_by(quint, treatment) %>% 
          summarize(mean_prob_m3 = mean(m3)) %>%
          mutate(MICE = micel_sel)
        # merge
        df_quantiles_m <- left_join(df_quantiles_m1, df_quantiles_m2, by = c("quint", "treatment", "MICE")) %>%
          left_join(df_quantiles_m3, by = c("quint", "treatment", "MICE"))
        
        df_quantiles_aggr <- rbind(df_quantiles_aggr, df_quantiles_m) 
      }
      df_quantiles_aggr <- df_quantiles_aggr %>%
        group_by(quint, treatment) %>%
        summarize(mean_prob_m1 = mean(mean_prob_m1), mean_prob_m2 = mean(mean_prob_m2), 
                  mean_prob_m3 = mean(mean_prob_m3)) %>%
        mutate(model_algo = model_algo_sel, outcome = outcome_var_sel) %>%
        dplyr::select(outcome, model_algo, everything())
      
      df_quantiles_multi <- rbind(df_quantiles_multi, df_quantiles_aggr)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}


#### Summary Statistics: Propensity Score Predictions ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Summary statistics are calculated before trimming. Precisely, the minimum,
# mean, and maximum of the predicted treatment probabilities is created.

## BINARY ##
df_pred_bef_trimming <- c()
for (model_algo_sel in c("lasso", "postlasso", "xgb", "rf")) {
  get_algo <- get(paste0(model_algo_sel, "_grades"))
  for (mice_sel in 1:length(get_algo)) {
    df_pred_bef_trimming <- rbind(
      df_pred_bef_trimming,
      get_algo[[mice_sel]]$pred_bef_trimming %>% dplyr::select(m) %>% 
        mutate(model_algo = model_algo_sel, outcome = "GPA", m0 = 1 - m, m1 = m)
    )
  }
}

for (outcome_sel in c("agree", "consc", "extra", "open", "neuro")) {
  for (mice_sel in 1:5) {
    get_algo <- get(paste0("postlasso_", outcome_sel))
    df_pred_bef_trimming <- rbind(
      df_pred_bef_trimming,
      get_algo[[mice_sel]]$pred_bef_trimming %>% dplyr::select(m) %>% 
        mutate(model_algo = "postlasso", outcome = outcome_sel, m0 = 1 - m, m1 = m)
    )
  }
}


df_pred_bef_trimming %>% group_by(model_algo, outcome) %>%
  summarize(min_m1 = min(m1), mean_m1 = mean(m1), max_m1 = max(m1),
            min_m0 = min(m0), mean_m0 = mean(m0), max_m0 = max(m0)) %>%
  ungroup() %>% as.data.frame()


## MULTI ##
df_pred_bef_trimming_multi <- c()
for (model_algo_sel in c("lasso", "postlasso", "xgb", "rf")) {
  get_algo <- get(paste0(model_algo_sel, "_grades_multi"))
  for (mice_sel in 1:length(get_algo)) {
    df_pred_bef_trimming_multi <- rbind(
      df_pred_bef_trimming_multi,
      get_algo[[mice_sel]]$pred_bef_trimming %>% dplyr::select(m1, m2, m3) %>% mutate(model_algo = model_algo_sel, outcome = "GPA")
    )
  }
}

for (outcome_sel in c("agree", "consc", "extra", "open", "neuro")) {
  for (mice_sel in 1:5) {
    get_algo <- get(paste0("postlasso_", outcome_sel, "_multi"))
    df_pred_bef_trimming_multi <- rbind(
      df_pred_bef_trimming_multi,
      get_algo[[mice_sel]]$pred_bef_trimming %>% dplyr::select(m1, m2, m3) %>% mutate(model_algo = "postlasso", outcome = outcome_sel)
    )
  }
}


df_pred_bef_trimming_multi %>% group_by(model_algo, outcome) %>%
  summarize(min_m1 = min(m1), mean_m1 = mean(m1), max_m1 = max(m1),
            min_m2 = min(m2), mean_m2 = mean(m2), max_m2 = max(m2),
            min_m3 = min(m3), mean_m3 = mean(m3), max_m3 = max(m3)) %>%
  as.data.frame()


## Unconditional Treatment Probabilities ##
data_grades_binary %>% group_by(treatment_sport) %>% count() %>%
  mutate(n = n / data_grades_binary %>% group_by(treatment_sport) %>% count() %>% pull(n) %>% sum()*100) %>%
  as.data.frame()

data_personality_binary %>% group_by(treatment_sport) %>% count() %>%
  mutate(n = n / data_personality_binary %>% group_by(treatment_sport) %>% count() %>% pull(n) %>% sum()*100) %>%
  as.data.frame()


data_grades_multi %>% group_by(treatment_sport_freq) %>% count() %>%
  mutate(n = n / data_grades_multi %>% group_by(treatment_sport_freq) %>% count() %>% pull(n) %>% sum()*100) %>%
  as.data.frame()

data_personality_multi %>% group_by(treatment_sport_freq) %>% count() %>%
  mutate(n = n / data_personality_multi %>% group_by(treatment_sport_freq) %>% count() %>% pull(n) %>% sum()*100) %>%
  as.data.frame()



#%%%%%%%%%%%%%%%%%%%%%%%#
#### HYPERPARAMETERS ####
#%%%%%%%%%%%%%%%%%%%%%%%#

## BINARY TREATMENT SETTING ##
df_hyperparam_lasso_all_binary <- data.frame()
for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
  for (model_algo_sel in c("lasso", "postlasso")) {
    
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_hyperparam_lasso <- data.frame()
      for (micel_sel in 1:5) {
        df_hyperparam_lasso_sub <- get(load_pred_algo)[[mice_sel]]$param
        df_hyperparam_lasso <- rbind(df_hyperparam_lasso, df_hyperparam_lasso_sub)
      } # close for loop over mice_sel
      df_hyperparam_lasso <- df_hyperparam_lasso %>%
        summarize(m = mean(m), g0 = mean(g0), g1 = mean(g1)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      df_hyperparam_lasso_all_binary <- rbind(df_hyperparam_lasso_all_binary, df_hyperparam_lasso)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop over model_algo_sel
} # close for loop over outcome_var_sel
  
  

## MULTIVALUED TREATMENT SETTING ##
df_hyperparam_lasso_all_multi <- data.frame()
for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
  for (model_algo_sel in c("lasso", "postlasso")) {
    
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_hyperparam_lasso <- data.frame()
      for (micel_sel in 1:5) {
        df_hyperparam_lasso_sub <- get(load_pred_algo)[[mice_sel]]$param
        df_hyperparam_lasso <- rbind(df_hyperparam_lasso, df_hyperparam_lasso_sub)
      } # close for loop over mice_sel
      df_hyperparam_lasso <- df_hyperparam_lasso %>%
        summarize(m1 = mean(m1), m2 = mean(m2), m3 = mean(m3),
                  g1 = mean(g1), g2 = mean(g2), g3 = mean(g3)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      df_hyperparam_lasso_all_multi <- rbind(df_hyperparam_lasso_all_multi, df_hyperparam_lasso)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop over model_algo_sel
} # close for loop over outcome_var_sel


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: SENSITIVITY WRT EXTRACURRICULAR ACTIVITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## include inactive students ##
df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc")) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", extra_act == "no") %>%
  dplyr::select(model_hyperparam_sel, num_predictors_m1, starts_with("ACC"),  starts_with("BACC"), starts_with("AUC"),
                starts_with("RMSE"), starts_with("MAPE"), starts_with("n_treats")) %>%
  distinct() %>%
  group_by(model_hyperparam_sel) %>%
  mutate(ACC = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
         AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), RMSE = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), 
         MAPE = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))) %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>% 
  dplyr::select(model_hyperparam_sel, n_treats_diff_perf, num_predictors_m1, starts_with("ACC"), 
                starts_with("BACC"), starts_with("AUC"), starts_with("RMSE")) %>%
  as.data.frame()


ml_grades_pred <- data.frame()
for (mice_sel in 1:length(postlasso_grades_rc2_noextra )) {
  ml_grades_pred_sub <- left_join(postlasso_grades_rc2_noextra[[mice_sel]]$pred, 
                                  postlasso_grades_rc2_noextra[[mice_sel]]$trimming, 
                                  by = "Repetition") %>%
    mutate(MICE = mice_sel)
  ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
}


df_pred <- ml_grades_pred %>% 
  mutate(
    outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
    outcome = outcome_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g1 = g1_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g2 = g2_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g3 = g3_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean
  )


df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
  dplyr::select(-c(Repetition, Fold))

df_error <- df_error_sub %>%
  mutate(RRMSE_g1 = (RMSE_g1 / data_stand_grades_multi$mean_weekly)*100, 
         RRMSE_g2 = (RMSE_g2 / data_stand_grades_multi$mean_monthly)*100,
         RRMSE_g3 = (RMSE_g3 / data_stand_grades_multi$mean_never)*100) 

df_error %>%
  dplyr::select(starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


## active within uni ##

postlasso_grades_extrauni <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extrauni_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

df_dml_main_multi %>% filter(extra_act == "uni") %>% pull(num_predictors_m1) %>% unique() # number of predictors: 158.77

df_dml_main_multi %>% # percentage of dropped observations: 1.9090%
  filter(extra_act == "uni") %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)

df_error_extrauni <- data.frame()
for (outcome_var_sel in c("grades")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_extrauni")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean_multi <- data_stand_grades_multi
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      } else {
        df_sd_mean_multi <- data_stand_pers_multi %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean_multi) <- 
          str_remove(colnames(df_sd_mean_multi), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g1 = (RMSE_g1 / df_sd_mean_multi$mean_weekly)*100, 
               RRMSE_g2 = (RMSE_g2 / df_sd_mean_multi$mean_monthly)*100,
               RRMSE_g3 = (RMSE_g3 / df_sd_mean_multi$mean_never)*100) 
      
      # bind row
      df_error_extrauni <- rbind(df_error_extrauni, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

df_error_extrauni %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: TREATMENT GENERATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


df_dml_main_multi %>% filter(cohort_prep == "controls_bef_all") %>% pull(num_predictors_m1) %>% unique() # number of predictors: 154.81

df_dml_main_multi %>% # percentage of dropped observations: 3.68%
  filter(cohort_prep == "controls_bef_all") %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)

df_error_befall <- data.frame()
for (outcome_var_sel in c("grades")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_rc1_befall")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean_multi <- data_stand_grades_multi
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      } else {
        df_sd_mean_multi <- data_stand_pers_multi %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean_multi) <- 
          str_remove(colnames(df_sd_mean_multi), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g1 = (RMSE_g1 / df_sd_mean_multi$mean_weekly)*100, 
               RRMSE_g2 = (RMSE_g2 / df_sd_mean_multi$mean_monthly)*100,
               RRMSE_g3 = (RMSE_g3 / df_sd_mean_multi$mean_never)*100) 
      
      # bind row
      df_error_befall <- rbind(df_error_befall, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

df_error_befall %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: SENSITIVITY WRT CONTROL VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## POLYS ##
df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc")) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_type == "allpoly") %>%
  dplyr::select(model_hyperparam_sel, num_predictors_m1, starts_with("ACC"),  starts_with("BACC"), starts_with("AUC"),
                starts_with("RMSE"), starts_with("MAPE"), starts_with("n_treats")) %>%
  distinct() %>%
  group_by(model_hyperparam_sel) %>%
  mutate(ACC = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
         AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), RMSE = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), 
         MAPE = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))) %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>% 
  dplyr::select(model_hyperparam_sel, n_treats_diff_perf, num_predictors_m1, starts_with("ACC"), 
                starts_with("BACC"), starts_with("AUC")) %>%
  as.data.frame()

postlasso_grades_rc5_polys[[1]]$coef %>% filter(estimate > 0) %>% filter(str_detect(term, "order"))

## NO ENDOGENOUS CONTROLS ##
df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc")) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_controls_endog == "no") %>%
  dplyr::select(model_hyperparam_sel, num_predictors_m1, starts_with("ACC"),  starts_with("BACC"), starts_with("AUC"),
                starts_with("RMSE"), starts_with("MAPE"), starts_with("n_treats")) %>%
  distinct() %>%
  group_by(model_hyperparam_sel) %>%
  mutate(ACC = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
         AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), RMSE = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), 
         MAPE = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))) %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>% 
  dplyr::select(model_hyperparam_sel, n_treats_diff_perf, num_predictors_m1) %>%
  as.data.frame()


ml_grades_pred <- data.frame()
for (mice_sel in 1:length(postlasso_grades_rc3_endog)) {
  ml_grades_pred_sub <- left_join(postlasso_grades_rc3_endog[[mice_sel]]$pred, 
                                  postlasso_grades_rc3_endog[[mice_sel]]$trimming, 
                                  by = "Repetition") %>%
    mutate(MICE = mice_sel)
  ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
}
      

df_pred <- ml_grades_pred %>% 
  mutate(
    # standardized outcomes
    outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
    # original scale
    outcome = outcome_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g1 = g1_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g2 = g2_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g3 = g3_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean
    )
  

df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
  dplyr::select(-c(Repetition, Fold))
      
df_error <- df_error_sub %>%
  mutate(RRMSE_g1 = (RMSE_g1 / data_stand_grades_multi$mean_weekly)*100, 
         RRMSE_g2 = (RMSE_g2 / data_stand_grades_multi$mean_monthly)*100,
         RRMSE_g3 = (RMSE_g3 / data_stand_grades_multi$mean_never)*100) 

df_error %>%
  dplyr::select(starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


## NO LAGS ##
df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc")) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_controls_lag == "only_lags") %>%
  dplyr::select(model_hyperparam_sel, num_predictors_m1, starts_with("ACC"),  starts_with("BACC"), starts_with("AUC"),
                starts_with("RMSE"), starts_with("MAPE"), starts_with("n_treats")) %>%
  distinct() %>%
  group_by(model_hyperparam_sel) %>%
  mutate(ACC = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
         AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), RMSE = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), 
         MAPE = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))) %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>% 
  dplyr::select(model_hyperparam_sel, n_treats_diff_perf, num_predictors_m1) %>%
  as.data.frame()


ml_grades_pred <- data.frame()
for (mice_sel in 1:length(postlasso_grades_rc4_lags)) {
  ml_grades_pred_sub <- left_join(postlasso_grades_rc4_lags[[mice_sel]]$pred, 
                                  postlasso_grades_rc4_lags[[mice_sel]]$trimming, 
                                  by = "Repetition") %>%
    mutate(MICE = mice_sel)
  ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
}


df_pred <- ml_grades_pred %>% 
  mutate(
    # standardized outcomes
    outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
    # original scale
    outcome = outcome_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g1 = g1_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g2 = g2_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean,
    g3 = g3_stand*data_stand_grades_multi$sd + data_stand_grades_multi$mean
  )


df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
  dplyr::select(-c(Repetition, Fold))

df_error <- df_error_sub %>%
  mutate(RRMSE_g1 = (RMSE_g1 / data_stand_grades_multi$mean_weekly)*100, 
         RRMSE_g2 = (RMSE_g2 / data_stand_grades_multi$mean_monthly)*100,
         RRMSE_g3 = (RMSE_g3 / data_stand_grades_multi$mean_never)*100) 

df_error %>%
  dplyr::select(starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: SENSITIVITY WRT HYPERPARAMETER CHOICES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# error metrics and trimming (not reported in paper, only written)
df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  model_hyperparam_sel == "best" &
      model_covbal == "yes", "main", "rc")) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_hyperparam_sel %in% c("1SE", "1SE_plus")) %>%
  dplyr::select(model_hyperparam_sel, num_predictors_m1, starts_with("ACC"),  starts_with("BACC"), starts_with("AUC"),
                starts_with("RMSE"), starts_with("MAPE"), starts_with("n_treats")) %>%
  distinct() %>%
  group_by(model_hyperparam_sel) %>%
  mutate(ACC = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
         AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), RMSE = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), 
         MAPE = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))) %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>% 
  dplyr::select(model_hyperparam_sel, n_treats_diff_perf, num_predictors_m1, starts_with("ACC"), 
                starts_with("BACC"), starts_with("AUC"), starts_with("RMSE")) %>%
  as.data.frame()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: SENSITIVITY WRT TRIMMING THRESHOLDS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## FROM ANALYSIS DIRECTLY ##

df_dml_main_multi %>%
  filter(model_algo == "postlasso", outcome == "grade", extra_act == main_extra_act,
         cohort_prep == main_cohort_prep, treatment_repl == main_treatment_repl,
         treatment_def == main_treatment_def, model_type == main_model_type,
         model_k == main_model_k, model_k_tuning == main_model_k_tuning,
         model_s_rep == main_model_s_rep, model_controls_lag == main_model_controls_lag,
         model_controls_endog == main_model_controls_endog, model_covbal == "yes", Prob_norm == "yes",
         model_hyperparam_sel == "best", Type %in% c("ATE"), Treatment == "monthly_weekly") %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100) %>%
  dplyr::select(model_trimming, n_treats_diff_perf)

# enforce other trimming thresholds to assess the sensitivity of the treatment
# effect estimates wrt to the trimming thresholds

## BINARY ##
df_effects_trimming_binary <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions
      for (mice_sel in 1:length(get(load_pred_algo))) { # iterate over MICE
        ml_grades_pred_sub <- get(load_pred_algo)[[mice_sel]]$pred_bef_trimming %>% 
          dplyr::select(Repetition, Fold, outcome, treatment, m, g1, g0) %>%
          rename(outcome_grade = outcome, treatment_sport = treatment) %>%
          mutate(treatment_sport = as.numeric(as.character(treatment_sport)))
        
        # apply trimming thresholds: min-max and 0.01 and 0.1
        for (rep_sel in 1:max(ml_grades_pred_sub$Repetition)) { # iterate over repetition
          # generate vectors to save results
          theta_ATE_no_all <- c()
          score_ATE_no_all <- c()
          theta_ATTE_no_all <- c()
          score_ATTE_no_all <- c()
          theta_ATE_001_all <- c()
          score_ATE_001_all <- c()
          theta_ATTE_001_all <- c()
          score_ATTE_001_all <- c()
          theta_ATE_01_all <- c()
          score_ATE_01_all <- c()
          theta_ATTE_01_all <- c()
          score_ATTE_01_all <- c()
          theta_ATE_minmax_all <- c()
          score_ATE_minmax_all <- c()
          theta_ATTE_minmax_all <- c()
          score_ATTE_minmax_all <- c()
          for (fold_sel in 1:max(ml_grades_pred_sub$Fold)) { # iterate over folds
            # subset on repetition and fold
            ml_grades_pred_sub_fold <- ml_grades_pred_sub %>% filter(Repetition == rep_sel, Fold == fold_sel)
            # 0.01 trimming
            indices_keep_001 <- which(between(ml_grades_pred_sub_fold$m, 0.01, 0.99))
            # 0.1 trimming
            indices_keep_01 <- which(between(ml_grades_pred_sub_fold$m, 0.1, 0.9))
            # min-max trimming
            df_minmax_trimming <- ml_grades_pred_sub_fold %>% group_by(treatment_sport) %>% 
              summarise(min_m = min(m), max_m = max(m))
            indices_kepp_minmax <- which(
              between(ml_grades_pred_sub_fold$m, max(df_minmax_trimming$min_m), min(df_minmax_trimming$max_m))
            )
            
            # enforce common support
            data_pred_no <- ml_grades_pred_sub_fold
            data_pred_001 <- ml_grades_pred_sub_fold[indices_keep_001, ]
            data_pred_01 <- ml_grades_pred_sub_fold[indices_keep_01, ]
            data_pred_minmax <- ml_grades_pred_sub_fold[indices_kepp_minmax, ]
            
            # calculate treatment effects
            ls_treatment_effects_no <- func_dml_theta_score("binary", data_pred_no, data_pred_no, "outcome_grade", "treatment_sport")
            ls_treatment_effects_001 <- func_dml_theta_score("binary", data_pred_001, data_pred_001, "outcome_grade", "treatment_sport")
            ls_treatment_effects_01 <- func_dml_theta_score("binary", data_pred_01, data_pred_01, "outcome_grade", "treatment_sport")
            ls_treatment_effects_minmax <- func_dml_theta_score("binary", data_pred_minmax, data_pred_minmax, "outcome_grade", "treatment_sport")
            
            # store them in vector for inference
            theta_ATE_no <- ls_treatment_effects_no$theta_ATE
            theta_ATE_no_all <- c(theta_ATE_no_all, theta_ATE_no)
            score_ATE_no <- ls_treatment_effects_no$score_ATE
            score_ATE_no_all <- c(score_ATE_no_all, score_ATE_no)
            theta_ATTE_no <- ls_treatment_effects_no$theta_ATTE
            theta_ATTE_no_all <- c(theta_ATTE_no_all, theta_ATTE_no)
            score_ATTE_no <- ls_treatment_effects_no$score_ATTE
            score_ATTE_no_all <- c(score_ATTE_no_all, score_ATTE_no)
            
            theta_ATE_001 <- ls_treatment_effects_001$theta_ATE
            theta_ATE_001_all <- c(theta_ATE_001_all, theta_ATE_001)
            score_ATE_001 <- ls_treatment_effects_001$score_ATE
            score_ATE_001_all <- c(score_ATE_001_all, score_ATE_001)
            theta_ATTE_001 <- ls_treatment_effects_001$theta_ATTE
            theta_ATTE_001_all <- c(theta_ATTE_001_all, theta_ATTE_001)
            score_ATTE_001 <- ls_treatment_effects_001$score_ATTE
            score_ATTE_001_all <- c(score_ATTE_001_all, score_ATTE_001)
            
            theta_ATE_01 <- ls_treatment_effects_01$theta_ATE
            theta_ATE_01_all <- c(theta_ATE_01_all, theta_ATE_01)
            score_ATE_01 <- ls_treatment_effects_01$score_ATE
            score_ATE_01_all <- c(score_ATE_01_all, score_ATE_01)
            theta_ATTE_01 <- ls_treatment_effects_01$theta_ATTE
            theta_ATTE_01_all <- c(theta_ATTE_01_all, theta_ATTE_01)
            score_ATTE_01 <- ls_treatment_effects_01$score_ATTE
            score_ATTE_01_all <- c(score_ATTE_01_all, score_ATTE_01)
            
            theta_ATE_minmax <- ls_treatment_effects_minmax$theta_ATE
            theta_ATE_minmax_all <- c(theta_ATE_minmax_all, theta_ATE_minmax)
            score_ATE_minmax <- ls_treatment_effects_minmax$score_ATE
            score_ATE_minmax_all <- c(score_ATE_minmax_all, score_ATE_minmax)
            theta_ATTE_minmax <- ls_treatment_effects_minmax$theta_ATTE
            theta_ATTE_minmax_all <- c(theta_ATTE_minmax_all, theta_ATTE_minmax)
            score_ATTE_minmax <- ls_treatment_effects_minmax$score_ATTE
            score_ATTE_minmax_all <- c(score_ATTE_minmax_all, score_ATTE_minmax)

          }
          
          # inference
          df_result_ATE_no <- func_dml_inference("binary", "ATE", theta_ATE_no_all, score_ATE_no_all, length(score_ATE_no_all), rep_sel) %>%
            mutate(trimming = "no")
          df_result_ATTE_no <- func_dml_inference("binary", "ATTE", theta_ATTE_no_all, score_ATTE_no_all, length(score_ATTE_no_all), rep_sel) %>%
            mutate(trimming = "no")
          
          df_result_ATE_001 <- func_dml_inference("binary", "ATE", theta_ATE_001_all, score_ATE_001_all, length(score_ATE_001_all), rep_sel) %>%
            mutate(trimming = "001")
          df_result_ATTE_001 <- func_dml_inference("binary", "ATTE", theta_ATTE_001_all, score_ATTE_001_all, length(score_ATTE_001_all), rep_sel) %>%
            mutate(trimming = "001")
          
          df_result_ATE_01 <- func_dml_inference("binary", "ATE", theta_ATE_01_all, score_ATE_01_all, length(score_ATE_01_all), rep_sel) %>%
            mutate(trimming = "01")
          df_result_ATTE_01 <- func_dml_inference("binary", "ATTE", theta_ATTE_01_all, score_ATTE_01_all, length(score_ATTE_01_all), rep_sel) %>%
            mutate(trimming = "01")
          
          df_result_ATE_minmax <- func_dml_inference("binary", "ATE", theta_ATE_minmax_all, score_ATE_minmax_all, length(score_ATE_minmax_all), rep_sel) %>%
            mutate(trimming = "minmax")
          df_result_ATTE_minmax <- func_dml_inference("binary", "ATTE", theta_ATTE_minmax_all, score_ATTE_minmax_all, length(score_ATTE_minmax_all), rep_sel) %>%
            mutate(trimming = "minmax")
          
          # store in data frame
          df_effects_trimming_binary <- rbind(
            df_effects_trimming_binary,
            rbind(df_result_ATE_no, df_result_ATTE_no, df_result_ATE_minmax, df_result_ATTE_minmax,
                  df_result_ATE_001, df_result_ATTE_001, df_result_ATE_01, df_result_ATTE_01) %>%
              mutate(outcome = outcome_var_sel, mlalgo = model_algo_sel, MICE = mice_sel)
          )
          
        } # close iteration over rep_sel
      } # close iteration over mice_sel
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

df_effects_trimming_binary <- df_effects_trimming_binary %>%
  group_by(outcome, mlalgo, trimming, Treatment, Type) %>% 
  summarize_all(median) %>%
  dplyr::select(-Rep, MICE) %>%
  as.data.frame()

## MULTI ##
df_effects_trimming_multi <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions
      for (mice_sel in 1:length(get(load_pred_algo))) { # iterate over MICE
        ml_grades_pred_sub <- get(load_pred_algo)[[mice_sel]]$pred_bef_trimming %>% 
          dplyr::select(Repetition, Fold, treatment, outcome, m1, m2, m3, g1, g2, g3) %>%
          mutate(
            treatment = as.numeric(as.character(treatment)),
            treatment_sport_freq_weekly_atleast = ifelse(treatment == 1, 1, 0),
            treatment_sport_freq_monthly_less = ifelse(treatment == 2, 1, 0),
            treatment_sport_freq_never = ifelse(treatment == 3, 1, 0)
          ) %>% rename(
            outcome_grade = outcome, treatment_sport_freq = treatment
          )
        
        # apply trimming thresholds: min-max and 0.01 and 0.1
        for (rep_sel in 1:max(ml_grades_pred_sub$Repetition)) { # iterate over repetition
          # generate vectors to save results
          theta_ATE_no_all <- c()
          score_ATE_no_all <- c()
          theta_ATTE_no_all <- c()
          score_ATTE_no_all <- c()
          theta_ATE_001_all <- c()
          score_ATE_001_all <- c()
          theta_ATTE_001_all <- c()
          score_ATTE_001_all <- c()
          theta_ATE_01_all <- c()
          score_ATE_01_all <- c()
          theta_ATTE_01_all <- c()
          score_ATTE_01_all <- c()
          theta_ATE_minmax_all <- c()
          score_ATE_minmax_all <- c()
          theta_ATTE_minmax_all <- c()
          score_ATTE_minmax_all <- c()
          for (fold_sel in 1:max(ml_grades_pred_sub$Fold)) { # iterate over folds
            # subset on repetition and fold
            ml_grades_pred_sub_fold <- ml_grades_pred_sub %>% filter(Repetition == rep_sel, Fold == fold_sel)
            # 0.01 trimming
            indices_keep_1 <- which(between(ml_grades_pred_sub_fold$m1, 0.01, 0.99))
            indices_keep_2 <- which(between(ml_grades_pred_sub_fold$m2, 0.01, 0.99))
            indices_keep_3 <- which(between(ml_grades_pred_sub_fold$m3, 0.01, 0.99))
            indices_keep_001 <- intersect(indices_keep_1, indices_keep_2)
            indices_keep_001 <- intersect(indices_keep_001, indices_keep_3)
            # 0.1 trimming
            indices_keep_1 <- which(between(ml_grades_pred_sub_fold$m1, 0.1, 0.9))
            indices_keep_2 <- which(between(ml_grades_pred_sub_fold$m2, 0.1, 0.9))
            indices_keep_3 <- which(between(ml_grades_pred_sub_fold$m3, 0.1, 0.9))
            indices_keep_01 <- intersect(indices_keep_1, indices_keep_2)
            indices_keep_01 <- intersect(indices_keep_01, indices_keep_3)
            # min-max trimming
            df_select_trimming <- 
              # m1
              ml_grades_pred_sub_fold %>% 
              mutate(treatment_1 = ifelse(treatment_sport_freq == 1, 1, 0)) %>%
              group_by(treatment_1) %>% 
              summarise(min_m = min(m1), max_m = max(m1)) %>%
              summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
              mutate(model = "m1") %>% rbind(
                # m2
                ml_grades_pred_sub_fold %>% 
                  mutate(treatment_2 = ifelse(treatment_sport_freq == 2, 1, 0)) %>%
                  group_by(treatment_2) %>% 
                  summarise(min_m = min(m2), max_m = max(m2)) %>%
                  summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
                  mutate(model = "m2")
              ) %>% rbind(
                # m3
                ml_grades_pred_sub_fold %>% 
                  mutate(treatment_3 = ifelse(treatment_sport_freq == 3, 1, 0)) %>%
                  group_by(treatment_3) %>% 
                  summarise(min_m = min(m3), max_m = max(m3)) %>%
                  summarise(min_trimming = max(min_m), max_trimming = min(max_m)) %>%
                  mutate(model = "m3")
              )
            
            indices_keep_1 <- which(between(ml_grades_pred_sub_fold$m1, df_select_trimming %>% filter(model == "m1") %>% pull(min_trimming), 
                                            df_select_trimming %>% filter(model == "m1") %>% pull(max_trimming)))
            indices_keep_2 <- which(between(ml_grades_pred_sub_fold$m2, df_select_trimming %>% filter(model == "m2") %>% pull(min_trimming), 
                                            df_select_trimming %>% filter(model == "m2") %>% pull(max_trimming)))
            indices_keep_3 <- which(between(ml_grades_pred_sub_fold$m3, df_select_trimming %>% filter(model == "m3") %>% pull(min_trimming), 
                                            df_select_trimming %>% filter(model == "m3") %>% pull(max_trimming)))
            indices_keep_minmax <- intersect(indices_keep_1, indices_keep_2)
            indices_keep_minmax <- intersect(indices_keep_minmax, indices_keep_3)
            
            # enforce common support
            data_prep_no <- ml_grades_pred_sub_fold
            data_pred_001 <- ml_grades_pred_sub_fold[indices_keep_001, ] 
            data_pred_01 <- ml_grades_pred_sub_fold[indices_keep_01, ]
            data_pred_minmax <- ml_grades_pred_sub_fold[indices_keep_minmax, ]
            
            # calculate treatment effects
            ls_treatment_effects_no <- func_dml_theta_score("multi", data_prep_no, data_prep_no, "outcome_grade", "treatment_sport_freq")
            ls_treatment_effects_001 <- func_dml_theta_score("multi", data_pred_001, data_pred_001, "outcome_grade", "treatment_sport_freq")
            ls_treatment_effects_01 <- func_dml_theta_score("multi", data_pred_01, data_pred_01, "outcome_grade", "treatment_sport_freq")
            ls_treatment_effects_minmax <- func_dml_theta_score("multi", data_pred_minmax, data_pred_minmax, "outcome_grade", "treatment_sport_freq")
            
            # store them in vector for inference
            theta_ATE_no <- ls_treatment_effects_no$theta_ATE
            theta_ATE_no_all <- rbind(theta_ATE_no_all, theta_ATE_no)
            score_ATE_no <- ls_treatment_effects_no$score_ATE
            score_ATE_no_all <- rbind(score_ATE_no_all, score_ATE_no)
            theta_ATTE_no <- ls_treatment_effects_no$theta_ATTE
            theta_ATTE_no_all <- rbind(theta_ATTE_no_all, theta_ATTE_no)
            score_ATTE_no <- ls_treatment_effects_no$score_ATTE
            score_ATTE_no_all <- rbind(score_ATTE_no_all, score_ATTE_no)
            
            theta_ATE_001 <- ls_treatment_effects_001$theta_ATE
            theta_ATE_001_all <- rbind(theta_ATE_001_all, theta_ATE_001)
            score_ATE_001 <- ls_treatment_effects_001$score_ATE
            score_ATE_001_all <- rbind(score_ATE_001_all, score_ATE_001)
            theta_ATTE_001 <- ls_treatment_effects_001$theta_ATTE
            theta_ATTE_001_all <- rbind(theta_ATTE_001_all, theta_ATTE_001)
            score_ATTE_001 <- ls_treatment_effects_001$score_ATTE
            score_ATTE_001_all <- rbind(score_ATTE_001_all, score_ATTE_001)
            
            theta_ATE_01 <- ls_treatment_effects_01$theta_ATE
            theta_ATE_01_all <- rbind(theta_ATE_01_all, theta_ATE_01)
            score_ATE_01 <- ls_treatment_effects_01$score_ATE
            score_ATE_01_all <- rbind(score_ATE_01_all, score_ATE_01)
            theta_ATTE_01 <- ls_treatment_effects_01$theta_ATTE
            theta_ATTE_01_all <- rbind(theta_ATTE_01_all, theta_ATTE_01)
            score_ATTE_01 <- ls_treatment_effects_01$score_ATTE
            score_ATTE_01_all <- rbind(score_ATTE_01_all, score_ATTE_01)
            
            theta_ATE_minmax <- ls_treatment_effects_minmax$theta_ATE
            theta_ATE_minmax_all <- rbind(theta_ATE_minmax_all, theta_ATE_minmax)
            score_ATE_minmax <- ls_treatment_effects_minmax$score_ATE
            score_ATE_minmax_all <- rbind(score_ATE_minmax_all, score_ATE_minmax)
            theta_ATTE_minmax <- ls_treatment_effects_minmax$theta_ATTE
            theta_ATTE_minmax_all <- rbind(theta_ATTE_minmax_all, theta_ATTE_minmax)
            score_ATTE_minmax <- ls_treatment_effects_minmax$score_ATTE
            score_ATTE_minmax_all <- rbind(score_ATTE_minmax_all, score_ATTE_minmax)
            
          } # close iteration over fold_sel
          
          # inference
          df_result_ATE_no <- func_dml_inference("multi", "ATE", theta_ATE_no_all, score_ATE_no_all, nrow(score_ATE_no_all), rep_sel) %>%
            mutate(trimming = "no")
          df_result_ATTE_no <- func_dml_inference("multi", "ATTE", theta_ATTE_no_all, score_ATTE_no_all, nrow(score_ATTE_no_all), rep_sel) %>%
            mutate(trimming = "no")
          
          df_result_ATE_001 <- func_dml_inference("multi", "ATE", theta_ATE_001_all, score_ATE_001_all, nrow(score_ATE_001_all), rep_sel) %>%
            mutate(trimming = "001")
          df_result_ATTE_001 <- func_dml_inference("multi", "ATTE", theta_ATTE_001_all, score_ATTE_001_all, nrow(score_ATTE_001_all), rep_sel) %>%
            mutate(trimming = "001")
          
          df_result_ATE_01 <- func_dml_inference("multi", "ATE", theta_ATE_01_all, score_ATE_01_all, nrow(score_ATE_01_all), rep_sel) %>%
            mutate(trimming = "01")
          df_result_ATTE_01 <- func_dml_inference("multi", "ATTE", theta_ATTE_01_all, score_ATTE_01_all, nrow(score_ATTE_01_all), rep_sel) %>%
            mutate(trimming = "01")
          
          df_result_ATE_minmax <- func_dml_inference("multi", "ATE", theta_ATE_minmax_all, score_ATE_minmax_all, nrow(score_ATE_minmax_all), rep_sel) %>%
            mutate(trimming = "minmax")
          df_result_ATTE_minmax <- func_dml_inference("multi", "ATTE", theta_ATTE_minmax_all, score_ATTE_minmax_all, nrow(score_ATTE_minmax_all), rep_sel) %>%
            mutate(trimming = "minmax")
          
          # store in data frame
          df_effects_trimming_multi <- rbind(
            df_effects_trimming_multi,
            rbind(df_result_ATE_no, df_result_ATTE_no, df_result_ATE_minmax, df_result_ATTE_minmax,
                  df_result_ATE_001, df_result_ATTE_001, df_result_ATE_01, df_result_ATTE_01) %>%
              mutate(outcome = outcome_var_sel, mlalgo = model_algo_sel, MICE = mice_sel)
            )
          
        } # close iteration over rep_sel
      } # close iteration over mice_sel
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel
df_effects_trimming_multi <- df_effects_trimming_multi %>%
  group_by(outcome, mlalgo, trimming, Treatment, Type) %>% 
  summarize_all(median) %>%
  dplyr::select(-MICE) %>%
  as.data.frame()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: Not Normalizing Treatment Probabilities ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
postlasso_grades_nonnorm <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, "_nonorm.rds"))

# number of predictors: 133.01
df_dml_main_multi %>% filter(Prob_norm == "no", model_trimming == "min-max_001") %>% pull(num_predictors_m1) %>% unique()

# percentage of dropped observations: 3.43%
df_dml_main_multi %>% filter(Prob_norm == "no", model_trimming == "min-max_001") %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)

# error metrics
df_error_nonnorm <- data.frame()
for (outcome_var_sel in c("grades")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_nonsep")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean_multi <- data_stand_grades_multi
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      } else {
        df_sd_mean_multi <- data_stand_pers_multi %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean_multi) <- 
          str_remove(colnames(df_sd_mean_multi), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g1 = (RMSE_g1 / df_sd_mean_multi$mean_weekly)*100, 
               RRMSE_g2 = (RMSE_g2 / df_sd_mean_multi$mean_monthly)*100,
               RRMSE_g3 = (RMSE_g3 / df_sd_mean_multi$mean_never)*100) 
      
      # bind row
      df_error_nonnorm <- rbind(df_error_nonnorm, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

# calculate error metrics for paper
df_error_nonnorm %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: Multiclass Classification ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
postlasso_grades_nonsep <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_nonseparate", cov_balance_save, ".rds"))

# number of predictors: 124.12
df_dml_sep_multi %>% pull(num_predictors_m) %>% unique()

# percentage of dropped observations: 3.01%
df_dml_sep_multi %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)

# error metrics
df_error_nonsep <- data.frame()
for (outcome_var_sel in c("grades")) {
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_nonsep")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:length(get(load_pred_algo))) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_sd_mean_multi <- data_stand_grades_multi
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      } else {
        df_sd_mean_multi <- data_stand_pers_multi %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(df_sd_mean_multi) <- 
          str_remove(colnames(df_sd_mean_multi), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g1 = g1_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g2 = g2_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean,
            g3 = g3_stand*df_sd_mean_multi$sd + df_sd_mean_multi$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, FALSE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      
      # calculate RRMSE
      df_error_sub <- df_error_sub %>%
        mutate(RRMSE_g1 = (RMSE_g1 / df_sd_mean_multi$mean_weekly)*100, 
               RRMSE_g2 = (RMSE_g2 / df_sd_mean_multi$mean_monthly)*100,
               RRMSE_g3 = (RMSE_g3 / df_sd_mean_multi$mean_never)*100) 
      
      # bind row
      df_error_nonsep <- rbind(df_error_nonsep, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel

# calculate error metrics for paper
df_error_nonsep %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("RRMSE"), starts_with("MAE")) %>%
  as.data.frame()
