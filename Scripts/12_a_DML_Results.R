#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML Analysis Results: Binary and Multivalued Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++ 
# by Lana Kern
#+++
# In this files, the DML results are analyzed and prepared so that they can
# be copied within the final paper. For instance, error metrics are displayed,
# common support plots are created etc. 
#+++


#%%%%%%%%%%%%%%%%%%%%#
#### LOAD RESULTS ####
#%%%%%%%%%%%%%%%%%%%%#

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}


## SUMMARY OF RESULTS ##
#++++++++++++++++++++++#

df_dml_main_binary <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")

df_dml_main_multi <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")


## LASSO ##
#+++++++++#

# BINARY
lasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI
lasso_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


## POST-LASSO ##
#++++++++++++++#

# BINARY: GPA
postlasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI: GPA
postlasso_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# BINARY: PERSONALITY
postlasso_agree <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_agreeableness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_consc <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_conscientiousness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


postlasso_extra <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_extraversion_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_open <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_openness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_neuro <- 
  readRDS(paste0("Output/DML/Estimation/Personality/binary_neuroticism_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# MULTI: PERSONALITY
postlasso_agree_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_agreeableness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_consc_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_conscientiousness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


postlasso_extra_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_extraversion_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_open_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_openness_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_neuro_multi <- 
  readRDS(paste0("Output/DML/Estimation/Personality/multi_neuroticism_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# Robustnesschecks regarding samples
postlasso_grades_rc1_all <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc2_lvcf <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_no_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc3_noextra_weekly <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc3_noextra_all <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_all_down_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc4_endog <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogno_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_rc5_nocovbal <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", ".rds"))


# Robustnesschecks regarding trimming
postlasso_grades_trimming001 <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 "0.01", "_K4-2_Rep5", cov_balance_save, ".rds"))

postlasso_grades_trimming01 <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 "0.1", "_K4-2_Rep5", cov_balance_save, ".rds"))


# Robustness checks regarding hyperparameters
postlasso_grades_1SE <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_1SE", cov_balance_save, ".rds"))

postlasso_grades_1SEplus <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_1SE_plus", cov_balance_save, ".rds"))


## XGBoosst ##
#++++++++++++#

# BINARY
xgb_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "xgboost", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))

# MULTI
xgb_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "xgboost", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


## Random Forests ##
#++++++++++++++++++#

# BINARY
rf_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "randomforests", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-1_Rep5", cov_balance_save, ".rds"))


# MULTI
rf_grades_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "randomforests", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-1_Rep5", cov_balance_save, ".rds"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++TREATMENT EFFECTS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Binary Treatment Setting ####
#++++++++++++++++++++++++++++++++#

# all
df_dml_main_binary %>% dplyr::select(
  cohort_prep, treatment_repl, treatment_def, extra_act,
  starts_with("model"), Treatment, Type, starts_with("theta"),
  starts_with("se"), starts_with("tvalue"), starts_with("pvalue"), 
  starts_with("CI")
) %>% filter(Type %in% c("ATE", "ATTE"))


# main model
df_treatment_effects_main_binary <- df_dml_main_binary %>% filter(
  cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
  treatment_repl == main_treatment_repl, extra_act == main_extra_act,
  model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
  model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
  model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
  model_covbal == "yes", Type %in% c("ATE", "ATTE")
) %>% 
  mutate(model = "main") %>%
  dplyr::select(model, outcome, model_algo, Type, theta_median, se_median, pvalue_median, time_stamp) 
df_treatment_effects_main_binary
saveRDS(df_treatment_effects_main_binary, "Output/DML/Treatment_Effects/binary_main_treatment_effects_paper.rds")


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
      filter(Type %in% c("ATE", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
  postlasso_grades_boxplot <- rbind(
    postlasso_grades_boxplot,
    postlasso_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
  rf_grades_boxplot <- rbind(
    rf_grades_boxplot,
    rf_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
  
  xgb_grades_boxplot <- rbind(
    xgb_grades_boxplot,
    xgb_grades[[mice_sel]]$detail %>%
      dplyr::select(ML_algo, Type, Rep, Treatment_Effect) %>%
      filter(Type %in% c("ATE", "ATTE")) %>%
      mutate(MICE = mice_sel)
  )
}

df_binary_boxplot <- rbind(lasso_grades_boxplot, postlasso_grades_boxplot)
df_binary_boxplot <- rbind(df_binary_boxplot, rf_grades_boxplot)
df_binary_boxplot <- rbind(df_binary_boxplot, xgb_grades_boxplot)

df_binary_boxplot <- df_binary_boxplot %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "LASSO", ML_algo == "postlasso" ~ "POST-LASSO",
      ML_algo == "randomforests" ~ "RANDOM FORESTS",
      ML_algo == "xgboost" ~ "XGBOOST"
    )
  )

dml_boxplot_binary <- 
  ggplot(df_binary_boxplot, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.1, 0) +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 26), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 26), # rotate x-axis ticks
    axis.title = element_text(size = 26), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 30) # size of facet text
    ) 
dml_boxplot_binary
ggsave("Output/DML/Treatment_Effects/plot_binary_main_grades_treatment_effects_variability.png",
       dml_boxplot_binary,
       width = 20, height = 15, dpi = 300, units = "in", device = 'png')


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
        mutate(MICE = mice_sel)
    )
  } # close iteration over mice_sel
} # close iteration over personality

postlasso_personality_boxplot <- postlasso_personality_boxplot %>%
  mutate(
    Outcome = case_when(
      Outcome == "agree" ~ "Agreeableness", Outcome == "extra" ~ "Extraversion",
      Outcome == "consc" ~ "Conscientiousness", Outcome == "open" ~ "Openness",
      Outcome == "neuro" ~ "Neuroticism", TRUE ~ as.character(NA)
    )
  )

dml_boxplot_binary_personality <- 
  ggplot(postlasso_personality_boxplot, aes(x = Outcome, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") + 
  ylim(-0.15, 0.25) +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 26), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 26), # rotate x-axis ticks
    axis.title = element_text(size = 26), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 30) # size of facet text
  ) 
dml_boxplot_binary_personality
ggsave("Output/DML/Treatment_Effects/plot_binary_main_personality_treatment_effects_variability.png",
       dml_boxplot_binary_personality,
       width = 20, height = 15, dpi = 300, units = "in", device = 'png')




#### Multivalued Treatment Setting ####
#+++++++++++++++++++++++++++++++++++++#

df_dml_main_multi %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act,
  starts_with("model"), Treatment, Type, starts_with("theta"),
  starts_with("se"), starts_with("tvalue"), starts_with("pvalue"), 
  starts_with("CI")
) %>% filter(Type %in% c("ATE", "ATTE"))


# main
df_treatment_effects_main_multi <- df_dml_main_multi %>% filter(
  cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
  treatment_repl == main_treatment_repl, extra_act == main_extra_act,
  model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
  model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
  model_controls_endog == main_model_controls_endog, 
  model_covbal == "yes", Type %in% c("ATE", "ATTE")
) %>% mutate(model = "main") %>%
  dplyr::select(model, outcome, model_algo, Type, Treatment, theta_median, se_median, pvalue_median) 
df_treatment_effects_main_multi
saveRDS(df_treatment_effects_main_multi, "Output/DML/Treatment_Effects/multi_main_treatment_effects_paper.rds")


# plot to show how variable estimates are across MICE data sets and repetitions
lasso_grades_boxplot_multi <- data.frame()
postlasso_grades_boxplot_multi <- data.frame()
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
}

df_multi_boxplot <- rbind(lasso_grades_boxplot_multi, postlasso_grades_boxplot_multi)

df_multi_boxplot <- df_multi_boxplot %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "LASSO", ML_algo == "postlasso" ~ "POST-LASSO",
      ML_algo == "randomforests" ~ "RANDOM FORESTS",
      ML_algo == "xgboost" ~ "XGBOOST"
    )
  )

df_multi_boxplot_ate <- df_multi_boxplot %>% filter(Type == "ATE")
dml_boxplot_ate_multi <- 
  ggplot(df_multi_boxplot_ate, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("Treatment Effect Estimates") + #ylab("\nTreatment Effect Estimates\n") +
  facet_wrap(~ Treatment) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 26), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 26), # rotate x-axis ticks
    axis.title = element_text(size = 26), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 30) # size of facet text
  ) 
dml_boxplot_ate_multi
ggsave("Output/DML/Treatment_Effects/plot_multi_main_ate_treatment_effects_variability.png",
       dml_boxplot_ate_multi)

df_multi_boxplot_atte <- df_multi_boxplot %>% filter(Type == "ATTE")
dml_boxplot_atte_multi <- 
  ggplot(df_multi_boxplot_atte, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("Treatment Effect Estimates") + #ylab("\nTreatment Effect Estimates\n") +
  facet_wrap(~ Treatment) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 26), # size of x-axis tick labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 26), # rotate x-axis ticks
    axis.title = element_text(size = 26), #, face = "bold"), # size of x-axis labels
    strip.text.x = element_text(size = 30) # size of facet text
  ) 
dml_boxplot_atte_multi
ggsave("Output/DML/Treatment_Effects/plot_multi_main_atte_treatment_effects_variability.png",
       dml_boxplot_atte_multi)



#### Robustness Checks ####
#+++++++++++++++++++++++++#

## BINARY ##

# robustness checks regarding sample sizes
df_treatment_effects_rc_binary <- df_dml_main_binary %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog & model_hyperparam_sel == "best" & 
      model_covbal == "yes", "main", "rc"
  )) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_trimming == "min-max") %>%
  dplyr::select(outcome, model_algo, cohort_prep, treatment_def, treatment_repl, extra_act,
                model_type, model_controls_lag, model_controls_endog, model_covbal, model_trimming,
                model_hyperparam_sel, Type, theta_median, se_median, pvalue_median, time_stamp) 
df_treatment_effects_rc_binary
saveRDS(df_treatment_effects_rc_binary, "Output/DML/Treatment_Effects/binary_rc_treatment_effects_paper.rds")

# robustness checks regarding trimming
df_treatment_effects_rc_trimming_binary <- df_dml_main_binary %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog & model_hyperparam_sel == "best" & 
      model_covbal == "yes", "main", "rc"
  )) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_trimming != "min-max") %>%
  dplyr::select(outcome, model_algo, cohort_prep, treatment_def, treatment_repl, extra_act,
                model_type, model_controls_lag, model_controls_endog, model_covbal, model_trimming,
                Type, theta_median, se_median, pvalue_median, time_stamp) 
df_treatment_effects_rc_trimming_binary
saveRDS(df_treatment_effects_rc_trimming_binary, "Output/DML/Treatment_Effects/binary_rc_trimming_treatment_effects_paper.rds")


## MULTI ##

# ADD:model_hyperparam_sel
df_treatment_effects_rc_multi <- df_dml_main_multi %>% 
  mutate(sample = ifelse(
    cohort_prep == main_cohort_prep & treatment_def == main_treatment_def & 
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_k == main_model_k & model_s_rep == main_model_s_rep & 
      model_trimming == main_model_trimming & model_controls_lag == main_model_controls_lag & 
      model_controls_endog == main_model_controls_endog &  
      model_covbal == "yes", "main", "rc"
  )) %>% 
  filter(Type %in% c("ATE", "ATTE"), sample == "rc", model_algo == "postlasso", 
         outcome == "grade", model_trimming == "min-max") %>%
  dplyr::select(outcome, model_algo, cohort_prep, treatment_def, treatment_repl, extra_act,
                model_type, model_controls_lag, model_controls_endog, model_covbal, model_trimming,
                Treatment, Type, theta_median, se_median, pvalue_median, time_stamp) 
df_treatment_effects_rc_multi


#%%%%%%%%%%%%%%%%#
#### ++ APE++ ####
#%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
#++++++++++++++++++++++++++++#

# the APE is identical across the MICE data sets and algorithms
# calculated APE is not for standardized outcome variable
lasso_grades[[1]]$ape # binary

## for standardized outcome variable ##

# APE
df_ape_binary <- readRDS("Output/Descriptives/Grades/MEAN_COMPARISON_STAND_BINARY.rds")
df_ape_binary <- df_ape_binary %>%
  filter(treatment_sport != "all") %>%
  dplyr::select(variable, treatment_sport, mean, starts_with("p_value")) %>%
  spread(treatment_sport, mean) %>%
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




## Multivalued Treatment Setting ##
#+++++++++++++++++++++++++++++++++#

# the APE is identical across the MICE data sets and algorithms
# calculated APE is not for standardized outcome variable
lasso_grades[[1]]$ape # binary

## for standardized outcome variable ##

# APE
df_ape_multi <- readRDS("Output/Descriptives/Grades/MEAN_COMPARISON_STAND_MULTI.rds")

# mean
df_mean_multi <- df_ape_multi %>%
  filter(treatment_sport_freq != "all") %>%
  dplyr::select(variable, treatment_sport_freq, mean) %>%
  spread(treatment_sport_freq, mean) %>%
  group_by(variable) %>%
  fill(all_of(c("monthly_less", "never", "weekly_atleast")), .direction = 'downup') %>%
  mutate(ape_weekly_monthly = `weekly_atleast` - `monthly_less`,
         ape_weekly_never = `weekly_atleast` - `never`,
         ape_monthly_never = `monthly_less` - `never`)

# p-value
df_p_multi <- 
  df_ape_multi %>% 
  dplyr::select(variable, treatment_sport_freq, p_value_daily, p_value_monthly) %>% 
  filter(!treatment_sport_freq %in% c("weekly_atleast", "all")) %>%   
  pivot_wider(
    names_from = treatment_sport_freq,
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
  left_join(df_multi_se_never_monthly, by = "variable")









#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++ERROR METRICS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# The error metrics obtained in the Excel files are calculated based on the
# standardized outcome variables. For better interpretability, the standardization
# is reversed.
# https://support.numxl.com/hc/en-us/articles/207841883-Transforming-standardized-values-to-non-standardized-values

# mean and standard deviation used for standardization (same across MICE data sets
# as outcome vars are not replaced as they do not contain any missing values)
data_stand_grades <- 
  readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", cov_balance_save, "_mice1.rds"))
data_stand_grades <- data_stand_grades %>%
  summarize(mean = mean(outcome_grade), sd = sd(outcome_grade)) 

data_stand_pers <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop",
                                  cov_balance_save, "_mice1_personality.rds"))
data_stand_pers <- data_stand_pers %>%
  summarize(agree_mean = mean(bigfive_agreeableness), agree_sd = sd(bigfive_agreeableness),
            consc_mean = mean(bigfive_conscientiousness), consc_sd = sd(bigfive_conscientiousness),
            extra_mean = mean(bigfive_extraversion), extra_sd = sd(bigfive_extraversion),
            open_mean = mean(bigfive_openness), open_sd = sd(bigfive_openness),
            neuro_mean = mean(bigfive_neuroticism), neuro_sd = sd(bigfive_neuroticism)) 



#### BINARY TREATMENT SETTING ####
#++++++++++++++++++++++++++++++++#

df_error_main_binary <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:5) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
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
      df_error_main_binary <- rbind(df_error_main_binary, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel


# calculate error metrics for paper
df_error_main_binary <- df_error_main_binary %>%
  dplyr::select(outcome, ml_algo, AUC_m, ACC_m, BACC_m, RMSE_g0, RMSE_g1, MAPE_g0, MAPE_g1)
saveRDS(df_error_main_binary, "Output/DML/binary_main_error_metrics.rds")


## ERROR METRICS BEFORE TRIMMING ##
df_error_main_binary_bef_trimming <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:5) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred_bef_trimming, # BEF_TRIMMING!!!
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
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
      df_error_main_binary_bef_trimming <- rbind(df_error_main_binary_bef_trimming, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel


df_error_main_binary_bef_trimming <- df_error_main_binary_bef_trimming %>%
  rowwise() %>%
  mutate(RMSE_g = mean(c(RMSE_g0, RMSE_g1)), MAPE_g = mean(c(MAPE_g0, MAPE_g1))) %>%
  dplyr::select(outcome, ml_algo, AUC_m, ACC_m, BACC_m, RMSE_g, MAPE_g)
saveRDS(df_error_main_binary_bef_trimming, "Output/DML/binary_main_error_metrics_bef_trimming.rds")


## ERROR METRICS FOR ROBUSTNESS CHECKS ##
df_error_main_binary_rc <- data.frame()
for (outcome_var_sel in c("grades_rc1_all", "grades_rc4_endog")) {
  
  # load standardization
  if (outcome_var_sel == "grades_rc1_all") {
    data_stand_grades_rc <- 
      readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_all_down_extradrop", cov_balance_save, "_mice1.rds"))
    data_stand_grades_rc <- data_stand_grades_rc %>%
      summarize(mean = mean(outcome_grade), sd = sd(outcome_grade)) 
  } else {
    data_stand_grades_rc <- data_stand_grades
  }
  
  
  for (model_algo_sel in c("postlasso")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:5) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (str_detect(outcome_var_sel, "grades")) {
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
            # original scale
            outcome = outcome_stand*data_stand_grades_rc$sd + data_stand_grades_rc$mean,
            g0 = g0_stand*data_stand_grades_rc$sd + data_stand_grades_rc$mean,
            g1 = g1_stand*data_stand_grades_rc$sd + data_stand_grades_rc$mean
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
      df_error_main_binary_rc <- rbind(df_error_main_binary_rc, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel


df_error_main_binary_rc <- df_error_main_binary_rc %>%
  rowwise() %>%
  mutate(RMSE_g = mean(c(RMSE_g0, RMSE_g1)), MAPE_g = mean(c(MAPE_g0, MAPE_g1))) %>%
  dplyr::select(outcome, ml_algo, AUC_m, ACC_m, BACC_m, RMSE_g, MAPE_g)

saveRDS(df_error_main_binary_rc, "Output/DML/binary_rc_error_metrics.rds")




#### MULTIVALUED TREATMENT SETTING ####
#+++++++++++++++++++++++++++++++++++++#

df_error_main_multi <- data.frame()
for (outcome_var_sel in c("grades", "agree", "consc", "extra", "neuro", "open")) {
  for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel, "_multi")
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      ml_grades_pred <- data.frame()
      for (mice_sel in 1:5) {
        ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                        get(load_pred_algo)[[mice_sel]]$trimming, 
                                        by = "Repetition") %>%
          mutate(MICE = mice_sel)
        ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
      }
      
      if (outcome_var_sel == "grades") {
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*data_stand_grades$sd + data_stand_grades$mean,
            g1 = g1_stand*data_stand_grades$sd + data_stand_grades$mean,
            g2 = g2_stand*data_stand_grades$sd + data_stand_grades$mean,
            g3 = g3_stand*data_stand_grades$sd + data_stand_grades$mean
          )
      } else {
        data_stand_pers_sub <- data_stand_pers %>%
          dplyr::select(starts_with(outcome_var_sel)) 
        colnames(data_stand_pers_sub) <- 
          str_remove(colnames(data_stand_pers_sub), paste0(outcome_var_sel, "_"))
        
        df_pred <- ml_grades_pred %>% 
          mutate(
            # standardized outcomes
            outcome_stand = outcome, g1_stand = g1,  g2_stand = g2,  g3_stand = g3,
            # original scale
            outcome = outcome_stand*data_stand_pers_sub$sd + data_stand_pers_sub$mean,
            g1 = g1_stand*data_stand_pers_sub$sd + data_stand_pers_sub$mean,
            g2 = g2_stand*data_stand_pers_sub$sd + data_stand_pers_sub$mean,
            g3 = g3_stand*data_stand_pers_sub$sd + data_stand_pers_sub$mean
          )
      }
      
      df_error_sub <- func_ml_error_metrics("multi", df_pred, 1, 1, TRUE) %>%
        dplyr::select(-c(Repetition, Fold)) %>%
        mutate(outcome = outcome_var_sel, ml_algo = model_algo_sel) %>%
        dplyr::select(outcome, ml_algo, everything())
      df_error_main_multi <- rbind(df_error_main_multi, df_error_sub)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # close tryCatch()
  } # close for loop model_algo_sel
} # close for loop outcome_var_sel


# calculate error metrics for paper
df_error_main_multi <- df_error_main_multi %>%
  dplyr::select(outcome, ml_algo, starts_with("AUC"), starts_with("ACC"), starts_with("BACC"), 
                starts_with("RMSE"), starts_with("MAPE")) %>%
  rowwise() %>%
  mutate(
    AUC_m = mean(c(AUC_m1, AUC_m2, AUC_m3)), ACC_m = mean(c(ACC_m1, ACC_m2, ACC_m3)), BACC_m = mean(c(BACC_m1, BACC_m2, BACC_m3)),
    RMSE_g = mean(c(RMSE_g1, RMSE_g2, RMSE_g3)), MAPE_g = mean(c(MAPE_g1, MAPE_g2, MAPE_g3))
    ) %>%
  dplyr::select(-matches("g[0-9]$"), -matches("m[0-9]$")) %>%
  as.data.frame()

saveRDS(df_error_main_multi, "Output/DML/multi_main_error_metrics.rds")



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
                                  
df_predictors_all_binary %>%
  dplyr::select(outcome, ml_algo, ends_with("_m"), ends_with("_g"))

saveRDS(df_predictors_all_binary, "Output/DML/binary_main_num_predictors.rds")



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

df_predictors_all_multi %>%
  dplyr::select(outcome, ml_algo, ends_with("_m"), ends_with("_g"))

saveRDS(df_predictors_all_multi, "Output/DML/multi_msin_num_predictors.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++COMMON SUPPORT++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Check for dropped observations ####
#++++++++++++++++++++++++++++++++++++++#

## Binary Treatment Setting ##

# main
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

saveRDS(df_trimming_drop_main_binary, "Output/DML/binary_main_trimming_obs.rds")

# rc wrt trimming
df_dml_main_binary %>%
  filter(
    cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
    treatment_repl == main_treatment_repl, extra_act == main_extra_act,
    model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
    model_trimming != main_model_trimming, model_controls_lag == main_model_controls_lag,
    model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
    model_covbal == "yes"
  ) %>%
  dplyr::select(outcome, model_algo, model_trimming, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, model_trimming,
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)

## Multivalued Treatment Setting ##
df_trimming_drop_main_multi <- df_dml_main_multi %>%
  filter(
    cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
    treatment_repl == main_treatment_repl, extra_act == main_extra_act,
    model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
    model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
    model_controls_endog == main_model_controls_endog, 
    model_covbal == "yes"
  ) %>%
  dplyr::select(outcome, model_algo, starts_with("n_treats")) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, 
         n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)


saveRDS(df_trimming_drop_main_multi, "Output/DML/multi_main_trimming_obs.rds")


#### Create Plots ####
#++++++++++++++++++++#

## Binary Treatment Setting ##
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
      for (mice_sel in 1:5) {
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
        "yes", model_algo_sel, dec_places_sel)
      list_binary_plot_common_support[[outcome_var_sel]][[model_algo_sel]] <- binary_plot_common_support_sub
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
    
}

# Grades
binary_plot_common_support <- ggarrange(
  list_binary_plot_common_support$grades$lasso + xlab("") + ggtitle("LASSO"), 
  list_binary_plot_common_support$grades$postlasso + xlab("") + ylab("") + ggtitle("POST-LASSO"),
  list_binary_plot_common_support$grades$rf + ggtitle("RANDOM FORESTS"), 
  list_binary_plot_common_support$grades$xgb + ykab("") + ggtitle("XGBOOST"),
  nrow = 2, ncol = 2, common.legend = T, legend = "bottom"
) 
ggsave(paste0("Output/DML/Common_Support/dml_plot_common_support_binary_grades_allalgos.png"), 
       binary_plot_common_support,
       width = 20, height = 15, dpi = 300, units = "in", device = 'png')

# Personality
binary_plot_common_support_personality <- ggarrange(
  list_binary_plot_common_support$agree$postlasso + xlab("") + xlim(-0.1, 1) + ggtitle("Agreeableness"), 
  list_binary_plot_common_support$consc$postlasso + xlab("") + xlim(-0.1, 1) + ylab("") + ggtitle("Conscientiousness"),
  list_binary_plot_common_support$extra$postlasso + ylab("") + xlim(-0.1, 1) + ggtitle("Extraversion"), 
  list_binary_plot_common_support$neuro$postlasso + xlim(-0.1, 1) + ggtitle("Neuroticism"),
  list_binary_plot_common_support$open$postlasso + ylab("") + xlim(-0.1, 1) + ggtitle("Openness"),
  ggplot() + theme_light(),
  nrow = 2, ncol = 3, common.legend = T, legend = "bottom"
) 
ggsave(paste0("Output/DML/Common_Support/dml_plot_common_support_binary_personality_postlasso.png"), 
       binary_plot_common_support_personality,
       width = 20, height = 15, dpi = 300, units = "in", device = 'png')



## Multivalued Treatment Setting ##

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
        "yes", model_algo_sel, 4)
      list_multi_plot_common_support[[outcome_var_sel]][[model_algo_sel]] <- multi_plot_common_support_sub
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
}


ggarrange(
  list_multi_plot_common_support$grades$lasso,
  list_multi_plot_common_support$grades$postlasso,
  list_multi_plot_common_support$grades$rf,
  list_multi_plot_common_support$grades$xgb,
  nrow = 1, common.legend = TRUE
)



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
      for (micel_sel in 1:5) {
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### SENSITIVITY WRT HYPERPARAMETER CHOICES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_dml_main_binary$model_hyperparam_sel %>% unique()


## TREATMENT EFFEKTE ##
df_dml_main_binary %>% 
  filter(model_hyperparam_sel != "best", Type %in% c("ATE", "ATTE")) %>%
  dplyr::select(model_hyperparam_sel, starts_with("n_treats"), starts_with("num_predictors"),
                Type, theta_median, se_median, pvalue_median) %>%
  mutate(n_treats_diff_perf = ((n_treats_before - n_treats_after) / n_treats_before)*100)
  

## ERROR METRICS ##
postlasso_grades_best <- postlasso_grades

df_error_rc_hyp <- data.frame()
for (load_pred_algo_sel in c("postlasso_grades_best", "postlasso_grades_1SE", "postlasso_grades_1SEplus")) {
  load_pred_algo <- load_pred_algo_sel
  ml_grades_pred <- data.frame()
  for (mice_sel in 1:5) {
    ml_grades_pred_sub <- left_join(get(load_pred_algo)[[mice_sel]]$pred, 
                                    get(load_pred_algo)[[mice_sel]]$trimming, 
                                    by = "Repetition") %>%
      mutate(MICE = mice_sel)
    ml_grades_pred <- rbind(ml_grades_pred, ml_grades_pred_sub)
  }
  

  df_pred <- ml_grades_pred %>% 
    mutate(
      # standardized outcomes
      outcome_stand = outcome, g0_stand = g0, g1_stand = g1,
      # original scale
      outcome = outcome_stand*data_stand_grades$sd + data_stand_grades$mean,
      g0 = g0_stand*data_stand_grades$sd + data_stand_grades$mean,
      g1 = g1_stand*data_stand_grades$sd + data_stand_grades$mean
    )

  
  df_error_sub <- func_ml_error_metrics("binary", df_pred, 1, 1, TRUE) %>%
    dplyr::select(-c(Repetition, Fold)) %>%
    mutate(hyperparam = str_remove(load_pred_algo_sel, "postlasso_grades_")) %>%
    dplyr::select(hyperparam, everything())
  df_error_rc_hyp <- rbind(df_error_rc_hyp, df_error_sub)
  
}

df_error_rc_hyp <- df_error_rc_hyp %>%
  rowwise() %>%
  mutate(RMSE_g = mean(c(RMSE_g0, RMSE_g1)), MAPE_g = mean(c(MAPE_g0, MAPE_g1))) %>%
  as.data.frame()
df_error_rc_hyp




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DETAILED LASSO ANALYSIS FOR GPA SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## MAIN RESULTS ##
#++++++++++++++++#

df_dml_main_binary %>% filter(model_algo == "lasso")


## RESULT ACROSS REPETITIONS ##
#+++++++++++++++++++++++++++++#

## S = 5 ##

lasso_theta_detail <- rbind(
  lasso_grades[[1]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 1),
  lasso_grades[[2]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 2)) %>%
  rbind(lasso_grades[[3]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 3)) %>%
  rbind(lasso_grades[[4]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 4)) %>%
  rbind(lasso_grades[[5]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 5)) 

# replicate overall treatment effect
lasso_theta_detail %>%
  group_by(MICE) %>%
  summarize(theta_median = median(Treatment_Effect)) %>%
  ungroup() %>% pull(theta_median) %>% median()

lasso_theta_detail %>%
  group_by(MICE) %>%
  summarize(theta_mean = mean(Treatment_Effect)) %>%
  ungroup() %>% pull(theta_mean) %>% mean()


# for paper: intermediate results
lasso_theta_detail %>%
  group_by(Rep) %>%
  summarize(theta_median = median(Treatment_Effect)) 



## S = 10 ##
lasso_grades_10 <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-4_Rep10.rds")

lasso_theta_detail_10 <- rbind(
  lasso_grades_10[[1]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 1),
  lasso_grades_10[[2]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 2)) %>%
  rbind(lasso_grades_10[[3]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 3)) %>%
  rbind(lasso_grades_10[[4]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 4)) %>%
  rbind(lasso_grades_10[[5]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 5)) 

lasso_theta_detail_10 %>%
  group_by(Rep) %>%
  summarize(theta_median = median(Treatment_Effect)) 



## COMMON SUPPORT ##
#++++++++++++++++++#

# MAIN MODEL: No treatment and outcome lags #
lasso_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_sub <- left_join(lasso_grades[[mice_sel]]$pred, lasso_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred <- rbind(lasso_grades_pred, lasso_grades_pred_sub)
}

lasso_grades_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred, unique(lasso_grades_pred$min_trimming), 
  unique(lasso_grades_pred$max_trimming), "lasso")

lasso_grades_plot_common_support <- lasso_grades_plot_common_support + 
  ggtitle(bquote(paste(atop(bold(.("LASSO")), "Propensity Score Overlap without Treatment and Outcome Lags")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# RC: Treatment and outcome lags #
lasso_rc_treatoutlags <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_all_endogyes_trimming0.01_K4-2_Rep5.rds")


lasso_rc_treatoutlags_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_lasso_rc_treatoutlags_pred_sub <- left_join(lasso_rc_treatoutlags[[mice_sel]]$pred, lasso_rc_treatoutlags[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_rc_treatoutlags_pred <- rbind(lasso_rc_treatoutlags_pred, lasso_lasso_rc_treatoutlags_pred_sub)
}

lasso_rc_treatoutlags_plot_common_support <- func_dml_common_support(
  "binary", lasso_rc_treatoutlags_pred, unique(lasso_rc_treatoutlags_pred$min_trimming), 
  unique(lasso_rc_treatoutlags_pred$max_trimming), "lasso")


lasso_rc_treatoutlags_plot_common_support <- lasso_rc_treatoutlags_plot_common_support + 
  ggtitle("Propensity Score Overlap with Treatment and Outcome Lags") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# RC: No Lags at all #
lasso_rc_nolags <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_nolags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_rc_nolags_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_lasso_rc_nolags_pred_sub <- left_join(lasso_rc_nolags[[mice_sel]]$pred, lasso_rc_nolags[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_rc_nolags_pred <- rbind(lasso_rc_nolags_pred, lasso_lasso_rc_nolags_pred_sub)
}

lasso_rc_nolags_plot_common_support <- func_dml_common_support(
  "binary", lasso_rc_nolags_pred, unique(lasso_rc_nolags_pred$min_trimming), 
  unique(lasso_rc_nolags_pred$max_trimming), "lasso")


lasso_rc_nolags_plot_common_support <- lasso_rc_nolags_plot_common_support + 
  ggtitle("Propensity Score Overlap without any Lags") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

ggarrange(lasso_grades_plot_common_support, lasso_rc_nolags_plot_common_support,
          lasso_rc_treatoutlags_plot_common_support,
          nrow = 3)


# MAIN BUT DIFFERENT TRIMMING #
lasso_grades_01 <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.1_K4-2_Rep5.rds")

lasso_grades_pred_01 <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_01_sub <- left_join(lasso_grades_01[[mice_sel]]$pred, lasso_grades_01[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred_01 <- rbind(lasso_grades_pred_01, lasso_grades_pred_01_sub)
}

lasso_grades_01_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred_01, unique(lasso_grades_pred_01$min_trimming), 
  unique(lasso_grades_pred_01$max_trimming), "lasso")

lasso_grades_01_plot_common_support <- lasso_grades_01_plot_common_support + 
  ggtitle("Propensity Score Overlap with Trimming Threshold of 0.1") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))



lasso_grades_minmax <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimmingmin-max_K4-2_Rep5.rds")

lasso_grades_pred_minmax <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_minmax_sub <- left_join(lasso_grades_minmax[[mice_sel]]$pred, lasso_grades_minmax[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred_minmax <- rbind(lasso_grades_pred_minmax, lasso_grades_pred_minmax_sub)
}

lasso_grades_minmax_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred_minmax, unique(lasso_grades_pred_minmax$min_trimming), 
  unique(lasso_grades_pred_minmax$max_trimming), "lasso")

lasso_grades_minmax_plot_common_support <- lasso_grades_minmax_plot_common_support + 
  ggtitle("Propensity Score Overlap with Min-Max Trimming") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# subplot
ggarrange(lasso_grades_plot_common_support + rremove("xlab") +
            ggtitle(bquote(paste(atop(bold(.("LASSO")), "Propensity Score Overlap with Trimming Threshold of 0.01")))) +
            theme(plot.title = element_text(hjust = 0.5, size = 10)),
          lasso_grades_01_plot_common_support + rremove("xlab"),
          lasso_grades_minmax_plot_common_support, 
          nrow = 3, common.legend = TRUE, legend = "bottom")


lasso_grades_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct()
lasso_grades_pred_01 %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)
lasso_grades_pred_minmax %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)


ggarrange(lasso_grades_plot_common_support + rremove("xlab"),
          lasso_rc_treatoutlags_plot_common_support, 
          nrow = 2, common.legend = TRUE, legend = "bottom")

lasso_grades_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct()
lasso_rc_treatoutlags_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DETAILED XGBoost ANALYSIS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




