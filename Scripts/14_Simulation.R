#%%%%%%%%%%%%%%%%%%#
#### Simulation ####
#%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, a small simulation study is conducted. Precisely, the sensitivity
# of the treatment effect estimates is studied wrt the predictive performance
# of the propensity score.
#+++

set.seed(1234)
outcome <- "outcome_grade"
treatment <- "treatment_sport"


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### LOAD REAL RESULTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# load post lasso predictions
postlasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 "min-max", "_K4-2_Rep5", "_covbal", ".rds"))

df_dml_main_binary <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
df_dml_main_binary %>% filter(
  cohort_prep == main_cohort_prep, treatment_def == main_treatment_def,
  treatment_repl == main_treatment_repl, extra_act == main_extra_act,
  model_type == main_model_type, model_k == main_model_k, model_s_rep == main_model_s_rep,
  model_trimming == main_model_trimming, model_controls_lag == main_model_controls_lag,
  model_controls_endog == main_model_controls_endog, model_hyperparam_sel == "best",
  model_covbal == "yes", Type %in% c("ATE"), model_algo == "postlasso", outcome == "grade"
) %>% 
  dplyr::select(theta_median, theta_mean) %>% distinct()


# create prediction and test data sets
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
df_inference_real <- data.frame()
df_inference_perf <- data.frame()
df_inference_good <- data.frame()
df_error_real <- data.frame()
df_error_perfect <- data.frame()
df_error_good <- data.frame()
for (mice_data_sel in 1:5) {
  print(paste0("Data Set: ", mice_data_sel))
  theta_ate_all <- c()
  score_ate_all <- c()
  theta_ate_perfect_all <- c()
  score_ate_perfect_all <- c()
  theta_ate_good_all <- c()
  score_ate_good_all <- c()
  for (iterate_row_sel in 1:nrow(df_iterate)) {
    iterate_sel <- df_iterate[iterate_row_sel, ]
    # extract predictions
    df_pred_sub <- postlasso_grades[[mice_data_sel]]$cov_balance[[mice_data_sel]][[iterate_sel$Rep]][[iterate_sel$Fold]]$pred %>% 
      mutate(MICE = mice_data_sel)
    
    # manipulate predictions
    df_pred_sub_sim_perfect <- df_pred_sub %>% rowwise() %>% mutate(
      m = case_when(treatment == 0 ~  runif(1, 0.01, 0.49), treatment == 1 ~  runif(1, 0.51, 0.99), TRUE ~ m)
    )
    
    df_pred_sub_sim_good <- df_pred_sub %>% rowwise() %>% mutate(
      m = case_when(treatment == 0 ~  runif(1, 0.01, 0.6), treatment == 1 ~  runif(1, 0.4, 0.99), TRUE ~ m)
    )
    
    if (mice_data_sel == 1 & iterate_row_sel == 1) {
      df_pred <- df_pred_sub
      df_pred_perfect <- df_pred_sub_sim_perfect
      df_pred_good <- df_pred_sub_sim_good
    }
    
    # extract data
    df_test_sub <- postlasso_grades[[mice_data_sel]]$cov_balance[[mice_data_sel]][[iterate_sel$Rep]][[iterate_sel$Fold]]$controls %>%
      dplyr::select(Repetition, Fold, outcome_grade, treatment_sport)
    
    # calculate treatment effects
    treatment_effects_real_sub <- func_dml_theta_score(treatment_setting, df_pred_sub, df_test_sub, outcome, treatment)
    theta_ate_all <- c(theta_ate_all, treatment_effects_real_sub$theta_ATE)
    score_ate_all <- c(score_ate_all, treatment_effects_real_sub$score_ATE)
    
    treatment_effects_perfect_sub <- func_dml_theta_score(treatment_setting, df_pred_sub_sim_perfect, df_test_sub, outcome, treatment)
    theta_ate_perfect_all <- c(theta_ate_all, treatment_effects_perfect_sub$theta_ATE)
    score_ate_perfect_all <- c(score_ate_all, treatment_effects_perfect_sub$score_ATE)
    
    treatment_effects_good_sub <- func_dml_theta_score(treatment_setting, df_pred_sub_sim_good, df_test_sub, outcome, treatment)
    theta_ate_good_all <- c(theta_ate_all, treatment_effects_good_sub$theta_ATE)
    score_ate_good_all <- c(score_ate_all, treatment_effects_good_sub$score_ATE)
    
    # calculate error metrics
    df_error_real <- rbind(df_error_real,
                           func_ml_error_metrics("binary", df_pred_sub, 1, 1, TRUE) %>% 
                             dplyr::select(Repetition, Fold, AUC_m, ACC_m, BACC_m) %>% 
                             mutate(MICE = mice_data_sel))
    
    df_error_perfect <- rbind(df_error_perfect,
                           func_ml_error_metrics("binary", df_pred_sub_sim_perfect, 1, 1, TRUE) %>% 
                             dplyr::select(Repetition, Fold, AUC_m, ACC_m, BACC_m) %>% 
                             mutate(MICE = mice_data_sel))
    
    df_error_good <- rbind(df_error_good,
                           func_ml_error_metrics("binary", df_pred_sub_sim_good, 1, 1, TRUE) %>% 
                             dplyr::select(Repetition, Fold, AUC_m, ACC_m, BACC_m) %>% 
                             mutate(MICE = mice_data_sel))
    
    if (iterate_sel$Fold == 4) {
      df_inference_real <- rbind(df_inference_real, 
                                 func_dml_inference("binary", "ATE", theta_ate_all, score_ate_all, length(score_ate_all), iterate_sel$Rep) %>%
                                   mutate(MICE = mice_data_sel))
      
      df_inference_perf <- rbind(df_inference_perf, 
                                 func_dml_inference("binary", "ATE", theta_ate_perfect_all, score_ate_perfect_all, length(score_ate_perfect_all), iterate_sel$Rep) %>%
                                   mutate(MICE = mice_data_sel))
      
      df_inference_good <- rbind(df_inference_good, 
                                 func_dml_inference("binary", "ATE", theta_ate_good_all, score_ate_good_all, length(score_ate_good_all), iterate_sel$Rep) %>%
                                   mutate(MICE = mice_data_sel))
      
      theta_ate_all <- c()
      score_ate_all <- c()
      theta_ate_perfect_all <- c()
      score_ate_perfect_all <- c()
      theta_ate_good_all <- c()
      score_ate_good_all <- c()
    } # close if()
  } # close iterate_row_sel
} # close mice

saveRDS(df_inference_real, "Output/DML/Simulation/df_inference_real.rds")
saveRDS(df_error_real, "Output/DML/Simulation/df_error_real.rds")

saveRDS(df_inference_perf, "Output/DML/Simulation/df_inference_perfect.rds")
saveRDS(df_error_perfect, "Output/DML/Simulation/df_error_perfect.rds")

saveRDS(df_inference_good, "Output/DML/Simulation/df_inference_good.rds")
saveRDS(df_error_good, "Output/DML/Simulation/df_error_good.rds")


df_result <- rbind(
  cbind(df_inference_real %>% summarize_all(median) %>% dplyr::select(-c("Rep", "MICE")),
        df_error_real %>% summarize_all(mean) %>% dplyr::select(-c("Repetition", "Fold", "MICE"))) %>%
    mutate(Type = "real"),
  cbind(df_inference_perf %>% summarize_all(median) %>% dplyr::select(-c("Rep", "MICE")),
        df_error_perfect %>% summarize_all(mean) %>% dplyr::select(-c("Repetition", "Fold", "MICE"))) %>%
    mutate(Type = "perfect"),
  cbind(df_inference_good %>% summarize_all(median) %>% dplyr::select(-c("Rep", "MICE")),
        df_error_good %>% summarize_all(mean) %>% dplyr::select(-c("Repetition", "Fold", "MICE"))) %>%
    mutate(Type = "good")
) %>% dplyr::select(-Treatment)


saveRDS(df_result, "Output/DML/Simulation/df_results.rds")


#%%%%%%%%%%%%%%%%%%%%%%#
#### Common Support ####
#%%%%%%%%%%%%%%%%%%%%%%#

saveRDS(df_pred, "Output/DML/Simulation/df_pred.rds")
saveRDS(df_pred_perfect, "Output/DML/Simulation/df_pred_perfect.rds")
saveRDS(df_pred_good, "Output/DML/Simulation/df_pred_good.rds")


ggsave("Output/DML/Simulation/common_support_simulation.png", 
ggarrange(
  func_dml_common_support("binary", df_pred, 0.01, 0.99, "no", "no", "postlasso", 4) + ggtitle("True Prediction"),
  func_dml_common_support("binary", df_pred_perfect, 0.01, 0.99, "no", "no", "postlasso", 4) + ylab("") + ggtitle("Perfect Prediction"),
  func_dml_common_support("binary", df_pred_good, 0.01, 0.99, "no", "no", "postlasso", 4) + ylab("") + ggtitle("Good Prediction"),
  ncol = 3, common.legend = T, legend = "bottom"
),
width = 20, height = 15, dpi = 300, units = "in", device = 'png')
