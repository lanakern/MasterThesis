#%%%%%%%%%%%%%%%%%%#
#### Simulation ####
#%%%%%%%%%%%%%%%%%%#

set.seed(1234)

#### Define Inputs and Load Data ####
#+++++++++++++++++++++++++++++++++++#

treatment_setting <- "binary"
outcome_var <- "outcome_grade"
outcome <- "outcome_grade"
treatment <- "treatment_sport"
group <- "group"
cohort_prep <- main_cohort_prep # "controls_bef_outcome"
treatment_repl <- main_treatment_repl # "no"
treatment_def <- main_treatment_def # "all"
extra_act <- main_extra_act # "no"
model_type <- main_model_type # "all_int_polys"
model_controls_lag <- main_model_controls_lag # "no_lags", "all"
model_controls_endog <- main_model_controls_endog # "no"
model_trimming <- main_model_trimming # 0.1, min-max
model_hyperparam_sel <- "best"
model_post_sel <- FALSE
cov_balance <- main_cov_balance
model_k <- 4 
model_k_tuning <- 2 # 4
model_s_rep <- 5 # 10
mice_data_sel <- 1
mlalgo <- "randomforests"


# load data
## extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}
## covariate balance ending
if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
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
           "_", treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, load_data_ending)
} else {
  load_data <- 
    paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
}

data_dml_raw <- readRDS(load_data)
data_dml <- data_dml_raw

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

# drop endogeneous variables if desired by user
if (model_controls_endog == "no") {
  # drop variables defined as endogeneous
  colnames_endog_drop <- 
    eval(parse(text = paste(paste("data_dml_raw", "%>%"), vars_endogenous, "%>% colnames()")))
  colnames_endog_drop <- colnames_endog_drop[colnames_endog_drop %in% colnames(data_dml)]
  data_dml <- data_dml %>% dplyr::select(-all_of(colnames_endog_drop))
  # also all interactions and polynominals are dropped
  data_dml <- data_dml %>% dplyr::select(-contains(":")) %>% dplyr::select(-contains("_order"))
} else {
  # keep endogeneous variables
  data_dml <- data_dml %>% as.data.frame()
}

# if personality is outcome further preparations are necessary
if (str_detect(outcome_var, "grade")) {
  # for grades no further steps are necessary
  data_dml <- data_dml
} else if (str_detect(outcome_var, "bigfive")) {
  # for personality selected outcome variable needs to be declared
  data_dml <- data_dml %>%
    rename_with(~ outcome_var, all_of(str_remove(outcome_var, "outcome_")))
  # # lags for all other personality variables are dropped
  # colnames_bigfive_lag_drop <- data_dml %>% 
  #   dplyr::select(starts_with("bigfive") & ends_with("lag") & !matches(outcome_var_old)) %>% colnames()
  # data_dml <- data_dml %>% 
  #   dplyr::select(-all_of(colnames_bigfive_lag_drop))
}

# select some covariates
data_dml <- data_dml %>% 
  dplyr::select(treatment_sport, outcome_grade, group, living_type_parents, extracurricular_num, 
                starts_with("social_integr"), starts_with("bigfive"))
                
# reduce obs
data_dml <- rbind(data_dml %>% filter(treatment_sport == 0) %>% head(10), 
                  data_dml %>% filter(treatment_sport == 1) %>% head(5))

data_dml %>% group_by(treatment_sport) %>% summarize(mean(outcome_grade))

# standardization
if (str_detect(mlalgo, "lasso")) {
  # select predictors that are standardized
  # -> all except outcome, treatment, and group variable
  if (treatment_setting == "multi") {
    data_cols <- data_dml %>% 
      dplyr::select(-c(all_of(outcome), all_of(treatment), group, starts_with("treatment_sport_freq") & !ends_with("na"))) %>% 
      colnames()
  } else {
    data_cols <- data_dml %>% 
      dplyr::select(-c(all_of(outcome), all_of(treatment), group)) %>% 
      colnames()
  }
  
  # standardize features (mean zero and standard deviation of one)
  data_dml <- data_dml %>%
    recipe(.) %>%
    update_role({{treatment}}, new_role = "outcome") %>%
    step_normalize(all_of(data_cols)) %>%
    prep() %>%
    bake(new_data = NULL)
} else {
  data_dml <- data_dml 
}

# However in any case the outcome variable is standardized.
data_dml <- data_dml %>%
  recipe(.) %>%
  step_normalize(all_of(outcome)) %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  as.data.frame()

# training and test data
data_train <- data_dml[c(1:5, 13:15), ]
data_test <- data_dml[-c(1:5, 13:15), ]

# ML
rf_grid <- expand.grid(
  trees = c(500), # default: 500
  mtry = c(ncol(data_dml)-2), # default: floor(sqrt(num_X)) for classification and floor(num_X/3) for regression
  min_n = c(5) # default: 5 for regression and 10 for classification
)
rf_ml <- func_ml_rf(treatment_setting, data_train, data_test, outcome, treatment, group, 1, rf_grid)
df_pred <- rf_ml$pred

ls_treatment_effects <- func_dml_theta_score(treatment_setting, df_pred, data_test, outcome, treatment)
ls_treatment_effects$theta_ATE

mean(df_pred$g1 - df_pred$g0 + (as.numeric(as.character(df_pred$treatment)) * (df_pred$outcome - df_pred$g1) / df_pred$m)  - 
  ((1-as.numeric(as.character(df_pred$treatment))) * (df_pred$outcome - df_pred$g0) / (1-df_pred$m)))

func_ml_error_metrics(treatment_setting, df_pred, 1, 1, TRUE)

# change predictions
df_pred_correct <- df_pred %>% mutate(g0 = c(-0.79, -0.79, 1.3, 0.06, 0.4, 0.5, 0.6))
ls_treatment_effects <- func_dml_theta_score(treatment_setting, df_pred_correct, data_test, outcome, treatment)
ls_treatment_effects$theta_ATE

func_ml_error_metrics(treatment_setting, df_pred_correct, 1, 1, TRUE)

df_pred_correct_2 <- df_pred %>% mutate(g0 = c(-0.79, -0.79, 1.3, 0.06, 0.4, 0.5, 0.6), 
                                        m = c(0.2, 0.4, 0.3, 0.3, 0.1, 0.6, 0.8))
ls_treatment_effects <- func_dml_theta_score(treatment_setting, df_pred_correct_2, data_test, outcome, treatment)
ls_treatment_effects$theta_ATE

func_ml_error_metrics(treatment_setting, df_pred_correct_2, 1, 1, TRUE)



#### LOAD REAL RESULTS ####
#+++++++++++++++++++++++++#

# load post lasso predictions
postlasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", 
                 "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
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
df_pred <- data.frame()
df_test <- data.frame()
for (mice_data_sel in 1:5) {
  for (iterate_row_sel in 1:nrow(df_iterate)) {
    iterate_sel <- df_iterate[iterate_row_sel, ]
    df_pred <- rbind(df_pred,
                     postlasso_grades[[mice_data_sel]]$cov_balance[[mice_data_sel]][[iterate_sel$Rep]][[iterate_sel$Fold]]$pred)
    df_test <- rbind(df_test,
                     postlasso_grades[[mice_data_sel]]$cov_balance[[mice_data_sel]][[iterate_sel$Rep]][[iterate_sel$Fold]]$controls)
  }
}
nrow(df_pred) # 260,251
nrow(df_test) # 260,251

saveRDS(df_pred, "Output/DML/Performance_Analysis/pred_postlasso_orig.rds")

# real
ls_treatment_effects_real <- func_dml_theta_score(treatment_setting, df_pred, df_test, outcome, treatment)
df_results <- data.frame("model" = "true", "ATE" = ls_treatment_effects_real$theta_ATE) %>% 
  cbind(func_ml_error_metrics(treatment_setting, df_pred, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))



# perfect outcome predictions
df_pred_perf_outcome <- df_pred %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome, TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome, TRUE ~ g1)
)
df_results <- rbind(
  df_results, 
  data.frame("model" = "outcome_perf", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_outcome, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_outcome, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_outcome, "Output/DML/Performance_Analysis/pred_postlasso_outcome_perfect.rds")

# perfect treatment predictions
df_pred_perf_treatment <- df_pred %>% rowwise() %>% mutate(
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.49), treatment == 1 ~  runif(1, 0.51, 0.99), TRUE ~ m)
)
func_ml_error_metrics(treatment_setting, df_pred_perf_treatment, 1, 1, TRUE) # PERFECT

df_results <- rbind(
  df_results, 
  data.frame("model" = "treatment_perf", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_treatment, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_treatment, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_treatment, "Output/DML/Performance_Analysis/pred_postlasso_treatment_perfect.rds")


# perfect treatment and outcome prediction
df_pred_perf_all <- data_pred %>% rowwise() %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome, TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome, TRUE ~ g1),
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.49), treatment == 1 ~  runif(1, 0.51, 0.99), TRUE ~ m)
)

df_results <- rbind(
  df_results, 
  data.frame("model" = "all_perf", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_all, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_all, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

mean(df_pred_perf_all$g1 - df_pred_perf_all$g0) # simply difference
df_pred_perf_all %>% filter(treatment == 1) %>% pull(outcome) %>% mean() - 
  df_pred_perf_all %>% filter(treatment == 0) %>% pull(outcome) %>% mean()


saveRDS(df_pred_perf_all, "Output/DML/Performance_Analysis/pred_postlasso_all_perfect.rds")


# good treatment and outcome prediction
## underprediction -> predict worse grades
df_pred_perf_good_all_1 <- data_pred %>% rowwise() %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome + runif(1, 0.01, 0.2), TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome + runif(1, 0.01, 0.2), TRUE ~ g1),
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.6), treatment == 1 ~  runif(1, 0.4, 0.99), TRUE ~ m)
)
df_results <- rbind(
  df_results, 
  data.frame("model" = "outcome_worse", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_good_all_1, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_good_all_1, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_good_all_1, "Output/DML/Performance_Analysis/pred_postlasso_all_worse.rds")


## overprediction
df_pred_perf_good_all_2 <- data_pred %>% rowwise() %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome - runif(1, 0.01, 0.2), TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome - runif(1, 0.01, 0.2), TRUE ~ g1),
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.6), treatment == 1 ~  runif(1, 0.4, 0.99), TRUE ~ m)
)
df_results <- rbind(
  df_results, 
  data.frame("model" = "outcome_better", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_good_all_2, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_good_all_2, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_good_all_2, "Output/DML/Performance_Analysis/pred_postlasso_all_better.rds")

## 0 better, 1 worse
df_pred_perf_good_all_3 <- data_pred %>% rowwise() %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome - runif(1, 0.01, 0.2), TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome + runif(1, 0.01, 0.2), TRUE ~ g1),
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.6), treatment == 1 ~  runif(1, 0.4, 0.99), TRUE ~ m)
)
df_results <- rbind(
  df_results, 
  data.frame("model" = "outcome_0better_1worse", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_good_all_3, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_good_all_3, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_good_all_3, "Output/DML/Performance_Analysis/pred_postlasso_0better_1worse.rds")

# 0 worse, 1 better
df_pred_perf_good_all_4 <- data_pred %>% rowwise() %>% mutate(
  g0 = case_when(treatment == 0 ~ outcome + runif(1, 0.01, 0.2), TRUE ~ g0),
  g1 = case_when(treatment == 1 ~ outcome - runif(1, 0.01, 0.2), TRUE ~ g1),
  m = case_when(treatment == 0 ~  runif(1, 0.01, 0.6), treatment == 1 ~  runif(1, 0.4, 0.99), TRUE ~ m)
)
df_results <- rbind(
  df_results, 
  data.frame("model" = "outcome_0worse_1better", "ATE" = func_dml_theta_score(treatment_setting, df_pred_perf_good_all_4, df_test, outcome, treatment)$theta_ATE) %>% 
    cbind(func_ml_error_metrics(treatment_setting, df_pred_perf_good_all_4, 1, 1, TRUE) %>% dplyr::select(AUC_m, ACC_m, BACC_m, starts_with("MAPE"), starts_with("RMSE")))
)

saveRDS(df_pred_perf_good_all_4, "Output/DML/Performance_Analysis/pred_postlasso_0worse_1better.rds")
