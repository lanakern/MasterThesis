#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML with R Function from Michael Knaus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, I confirm the hand-coded DML results for the binary 
# setting by using the dmlmt function from Michael Knaus.
# Note, the function from Knaus does not support cross-fitting. 
# Results are confirmed for GPA as outcome and all ML algorithms.
# NOTE: FUNCTION IS NOT WORKING FOR MULTIVALUED TREATMENT SETTING!
#+++
# For interpretation:
# -> T0: Daily + Weekly
# -> T1: Monthly or less
# -> T2: Never
#+++

# load functions (they are loaded here as they make trouble with other packages)
if (!require("devtools")) install.packages("devtools")
library(devtools)  # for install_github()

# install_github(repo = "MCKnaus/dmlmt") # download package
library(dmlmt) # for DML in multivalued treatment setting

if (!require("grf")) install.packages("grf")
library(grf) # random forests

if (!require("rms")) install.packages("rms")
library(rms) # regression modelling strategies (used in dmlmt function)

if (!require("kableExtra")) install.packages("kableExtra")
library(kableExtra) # needed for dmlmt function

# define ML algos
model_algo <- c("lasso", "postlasso", "rf", "xgboost")

# generate list for results
df_result_function_list_binary <- list()
df_result_function_list_multi <- list()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# set seed for reproducible results
set.seed(1234)

# iterate over mice data sets
for (mice_data_sel in 1:5) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
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
  print(paste("Number of predictors:", ncol(data_dml)))
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
  }
  
  # remove linearly dependent terms
  data_train_test_m <- data_dml %>% dplyr::select(-all_of(outcome_var))
  data_train_test_g0 <- data_dml %>% filter(treatment_sport == 0) %>% dplyr::select(-treatment_sport)
  data_train_test_g1 <- data_dml %>% filter(treatment_sport == 1) %>% dplyr::select(-treatment_sport)
  
  # remove alias coefficients
  model_m <- glm(paste("treatment_sport", "~ ."), family = binomial(link = "logit"), data = data_train_test_m)
  model_lm_0 <- lm(paste(outcome_var, "~ ."), data = data_train_test_g0)
  model_lm_1 <- lm(paste(outcome_var, "~ ."), data = data_train_test_g1)
  
  vars_multicoll_drop <- c(
    attributes(alias(model_m)$Complete)$dimnames[[1]],
    attributes(alias(model_lm_0)$Complete)$dimnames[[1]], 
    attributes(alias(model_lm_1)$Complete)$dimnames[[1]]) %>%
    unique()
  
  vars_multicoll_drop <- vars_multicoll_drop[!str_detect(
    vars_multicoll_drop, "treatment_sport|outcome_grade|grade|agreeableness|extraversion|neuroticism|openness|conscientiousness"
  )]
  
  if (length(vars_multicoll_drop) < 30) {data_dml <- data_dml %>% dplyr::select(-all_of(vars_multicoll_drop))}
  
  # standardize outcome
  data_cols <- data_dml %>% 
    dplyr::select(-c(all_of(treatment), group)) %>% 
    colnames()
  
  data_dml <- data_dml %>%
    recipe(.) %>%
    update_role({{treatment}}, new_role = "outcome") %>%
    step_normalize(all_of(data_cols)) %>%
    prep() %>%
    bake(new_data = NULL)
  
  # adjust column names because in function, e.g. brackets make problems
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\(", "")
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\)", "")
  
  data_dml <- data_dml %>% as.data.frame()
  
  # Define outcome, treatment, and controls
  Y <- data_dml %>% dplyr::select(outcome_grade) %>% pull()
  D <- data_dml %>% dplyr::select(treatment_sport) %>% pull() 
  X <- data_dml %>% dplyr::select(-c(outcome_grade, treatment_sport, group))
  X <- model.matrix(~ ., data = X)
  values <- sort(unique(D))
  
  ## LASSO ##
  lasso_ATE <- dmlmt(X, D, Y, pl = FALSE)
  lasso_ATE <- data.frame(lasso_ATE$ATE)
  lasso_ATE$ml_model <- "lasso"
  rownames(lasso_ATE) <- NULL
  
  ## POST-LASSO ##
  postlasso_ATE <- dmlmt(X, D, Y, pl = TRUE) # post-lasso 
  postlasso_ATE <- data.frame(postlasso_ATE$ATE)
  postlasso_ATE$ml_model <- "postlasso"
  rownames(postlasso_ATE) <- NULL
  
  ## RANDOM FORESTS ##
  ps_mat <- t_mat <- y_mat <- matrix(NA,length(Y),length(values))
  for (tr in 1:length(values)) {
    t_mat[,tr] <- as.numeric(D == values[tr])
    rf_p <- regression_forest(X,t_mat[,tr])
    ps_mat[,tr] <- predict(rf_p, X)$predictions
    rf_y <- regression_forest(X[t_mat[,tr] == 1,],Y[t_mat[,tr] == 1])
    y_mat[,tr] <- predict(rf_y, X)$predictions
  }
  rf_gps <- gps_cs(ps_mat,t_mat)
  rf_PO <- PO_dmlmt(t_mat, Y, y_mat, rf_gps$p, cs_i = rf_gps$cs)
  rf_ATE <- data.frame(TE_dmlmt(rf_PO$mu,rf_gps$cs))
  rf_ATE$ml_model <- "randomforests"
  rownames(rf_ATE) <- NULL
  
  ## XGBOOST ##
  ps_mat <- t_mat <- y_mat <- matrix(NA,length(Y),length(values))
  for (tr in 1:length(values)) {
    t_mat[,tr] <- as.numeric(D == values[tr])
    xgb_p <- xgboost(X,t_mat[,tr], nrounds = 2)
    ps_mat[,tr] <- predict(xgb_p, X)
    rf_y <- xgboost(X[t_mat[,tr] == 1,],Y[t_mat[,tr] == 1], nrounds = 2)
    y_mat[,tr] <- predict(rf_y, X)
  }
  xgb_gps <- gps_cs(ps_mat,t_mat)
  xgb_PO <- PO_dmlmt(t_mat, Y, y_mat, xgb_gps$p,cs_i = xgb_gps$cs)
  xgb_ATE <- data.frame(TE_dmlmt(xgb_PO$mu,xgb_gps$cs))
  xgb_ATE$ml_model <- "xgboost"
  rownames(xgb_ATE) <- NULL
  
  # generate data frame with results
  df_result_function <- rbind(lasso_ATE, postlasso_ATE, rf_ATE, xgb_ATE) %>%
    mutate(MICE = mice_data_sel) %>%
    dplyr::select(MICE, ml_model, everything()) 
  df_result_function_list_binary <- append(df_result_function_list_binary, list(df_result_function))
  
  gc()
}

# Pool results across data sets by taking the mean
dml_final <- data.frame()
for (mice_data_sel in 1:5) {
  dml_final <- rbind(
    dml_final,
    df_result_function_list_binary[[mice_data_sel]] %>% mutate(MICE = mice_data_sel) %>%
      dplyr::select(MICE, everything())
  )
}

dml_final <-
  dml_final %>% 
  group_by(ml_model) %>%
  mutate(across(c(TE, SE, t, p), ~ mean(.))) %>%
  dplyr::select(-MICE) %>% distinct() %>% as.data.frame()

saveRDS(dml_final, "Output/DML/Treatment_Effects/knaus_binary_aggregated.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# # set seed for reproducible results
# # (again because I cleared everything due to memory error)
# set.seed(1234)
# treatment <- "treatment_sport_freq"
# 
# # iterate over mice data sets
# for (mice_data_sel in 1:5) {
#   
#   # data set number
#   print(paste("Data Set", mice_data_sel))
#   
#   # load data
#   ## extract extracurricular activity ending
#   if (extra_act == "yes") {
#     extra_act_save <- "_extradrop"
#   } else {
#     extra_act_save <- ""
#   }
#   ## covariate balance ending
#   if (cov_balance == "yes") {
#     cov_balance_save <- "_covbal"
#   } else {
#     cov_balance_save <- ""
#   }
#   ## extract outcome
#   if (str_detect(outcome_var, "grade")) {
#     load_data_folder <- "Data/Grades/"
#     load_data_ending <- ".rds"
#   } else if (str_detect(outcome_var, "bigfive")) {
#     load_data_folder <- "Data/Personality/"
#     load_data_ending <- "_personality.rds"
#   } else {
#     stop("Please specify correct outcome variable")
#   }
#   
#   ## cohort prep
#   if (cohort_prep == "controls_same_outcome") {
#     load_data <- 
#       paste0(load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
#              "_", treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, load_data_ending)
#   } else {
#     load_data <- 
#       paste0(load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
#              "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
#   }
#   
#   data_dml_raw <- readRDS(load_data)
#   data_dml <- data_dml_raw
#   print(paste("Number of predictors:", ncol(data_dml)))
#   # drop lags if desired by user
#   if (model_controls_lag == "no_lags") {
#     # drop all lags
#     data_dml <- data_dml %>% 
#       dplyr::select(-c(contains("_lag"))) %>% 
#       as.data.frame()
#   } else if (model_controls_lag == "no_treatment_outcome_lags") {
#     # drop only treatment and outcome lags
#     # here differentiate between GPA and personality outcome
#     if (str_detect(outcome_var, "grade")) {
#       data_dml <- data_dml %>% 
#         dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
#         dplyr::select(-c(starts_with("outcome") & contains("_lag"))) %>%
#         as.data.frame()
#     } else {
#       data_dml <- data_dml %>% 
#         dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
#         dplyr::select(-c(starts_with(str_remove(outcome_var, "outcome_")) & contains("_lag"))) %>%
#         as.data.frame()
#     }
#   } else {
#     # keep all lags
#     data_dml <- data_dml %>% as.data.frame()
#   }
#   
#   # drop endogeneous variables if desired by user
#   if (model_controls_endog == "no") {
#     # drop variables defined as endogeneous
#     colnames_endog_drop <- 
#       eval(parse(text = paste(paste("data_dml_raw", "%>%"), vars_endogenous, "%>% colnames()")))
#     colnames_endog_drop <- colnames_endog_drop[colnames_endog_drop %in% colnames(data_dml)]
#     data_dml <- data_dml %>% dplyr::select(-all_of(colnames_endog_drop))
#     # also all interactions and polynominals are dropped
#     data_dml <- data_dml %>% dplyr::select(-contains(":")) %>% dplyr::select(-contains("_order"))
#   } else {
#     # keep endogeneous variables
#     data_dml <- data_dml %>% as.data.frame()
#   }
#   
#   # if personality is outcome further preparations are necessary
#   if (str_detect(outcome_var, "grade")) {
#     # for grades no further steps are necessary
#     data_dml <- data_dml
#   } else if (str_detect(outcome_var, "bigfive")) {
#     # for personality selected outcome variable needs to be declared
#     data_dml <- data_dml %>%
#       rename_with(~ outcome_var, all_of(str_remove(outcome_var, "outcome_")))
#   }
#   
#   # remove linearly dependent terms
#   data_train_test_m1 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
#     mutate(treatment_sport_freq_weekly_atleast = data_dml$treatment_sport_freq_weekly_atleast)
#   data_train_test_m2 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
#     mutate(treatment_sport_freq_monthly_less = data_dml$treatment_sport_freq_monthly_less)
#   data_train_test_m3 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
#     mutate(treatment_sport_freq_never = data_dml$treatment_sport_freq_never)
#   
#   data_train_test_g1 <- data_dml %>% filter(treatment_sport_freq == 1) %>% dplyr::select(-treatment_sport_freq)
#   data_train_test_g2 <- data_dml %>% filter(treatment_sport_freq == 2) %>% dplyr::select(-treatment_sport_freq)
#   data_train_test_g3 <- data_dml %>% filter(treatment_sport_freq == 3) %>% dplyr::select(-treatment_sport_freq)
#   
#   # remove alias coefficients
#   model_m1 <- glm(paste("treatment_sport_freq_weekly_atleast", "~ ."), family = binomial(link = "logit"), data = data_train_test_m1)
#   model_m2 <- glm(paste("treatment_sport_freq_monthly_less", "~ ."), family = binomial(link = "logit"), data = data_train_test_m2)
#   model_m3 <- glm(paste("treatment_sport_freq_never", "~ ."), family = binomial(link = "logit"), data = data_train_test_m3)
#   
#   model_lm_1 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g1)
#   model_lm_2 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g2)
#   model_lm_3 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g3)
#   
#   vars_multicoll_drop <- c(
#     attributes(alias(model_m1)$Complete)$dimnames[[1]], attributes(alias(model_m2)$Complete)$dimnames[[1]],
#     attributes(alias(model_m3)$Complete)$dimnames[[1]],
#     attributes(alias(model_lm_1)$Complete)$dimnames[[1]], attributes(alias(model_lm_2)$Complete)$dimnames[[1]],
#     attributes(alias(model_lm_3)$Complete)$dimnames[[1]]) %>%
#     unique()
#   
#   vars_multicoll_drop <- vars_multicoll_drop[!str_detect(
#     vars_multicoll_drop, "treatment_sport|outcome_grade|grade|agreeableness|extraversion|neuroticism|openness|conscientiousness"
#   )]
#   
#   if (length(vars_multicoll_drop) < 30) {data_dml <- data_dml %>% dplyr::select(-all_of(vars_multicoll_drop))}
#   
#   print(paste("Number of predictors after dropping linearly dependent columns:", ncol(data_dml)))
#   
#   # standardize outcome
#   data_cols <- data_dml %>% 
#     dplyr::select(-c("outcome_grade", "treatment_sport_freq", "group", starts_with("treatment_sport_freq") & !ends_with("na"))) %>% 
#     colnames()
#   
#   data_dml <- data_dml %>%
#     recipe(.) %>%
#     update_role({{treatment}}, new_role = "outcome") %>%
#     step_normalize(all_of(data_cols)) %>%
#     prep() %>%
#     bake(new_data = NULL)
#   
#   data_dml <- data_dml %>%
#     recipe(.) %>%
#     step_normalize("outcome_grade") %>%
#     prep() %>%
#     bake(new_data = NULL) %>% 
#     as.data.frame()
#   
#   # adjust column names because in function, e.g. brackets make problems
#   colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\(", "")
#   colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\)", "")
#   
#   data_dml <- data_dml %>% as.data.frame()
#   # Define outcome, treatment, and controls
#   Y <- data_dml %>% dplyr::select(all_of(outcome_var)) %>% pull()
#   D <- data_dml %>% mutate(treatment_sport_freq = as.factor(treatment_sport_freq)) %>% dplyr::select(treatment_sport_freq) %>% pull() 
#   X <- data_dml %>% dplyr::select(-c(outcome_var, treatment_sport_freq, group))
#   X <- model.matrix(~ ., data = X)
#   values <- sort(unique(D))
#   
#   ## LASSO ##
#   lasso_ATE <- dmlmt(X, D, Y, pl = FALSE)
#   lasso_ATE <- data.frame(lasso_ATE$ATE)
#   lasso_ATE$ml_model <- "lasso"
#   lasso_ATE$Treatment <- rownames(lasso_ATE)
#   rownames(lasso_ATE) <- NULL
#   
#   ## POST-LASSO ##
#   postlasso_ATE <- dmlmt(X, D, Y, pl = TRUE) # post-lasso 
#   postlasso_ATE <- data.frame(postlasso_ATE$ATE)
#   postlasso_ATE$ml_model <- "postlasso"
#   postlasso_ATE$Treatment <- rownames(postlasso_ATE)
#   rownames(postlasso_ATE) <- NULL
#   
#   ## RANDOM FORESTS ##
#   ps_mat <- t_mat <- y_mat <- matrix(NA,length(Y),length(values))
#   for (tr in 1:length(values)) {
#     t_mat[,tr] <- as.numeric(D == values[tr])
#     rf_p <- regression_forest(X,t_mat[,tr])
#     ps_mat[,tr] <- predict(rf_p, X)$predictions
#     rf_y <- regression_forest(X[t_mat[,tr] == 1,],Y[t_mat[,tr] == 1])
#     y_mat[,tr] <- predict(rf_y, X)$predictions
#   }
#   rf_gps <- gps_cs(ps_mat,t_mat)
#   rf_PO <- PO_dmlmt(t_mat, Y, y_mat, rf_gps$p, cs_i = rf_gps$cs)
#   rf_ATE <- data.frame(TE_dmlmt(rf_PO$mu,rf_gps$cs))
#   rf_ATE$ml_model <- "randomforests"
#   rf_ATE$Treatment <- rownames(rf_ATE)
#   rownames(rf_ATE) <- NULL
#   
#   ## XGBOOST ##
#   ps_mat <- t_mat <- y_mat <- matrix(NA,length(Y),length(values))
#   for (tr in 1:length(values)) {
#     t_mat[,tr] <- as.numeric(D == values[tr])
#     xgb_p <- xgboost(X,t_mat[,tr], nrounds = 2)
#     ps_mat[,tr] <- predict(xgb_p, X)
#     rf_y <- xgboost(X[t_mat[,tr] == 1,],Y[t_mat[,tr] == 1], nrounds = 2)
#     y_mat[,tr] <- predict(rf_y, X)
#   }
#   xgb_gps <- gps_cs(ps_mat,t_mat)
#   xgb_PO <- PO_dmlmt(t_mat, Y, y_mat, xgb_gps$p,cs_i = xgb_gps$cs)
#   xgb_ATE <- data.frame(TE_dmlmt(xgb_PO$mu,xgb_gps$cs))
#   xgb_ATE$ml_model <- "xgboost"
#   xgb_ATE$Treatment <- rownames(xgb_ATE)
#   rownames(xgb_ATE) <- NULL
#   
#   # generate data frame with results
#   df_result_function <- rbind(lasso_ATE, postlasso_ATE, rf_ATE, xgb_ATE) %>%
#     mutate(MICE = mice_data_sel) %>%
#     dplyr::select(MICE, Treatment, ml_model, everything()) %>%
#     mutate(Treatment = case_when(
#       Treatment == "T1 - T0" ~ "monthly_weekly",
#       Treatment == "T2 - T0" ~ "never_weekly",
#       Treatment == "T2 - T1" ~ "never_monthly", 
#       TRUE ~ NA
#     ))
#   df_result_function_list_multi <- append(df_result_function_list_multi, list(df_result_function))
#   
#   gc()
# }
# 
# # Pool results across data sets by taking the mean
# dml_final_multi <- data.frame()
# for (mice_data_sel in 1:5) {
#   dml_final_multi <- rbind(
#     dml_final_multi,
#     df_result_function_list_multi[[mice_data_sel]] %>% mutate(MICE = mice_data_sel) %>%
#       dplyr::select(MICE, everything())
#   )
# }
# 
# 
# dml_final_multi <-
#   dml_final_multi %>% 
#   group_by(Treatment, ml_model) %>%
#   mutate(across(c(TE, SE, t, p), ~ mean(.))) %>%
#   dplyr::select(-MICE) %>% distinct() %>% as.data.frame()
# 
# 
# saveRDS(dml_final_multi, "Output/DML/Treatment_Effects/knaus_multi_aggregated.rds")



