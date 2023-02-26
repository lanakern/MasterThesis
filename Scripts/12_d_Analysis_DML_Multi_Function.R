#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML with R Function from Michael Knaus ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the DML results for the multivalued treatment setting
# in file "12_c" are confirmed by using an already existing R function.
# Since the DoubleML function only accepts binary treatment variables,
# the dmlmt function from Michael Knaus is used.
# Note: Results are only confirmed for main model and all four machine 
# learning algorithms.
#+++
# For interpretation:
# -> T0: Daily + Weekly
# -> T1: Monthly or less
# -> T2: Never
#+++

# load packages
if (!require("devtools")) install.packages("devtools")
library(devtools)  # for install_github()
# install_github(repo = "MCKnaus/dmlmt") # download package
library(dmlmt) # for DML in multivalued treatment setting
library(grf) # random forests

# define inputs
model_type <- "all"
model_algo <- c("lasso", "postlasso", "rf", "xgboost")
model_k <- 4
model_s_rep <- 2
model_trimming <- 0.01
model_outcome <- "stand"
model_controls <- "no_lags"
cohort_prep <- "controls_same_outcome"
treatment_def <- "weekly"
treatment_repl <- "down"
extra_act <- "yes"


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
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
  } else {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "robustcheck_mice", mice_data_sel, ".rds")
  }
  load_data <- str_replace(load_data, "_level", "") # drop level
  data_dml <- readRDS(load_data)
  
  if (model_controls == "no_lags") {
    data_dml <- data_dml %>% select(-c(ends_with("_lag"))) %>% as.data.table()
  } else {
    data_dml <- as.data.table(data_dml)
  }
  

  # outcome variable depends on selection
  if (model_outcome == "level") {
    outcome_var <- "outcome_grade"
  } else if (model_outcome == "stand") {
    outcome_var <- "outcome_grade_stand"
  }
  
  # adjust column names because in function, e.g. brackets make problems
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\(", "")
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\)", "")
  
  # Define outcome, treatment, and controls
  Y <- data_dml %>% select(outcome_grade_stand) %>% pull()
  D <- data_dml %>% select(treatment_sport) %>% pull() 
  X <- data_dml %>% select(-c(outcome_grade_stand, treatment_sport, group))
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
    select(MICE, ml_model, everything()) 
  df_result_function_list_binary <- append(df_result_function_list_binary, list(df_result_function))
  
  gc()
}

# Pool results across data sets by taking the mean
dml_final <- data.frame()
for (mice_data_sel in 1:5) {
  dml_final <- rbind(
    dml_final,
    df_result_function_list_binary[[mice_data_sel]] %>% mutate(MICE = mice_data_sel) %>%
      select(MICE, everything())
  )
}

dml_final <-
  dml_final %>% 
  group_by(ml_model) %>%
  mutate(across(c(TE, SE, t, p), ~ mean(.))) %>%
  select(-MICE) %>% distinct() %>% as.data.frame()

saveRDS(dml_final, "Output/DML/FUNCTION_KNAUS_BINARY.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# set seed for reproducible results
# (again because I cleared everything due to memory error)
set.seed(1234)

# iterate over mice data sets
for (mice_data_sel in 1:5) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
  # load data
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_multi_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
  } else {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_multi_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "robustcheck_mice", mice_data_sel, ".rds")
  }
  load_data <- str_replace(load_data, "_level", "") # drop level
  data_dml <- readRDS(load_data)
  
  if (model_controls == "no_lags") {
    data_dml <- data_dml %>% select(-c(ends_with("_lag"))) %>% as.data.table()
  } else {
    data_dml <- as.data.table(data_dml)
  }
  
  # drop other treatment_freq columns
  data_dml <- data_dml %>% select(-c(starts_with("treatment_sport_freq_") & !ends_with("_na")))
  
  # outcome variable depends on selection
  if (model_outcome == "level") {
    outcome_var <- "outcome_grade"
  } else if (model_outcome == "stand") {
    outcome_var <- "outcome_grade_stand"
  }
  
  # adjust column names because in function, e.g. brackets make problems
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\(", "")
  colnames(data_dml) <- str_replace_all(colnames(data_dml), "\\)", "")
  
  # Define outcome, treatment, and controls
  Y <- data_dml %>% select(all_of(outcome_var)) %>% pull()
  D <- data_dml %>% mutate(treatment_sport_freq = as.factor(treatment_sport_freq - 1)) %>% select(treatment_sport_freq) %>% pull() 
  X <- data_dml %>% select(-c(outcome_var, treatment_sport_freq, group))
  X <- model.matrix(~ ., data = X)
  values <- sort(unique(D))
  
  ## LASSO ##
  lasso_ATE <- dmlmt(X, D, Y, pl = FALSE)
  lasso_ATE <- data.frame(lasso_ATE$ATE)
  lasso_ATE$ml_model <- "lasso"
  lasso_ATE$Treatment <- rownames(lasso_ATE)
  rownames(lasso_ATE) <- NULL
  
  ## POST-LASSO ##
  postlasso_ATE <- dmlmt(X, D, Y, pl = TRUE) # post-lasso 
  postlasso_ATE <- data.frame(postlasso_ATE$ATE)
  postlasso_ATE$ml_model <- "postlasso"
  postlasso_ATE$Treatment <- rownames(postlasso_ATE)
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
  rf_ATE$Treatment <- rownames(rf_ATE)
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
  xgb_ATE$Treatment <- rownames(xgb_ATE)
  rownames(xgb_ATE) <- NULL
  
  # generate data frame with results
  df_result_function <- rbind(lasso_ATE, postlasso_ATE, rf_ATE, xgb_ATE) %>%
    mutate(MICE = mice_data_sel) %>%
    select(MICE, Treatment, ml_model, everything()) %>%
    mutate(Treatment = case_when(
      Treatment == "T1 - T0" ~ "monthly_weekly",
      Treatment == "T2 - T0" ~ "never_weekly",
      Treatment == "T2 - T1" ~ "never_monthly", 
      TRUE ~ NA
    ))
  df_result_function_list_multi <- append(df_result_function_list_multi, list(df_result_function))
  
  gc()
}

# Pool results across data sets by taking the mean
dml_final_multi <- data.frame()
for (mice_data_sel in 1:5) {
  dml_final_multi <- rbind(
    dml_final_multi,
    df_result_function_list_multi[[mice_data_sel]] %>% mutate(MICE = mice_data_sel) %>%
      select(MICE, everything())
  )
}


dml_final_multi <-
  dml_final_multi %>% 
  group_by(Treatment, ml_model) %>%
  mutate(across(c(TE, SE, t, p), ~ mean(.))) %>%
  select(-MICE) %>% distinct() %>% as.data.frame()

saveRDS(dml_final_multi, "Output/DML/FUNCTION_KNAUS_MULTI.rds")




