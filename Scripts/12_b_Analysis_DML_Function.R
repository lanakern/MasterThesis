#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DoubleML with R Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# OHNE LAGS: NICHT STATISTISCH SIGNIFIKANT
# MIT LAGS: STATISTISCH SIGNIFIKANT

# set seed for reproducible results
set.seed(1234)

# generate list for results
df_result_function_list <- list()

# iterate over mice data sets
for (mice_data_sel in 1:2) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
  # load data
  load_data <- paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
                      "_", treatment_def, "_", treatment_repl, "_mice", mice_data_sel, ".rds")
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
  
  # declare outcome, treatment, and controls
  data_func_model <- double_ml_data_from_data_frame(
    data_dml, y_col = outcome_var, d_cols = "treatment_sport",
    x_cols = data_dml %>% select(-c(all_of(outcome_var), treatment_sport, group)) %>% colnames()
  )
  
  # set up machine learning
  ml_g <- lrn("regr.cv_glmnet", nfolds = model_k_tuning, s = "lambda.min")
  ml_m <- lrn("classif.cv_glmnet", nfolds = model_k_tuning, s = "lambda.min")

  # run DML
    ## ATE
  dml_irm_ate <- DoubleMLIRM$new(
    data_func_model, ml_g, ml_m, score = "ATE",
    dml_procedure = "dml2", n_folds = model_k, n_rep = model_s_rep
  )
  dml_irm_ate$fit()
    ## ATTE
  dml_irm_atte <- DoubleMLIRM$new(
    data_func_model, ml_g, ml_m, score = "ATTE",
    dml_procedure = "dml2", n_folds = model_k, n_rep = model_s_rep
  )
  dml_irm_atte$fit()
  
  # calculate standard errors
  se_mean_ate <- sqrt(mean(c(
    dml_irm_ate$all_se[1,1]^2 + (dml_irm_ate$all_coef[1,1] - dml_irm_ate$coef)^2,
    dml_irm_ate$all_se[1,2]^2 + (dml_irm_ate$all_coef[1,2] - dml_irm_ate$coef)^2)))
  se_median_ate <- sqrt(median(c(
    dml_irm_ate$all_se[1,1]^2 + (dml_irm_ate$all_coef[1,1] - dml_irm_ate$coef)^2,
    dml_irm_ate$all_se[1,2]^2 + (dml_irm_ate$all_coef[1,2] - dml_irm_ate$coef)^2)))
  
  se_mean_atte <- sqrt(mean(c(
    dml_irm_atte$all_se[1,1]^2 + (dml_irm_atte$all_coef[1,1] - dml_irm_atte$coef)^2,
    dml_irm_atte$all_se[1,2]^2 + (dml_irm_atte$all_coef[1,2] - dml_irm_atte$coef)^2)))
  se_median_atte <- sqrt(median(c(
    dml_irm_atte$all_se[1,1]^2 + (dml_irm_atte$all_coef[1,1] - dml_irm_atte$coef)^2,
    dml_irm_atte$all_se[1,2]^2 + (dml_irm_atte$all_coef[1,2] - dml_irm_atte$coef)^2)))
  
  # calculate t-value
  tvalue_mean_ate <- mean(dml_irm_ate$all_coef) / se_mean_ate 
  tvalue_median_ate <- median(dml_irm_ate$all_coef) / se_median_ate 
  tvalue_mean_atte <- mean(dml_irm_atte$all_coef) / se_mean_atte 
  tvalue_median_atte <- median(dml_irm_atte$all_coef) / se_median_atte 
  
  # calculate p-value
  N <- nrow(data_dml)
  pvalue_mean_ate <- 2 * pt(abs(tvalue_mean_ate), N, lower.tail = FALSE) 
  pvalue_median_ate <- 2 * pt(abs(tvalue_median_ate), N, lower.tail = FALSE) 
  pvalue_mean_atte <- 2 * pt(abs(tvalue_mean_atte), N, lower.tail = FALSE) 
  pvalue_median_atte <- 2 * pt(abs(tvalue_median_atte), N, lower.tail = FALSE) 
  
  # generate data frame with results
  df_result_function <- data.frame(
    "Type" = c("ATE", "ATET"), "theta_mean" = c(mean(dml_irm_ate$all_coef), mean(dml_irm_atte$all_coef)),
    "theta_median" = c(median(dml_irm_ate$all_coef), median(dml_irm_atte$all_coef)),
    "se_mean" = c(se_mean_ate, se_median_ate), "se_median" = c(se_median_ate, se_median_atte),
    "tvalue_mean" = c(tvalue_mean_ate, tvalue_mean_atte),
    "tvalue_median" = c(tvalue_median_ate, tvalue_median_atte),
    "pvalue_mean" = c(pvalue_mean_ate, pvalue_mean_atte),
    "pvalue_median" = c(pvalue_median_ate, pvalue_median_atte)
  )
  df_result_function_list <- append(df_result_function_list, list(df_result_function))
}


# pool results from MICE
# calculate pooled estimate over multiple mice data sets
dml_result_function_pooled <- func_dml_pool_mice(df_result_function_list, nrow(data_dml), 2)

# append columns
dml_result_function_pooled <- dml_result_function_pooled %>%
  mutate(
    # append user selections
    model_type = model_type, model_algo = model_algo, model_k = model_k, 
    model_k_tuning = model_k_tuning, model_s_rep = model_s_rep, 
    model_outcome = model_outcome, model_controls = model_controls,
    # add date
    time_stamp = as.character(Sys.time())) %>%
  select(starts_with("model"), everything())


# save
dml_result_save <- as.data.frame(dml_result_function_pooled)
## replace same estimations
if (file.exists("Output/ESTIMATION_RESULTS_FUNCTION.xlsx")) {
  dml_result_save_all <- read.xlsx("Output/ESTIMATION_RESULTS_FUNCTION.xlsx", sheetName = "Sheet1")
  dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
  dml_result_save_all <- dml_result_save_all %>%
    group_by(across(starts_with("model"))) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  ## save
  write.xlsx(dml_result_save_all, "Output/ESTIMATION_RESULTS_FUNCTION.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
} else {
  write.xlsx(dml_result_save, "Output/ESTIMATION_RESULTS_FUNCTION.xlsx", row.names = FALSE)
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# data_lasso_func <- readRDS(paste0("Data/Prep_11/prep_11_dml_binary_base", "_mice", 1, ".rds"))
# data_lasso_func <- data_lasso_func %>% select(-c(ends_with("_lag")))
# data_lasso_func <- as.data.table(data_lasso_func)
# colnames(data_lasso_func) <- str_replace_all(colnames(data_lasso_func), "\\(", "")
# colnames(data_lasso_func) <- str_replace_all(colnames(data_lasso_func), "\\)", "")
# 
# data_func_model <- double_ml_data_from_data_frame(
#   data_lasso_func, y_col = "outcome_grade", d_cols = "treatment_sport",
#   x_cols = data_lasso_func %>% select(-c(outcome_grade, starts_with("treatment"), group)) %>% colnames()
#   )
# print(data_func_model)
# 
# 
# ml_g <- lrn("regr.cv_glmnet", nfolds = 5, s = "lambda.min")
# ml_m <- lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")
# 
# dml_irm_atte <- DoubleMLIRM$new(
#   data_func_model, ml_g, ml_m, score = "ATTE",
#   dml_procedure = "dml2", n_folds = 5, n_rep = 2
#   )
# dml_irm_atte$fit()
# dml_irm_atte$summary()
# print(dml_irm_atte)
# 
# 
# dml_irm_atte$coef
# dml_irm_atte$all_coef
# median(dml_irm_atte$all_coef)
# 
# dml_irm_atte$all_se
# dml_irm_atte$all_se[1,1]^2 + (dml_irm_atte$all_coef[1,1] - dml_irm_atte$coef)^2
# dml_irm_atte$all_se[1,2]^2 + (dml_irm_atte$all_coef[1,2] - dml_irm_atte$coef)^2
# sqrt(mean(c(dml_irm_atte$all_se[1,1]^2 + (dml_irm_atte$all_coef[1,1] - dml_irm_atte$coef)^2,
#             dml_irm_atte$all_se[1,2]^2 + (dml_irm_atte$all_coef[1,2] - dml_irm_atte$coef)^2)))
# dml_irm_atte$se
# 
# 
# sqrt(var(dml_irm_atte$psi)/N)
# 
# 
# with parameter tuning
# remotes::install_github("mlr-org/paradox")
# library(paradox)
# library(mlr3tuning)
# # set logger to omit messages during tuning and fitting
# lgr::get_logger("mlr3")$set_threshold("warn")
# lgr::get_logger("bbotk")$set_threshold("warn")
# 
# ml_g <- lrn("regr.cv_glmnet") # do not specify any parameter
# ml_m <- lrn("classif.cv_glmnet") # do not specify any parameter
# dml_irm_atte <- DoubleMLIRM$new(data_func_model, ml_g, ml_m, n_folds = model_k, n_rep = model_s_rep, dml_procedure = "dml2", score = "ATTE")
#   ## specify grid of values
# par_grids = list(
#   "ml_g" = ParamSet$new(list(
#     ParamDbl$new("lambda", lower = 0.05, upper = 0.1))),
#   "ml_m" = ParamSet$new(list(
#     ParamDbl$new("lambda", lower = 0.05, upper = 0.1))))
# # generate_design_grid(par_grids$ml_m, resolution = 11)
#   ## provide tune settings
# tune_settings <- list(terminator = trm("evals", n_evals = 100),
#                       algorithm = tnr("grid_search", resolution = 11),
#                       rsmp_tune = rsmp("cv", folds = 5))
#   ## tune
# dml_irm_atte$tune(param_set = par_grids, tune_settings = tune_settings, tune_on_folds = TRUE)
#   ## acces tuning results for target variable "X1"
# dml_irm_atte$tuning_res$X1
#   ## tuned parameters
# str(dml_irm_atte$params)
#   ## estimate model and summary
# dml_irm_atte$fit()
# dml_irm_atte$summary()
