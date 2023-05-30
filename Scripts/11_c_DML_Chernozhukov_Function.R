#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DoubleML with R Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++ 
# by Lana Kern
#+++
# In this file, the credibility of the hand-coded DML procedure is assessed
# by comparing the ATE and ATTE estimates based on the nuisance parameter
# predictions by LASSO with the results obtained by the DoubleML function of
# Chernozhukov et al. 2017.
# Note that they require at least 3-fold CV in the parameter tuning process.
#+++

# set seed for reproducible results
set.seed(1234)

# generate list for results
df_result_function_list <- list()
outcome <- "outcome_grade"
outcome_var <- "outcome_grade"
treatment <- "treatment_sport"

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
    # # lags for all other personality variables are dropped
    # colnames_bigfive_lag_drop <- data_dml %>% 
    #   dplyr::select(starts_with("bigfive") & ends_with("lag") & !matches(outcome_var_old)) %>% colnames()
    # data_dml <- data_dml %>% 
    #   dplyr::select(-all_of(colnames_bigfive_lag_drop))
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
  
  # declare outcome, treatment, and controls
  data_func_model <- double_ml_data_from_data_frame(
    data_dml, y_col = outcome_var, d_cols = "treatment_sport",
    x_cols = data_dml %>% dplyr::select(-c(all_of(outcome_var), treatment_sport, group)) %>% colnames()
  )
  
  # set up machine learning
  ml_g <- lrn("regr.cv_glmnet", nfolds = 3, s = "lambda.min")
  ml_m <- lrn("classif.cv_glmnet", nfolds = 3, s = "lambda.min")

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
    "Treatment" = "no_yes",
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
dml_result_function_pooled <- func_dml_pool_mice(df_result_function_list, nrow(data_dml), 5)

# append columns
dml_result_function_pooled <- dml_result_function_pooled %>%
  mutate(
    # append outcome
    outcome = str_remove(outcome_var, "outcome_"),
    # append model selections
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, treatment_def = treatment_def, 
    extra_act = extra_act, 
    # append user selections
    model_type = model_type, model_algo = "lasso", model_k = model_k, 
    model_k_tuning = model_k_tuning, model_s_rep = model_s_rep, model_trimming = model_trimming, 
    model_controls_lag = model_controls_lag, model_controls_endog = model_controls_endog,
    model_hyperparam_sel = "best",
    # add date
    time_stamp = as.character(Sys.time())) %>%
  dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act,  
                starts_with("model"), everything())


# save
dml_result_save <- as.data.frame(dml_result_function_pooled)
saveRDS(dml_result_save, "Output/DML/Treatment_Effects/chernozhukov_binary_lasso_aggregated.rds")
saveRDS(df_result_function_list, "Output/DML/Treatment_Effects/chernozhukov_binary_lasso_detail.rds")

