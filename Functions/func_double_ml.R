#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION DOUBLE ML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

# PROPENSITY SCORES
# "To reduce the disproportionate impact of extreme propensity score weights in 
# the interactive model we trim the propensity scores which are close to the bounds."

library(rsample) # for group_vfold_cv()
library(glmnet) # for glmnet() (-> lasso)
library(hdm) # for rlasso() (-> post-lasso)
library(xgboost) # for xgboost()
library(dplyr)


func_dml <- function(data, outcome, treatment, group, K, S, mlalgo, trimming) {
  
  # generate empty data frames
  df_result_all_detailed <- data.frame()
  data_pred_lasso_error_all <- data.frame()
  
  # Accounting for uncertainty by repeating the process S times
  for (S_rep in 1:S) {
    
    print(paste("Repetition", S_rep))
    
    # generate folds based on ID grouping and stratified sampling
    # "v": number of folds / partitions specified by K 
    # "group": specifies the grouping variable
    # "strata": outcome variable used for conducting stratified sampling (numeric variable is binned into quartiles)
    # balance = "observations": assigns roughly the same number of observations to each fold
    # https://rsample.tidymodels.org/reference/group_vfold_cv.html
    # https://scikit-learn.org/stable/modules/cross_validation.html#group-k-fold
    K_folds <- group_vfold_cv(
      data = data,  v = K, group = "group", strata = outcome, balance = "observations"
    )
    
    
    #++++++++++++++++++++++++++++++++++++++++#
    ## PREDICT NUISANCE PARAMETER FUNCTIONS ##
    #++++++++++++++++++++++++++++++++++++++++#

    # select machine learning algorithm based on user selection
    if (mlalgo == "postlasso") {
      model_func <- rlasso
    } else if (mlalgo == "lasso") {
      ls_pred_lasso <- func_ml_lasso(data, outcome, treatment, K_folds, S_rep, trimming)
      data_pred_lasso_error <- ls_pred_lasso$error_metrics
      data_pred_lasso_error_all <- rbind(data_pred_lasso_error_all, data_pred_lasso_error)
      data_pred_lasso <- ls_pred_lasso$pred
    } else if (mlalgo == "xgboost") {
      model_func <- xgboost
    } else {
      stop("Please select a Machine Learning Algorithm to predict the nuisance parameters.")
    }
  

    #+++++++++++++++++++++#
    ## TREATMENT EFFECTS ##
    #+++++++++++++++++++++#
    
    # calculate ATE and ATTE as well as their respective score function values
    # across the K folds
    ls_treatment_effects <- 
      func_dml_theta_score(data, "outcome_grade", "treatment_sport", data_pred_lasso, K_folds)
    theta_ATE_all <- ls_treatment_effects$theta_ATE_all
    score_ATE_all <- ls_treatment_effects$score_ATE_all
    theta_ATTE_all <- ls_treatment_effects$theta_ATTE_all
    score_ATTE_all <- ls_treatment_effects$score_ATTE_all
    

    #+++++++++++++#
    ## INFERENCE ##
    #+++++++++++++#
    
    # number of observations in complete data set 
    N <- nrow(data) 
    
    # inference
    df_result_ATE <- func_dml_inference("ATE", theta_ATE_all, score_ATE_all, N, S_rep)
    df_result_ATTE <- func_dml_inference("ATTE", theta_ATTE_all, score_ATTE_all, N, S_rep)
    
    df_result <- rbind(df_result_ATE, df_result_ATTE) 
    df_result_all_detailed <- rbind(df_result_all_detailed, df_result)
    
  } # close iteration over S repetitions  

  # detailed output
  df_result_all_detailed <- df_result_all_detailed %>% 
    mutate(ML_algo = mlalgo) %>%
    select(ML_algo, everything())

  # final output: take mean and median across folds
  df_result_all <- df_result_all_detailed %>%
    select(Type, ML_algo, Treatment_Effect, Standard_Error) %>%
    group_by(Type) %>% 
    mutate(
      theta_mean = mean(Treatment_Effect),
      theta_median = median(Treatment_Effect),
      se_mean = sqrt(mean(Standard_Error^2 + (Treatment_Effect - theta_mean)^2)),
      se_median = median(sqrt(Standard_Error^2 + (Treatment_Effect - theta_median)^2)),
      tvalue_mean = theta_mean / se_mean,
      tvalue_median = theta_median / se_median ,
      pvalue_mean = 2 * pt(abs(tvalue_mean), N, lower.tail = FALSE),
      pvalue_median = 2 * pt(abs(tvalue_median), N, lower.tail = FALSE),
      CI_lower_mean_95 = theta_mean - qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_mean / sqrt(N),
      CI_upper_mean_95 = theta_mean + qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_mean / sqrt(N),
      CI_lower_median_95 = theta_median - qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_median / sqrt(N),
      CI_upper_median_95 = theta_median + qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_median / sqrt(N)
    ) %>%
    select(-c(Treatment_Effect, Standard_Error)) %>% distinct()
    
  
  return(list("final" = df_result_all, "detail" = df_result_all_detailed))
  
}

set.seed(12345)
data <- readRDS("Data/Prep_11/prep_11_final_data_binary_lasso_base.rds")
data <- data %>% select(-c(ends_with("_lag")))
ls_dml_result <- func_dml(data, "outcome_grade", "treatment_sport", "group", 2, 2, "lasso")
ls_dml_result$final
ls_dml_result$detail