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


func_dml <- function(data, outcome, treatment, group, K, K_tuning, S, mlalgo, trimming) {
  
  # define inputs
  lambda_val <- 1000
  
  # generate empty data frames
  data_pred_all <- data.frame()
  data_error_all <- data.frame()
  df_result_all_detailed <- data.frame()
  
  # generate empty vectors
  theta_ATE_all <- c()
  score_ATE_all <- c()
  theta_ATTE_all <- c()
  score_ATTE_all <- c()
  
  
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
    
    
    # iterate over the folds #
    #++++++++++++++++++++++++#
    
    for (fold_sel in 1:K) {
      
      print(paste("Fold", fold_sel))
      
      # if lasso is selected, predictors need to be standardized
      # otherwise, predictors are left as they are
      if (str_detect(mlalgo, "lasso")) {
        # select predictors that are standardized
        # -> all except outcome, treatment, and group variable
        data_cols <- data %>% 
          select(-c(all_of(outcome), all_of(treatment), group)) %>% 
          colnames()
        
        # standardize features (mean zero and standard deviation of one)
        data <- data %>%
          recipe(treatment_sport ~ .) %>%
          step_normalize(all_of(data_cols)) %>%
          prep() %>%
          bake(new_data = NULL)
      } else {
        data <- data
      }

      # extract training and test data
      indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
      data_train <- data[indices_fold_sel, ] 
      data_test <- data[-indices_fold_sel, ]
      
      
      # NICHT SICHER - NOCHMAL ÜBERLEGEN
      # training and test data is different for m(X) and g(D, X) prediction
        ## for m(X) drop all columns referring to the treatment + outcome
      cols_treatment_drop <- data_train %>% select(starts_with("treatment")) %>% colnames()
      cols_treatment_drop <- cols_treatment_drop[!str_detect(
        cols_treatment_drop, paste0("^", treatment, "$"))]
      
      data_train_m <- data_train %>% 
        select(-c(starts_with("outcome"), all_of(cols_treatment_drop))) %>%
        mutate({{treatment}} := as.factor(!!sym(treatment))) # treatment variable as factor
        ## for g(D, X) drop all outcome variables
      cols_outcome_drop <- data_train %>% select(starts_with("outcome")) %>% colnames()
      cols_outcome_drop <- cols_outcome_drop[!str_detect(cols_outcome_drop, paste0("^", outcome, "$"))]
      
      data_train_g <- data_train %>% select(-c(all_of(cols_treatment_drop), all_of(cols_outcome_drop)))
      
      
      #++++++++++++++++++++++++++++++++++++++++#
      ## PREDICT NUISANCE PARAMETER FUNCTIONS ##
      #++++++++++++++++++++++++++++++++++++++++#
      
      # select machine learning algorithm based on user selection and make predictions
      if (mlalgo == "postlasso") {
        model_func <- rlasso
      } else if (mlalgo == "lasso") {
        data_pred <- func_ml_lasso(data_train_m, data_train_g, data_test, outcome, treatment, K_tuning, lambda_val)
      } else if (mlalgo == "xgboost") {
        model_func <- xgboost
      } else {
        stop("Please select a Machine Learning Algorithm to predict the nuisance parameters.")
      }
      
      # append predictions to data frame
      data_pred <- data_pred %>% mutate(fold = fold_sel)
      data_pred_all <- rbind(data_pred_all, data_pred)
      
      
      #++++++++++++#
      ## TRIMMING ##
      #++++++++++++#
      
      # for sub-setting adjust row names / numbering of rows
      data_pred <- data.frame(data_pred)
      data_test <- data.frame(data_test)
      
      rownames(data_pred) <- 1:nrow(data_pred)
      rownames(data_test) <- 1:nrow(data_test)
      
      ls_trimming <- func_dml_trimming(data_pred, data_test, "min-max")
      data_pred <- ls_trimming$data_pred
      data_test <- ls_trimming$data_test
      
      
      #+++++++++++++++++#
      ## ERROR METRICS ##
      #+++++++++++++++++#
      
      
      df_error <- func_ml_error_metrics(data_pred, S_rep, fold_sel)
      data_error_all <- rbind(df_error)
      
      #+++++++++++++++++++++#
      ## TREATMENT EFFECTS ##
      #+++++++++++++++++++++#

      # calculate ATE and ATTE as well as their respective score function values
      # across the K folds
      ls_treatment_effects <- func_dml_theta_score(data_pred, data_test, outcome, treatment)
      
      theta_ATE <- ls_treatment_effects$theta_ATE
      theta_ATE_all <- c(theta_ATE_all, theta_ATE)
      
      score_ATE <- ls_treatment_effects$score_ATE
      score_ATE_all <- c(score_ATE_all, score_ATE)
      
      theta_ATTE <- ls_treatment_effects$theta_ATTE
      theta_ATTE_all <- c(theta_ATTE_all, theta_ATTE)
      
      score_ATTE <- ls_treatment_effects$score_ATTE
      score_ATTE_all <- c(score_ATTE_all, score_ATTE)
      
    } # close iteration over k folds
    
    
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
    
  
  return(list("final" = df_result_all, "detail" = df_result_all_detailed,
              "error" = df_error))
  
}

set.seed(12345)
data <- readRDS("Data/Prep_11/prep_11_final_data_binary_lasso_base.rds")
data <- data %>% select(-c(ends_with("_lag")))
ls_dml_result <- func_dml(data, "outcome_grade", "treatment_sport", "group", 5, 5, 2, "lasso", "min-max")
# ls_dml_result$final
# ls_dml_result$detail