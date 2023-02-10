#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION DOUBLE ML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function performs the DML estimator.
# INPUTS:
# -> data: data set containing outcome, treatment, and all confounding variables
# -> outcome: string containing the name of the outcome variable in data
# -> treatment: string containing the name of the treatment variable in data
# -> group: string containing the variable of the grouping variable in data;
# this is the same individual has the same number (user for k-fold cross-fitting)
# -> K: Number of data partitions used for K-fold cross-fitting
# -> K_tuning: Number of data partitions for parameter tuning
# -> S: Number of repetitions
# -> mlalgo: Machine learning algorithm used for the prediction of the nuisance
# parameters.
# -> trimming: Trimming threshold to drop observations with an extreme propensity
# score estimate. Possible selections: 0.01, 0.1, and min-max.
# OUTPUT: List containing the following elements
# -> "final": data frame with aggregated mean and median treatment effect,
# corresponding standard error, t- and p-value as well as confidence interval,
# Aggregations are made across the K folds and S repetitions
# -> "detail": same as final however not aggregated but for each repetition S
# -> "error": error metrics across all K and S
# -> "param": selected hyperparameters during K_tuning-CV for each K and S
# -> "trimming": number of treatment periods after conducting trimming for each S
# -> "predictors": number of predictors (X)
# -> "coef": if lasso algorithm is selected, all non-zero coefficients are returned
#+++
# In this file, the following subfunctions are used:
# -> Functions for predicting the nuisance parameters: func_ml_lasso
# -> Function for trimming: func_dml_trimming
# -> Function to plot common support: func_dml_common_support
# -> Function for error metrics: func_dml_error_metrics
# -> Function for calculating treatment effect and corresponding score: func_dml_theta_score
# -> Function for inference (se, p-value etc.): func_dml_inference
#+++
# Further notes:
# - If (post-)lasso is selected, all features (except the outcome and treatment)
# are standardized, i.e., to have mean zero and unit variance.
# Note: depending on the specification the outcome variable may already be
# standardized in the original data set.
#+++


func_dml <- function(data, outcome, treatment, group, K, K_tuning, S, mlalgo, trimming, save_trimming) {
  
  # define hyperparameters
  num_X <- ncol(data) - 3 # number of controls (minus outcome, treatment, and group)
    ## lasso
  # lambda_val <- 1000
  #   ## xgboost
  # xgb_grid <- expand.grid(
  #   tree_depth = c(3, 6, 9), # default: 6
  #   trees = c(5, 15, 100), # default: 15
  #   learn_rate = c(0.01, 0.1, 0.3), # default: 0.3
  #   mtry = c(10, round(sqrt(num_X)), round(num_X)), # default: p (X)
  #   min_n = c(1, 5) # default: 1
  # )
  #   ## random forests
  # rf_grid <- expand.grid(
  #   trees = c(1000), # default: 500
  #   mtry = c(5, 10, floor(sqrt(num_X)), floor(num_X/3), round(num_X)), # default: floor(sqrt(num_X)) for classification and floor(num_X/3) for regression
  #   min_n = c(1, 5, 10, 15) # default: 5 for regression and 10 for classification
  # )
  
  # SMALL GRIDS
  lambda_val <- 100
  ## xgboost
  xgb_grid <- expand.grid(
    tree_depth = c(3, 6), # default: 6
    trees = c(15), # default: 15
    learn_rate = c(0.01, 0.3), # default: 0.3
    mtry = c(floor(sqrt(num_X)), round(num_X)), # default: p (X)
    min_n = c(1) # default: 1
  )
  ## random forests
  rf_grid <- expand.grid(
    trees = c(500), # default: 500
    mtry = c(floor(sqrt(num_X)), floor(num_X/3), round(num_X)), # default: floor(sqrt(num_X)) for classification and floor(num_X/3) for regression
    min_n = c(5) # default: 5 for regression and 10 for classification
  )
  
  # generate empty data frames
  df_pred_all <- data.frame()
  df_param_all <- data.frame()
  df_coef_all <- data.frame()
  df_error_all <- data.frame()
  df_result_all_detailed <- data.frame()
  df_trimming_all <- data.frame()
  
  # generate empty vectors
  min_trimming_all <- c()
  max_trimming_all <- c()
  theta_ATE_all <- c()
  score_ATE_all <- c()
  theta_ATTE_all <- c()
  score_ATTE_all <- c()
  APO_0_all <- c()
  APO_1_all <- c()

  
  # Accounting for uncertainty by repeating the process S times #
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  for (S_rep in 1:S) {
    
    print(paste("Repetition", S_rep))
    
    # generate folds based on ID grouping and stratified sampling
    # "v": number of folds / partitions specified by K 
    # "group": specifies the grouping variable
    # "strata": outcome variable used for conducting stratified sampling (numeric variable is binned into quartiles)
    # balance = "observations": assigns roughly the same number of observations to each fold
    # https://rsample.tidymodels.org/reference/group_vfold_cv.html
    # https://scikit-learn.org/stable/modules/cross_validation.html#group-k-fold
    K_folds <- rsample::group_vfold_cv(
      data = data,  v = K, group = group, 
      strata = c(all_of(outcome), all_of(treatment)), balance = "observations"
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
      
      
      #++++++++++++++++++++++++++++++++++++++++++++#
      #### PREDICT NUISANCE PARAMETER FUNCTIONS ####
      #++++++++++++++++++++++++++++++++++++++++++++#
      
      # select machine learning algorithm based on user selection and make predictions
      if (mlalgo == "postlasso") {
        
        ## POST-LASSO ##
        #++++++++++++++#
        
        # predict nuisance functions via lasso
        pls_ml <- func_ml_postlasso(data_train, data_test, outcome, treatment, group, K_tuning, lambda_val)
        
        # append predictions to data frame
        data_pred <- pls_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- pls_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        # append non-zero coefficients to data frame
        data_coef <- pls_ml$coef
        data_coef <- data_coef %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_coef_all <- rbind(df_coef_all, data_coef)
        
      } else if (mlalgo == "lasso") {
        
        ## LASSO ##
        #+++++++++#
        
        # predict nuisance functions via lasso
        ls_ml <- func_ml_lasso(data_train, data_test, outcome, treatment, group, K_tuning, lambda_val)
        
        # append predictions to data frame
        data_pred <- ls_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- ls_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        # append non-zero coefficients to data frame
        data_coef <- ls_ml$coef
        data_coef <- data_coef %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_coef_all <- rbind(df_coef_all, data_coef)
        
      } else if (mlalgo == "xgboost") {
        
        ## XGBoost ##
        #+++++++++++#
        
        xgb_ml <- func_ml_xgboost(data_train, data_test, outcome, treatment, group, K_tuning, xgb_grid)
        
        # append predictions to data frame
        data_pred <- xgb_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- xgb_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        
      } else if (mlalgo == "randomforests") {
        
        ## RANDOM FORESTS ##
        #++++++++++++++++++#
        
        rf_ml <- func_ml_rf(data_train, data_test, outcome, treatment, group, K_tuning, rf_grid)
        
        # append predictions to data frame
        data_pred <- rf_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- rf_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        
      } else {
        stop("Please select a Machine Learning Algorithm to predict the nuisance parameters.")
      }
      

      
      #++++++++++++++++#
      #### TRIMMING ####
      #++++++++++++++++#
      
      # for sub-setting adjust row names / numbering of rows
      data_pred <- data.frame(data_pred)
      data_test <- data.frame(data_test)
      
      rownames(data_pred) <- 1:nrow(data_pred)
      rownames(data_test) <- 1:nrow(data_test)
      
      ls_trimming <- func_dml_trimming(data_pred, data_test, trimming)
      data_pred <- ls_trimming$data_pred
      data_test <- ls_trimming$data_test
      min_trimming <- ls_trimming$min_trimming
      min_trimming_all <- c(min_trimming_all, min_trimming)
      max_trimming <- ls_trimming$max_trimming
      max_trimming_all <- c(max_trimming_all, max_trimming)
      
      # generate data frame with number of treatment periods kept
      df_trimming <- data.frame("Repetition" = S_rep, "Fold" = fold_sel, 
                                "n_treats" = nrow(data_pred))
      df_trimming_all <- rbind(df_trimming_all, df_trimming)
      
      
      #+++++++++++++++++++++#
      #### ERROR METRICS ####
      #+++++++++++++++++++++#
      
      df_error <- func_ml_error_metrics(data_pred, S_rep, fold_sel)
      df_error_all <- rbind(df_error_all, df_error)
      
      
      #+++++++++++++++++++++++++#
      #### TREATMENT EFFECTS ####
      #+++++++++++++++++++++++++#

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
      
      APO_0 <- ls_treatment_effects$APO_0
      APO_0_all <- c(APO_0_all, APO_0)
      APO_1 <- ls_treatment_effects$APO_1
      APO_1_all <- c(APO_1_all, APO_1)
      
    } # close iteration over k folds
    
    
    #++++++++++++++++++++++#
    #### COMMON SUPPORT ####
    #++++++++++++++++++++++#
    
    # only save common support plot for first iteration (S_rep = 1) and if it
    # is wished to save (that is only for main model)
    if (S_rep == 1 & save_trimming == TRUE) {
      plot_common_support <- func_dml_common_support(df_pred_all, min_trimming_all, max_trimming_all)
      ggsave(paste0("Output/plot_common_support", model_algo, ".png"), plot_common_support)
    }
    
    
    #+++++++++++++++++#
    #### INFERENCE ####
    #+++++++++++++++++#
    
    # number of observations in complete data set 
    N <- nrow(data) 
    
    # inference
    df_result_ATE <- func_dml_inference("ATE", theta_ATE_all, score_ATE_all, N, S_rep)
    df_result_ATTE <- func_dml_inference("ATTE", theta_ATTE_all, score_ATTE_all, N, S_rep)
    
    # APO
    df_result_AP00 <- data.frame(
      "Type" = "APO_0", "Rep" = S_rep, "Treatment_Effect" = mean(APO_0_all),
      Variance = NA, Standard_Error = NA, T_Value = NA, P_Value = NA
    )
    
    df_result_AP01 <- data.frame(
      "Type" = "APO_1", "Rep" = S_rep, "Treatment_Effect" = mean(APO_1_all),
      Variance = NA, Standard_Error = NA, T_Value = NA, P_Value = NA
      )
    
    df_result <- rbind(df_result_ATE, df_result_ATTE, df_result_AP00, df_result_AP01) 
    df_result_all_detailed <- rbind(df_result_all_detailed, df_result)
    
  } # close iteration over S repetitions  

  
  #+++++++++++++++++++++#
  #### FINAL RESULTS ####
  #+++++++++++++++++++++#
  
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
    
  
  # trimming: sum over folds
  df_trimming_all <- df_trimming_all %>%
    group_by(Repetition) %>%
    summarize(n_treats = sum(n_treats))
  
  
  # number of predictors
  df_pred_all <- df_pred_all %>% select(Repetition, Fold, starts_with("num_pred")) %>% distinct()
  
  
  # coefficients are only returned for lasso
  if (str_detect(mlalgo, "lasso")) {
    return(list("final" = df_result_all, "detail" = df_result_all_detailed,
                "error" = df_error_all, "param" = df_param_all,
                "trimming" = df_trimming_all, "predictors" = df_pred_all,
                "coef" = df_coef_all))
  } else {
    return(list("final" = df_result_all, "detail" = df_result_all_detailed,
                "error" = df_error_all, "param" = df_param_all,
                "trimming" = df_trimming_all, "predictors" = df_pred_all))
  }

  
  
}


## EXAMPLE ##
#+++++++++++#

# set.seed(12345)
# data <- readRDS("Data/Prep_11/prep_11_dml_binary_base_weekly_down_mice1.rds")
# data <- data %>% select(-c(ends_with("_lag")))
# outcome <- "outcome_grade"
# treatment <- "treatment_sport"
# group <- "group"
# K <- 2
# K_tuning <- 2
# S <- 2
# mlalgo <- "lasso"
# trimming <- "min-max"
# ls_dml_result <- func_dml(data, "outcome_grade", "treatment_sport", "group", 5, 10, 2, "lasso", "min-max")
# ls_dml_result$final
# ls_dml_result$detail
# ls_dml_result$error
# ls_dml_result$param
# ls_dml_result$trimming