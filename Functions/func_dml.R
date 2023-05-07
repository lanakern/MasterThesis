#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### WRAPPER FUNCTION FOR DML PROCEDURE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function performs the DML estimator in both the binary and multivalued
# treatment setting. Moreover, it can be used for both outcomes (grades and the
# big five personality traits). It is a wrapper function including multiple smaller 
# sub-functions which are needed to obtain the final result.
#+++
# In this file, the following subfunctions are used:
# -> Functions for predicting the nuisance parameters: func_ml_lasso, func_ml_postlasso,
# func_ml_postlasso_tuning_binary, func_ml_postlasso_tuning_multi, 
# func_ml_xgboost, func_ml_rf
# -> Function for trimming: func_dml_trimming
# -> Function to plot common support: func_dml_common_support
# -> Function for error metrics: func_dml_error_metrics
# -> Function for calculating treatment effect and corresponding score: func_dml_theta_score
# -> Function for inference (se, p-value etc.): func_dml_inference
#+++
# INPUTS:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> data: data set containing outcome, treatment, and all confounding variables
# -> outcome: string containing the name of the outcome variable in data. Must start with "outcome_".
# -> treatment: string containing the name of the treatment variable in data. Must start with "treatment_".
# -> group: string containing the variable of the grouping variable in data;
# this is the same individual has the same number (used for k-fold cross-fitting)
# -> K: Number of data partitions used for K-fold cross-fitting. Must be at least 2.
# -> K_tuning: Number of data partitions for parameter tuning. Must be at least 2, for
# random forests it can also be 1 (no parameter tuning is conducted).
# -> S: Number of repetitions
# -> mlalgo: Machine learning algorithm used for the prediction of the nuisance
# parameters. Choices are: "lasso", "postlasso", "xgboost", and "randomforests"
# -> trimming: Trimming threshold to drop observations with an extreme propensity
# score estimate. Possible selections: 0.01, 0.1, and min-max.
# -> save_trimming: If common support plot is saved.
# -> "probscore_separate": only relevant for multivalued treatment setting ->
# if TRUE (default) multivalued treatment setting is split in binary treatment setting
# and separate binary logistic regressions are performed. 
# -> "mice_sel": mice data frame
#+++
# OUTPUT: List containing the following elements
# -> "final": data frame with aggregated mean and median treatment effect,
# corresponding standard error, t- and p-value as well as confidence interval,
# Aggregations are made across the K folds and S repetitions
# -> "detail": same as final however not aggregated but for each repetition S
# -> "error": error metrics across all K and S
# -> "param": selected hyperparameters during K_tuning-CV for each K and S
# -> "trimming": number of treatment periods after conducting trimming for each S
# -> "predictors": number of predictors (X)
# -> "pred": nuisance parameter predictions
# -> "coef": if (post-)lasso algorithm is selected, all non-zero coefficients are returned
# -> "cov_balance": if post-lasso is selected covariate balance assessment is returned
# -> "hyperparam_sel": how to select hyperparameter combination in parameter tuning.
# "best" according to smallest RMSE / highest AUC or "1SE" according to one standard
# error rule in favor of more simpler model or "1SE_plus" in favor of more
# complex model (used as sensitivity check).
# -> "post": determines if normal LASSO or post-LASSO is performed (post = TRUE)
#+++
# Further notes:
# - The outcome variable is always standardized, i.e., to have mean zero and unit variance.
# - If (post-)lasso is selected, all features are standardized by default. 
#+++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_dml <- function(treatment_setting, data, outcome, treatment, group, K, K_tuning, S, mlalgo, 
                     trimming, save_trimming, probscore_separate = TRUE, mice_sel, 
                     hyperparam_sel = "best", post = FALSE) {
  
  # check for missings in date
  if (sum(is.na(data)) > 0) {
    stop("Data contains missing values.")
  }
  
  ## FOR SAVING RESULTS ##
  
  # generate empty data frames / lists
  df_pred_all <- data.frame()
  df_param_all <- data.frame()
  df_imp_all <- data.frame()
  df_coef_all <- data.frame()
  data_coef_lasso_all <- data.frame()
  data_cov_bal_fold <- list()
  data_cov_bal_rep <- list()
  df_cov_bal_all <- list()
  df_error_all <- data.frame()
  df_result_all_detailed <- data.frame()
  df_trimming_all <- data.frame()
  df_pred_bef_trimming_all <- data.frame()
  
  # generate empty vectors
  min_trimming_all <- c()
  max_trimming_all <- c()
  
  if (treatment_setting == "binary") {
    theta_ATE_all <- c()
    score_ATE_all <- c()
    theta_ATTE_all <- c()
    score_ATTE_all <- c()
    APO_0_all <- c()
    APO_1_all <- c()
  } else if (treatment_setting == "multi") {
    theta_ATE_all <- data.frame()
    score_ATE_all <- data.frame()
    theta_ATTE_all <- data.frame()
    score_ATTE_all <- data.frame()
    APO_1_all <- c()
    APO_2_all <- c()
    APO_3_all <- c()
  }
  
  
  #### HYPERPARAMETERS ####
  #+++++++++++++++++++++++#
  
  # number of predictors
  if (treatment_setting == "binary") {
    num_X <- ncol(data) - 3 # number of controls (minus outcome, treatment, and group)
  } else {
    num_X <- ncol(data) - 6 # number of controls (minus outcome, treatment, three treatment dummies, and group)
  }
  
  ## (post-)lasso
  lambda_val <- 100
  # if (mlalgo == "lasso") {
  #   lambda_val <- 100
  # } else {
  #   lambda_val <- 20 # for post-lasso as it is computationally more expensive
  # }
  
  ## xgboost
  if (str_detect(outcome, "grade")) {
    xgb_grid <- expand.grid(
      tree_depth = c(3, 6, 9), # default: 6
      trees = 50, #c(15, 50, 100), # default: 15 
      learn_rate = c(0.01, 0.3), # default: 0.3
      mtry = c(floor(sqrt(num_X)), round(num_X)/2, round(num_X)), # default: p (X)
      min_n = c(1, 3) # default: 1
    )
  } else {
    xgb_grid <- expand.grid(
      tree_depth = c(3, 6, 9), # default: 6
      trees = c(50), # default: 15 (changed based on previous results, theory, and computation time)
      learn_rate = c(0.01, 0.3), # default: 0.3
      mtry = c(floor(sqrt(num_X)), round(num_X / 2), round(num_X)), # default: p (X)
      min_n = c(1, 3) # default: 1
    )
  }

  # xgb_grid <- expand.grid(
  #   tree_depth = c(6), # default: 6
  #   trees = c(15), # default: 15
  #   learn_rate = c(0.3), # default: 0.3
  #   mtry = c(round(num_X)), # default: p (X)
  #   min_n = c(1) # default: 1
  # )
  
  ## random forests
  rf_grid <- expand.grid(
    trees = c(500), # default: 500
    mtry = c(floor(sqrt(num_X)), floor(num_X/3), round(num_X)), # default: floor(sqrt(num_X)) for classification and floor(num_X/3) for regression
    min_n = c(5, 10) # default: 5 for regression and 10 for classification
  )
  
  
  #### STANDARDIZATION ####
  #+++++++++++++++++++++++#
  
  # If lasso is selected, predictors need to be standardized.
  # Otherwise, predictors are left as they are.
  if (str_detect(mlalgo, "lasso")) {
    # select predictors that are standardized
    # -> all except outcome, treatment, and group variable
    if (treatment_setting == "multi") {
      data_cols <- data %>% 
        dplyr::select(-c(all_of(outcome), all_of(treatment), group, starts_with("treatment_sport_freq") & !ends_with("na"))) %>% 
        colnames()
    } else {
      data_cols <- data %>% 
        dplyr::select(-c(all_of(outcome), all_of(treatment), group)) %>% 
        colnames()
    }
    
    # standardize features (mean zero and standard deviation of one)
    data <- data %>%
      recipe(.) %>%
      update_role({{treatment}}, new_role = "outcome") %>%
      step_normalize(all_of(data_cols)) %>%
      prep() %>%
      bake(new_data = NULL)
    
  } else {
    
    data <- data 
  }
  
  # However in any case the outcome variable is standardized.
  data <- data %>%
    recipe(.) %>%
    step_normalize(all_of(outcome)) %>%
    prep() %>%
    bake(new_data = NULL) %>% 
    as.data.frame()
  
  
  #### REPEATING THE PROCESS S TIMES ####
  #+++++++++++++++++++++++++++++++++++++#
  
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
    
    # calculate means across group since group_vfold_cv needs constant strata
    # across groups
    data_folds <- data %>%
      group_by(group) %>% 
      mutate(treatment_fold = mean(!!rlang::sym(treatment))) %>%
      dplyr::select(group, treatment_fold) %>%
      ungroup()
    
    K_folds <- rsample::group_vfold_cv(
      data = data_folds,  v = K, group = group, 
      strata = treatment_fold, balance = "observations"
    )
    
    
    # iterate over the folds #
    #++++++++++++++++++++++++#
    
    for (fold_sel in 1:K) {
      
      print(paste("Fold", fold_sel))
      
      # extract training and test data
      indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
      data_train <- data[indices_fold_sel, ]
      data_test <- data[-indices_fold_sel, ]
      
      #++++++++++++++++++++++++++++++++++++++++++++#
      #### PREDICT NUISANCE PARAMETER FUNCTIONS ####
      #++++++++++++++++++++++++++++++++++++++++++++#
      
      # select machine learning algorithm based on user selection and make predictions
      # if (mlalgo == "postlasso") {
      #   
      #   ## POST-LASSO ##
      #   #++++++++++++++#
      #   
      #   # predict nuisance functions via lasso
      #   pls_ml <- func_ml_postlasso(
      #     treatment_setting = treatment_setting, data_train = data_train, data_test = data_test, 
      #     outcome = outcome, treatment = treatment, group = group, K = K_tuning, lambda_val = lambda_val
      #     )
      #   
      #   # append predictions to data frame
      #   data_pred <- pls_ml$pred
      #   data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
      #   df_pred_all <- rbind(df_pred_all, data_pred)
      #   
      #   # append tuning parameters to data frame
      #   data_param <- pls_ml$param
      #   data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
      #     dplyr::select(Repetition, Fold, everything())
      #   df_param_all <- rbind(df_param_all, data_param)
      #   
      #   # append non-zero coefficients to data frame
      #   data_coef <- pls_ml$coef
      #   data_coef <- data_coef %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
      #     dplyr::select(Repetition, Fold, everything())
      #   df_coef_all <- rbind(df_coef_all, data_coef)
      #   
      # } 
      if (str_detect(mlalgo, "lasso")) {
        
        ## LASSO ##
        #+++++++++#
        
        # predict nuisance functions via lasso
        ls_ml <- func_ml_lasso(
          treatment_setting = treatment_setting, data_train = data_train, data_test = data_test, 
          outcome = outcome, treatment = treatment, group = group, K = K_tuning, lambda_val = lambda_val,
          hyperparam_sel = hyperparam_sel, post = post
          )

        
        # append predictions to data frame
        data_pred <- ls_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        # df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- ls_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          dplyr::select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        # append non-zero coefficients to data frame
        data_coef <- ls_ml$coef
        data_coef <- data_coef %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          dplyr::select(Repetition, Fold, everything())
        df_coef_all <- rbind(df_coef_all, data_coef)
        
        # lasso coefficients for post lasso
        if (post == TRUE) {
          data_coef_lasso <- ls_ml$coef_lasso %>% mutate(Fold = fold_sel, Repetition = S_rep)
          data_coef_lasso_all <- rbind(data_coef_lasso_all, data_coef_lasso)
        }
        
      } else if (mlalgo == "xgboost") {
        
        ## XGBoost ##
        #+++++++++++#
        
        # make predictions
        xgb_ml <- func_ml_xgboost(
          treatment_setting, data_train, data_test, outcome, treatment, group, K_tuning, 
          xgb_grid, probscore_separate = probscore_separate, probscore_normalize = TRUE,
          model_hyperparam_sel
          )
        
        # append predictions to data frame
        data_pred <- xgb_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        # df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- xgb_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          dplyr::select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        # feature importance
        data_imp <- xgb_ml$imp %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_imp_all <- rbind(df_imp_all, data_imp)
        
        
      } else if (mlalgo == "randomforests") {
        
        ## RANDOM FORESTS ##
        #++++++++++++++++++#
        
        # make predictions
        rf_ml <- func_ml_rf(treatment_setting, data_train, data_test, outcome, treatment, group, K_tuning, rf_grid)
        
        # append predictions to data frame
        data_pred <- rf_ml$pred
        data_pred <- data_pred %>% mutate(Repetition = S_rep, Fold = fold_sel)
        # df_pred_all <- rbind(df_pred_all, data_pred)
        
        # append tuning parameters to data frame
        data_param <- rf_ml$param
        data_param <- data_param %>% mutate(Fold = fold_sel, Repetition = S_rep) %>%
          dplyr::select(Repetition, Fold, everything())
        df_param_all <- rbind(df_param_all, data_param)
        
        # feature importance
        data_imp <- xgb_ml$imp %>% mutate(Repetition = S_rep, Fold = fold_sel)
        df_imp_all <- rbind(df_imp_all, data_imp)
        
      } else {
        stop("Please select a Machine Learning Algorithm to predict the nuisance parameters.")
      }
      

      
      #++++++++++++++++#
      #### TRIMMING ####
      #++++++++++++++++#
      
      # number of rows before trimming
      num_row_bef_trimming <- nrow(data_pred)
      
      # for sub-setting adjust row names / numbering of rows
      data_pred <- data.frame(data_pred)
      df_pred_bef_trimming_all <- rbind(df_pred_bef_trimming_all, data_pred)
      data_test <- data.frame(data_test)
      
      rownames(data_pred) <- 1:nrow(data_pred)
      rownames(data_test) <- 1:nrow(data_test)
      
      ls_trimming <- func_dml_trimming(treatment_setting, data_pred, data_test, trimming)
      data_pred <- ls_trimming$data_pred
      df_pred_all <- rbind(df_pred_all, data_pred)
      data_test <- ls_trimming$data_test
      min_trimming <- ls_trimming$min_trimming
      min_trimming_all <- c(min_trimming_all, min_trimming)
      max_trimming <- ls_trimming$max_trimming
      max_trimming_all <- c(max_trimming_all, max_trimming)
      
      # generate data frame with number of treatment periods kept
      df_trimming <- data.frame(
        "Repetition" = S_rep, "Fold" = fold_sel, 
        "min_trimming" = min_trimming, "max_trimming" = max_trimming,
        "n_treats_before" = num_row_bef_trimming, 
        "n_treats_after" = nrow(data_pred)
        )
      df_trimming_all <- rbind(df_trimming_all, df_trimming)
      
      
      #+++++++++++++++++++++++++++#
      #### Covariate Balancing ####
      #+++++++++++++++++++++++++++#
      
      # only prepare data set for postlasso
      if (mlalgo == "postlasso") {
        # extract coef
        pls_coef_keep <- unique(ls_ml$coef$term)
        pls_coef_keep <- pls_coef_keep[pls_coef_keep %in% colnames(data_test)]
        
        # data frame for covariate balance assessment with only controls
        # selected in post-lasso
        data_cov_bal_pred <- data_pred %>% dplyr::select(-starts_with("num"))
        data_cov_bal_x <- data_test %>% 
          # dplyr::select(all_of(pls_coef_keep)) %>%
          mutate(Fold = fold_sel, Repetition = S_rep) %>%
          dplyr::select(Repetition, Fold, everything())
        
        data_cov_bal_fold[[fold_sel]] <- list("pred" = data_cov_bal_pred, "controls" = data_cov_bal_x)
      }

      
      
      #+++++++++++++++++++++#
      #### ERROR METRICS ####
      #+++++++++++++++++++++#
      
      df_error <- func_ml_error_metrics(treatment_setting, data_pred, S_rep, fold_sel, probscore_separate)
      df_error_all <- rbind(df_error_all, df_error)
      
      
      #+++++++++++++++++++++++++#
      #### TREATMENT EFFECTS ####
      #+++++++++++++++++++++++++#

      # calculate ATE and ATTE as well as their respective score function values
      # across the K folds
      ls_treatment_effects <- func_dml_theta_score(treatment_setting, data_pred, data_test, outcome, treatment)
      
      if (treatment_setting == "binary") {
        
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
        
      } else if (treatment_setting == "multi") {
        
        theta_ATE <- ls_treatment_effects$theta_ATE
        theta_ATE_all <- rbind(theta_ATE_all, theta_ATE)
        
        score_ATE <- ls_treatment_effects$score_ATE
        score_ATE_all <- rbind(score_ATE_all, score_ATE)
        
        theta_ATTE <- ls_treatment_effects$theta_ATTE
        theta_ATTE_all <- rbind(theta_ATTE_all, theta_ATTE)
        
        score_ATTE <- ls_treatment_effects$score_ATTE
        score_ATTE_all <- rbind(score_ATTE_all, score_ATTE)
        
        APO_1 <- ls_treatment_effects$APO_1
        APO_1_all <- c(APO_1_all, APO_1)
        APO_2 <- ls_treatment_effects$APO_2
        APO_2_all <- c(APO_2_all, APO_2)
        APO_3 <- ls_treatment_effects$APO_3
        APO_3_all <- c(APO_3_all, APO_3)
        
      }

      
    } # close iteration over k folds
    
    
    #++++++++++++++++++++++#
    #### COMMON SUPPORT ####
    #++++++++++++++++++++++#
    
    # only save common support plot for first iteration (S_rep = 1) and if it
    # is wished to save (that is only for main model)
    if (S_rep == 1 & save_trimming == TRUE) {
      plot_common_support <- func_dml_common_support(treatment_setting, df_pred_all, min_trimming_all, max_trimming_all, mlalgo)
      if (treatment_setting == "binary") {
        ggsave(paste0("Output/DML/Common_Support/MICE/dml_plot_common_support_", treatment_setting, "_", mlalgo, "_",
                      str_remove_all(cohort_prep, "_"), "_", treatment_def, "_", treatment_repl, extra_act_save, 
                      "_mice", mice_sel, ".png"), 
               plot_common_support)
      } else {
        ggsave(paste0("Output/DML/Common_Support/MICE/dml_plot_common_support_", treatment_setting, "_", mlalgo, "_",
                      str_remove_all(cohort_prep, "_"), "_", treatment_def, "_", treatment_repl, extra_act_save, 
                      "_mice", mice_sel, ".png"), 
               plot_common_support, width = 20, height = 20, units = "cm")
      }

    }
    
    
    #+++++++++++++++++#
    #### INFERENCE ####
    #+++++++++++++++++#
    
    # number of observations in complete data set 
    N <- nrow(data) 
    
    # inference
    df_result_ATE <- func_dml_inference(treatment_setting, "ATE", theta_ATE_all, score_ATE_all, N, S_rep)
    df_result_ATTE <- func_dml_inference(treatment_setting, "ATTE", theta_ATTE_all, score_ATTE_all, N, S_rep)
    
    # APO
    if (treatment_setting == "binary") {
      
      # APO
      df_result_AP00 <- data.frame("Treatment" = "no", "Type" = "APO_0", "Rep" = S_rep, "Treatment_Effect" = mean(APO_0_all)) %>%
        mutate(
          Variance = stats::var(APO_0_all), Standard_Error = sqrt(Variance / length(APO_0_all)), 
          T_Value = Treatment_Effect / Standard_Error, P_Value =  2 * pt(abs(T_Value), N, lower.tail = FALSE) 
        )
      
      df_result_AP01 <- data.frame("Treatment" = "yes", "Type" = "APO_1", "Rep" = S_rep, "Treatment_Effect" = mean(APO_1_all)) %>%
        mutate(
          Variance = stats::var(APO_1_all), Standard_Error = sqrt(Variance / length(APO_1_all)), 
          T_Value = Treatment_Effect / Standard_Error, P_Value =  2 * pt(abs(T_Value), N, lower.tail = FALSE) 
        )
      
      # all results in one data frame
      df_result <- rbind(df_result_ATE, df_result_ATTE, df_result_AP00, df_result_AP01) 
      df_result_all_detailed <- rbind(df_result_all_detailed, df_result)
      
    } else if (treatment_setting == "multi") {
      
      # APO
      df_result_AP01 <- data.frame("Treatment" = "weekly", "Type" = "APO_1", "Rep" = S_rep, "Treatment_Effect" = mean(APO_1_all)) %>%
        mutate(
          Variance = stats::var(APO_1_all), Standard_Error = sqrt(Variance / length(APO_1_all)), 
          T_Value = Treatment_Effect / Standard_Error, P_Value =  2 * pt(abs(T_Value), N, lower.tail = FALSE) 
        )

    
      df_result_AP02 <- data.frame("Treatment" = "monthly", "Type" = "APO_2", "Rep" = S_rep, "Treatment_Effect" = mean(APO_2_all)) %>%
        mutate(
          Variance = stats::var(APO_2_all), Standard_Error = sqrt(Variance / length(APO_2_all)), 
          T_Value = Treatment_Effect / Standard_Error, P_Value =  2 * pt(abs(T_Value), N, lower.tail = FALSE) 
        )
      
      df_result_AP03 <- data.frame("Treatment" = "never", "Type" = "APO_3", "Rep" = S_rep, "Treatment_Effect" = mean(APO_3_all)) %>%
        mutate(
          Variance = stats::var(APO_3_all), Standard_Error = sqrt(Variance / length(APO_3_all)), 
          T_Value = Treatment_Effect / Standard_Error, P_Value =  2 * pt(abs(T_Value), N, lower.tail = FALSE) 
        )
      
      # all results in one data frame
      df_result <- rbind(df_result_ATE, df_result_ATTE, df_result_AP01, df_result_AP02, df_result_AP03) 
      df_result_all_detailed <- rbind(df_result_all_detailed, df_result)
    }
    

    ## Covariate Balancing ##
    #+++++++++++++++++++++++#
    
    # only prepare data set for postlasso
    if (mlalgo == "postlasso") {
      data_cov_bal_rep[[S_rep]] <- data_cov_bal_fold
    }

    
  } # close iteration over S repetitions  

  
  #+++++++++++++++++++++#
  #### FINAL RESULTS ####
  #+++++++++++++++++++++#
  
  # detailed output
  df_result_all_detailed <- df_result_all_detailed %>% 
    mutate(ML_algo = mlalgo) %>%
    dplyr::select(ML_algo, everything())

  # final output: take mean and median across folds
  df_result_all <- df_result_all_detailed %>%
    dplyr::select(Treatment, Type, ML_algo, Treatment_Effect, Standard_Error) %>%
    group_by(Treatment, Type) %>% 
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
    dplyr::select(-c(Treatment_Effect, Standard_Error)) %>% distinct()
    
  
  # trimming: sum over folds
  df_trimming_all <- df_trimming_all %>%
    group_by(Repetition) %>%
    summarize(n_treats_after = sum(n_treats_after), n_treats_before = sum(n_treats_before),
              min_trimming = max(min_trimming), max_trimming = min(max_trimming))
  
  
  # number of predictors
  df_predictors_all <- df_pred_all %>% dplyr::select(Repetition, Fold, starts_with("num_pred")) %>% distinct()
  
  # covariate balance
  df_cov_bal_all[[mice_data_sel]] <- data_cov_bal_rep
  
  # coefficients are only returned for lasso
  if (str_detect(mlalgo, "lasso")) {
    # for post-lasso also covariate balance is returned
    if (mlalgo == "postlasso") {
      return(list("final" = df_result_all, "detail" = df_result_all_detailed,
                  "error" = df_error_all, "param" = df_param_all,
                  "trimming" = df_trimming_all, "predictors" = df_predictors_all,
                  "pred" = df_pred_all, "pred_bef_trimming" = df_pred_bef_trimming_all,
                  "coef" = df_coef_all, "coef_lasso" = data_coef_lasso_all, 
                  "cov_balance" = df_cov_bal_all))
    } else {
      return(list("final" = df_result_all, "detail" = df_result_all_detailed,
                  "error" = df_error_all, "param" = df_param_all,
                  "trimming" = df_trimming_all, "predictors" = df_predictors_all,
                  "pred" = df_pred_all, "pred_bef_trimming" = df_pred_bef_trimming_all,
                  "coef" = df_coef_all))
    }
  } else {
    return(list("final" = df_result_all, "detail" = df_result_all_detailed,
                "error" = df_error_all, "param" = df_param_all, "imp" = df_imp_all,
                "trimming" = df_trimming_all, "predictors" = df_predictors_all,
                "pred" = df_pred_all, "pred_bef_trimming" = df_pred_bef_trimming_all))
  }

}