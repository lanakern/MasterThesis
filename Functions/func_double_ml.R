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

set.seed(12345)


data <- readRDS("Data/Prep_11/prep_11_final_data_binary_lasso_base.rds")
data <- data %>% select(-c(ends_with("_lag")))


# data <- readRDS("Data/prep_8_FINAL.rds")
# data_sub <- data %>% select(-c(starts_with("interview"),"end_date_adj"))
#   #subset(ID_t %in% unique(data$ID_t)[1:2000]) %>%
#   # dplyr::select(ID_t, treatment_sport, outcome_grade, starts_with("educ"),
#   #        starts_with("interest"), age, starts_with("health"),
#   #        starts_with("mother"), starts_with("father"))
# data_sub <- data_sub[!duplicated(data_sub %>% dplyr::select(ID_t, treatment_sport, outcome_grade)), ]
# data_sub$group <- as.integer(factor(data_sub$ID_t,levels = unique(data_sub$ID_t)))
# data_sub <- data_sub %>% dplyr::select(-ID_t) %>% na.omit()
# 
# data <- data_sub
# outcome <- "outcome_grade"
# treatment <- "treatment_sport"
# group <- "group"
# K <- 2 # 5
# S <- 3 # 100
# mlalgo <- "postlasso"


func_double_ml <- function(data, outcome, treatment, group, K, S, mlalgo) {
  
  # generate empty data frames
  df_result_all_detailed <- data.frame()
  df_error_all <- data.frame()
  
  # data frame for splitting
  data_split <- data 
  
  # drop group variable as this is only used for data splitting
  data <- data %>% dplyr::select(-group)
  
  # Accounting for uncertainty by repeating the process S times
  for (S_rep in 1:S) {
    
    print(paste("Repetition", S_rep))
    
    ## Generate Folds ##
    #++++++++++++++++++#
    
    # "v": number of folds / partitions specified by K 
    # "group": specifies the grouping variable
    # "strata": outcome variable used for conducting stratified sampling (numeric variable is binned into quartiles)
    # balance = "observations": assigns roughly the same number of observations to each fold
    # https://rsample.tidymodels.org/reference/group_vfold_cv.html
    # https://scikit-learn.org/stable/modules/cross_validation.html#group-k-fold
    K_folds <- group_vfold_cv(
      data = data_split,  v = K, group = group, strata = outcome, balance = "observations"
    )
    

  
    ## NUISANCE PARAMETER PREDICTIONS ##
    #++++++++++++++++++++++++++++++++++#
    
    # define variables and create empty vectors to store computations within loop
    N <- nrow(data) # number of observations in complete data set
      ## to store treatment effect estimates
    theta_ATE_all <- c()
    theta_ATET_all <- c()
      ## to store values of the score function
    score_ATE_all <- c()
    score_ATET_all <- c()
    
    # iterate over folds to train model and make predictions (of nuisance functions)
    for (fold_sel in 1:K) {
      
      # extract training and test data
      indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
      data_train <- data[indices_fold_sel, ] 
      data_test <- data[-indices_fold_sel, ] 
      
      # select machine learning algorithm based on user selection
      if (mlalgo == "postlasso") {
        model_func <- rlasso
      } else if (mlalgo == "lasso") {
        model_func <- glmnet
      } else if (mlalgo == "xgboost") {
        model_func <- xgboost
      } else {
        stop("Please select a Machine Learning Algorithm to predict the nuisance parameters.")
      }
      
      # create prediction models in training data
        ## model for m(X) = E(D|X): prediction of treatment
      model_m <- model_func(
        data_train %>% dplyr::select(-c(all_of(outcome), starts_with("treatment"))), # Controls
        data_train %>% dplyr::select(all_of(treatment)) %>% pull() # Treatment
      )
        ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
      model_g0 <- model_func(
        data_train %>% dplyr::filter(!!sym(treatment) == 0) %>% dplyr::select(-c(all_of(outcome), all_of(treatment))), # Controls
        data_train %>% dplyr::filter(!!sym(treatment) == 0) %>% dplyr::select(all_of(outcome)) %>% pull() # Outcome
      )
        ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
      model_g1 <- model_func(
        data_train %>% dplyr::filter(!!sym(treatment) == 1) %>% dplyr::select(-c(all_of(outcome), all_of(treatment))), # Controls
        data_train %>% dplyr::filter(!!sym(treatment) == 1) %>% dplyr::select(all_of(outcome)) %>% pull() # Outcome
      )
      
      # make predictions using test data
        ## predictions for m(X)
      m_pred_fold <- predict(
        model_m, newdata = data_test %>% dplyr::select(-c(all_of(outcome), starts_with("treatment")))
      )[, 1]
        ## predictions for g(0, X)
      g0_pred_fold <- predict(
        model_g0, newdata = data_test %>% dplyr::select(-c(all_of(outcome), all_of(treatment)))
      )[, 1]
        ## predictions for g(1, X)
      g1_pred_fold <- predict(
        model_g1, newdata = data_test %>% dplyr::select(-c(all_of(outcome), all_of(treatment)))
      )[, 1]
      
      
      # calculate error metrics (CHECKEN OB DAS SO STIMMT)
        ## calculate predictions
      df_pred <- data.frame(
        m_pred = round(m_pred_fold), m_true = data_test$treatment_sport,
        g0_pred = g0_pred_fold, g1_pred = g1_pred_fold, g_true = data_test$outcome_grade
      )
        ## create error metrics
      treatment_accuracy <- sum(df_pred$m_pred == df_pred$m_true) / nrow(df_pred)
      g0_mse <- df_pred %>% filter(m_true == 0) %>% summarize(mean((g_true - g0_pred)^2)) %>% pull()
      g0_rmse <- sqrt(g0_mse)
      g1_mse <- df_pred %>% filter(m_true == 1) %>% summarize(mean((g_true - g1_pred)^2)) %>% pull()
      g1_rmse <- sqrt(g1_mse)
      df_error <- data.frame("Repetituon" = S_rep, "Fold" = fold_sel, 
                             "ACC" = treatment_accuracy, "RMSE_g0" = g0_rmse, 
                             "RMSE_g1" = g1_rmse)
      df_error_all <- rbind(df_error_all, df_error)
      
      
      ## TREATMENT EFFECTS ##
      #+++++++++++++++++++++#
      
      Y <- data_test[, outcome] %>% pull() # outcome
      D <- data_test[, treatment] %>% pull() # treatment
      n <- nrow(data_test) # number of observations in test data
      m <- sum(data_test %>% dplyr::select(all_of(treatment)) %>% pull()) / n # E(D)
      
      ## ATE ##
      
      # calculate theta
      score_a_ATE <- rep(-1, nrow(data_test))
      pseudo_0_ATE <- g0_pred_fold + (1 - D) * (Y - g0_pred_fold) / (1 - m_pred_fold)
      pseudo_1_ATE <- g1_pred_fold + D * (Y - g1_pred_fold) / m_pred_fold
      score_b_ATE <- pseudo_1_ATE - pseudo_0_ATE
      theta_ATE <- -sum(score_b_ATE) / sum(score_a_ATE)
      theta_ATE_all <- c(theta_ATE_all, theta_ATE)
      
      # calculate score
      score_ATE <- theta_ATE * score_a_ATE + score_b_ATE
      score_ATE_all <- c(score_ATE_all, score_ATE)
      
      
      ## ATET ##
      
      # calculate theta
      score_a_ATET <- -D / m 
      score_b_ATET <- D * (Y - g0_pred_fold) / m - 
        m_pred_fold * (1 - D) * (Y - g0_pred_fold) * (m*(1 - m_pred_fold))
      theta_ATET <- -sum(score_b_ATET) / sum(score_a_ATET)
      theta_ATET_all <- c(theta_ATET_all, theta_ATET)
      
      # calculate score
      score_ATET <- theta_ATET * score_a_ATET + score_b_ATET
      score_ATET_all <- c(score_ATET_all, score_ATET)
      
    } # close loop over folds
    

    ## INFERENCE ##
    #+++++++++++++#
    
    # generate function for easier computation
    func_double_ml_inference <- function(effect, theta, score, N) {
      # treatment effect is mean over the K estimators
      theta <- mean(theta) 
      # variance is the variance of the score function (same as sum(score^2) / N)
      # score_influence <- -score / mean(score_a_ATE) # same result with influence function
      variance <- var(score) 
      # asymptotic standard error
      stderror <- sqrt(variance / N) 
      # t-value
      tvalue <- theta / stderror 
      # p-value
      pvalue <- 2 * pt(abs(tvalue), N, lower.tail = FALSE) 
      
      df_result <- data.frame(
        "Type" = effect, 
        "Rep" = S_rep, "Treatment_Effect" = theta, 
        "Variance" = variance, "Standard_Error" = stderror,
        "T_Value" = tvalue, "P_Value" = pvalue
      )
      
      return(df_result)
      
    }
    
    df_result_ATE <- func_double_ml_inference("ATE", theta_ATE_all, score_ATE_all, N)
    df_result_ATET <- func_double_ml_inference("ATET", theta_ATET_all, score_ATET_all, N)
    
    df_result <- rbind(df_result_ATE, df_result_ATET) 
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

# ls_dml_result <- func_double_ml(data_sub, "outcome_grade", "treatment_sport", "group", 2, 3, "postlasso")
# ls_dml_result$final
# ls_dml_result$detail