#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION DOUBLE ML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


library(rsample) # for group_vfold_cv()
library(glmnet) # for glmnet() (-> lasso)
library(hdm) # for rlasso() (-> post-lasso)
library(xgboost) # for xgboost()

set.seed(12345)


data <- readRDS("Data/prep_8_FINAL.rds")
data_sub <- data %>% 
  subset(ID_t %in% unique(data$ID_t)[1:1000]) %>%
  dplyr::select(ID_t, treatment_sport, outcome_grade, starts_with("educ"),
         starts_with("interest"), age, starts_with("health"),
         starts_with("mother"), starts_with("father"))
data_sub <- data_sub[!duplicated(data_sub %>% dplyr::select(ID_t, treatment_sport, outcome_grade)), ]
data_sub$group <- as.integer(factor(data_sub$ID_t,levels = unique(data_sub$ID_t)))
data_sub <- data_sub %>% dplyr::select(-ID_t) %>% na.omit()

data <- data_sub
outcome <- "outcome_grade"
treatment <- "treatment_sport"
group <- "group"
K <- 2
mlalgo <- "postlasso"


func_double_ml <- function(data, outcome, treatment, group, K, mlalgo) {
  
  # generate folds
    ## "v": number of folds / partitions specified by K 
    ## "group": specifies the grouping variable
    ## "strata": outcome variable used for conducting stratified sampling (numeric variable is binned into quartiles)
    ## balance = "observations": assigns roughly the same number of observations to each fold
    ## https://rsample.tidymodels.org/reference/group_vfold_cv.html
  K_folds <- group_vfold_cv(
    data = data,  v = K, group = group, strata = outcome, balance = "observations"
    )
  
  # drop group variable as this was only used for data splitting
  data <- data %>% dplyr::select(-group)
  
  
  # iterate over folds to train model and make predictions (of nuisance functions)
  n <- nrow(data)
  g0_pred <- rep(NA, n)
  g1_pred <- rep(NA, n)
  m_pred <- rep(NA, n)
  m <- mean(data %>% dplyr::select(all_of(treatment)) %>% pull()) 
  
  for (fold_sel in 1:K) {
    
    # extract training and test data
    indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
    data_train <- data[indices_fold_sel, ] 
    data_test <- data[-indices_fold_sel, ] 

    
    ## PREDICTION OF NUISANCE FUNCTIONS ##
    #++++++++++++++++++++++++++++++++++++#

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
      data_train %>% dplyr::select(-c(all_of(outcome), all_of(treatment))), # Controls
      data_train %>% dplyr::select(all_of(treatment)) %>% pull() # Treatment
      )
      ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
    model_g0 <- model_func(
      data_train %>% dplyr::filter(!!sym(treatment) == 0) %>% dplyr::select(-c(all_of(outcome), all_of(treatment))),
      data_train %>% dplyr::filter(!!sym(treatment) == 0) %>% dplyr::select(all_of(outcome)) %>% pull()
      )
      ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
    model_g1 <- model_func(
      data_train %>% dplyr::filter(!!sym(treatment) == 1) %>% dplyr::select(-c(all_of(outcome), all_of(treatment))),
      data_train %>% dplyr::filter(!!sym(treatment) == 1) %>% dplyr::select(all_of(outcome)) %>% pull()
      )
    
    
    # make predictions using test data
      ## predictions for m(X)
    m_pred[-indices_fold_sel] <- predict(
      model_m, newdata = data_test %>% dplyr::select(-c(all_of(outcome), all_of(treatment)))
    )[, 1]
      ## predictions for g(0, X)
    g0_pred[-indices_fold_sel] <- predict(
      model_g0, newdata = data_test %>% dplyr::select(-c(all_of(outcome), all_of(treatment)))
    )[, 1]
      ## predictions for g(1, X)
    g1_pred[-indices_fold_sel] <- predict(
      model_g1, newdata = data_test %>% dplyr::select(-c(all_of(outcome), all_of(treatment)))
    )[, 1]
  }
  
  
  # Treatment Effects #
  #+++++++++++++++++++#
  
  Y <- data[, outcome] %>% pull() # outcome
  D <- data[, treatment] %>% pull() # treatment
  
  ## CALCULATE ATE ##
  
  score_a_ATE <- rep(-1, n)
  pseudo_0_ATE <- g0_pred + (1 - D) * (Y - g0_pred) / (1 - m_pred)
  pseudo_1_ATE <- g1_pred + D * (Y - g1_pred) / m_pred
  score_b_ATE <- pseudo_1_ATE - pseudo_0_ATE
  theta_ATE <- -sum(score_b_ATE) / sum(score_a_ATE)
  
  
  ## CALCULATE ATET ##
  
  score_a_ATET <- -D / m 
  score_b_ATET <- D * (Y - g0_pred) / m - 
    m_pred * (1 - D) * (Y - g0_pred) * (m*(1 - m_pred))
  theta_ATET <- -sum(score_b_ATET) / sum(score_a_ATET)
  
  
  
  ## INFERENCE ##
  func_double_ml_inference <- function(theta, score_a, score_b) {
    score <- theta * score_a + score_b
    influence <- -score / mean(score)
    variance <- var(influence)
    stderror <- sqrt(variance / n)
    tvalue <- theta / stderror
    pvalue <- 2 * pt(abs(tvalue), n, lower.tail = FALSE)
    
    df_result <- data.frame(
      "Treatment_Effect" = theta, "Standard_Error" = stderror,
      "T_Value" = tvalue, "P_Value" = pvalue
    )
    
    return(df_result)
  }
  
  df_result_ATE <- func_double_ml_inference(theta_ATE, score_a_ATE, score_b_ATE) 
  rownames(df_result_ATE) <- "ATE"
  df_result_ATET <- func_double_ml_inference(theta_ATET, score_a_ATET, score_b_ATET)
  rownames(df_result_ATET) <- "ATET"
  
  df_result <- rbind(df_result_ATE, df_result_ATET)
  return(df_result)
  
}

func_double_ml(data_sub, "outcome_grade", "treatment_sport", "group", 2, "postlasso")