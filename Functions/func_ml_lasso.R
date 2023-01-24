#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses lasso to predict the nuisance parameters. 
#++++
# INPUT:
# -> data: data frame which is used to train the model and make predictions
# -> outcome: name of outcome variable
# -> treatment: name of treatment variable
# -> K_folds: partition
# -> S_rep: number of total repetitions
# -> trimming: trimming threshold for propensity score
#     -> "no": no trimming
#     -> 0.01; 0.001; 
#     -> "min-max": drop observations with PS below largest min PS and smalles max PS
#++++
# OUTPUT:
# -> error_metrics: data frame with accuracy and rmse across folds
# -> pred: data frame with nuisance parameter predictions across folds
#++++


func_ml_lasso <- function(data, outcome, treatment, K_folds, S_rep, trimming) {
  
  # generate empty data frames and vectors to store results
  df_error_all <- data.frame()
  df_pred_all <- data.frame()

  # select predictors that are standardized
  # -> all except outcome, treatment, and group variable
  data_cols <- data %>% select(-c(all_of(outcome), all_of(treatment), group)) %>% colnames()
  
  # standardize features (mean zero and standard deviation of one)
  data_stand <- data %>%
    recipe(treatment_sport ~ .) %>%
    step_normalize(all_of(data_cols)) %>%
    prep() %>%
    bake(new_data = NULL) # %>%
    # mutate(treatment_sport = as.factor(treatment_sport))
  
  # specify the model
    ## logistic regression with lasso for treatment; linear regression for outcome
    ## mixture = 1 -> alpha = 1 -> lasso regression
    ## penalty (-> lambda) is tuned
    ## classification for treatment prediction, regression for outcome
  lasso_spec_m <- 
    logistic_reg(penalty = tune(), mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("classification")  
  
  lasso_spec_g <- 
    linear_reg(penalty = tune(), mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("regression")  
  
  # define a parameter grid with 1,000 random values for the penalty term
  lasso_grid <- grid_regular(penalty(), levels = 1000)
  
  # iterate over folds
  for (fold_sel in 1:nrow(K_folds)) {
    
    # extract training and test data
    indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
    data_train <- data_stand[indices_fold_sel, ] 
    data_test <- data_stand[-indices_fold_sel, ]
    
    # training and test data is different for m(X) and g(D, X) prediction
    cols_treatment_drop <- data_train %>% select(starts_with("treatment")) %>% colnames()
    cols_treatment_drop <- cols_treatment_drop[!str_detect(
      cols_treatment_drop, paste0("^", treatment, "$"))]
    
    cols_outcome_drop <- data_train %>% select(starts_with("outcome")) %>% colnames()
    cols_outcome_drop <- cols_outcome_drop[!str_detect(cols_outcome_drop, paste0("^", outcome, "$"))]
    
    data_train_m <- data_train %>% 
      select(-c(starts_with("outcome"), all_of(cols_treatment_drop))) %>%
      mutate({{treatment}} := as.factor(!!sym(treatment)))
    data_train_g <- data_train %>% select(-c(all_of(cols_treatment_drop), all_of(cols_outcome_drop)))
    
    # generate recipe
      ## m(x)
    lasso_recipe_m <- 
      data_train_m %>%  
      recipe(.) %>%
      # price variable is outcome
      update_role({{treatment}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role(all_of(data_train_m %>% select(-all_of(treatment)) %>% colnames()), new_role = "predictor")
      ## g(D, X)
    lasso_recipe_g <- 
      data_train_g %>%  
      recipe(.) %>%
      # price variable is outcome
      update_role({{outcome}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role(all_of(data_train_g %>% select(-outcome) %>% colnames()), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_m <- 
      workflow() %>%
      add_model(lasso_spec_m) %>%
      add_recipe(lasso_recipe_m)
    
    lasso_workflow_g <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g)
    

    
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    #### PARAMETER TUNING ####
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # parameter tuning via 5-fold CV
    # this means that training data is again partitioned into 5 folds
    K_folds_inner_m <- group_vfold_cv(
      data = data_train_m %>% mutate({{treatment}} := as.factor(!!!syms(treatment))), 
      v = nrow(K_folds), group = "group", strata = "treatment_sport", balance = "observations"
    )
    K_folds_inner_g0 <- group_vfold_cv(
      data = data_train_g %>% filter(!!sym(treatment) == 0),  
      v = nrow(K_folds), group = "group", strata = "outcome_grade", balance = "observations"
    )
    K_folds_inner_g1 <- group_vfold_cv(
      data = data_train_g %>% filter(!!sym(treatment) == 1),  
      v = nrow(K_folds), group = "group", strata = "outcome_grade", balance = "observations"
    )
    
    
    # conduct parameter tuning
      ## m(X)
    lasso_grid_search_m <- 
      lasso_workflow_m %>%
      tune_grid(
        # specify 5-fold CV
        resamples = K_folds_inner_m,
        # add parameter values used for tuning
        grid = lasso_grid,
        # define performance metrics (only AUC is used to select the best model)
        metrics = metric_set(roc_auc)
      )
      ## g(0, X)
    lasso_grid_search_g0 <- 
      lasso_workflow_g %>%
      tune_grid(resamples = K_folds_inner_g0, grid = lasso_grid, 
                metrics = metric_set(rmse))
      ## g(1, X)
    lasso_grid_search_g1 <- 
      lasso_workflow_g %>%
      tune_grid(resamples = K_folds_inner_g1, grid = lasso_grid, 
                metrics = metric_set(rmse))
    
    
    # select best penalty parameter: parameter with highest AUC
    lasso_best_param_m <- lasso_grid_search_m %>% select_best("roc_auc")
    lasso_best_param_m <- lasso_best_param_m$penalty
    
    lasso_best_param_g0 <- lasso_grid_search_g0 %>% select_best("rmse")
    lasso_best_param_g0 <- lasso_best_param_g0$penalty
    
    lasso_best_param_g1 <- lasso_grid_search_g1 %>% select_best("rmse")
    lasso_best_param_g1 <- lasso_best_param_g1$penalty
    
    
  
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### FINAL MODEL TRAINING ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # specify the models
      ## model for m(X) = E(D|X): prediction of treatment
    lasso_spec_final_m <- 
      logistic_reg(penalty = lasso_best_param_m, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
      ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
    lasso_spec_final_g0 <- 
      linear_reg(penalty = lasso_best_param_g0, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression") 
      ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
    lasso_spec_final_g1 <- 
      linear_reg(penalty = lasso_best_param_g1, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")
    
    # generate workflow
    lasso_workflow_final_m <- 
      workflow() %>%
      add_model(lasso_spec_final_m) %>%
      add_recipe(lasso_recipe_m)
    
    lasso_workflow_final_g0 <- 
      workflow() %>%
      add_model(lasso_spec_final_g0) %>%
      add_recipe(lasso_recipe_g)
    
    lasso_workflow_final_g1 <- 
      workflow() %>%
      add_model(lasso_spec_final_g1) %>%
      add_recipe(lasso_recipe_g)
    
    # fit the model
      ## m(X)
    lasso_fit_final_m <- 
      lasso_workflow_final_m %>%
      fit(data_train_m)
      ## g(0, X)
    lasso_fit_final_g0 <- 
      lasso_workflow_final_g0 %>%
      fit(data_train_g)
      ## g(1, X)
    lasso_fit_final_g1 <- 
      lasso_workflow_final_g1 %>%
      fit(data_train_g)
    
    # # extract coefficients
    # lasso_coef <- tidy(lasso_fit_final_treatment)
    #   ## add fold 
    # lasso_coef$fold <- fold_sel
    # ## append extracted coefficients to final data frame
    # df_coef_logreg_final <- rbind(df_coef_logreg_final, logreg_coef)
    
    
    #%%%%%%%%%%%%%%%%%%%#
    #### PREDICTIONS ####
    #%%%%%%%%%%%%%%%%%%%#
    
    
    # make predictions on test data
    lasso_pred_m <- predict(lasso_fit_final_m, data_test, type = "prob")
    lasso_pred_m <- lasso_pred_m$.pred_1 # probability for class 1
    
    lasso_pred_g0 <- predict(lasso_fit_final_g0, data_test)
    lasso_pred_g0 <- lasso_pred_g0$.pred 
    
    lasso_pred_g1 <- predict(lasso_fit_final_g1, data_test)
    lasso_pred_g1 <- lasso_pred_g1$.pred
    
    # create prediction data frame
    df_pred <- data.frame("fold" = fold_sel, "m" = lasso_pred_m, 
                          "g0" = lasso_pred_g0, "g1" = lasso_pred_g1,
                          "treatment" = data_test %>% pull(treatment),
                          "outcome" = data_test %>% pull(outcome))
    
    # apply trimming
    if (trimming == "no") {
      df_pred <- df_pred
    } else if (trimming == 0.01) {
      df_pred <- df_pred %>% filter(m >= 0.01 & m <= (1 - 0.01))
    } else if (trimming == 0.1) {
      df_pred <- df_pred %>% filter(m >= 0.1 & m <= (1 - 0.1))
    } else if (trimming == "min-max") {
      df_select_trimming <- df_pred %>% group_by(treatment) %>% 
        summarise(min_m = min(m), max_m = max(m))
      df_pred <- df_pred %>% filter(
        m >= max(df_select_trimming$min_m) & m <= min(df_select_trimming$max_m)
      )
    } else {
      stop("Please specify trimming threshold.")
    }
    
    # append to full data frame
    df_pred_all <- rbind(df_pred_all, df_pred)
    
    # calculate error metrics (CHECKEN OB DAS SO STIMMT)
      ## calculate predictions
    df_error <- data.frame(
      m_pred = as.factor(round(df_pred$m)), 
      m_true = as.factor(data_test %>% select(all_of(treatment)) %>% pull()),
      g0_pred = df_pred$g0, g1_pred = df_pred$g1, 
      g_true = data_test %>% select(outcome) %>% pull()
    )
      ## create error metrics
    m_acc <- accuracy(df_error, truth = m_true, estimate = m_pred) %>%
      select(.estimate) %>% pull()
    m_bacc <- bal_accuracy(df_error, truth = m_true, estimate = m_pred) %>%
      select(.estimate) %>% pull()
    g0_mse <- df_error %>% filter(m_true == 0) %>% summarize(mean((g_true - g0_pred)^2)) %>% pull()
    g0_rmse <- sqrt(g0_mse)
    g1_mse <- df_error %>% filter(m_true == 1) %>% summarize(mean((g_true - g1_pred)^2)) %>% pull()
    g1_rmse <- sqrt(g1_mse)
    df_error <- data.frame("Repetition" = S_rep, "Fold" = fold_sel, 
                           "ACC" = m_acc, "BACC" = m_bacc, 
                           "RMSE_g0" = g0_rmse, "RMSE_g1" = g1_rmse)
    df_error_all <- rbind(df_error_all, df_error)
    

  } # close iteration over K-folds
  
  # return error metrics and predictions
  return(list("error_metrics" = df_error_all, "pred" = df_pred_all))
  
} # close function() 


# example
data <- readRDS("Data/Prep_11/prep_11_final_data_binary_xgboost.rds")
data <- data %>% select(-c(ends_with("_lag")))
outcome <- "outcome_grade"
treatment <- "treatment_sport"
K <- 5
S_rep <- 1
trimming <- "no"
ls_lasso <- func_ml_lasso(data, outcome, treatment, K_folds, S_rep, trimming)




