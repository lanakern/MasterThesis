#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses lasso to predict the nuisance parameters. 
#++++
# INPUT:
# -> "data_train": training data
# -> "data_test": test data
# -> "outcome": name of outcome variable included in data_train and data_test
# -> "treatment": name of treatment variable included in data_train and data_test
# -> "group": group variable included in data_train and data_test
# -> "K": number of folds generated for parameter tuning
# -> "lambda_val": number of lambda values used in tuning process
#++++
# OUTPUT:
# -> "pred": data frame with nuisance parameter predictions and true values
# -> "param": data frame including the value of lambda that is used for
# final model training
#++++


func_ml_lasso <- function(data_train, data_test, outcome, treatment, group, K, lambda_val) {
  
  # ensure that treatment variable is factor
  data_train <- data_train %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  data_test <- data_test %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  
  # specify the model
    ## logistic regression with lasso for treatment; linear regression with lasso for outcome
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
  
  # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except treatment, outcome, and group
  X_controls <- data_train %>% select(-c(all_of(outcome), all_of(treatment), all_of(group))) %>% colnames()
    ## m(x)
  lasso_recipe_m <- 
    data_train %>%
    recipe(.) %>%
    # price variable is outcome
    update_role({{treatment}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
    ## g(D, X)
  lasso_recipe_g <- 
    data_train %>%  
    recipe(.) %>%
    # price variable is outcome
    update_role({{outcome}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome and treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
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
    
  # define a parameter grid with 1,000 random values for the penalty term
  lasso_grid <- grid_regular(penalty(), levels = lambda_val)
  
  # parameter tuning via 5-fold CV
  # this means that training data is again partitioned into 5 folds
  K_folds_inner_m <- rsample::group_vfold_cv(
    data = data_train, 
    v = K, group = group, strata = all_of(treatment), balance = "observations"
  )
  K_folds_inner_g0 <- rsample::group_vfold_cv(
    data = data_train %>% filter(!!sym(treatment) == 0),  
    v = K, group = group, strata = all_of(outcome), balance = "observations"
  )
  K_folds_inner_g1 <- rsample::group_vfold_cv(
    data = data_train %>% filter(!!sym(treatment) == 1),  
    v = K, group = group, strata = all_of(outcome), balance = "observations"
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
  
  df_best_param <- data.frame(
    "m" = lasso_best_param_m, "g0" = lasso_best_param_g0, "g1" = lasso_best_param_g1
  )
  
  
  
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
    fit(data_train)
    ## g(0, X)
  lasso_fit_final_g0 <- 
    lasso_workflow_final_g0 %>%
    fit(data_train)
    ## g(1, X)
  lasso_fit_final_g1 <- 
    lasso_workflow_final_g1 %>%
    fit(data_train)
  
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
  df_pred <- data.frame(
    "m" = lasso_pred_m, "g0" = lasso_pred_g0, "g1" = lasso_pred_g1,
    "treatment" = data_test %>% pull(treatment),
    "outcome" = data_test %>% pull(outcome)
    )
  
  # return data frame with predictions
  return(list("pred" = df_pred, "param" = df_best_param))
  
} # close function() 
