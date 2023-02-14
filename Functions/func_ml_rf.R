#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH RANDOM FORESTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses xgboost to predict the nuisance parameters. 
#++++
# INPUT:
# -> "data_train": training data
# -> "data_test": test data
# -> "outcome": name of outcome variable included in data_train and data_test
# -> "treatment": name of treatment variable included in data_train and data_test
# -> "group": group variable included in data_train and data_test
# -> "K": number of folds generated for parameter tuning
# -> "rf_grid": hyperparameter grid consisting of
  # - "trees": number of tress in the forest. Set to 1,000 (NOT TUNED). Default is 500.
  # - "mtry": randomly selected predictors at each split. 
  # Default: floor(sqrt(ncol(x))) for classification, floor(ncol(x)/3) for regression. 
  # - "min_n": minimal size of terminal node. Default: 5 for regression; 10 for classification.
  # https://parsnip.tidymodels.org/reference/details_rand_forest_randomForest.html
#++++
# OUTPUT:
# -> "pred": data frame with nuisance parameter predictions and true values
# -> "param": data frame including the value of lambda that is used for
# final model training
#++++


func_ml_rf <- function(data_train, data_test, outcome, treatment, group, K, rf_grid) {
  
  # ensure that treatment variable is factor
  data_train <- data_train %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  data_test <- data_test %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  
  # separate training data for g0 and g1 prediction
  data_train_g1 <- data_train %>% filter(!!sym(treatment) == 1)
  data_train_g0 <- data_train %>% filter(!!sym(treatment) == 0)
  
  # specify the model
  ## treatment is predicted via binary classification and outcome via regression
  # specify the model
  trees_sel <- unique(rf_grid$trees)[1]
  rf_spec_m <- 
    rand_forest(trees = {{trees_sel}}, mtry = tune(), min_n = tune()) %>% 
    set_engine("randomForest") %>% 
    set_mode("classification")
  
  rf_spec_g <- 
    rand_forest(trees = {{trees_sel}}, mtry = tune(), min_n = tune()) %>% 
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except treatment, outcome, and group
  X_controls <- data_train %>% select(-c(all_of(outcome), all_of(treatment), all_of(group))) %>% colnames()
    ## m(x)
  rf_recipe_m <- 
    data_train %>%
    recipe(.) %>%
    # price variable is outcome
    update_role({{treatment}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
   ## g(0, X)
  rf_recipe_g0 <- 
    data_train_g0 %>%  
    recipe(.) %>%
    # price variable is outcome
    update_role({{outcome}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome and treatment)
    update_role(all_of(X_controls), new_role = "predictor")
    ## g(1, X)
  rf_recipe_g1 <- 
    data_train_g1 %>%  
    recipe(.) %>%
    # price variable is outcome
    update_role({{outcome}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome and treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  # generate workflow
  rf_workflow_m <- 
    workflow() %>%
    add_model(rf_spec_m) %>%
    add_recipe(rf_recipe_m)
  
  rf_workflow_g1 <- 
    workflow() %>%
    add_model(rf_spec_g) %>%
    add_recipe(rf_recipe_g1)
  
  rf_workflow_g0 <- 
    workflow() %>%
    add_model(rf_spec_g) %>%
    add_recipe(rf_recipe_g0)
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  #### PARAMETER TUNING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # remove trees from grid as trees are not tuned
  rf_grid <- rf_grid %>% select(-trees) %>% distinct()
  
  
  # parameter tuning via 5-fold CV
  # this means that training data is again partitioned into 5 folds
  K_folds_inner_m <- rsample::group_vfold_cv(
    data = data_train, 
    v = K, group = group, strata = all_of(treatment), balance = "observations"
  )
  K_folds_inner_g0 <- rsample::group_vfold_cv(
    data = data_train_g0,  
    v = K, group = group, strata = all_of(outcome), balance = "observations"
  )
  K_folds_inner_g1 <- rsample::group_vfold_cv(
    data = data_train_g1,  
    v = K, group = group, strata = all_of(outcome), balance = "observations"
  )
  
  
  # conduct parameter tuning
  ## m(X)
  rf_grid_search_m <- 
    rf_workflow_m %>%
    tune_grid(
      # specify 5-fold CV
      resamples = K_folds_inner_m,
      # add parameter values used for tuning
      grid = rf_grid,
      # define performance metrics (only AUC is used to select the best model)
      metrics = metric_set(roc_auc)
    )
  ## g(0, X)
  rf_grid_search_g0 <- 
    rf_workflow_g0 %>%
    tune_grid(resamples = K_folds_inner_g0, grid = rf_grid, 
              metrics = metric_set(rmse))
  ## g(1, X)
  rf_grid_search_g1 <- 
    rf_workflow_g1 %>%
    tune_grid(resamples = K_folds_inner_g1, grid = rf_grid, 
              metrics = metric_set(rmse))
  
  
  # select best penalty parameter: parameter with highest AUC
  rf_best_param_m <- rf_grid_search_m %>% select_best("roc_auc")
  rf_best_param_g0 <- rf_grid_search_g0 %>% select_best("rmse")
  rf_best_param_g1 <- rf_grid_search_g1 %>% select_best("rmse")
  
  
  df_best_param <- data.frame(
    "m_trees" = trees_sel, "m_mtry" = rf_best_param_m$mtry,
    "m_min_n" = rf_best_param_m$min_n,
    "g0_trees" = trees_sel, "g0_mtry" = rf_best_param_g0$mtry,
    "g0_min_n" = rf_best_param_g0$min_n,
    "g1_trees" = trees_sel, "g1_mtry" = rf_best_param_g1$mtry,
    "g1_min_n" = rf_best_param_g1$min_n
  )
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### FINAL MODEL TRAINING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # specify the models
    ## model for m(X) = E(D|X): prediction of treatment
  trees_m <- df_best_param$m_trees
  mtry_m <- df_best_param$m_mtry
  min_n_m <- df_best_param$m_min_n
  
  rf_spec_final_m <- 
    rand_forest(trees = {{trees_m}}, mtry = {{mtry_m}}, min_n = {{min_n_m}}) %>%
    set_engine("randomForest") %>% 
    set_mode("classification")
    ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
  trees_g0 <- df_best_param$g0_trees
  mtry_g0 <- df_best_param$g0_mtry
  min_n_g0 <- df_best_param$g0_min_n
  
  rf_spec_final_g0 <- 
    rand_forest(trees = {{trees_g0}}, mtry = {{mtry_g0}}, min_n = {{min_n_g0}}) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
      ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
  trees_g1 <- df_best_param$g1_trees
  mtry_g1 <- df_best_param$g1_mtry
  min_n_g1 <- df_best_param$g1_min_n
  
  rf_spec_final_g1 <- 
    rand_forest(trees = {{trees_g1}}, 
                mtry = {{mtry_g1}}, 
                min_n = {{min_n_g1}}
                ) %>%
    set_engine("randomForest") %>% 
    set_mode("regression")
  
  # generate workflow
  rf_workflow_final_m <- 
    workflow() %>%
    add_model(rf_spec_final_m) %>%
    add_recipe(rf_recipe_m)
  
  rf_workflow_final_g0 <- 
    workflow() %>%
    add_model(rf_spec_final_g0) %>%
    add_recipe(rf_recipe_g0)
  
  rf_workflow_final_g1 <- 
    workflow() %>%
    add_model(rf_spec_final_g1) %>%
    add_recipe(rf_recipe_g1)
  
  # fit the model
    ## m(X)
  rf_fit_final_m <- 
    rf_workflow_final_m %>%
    fit(data_train)
    ## g(0, X)
  rf_fit_final_g0 <- 
    rf_workflow_final_g0 %>%
    fit(data_train_g0)
    ## g(1, X)
  rf_fit_final_g1 <- 
    rf_workflow_final_g1 %>%
    fit(data_train_g1)
  
  
  #%%%%%%%%%%%%%%%%%%%#
  #### PREDICTIONS ####
  #%%%%%%%%%%%%%%%%%%%#
  
  # make predictions on test data
  rf_pred_m <- predict(rf_fit_final_m, data_test, type = "prob")
  rf_pred_m <- rf_pred_m$.pred_1 # probability for class 1
  
  rf_pred_g0 <- predict(rf_fit_final_g0, data_test)
  rf_pred_g0 <- rf_pred_g0$.pred 
  
  rf_pred_g1 <- predict(rf_fit_final_g1, data_test)
  rf_pred_g1 <- rf_pred_g1$.pred
  
  # create prediction data frame
  df_pred <- data.frame(
    # predictions
    "m" = rf_pred_m, "g0" = rf_pred_g0, "g1" = rf_pred_g1,
    # true values
    "treatment" = data_test %>% pull(treatment), 
    "outcome" = data_test %>% pull(outcome),
    # number of predictors
    "num_pred_m" = ncol(rf_fit_final_m$pre$mold$predictors),
    "num_pred_g0" = ncol(rf_fit_final_g0$pre$mold$predictors),
    "num_pred_g1" = ncol(rf_fit_final_g1$pre$mold$predictors)
  )
  
  # return data frame with predictions
  return(list("pred" = df_pred, "param" = df_best_param))
  
} # close function() 