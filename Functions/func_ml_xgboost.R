#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH XGBOOST ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
# -> "xgb_grid": hyperparameter grid consisting of
  # - "learn_rate": hyperparameter that is used to prevent overfitting by 
  # making the boosting process more conservative.  Low values makes the model more robust 
  # to overfitting but slower to compute. 0 < learn_rate < 1; default: 0.3.
  # - "trees": number of tress in the forest. Default: 15
  # - "tree_depth": higher values make the model more likely to 
  # overfit and complex. Default: 6. 
  # - "mtry": randomly selected predictors at each split. Default: p (all predictors)
  # - "min_n": minimal size of terminal node. Default: 1
  # https://parsnip.tidymodels.org/reference/details_boost_tree_xgboost.html
#++++
# OUTPUT:
# -> "pred": data frame with nuisance parameter predictions and true values
# -> "param": data frame including the value of lambda that is used for
# final model training
#++++


func_ml_xgboost <- function(data_train, data_test, outcome, treatment, group, K, xgb_grid) {
  
  # ensure that treatment variable is factor
  data_train <- data_train %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  data_test <- data_test %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  
  # separate training data for g0 and g1 prediction
  data_train_g1 <- data_train %>% filter(treatment_sport == 1)
  data_train_g0 <- data_train %>% filter(treatment_sport == 0)
  
  # specify the model
  ## treatment is predicted via binary classification and outcome via regression
  # specify the model
  xgb_spec_m <- 
    boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
               mtry = tune(), min_n = tune()) %>% 
    set_engine("xgboost", objective = "binary:logistic") %>% 
    set_mode("classification")
  
  xgb_spec_g <- 
    boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
               mtry = tune(), min_n = tune()) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")
  

  
  # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except treatment, outcome, and group
  X_controls <- data_train %>% select(-c(all_of(outcome), all_of(treatment), all_of(group))) %>% colnames()
    ## m(x)
  xgb_recipe_m <- 
    data_train %>%
    recipe(.) %>%
    # price variable is outcome
    update_role({{treatment}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
    ## g(1, X)
  xgb_recipe_g1 <- 
    data_train_g1 %>%  
    recipe(.) %>%
    # price variable is outcome
    update_role({{outcome}}, new_role = "outcome") %>%
    # all other variables are predictors (drop outcome and treatment)
    update_role(all_of(X_controls), new_role = "predictor")
    ## g(0, X)
  xgb_recipe_g0 <- 
    data_train_g0 %>%  
    recipe(.) %>%
    update_role({{outcome}}, new_role = "outcome") %>%
    update_role(all_of(X_controls), new_role = "predictor")
  
  # generate workflow
  xgb_workflow_m <- 
    workflow() %>%
    add_model(xgb_spec_m) %>%
    add_recipe(xgb_recipe_m)
  
  xgb_workflow_g1 <- 
    workflow() %>%
    add_model(xgb_spec_g) %>%
    add_recipe(xgb_recipe_g1)
  
  xgb_workflow_g0 <- 
    workflow() %>%
    add_model(xgb_spec_g) %>%
    add_recipe(xgb_recipe_g0)
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  #### PARAMETER TUNING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
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
  xgb_grid_search_m <- 
    xgb_workflow_m %>%
    tune_grid(
      # specify 5-fold CV
      resamples = K_folds_inner_m,
      # add parameter values used for tuning
      grid = xgb_grid,
      # define performance metrics (only AUC is used to select the best model)
      metrics = metric_set(roc_auc)
    )
  ## g(0, X)
  xgb_grid_search_g0 <- 
    xgb_workflow_g0 %>%
    tune_grid(resamples = K_folds_inner_g0, grid = xgb_grid, 
              metrics = metric_set(rmse))
  ## g(1, X)
  xgb_grid_search_g1 <- 
    xgb_workflow_g1 %>%
    tune_grid(resamples = K_folds_inner_g1, grid = xgb_grid, 
              metrics = metric_set(rmse))
  
  
  # select best penalty parameter: parameter with highest AUC
  xgo_best_param_m <- xgb_grid_search_m %>% select_best("roc_auc")
  xgb_best_param_g0 <- xgb_grid_search_g0 %>% select_best("rmse")
  xgb_best_param_g1 <- xgb_grid_search_g1 %>% select_best("rmse")

  
  df_best_param <- data.frame(
    "m_learn_rate" = xgo_best_param_m$learn_rate, "m_trees" = xgo_best_param_m$trees,
    "m_tree_depth" = xgo_best_param_m$tree_depth, "m_mtry" = xgo_best_param_m$mtry,
    "m_min_n" = xgo_best_param_m$min_n,
    "g0_learn_rate" = xgb_best_param_g0$learn_rate, "g0_trees" = xgb_best_param_g0$trees,
    "g0_tree_depth" = xgb_best_param_g0$tree_depth, "g0_mtry" = xgb_best_param_g0$mtry,
    "g0_min_n" = xgb_best_param_g0$min_n,
    "g1_learn_rate" = xgb_best_param_g1$learn_rate, "g1_trees" = xgb_best_param_g1$trees,
    "g1_tree_depth" = xgb_best_param_g1$tree_depth, "g1_mtry" = xgb_best_param_g1$mtry,
    "g1_min_n" = xgb_best_param_g1$min_n
  )
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### FINAL MODEL TRAINING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # specify the models
    ## model for m(X) = E(D|X): prediction of treatment
  tree_depth_m <- df_best_param$m_tree_depth
  trees_m <- df_best_param$m_trees
  learn_rate_m <- df_best_param$m_learn_rate
  mtry_m <- df_best_param$m_mtry
  min_n_m <- df_best_param$m_min_n
  
  xgb_spec_final_m <- 
    boost_tree(tree_depth = {{tree_depth_m}}, trees = {{trees_m}}, 
               learn_rate = {{learn_rate_m}}, mtry = {{mtry_m}}, min_n = {{min_n_m}}
               ) %>%
    set_engine("xgboost", objective = "binary:logistic") %>% 
    set_mode("classification")
  
    ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
  tree_depth_g0 <- df_best_param$g0_tree_depth
  trees_g0 <- df_best_param$g0_trees
  learn_rate_g0 <- df_best_param$g0_learn_rate
  mtry_g0 <- df_best_param$g0_mtry
  min_n_g0 <- df_best_param$g0_min_n
  
  
  xgb_spec_final_g0 <- 
    boost_tree(tree_depth = {{tree_depth_g0}}, trees = {{trees_g0}}, 
               learn_rate = {{learn_rate_g0}}, mtry = {{mtry_g0}},min_n = {{min_n_g0}}
               ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
    ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
  tree_depth_g1 <- df_best_param$g1_tree_depth
  trees_g1 <- df_best_param$g1_trees
  learn_rate_g1 <- df_best_param$g1_learn_rate
  mtry_g1 <- df_best_param$g1_mtry
  min_n_g1 <- df_best_param$g1_min_n
  
  xgb_spec_final_g1 <- 
    boost_tree(tree_depth = {{tree_depth_g1}}, trees = {{trees_g1}}, 
               learn_rate = {{learn_rate_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}
    ) %>%
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  # generate workflow
  xgb_workflow_final_m <- 
    workflow() %>%
    add_model(xgb_spec_final_m) %>%
    add_recipe(xgb_recipe_m)
  
  xgb_workflow_final_g0 <- 
    workflow() %>%
    add_model(xgb_spec_final_g0) %>%
    add_recipe(xgb_recipe_g0)
  
  xgb_workflow_final_g1 <- 
    workflow() %>%
    add_model(xgb_spec_final_g1) %>%
    add_recipe(xgb_recipe_g1)
  
  # fit the model
  ## m(X)
  xgb_fit_final_m <- 
    xgb_workflow_final_m %>%
    fit(data_train)
  ## g(0, X)
  xgb_fit_final_g0 <- 
    xgb_workflow_final_g0 %>%
    fit(data_train_g0)
  ## g(1, X)
  xgb_fit_final_g1 <- 
    xgb_workflow_final_g1 %>%
    fit(data_train_g1)
  

  #%%%%%%%%%%%%%%%%%%%#
  #### PREDICTIONS ####
  #%%%%%%%%%%%%%%%%%%%#
  
  # make predictions on test data
  xgb_pred_m <- predict(xgb_fit_final_m, data_test, type = "prob")
  xgb_pred_m <- xgb_pred_m$.pred_1 # probability for class 1
  
  xgb_pred_g0 <- predict(xgb_fit_final_g0, data_test)
  xgb_pred_g0 <- xgb_pred_g0$.pred 
  
  xgb_pred_g1 <- predict(xgb_fit_final_g1, data_test)
  xgb_pred_g1 <- xgb_pred_g1$.pred
  
  # create prediction data frame
  df_pred <- data.frame(
    # predictions
    "m" = xgb_pred_m, "g0" = xgb_pred_g0, "g1" = xgb_pred_g1,
    # true values
    "treatment" = data_test %>% pull(treatment), 
    "outcome" = data_test %>% pull(outcome),
    # number of predictors
    "num_pred_m" = ncol(xgb_fit_final_m$pre$mold$predictors),
    "num_pred_g0" = ncol(xgb_fit_final_g0$pre$mold$predictors),
    "num_pred_g1" = ncol(xgb_fit_final_g1$pre$mold$predictors)
  )
  
  # return data frame with predictions
  return(list("pred" = df_pred, "param" = df_best_param))
  
} # close function() 