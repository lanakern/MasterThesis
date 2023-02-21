#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH RANDOM FORESTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses random forests to predict the nuisance parameters in both
# the binary and multivalued treatment setting. For the multivalued treatment setting
# separate logistic regressions are performed for each treatment level.
#++++
# INPUT:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
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


func_ml_rf <- function(treatment_setting, data_train, data_test, outcome, treatment, group, K, rf_grid) {
  
  if (!treatment_setting %in% c("binary", "multi")) {
    stop("Treatment setting: binary or multi")
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
    
    # ensure that treatment variable is factor
    data_train <- data_train %>% mutate("treatment_sport" = as.factor(treatment_sport)) 
    data_test <- data_test %>% mutate("treatment_sport" = as.factor(treatment_sport)) 
    
    # separate training data for g0 and g1 prediction
    data_train_g1 <- data_train %>% filter(treatment_sport == 1)
    data_train_g0 <- data_train %>% filter(treatment_sport == 0)
    
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
    
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### MULTIVALUED TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    # ensure that treatment variables are factor
    data_train <- data_train %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
    data_test <- data_test %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
    
    data_train <- data_train %>% mutate("treatment_sport_freq_weekly_atleast" = as.factor(treatment_sport_freq_weekly_atleast))
    data_test <- data_test %>% mutate("treatment_sport_freq_weekly_atleast" = as.factor(treatment_sport_freq_weekly_atleast))
    
    data_train <- data_train %>% mutate("treatment_sport_freq_monthly_less" = as.factor(treatment_sport_freq_monthly_less))
    data_test <- data_test %>% mutate("treatment_sport_freq_monthly_less" = as.factor(treatment_sport_freq_monthly_less))
    
    data_train <- data_train %>% mutate("treatment_sport_freq_never" = as.factor(treatment_sport_freq_never)) 
    data_test <- data_test %>% mutate("treatment_sport_freq_never" = as.factor(treatment_sport_freq_never))
    
    # separate training data for g(1, X), g(2, X), and g(3, X) prediction
    data_train_g1 <- data_train %>% filter(!!sym(treatment) == 1)
    data_train_g2 <- data_train %>% filter(!!sym(treatment) == 2)
    data_train_g3 <- data_train %>% filter(!!sym(treatment) == 3)
    
    # specify the model: treatment is predicted via binary classification 
    # and outcome via regression
    rf_spec_m <- 
      rand_forest(trees = {{trees_sel}}, mtry = tune(), min_n = tune()) %>% 
      set_engine("randomForest") %>% 
      set_mode("classification")
    
    rf_spec_g <- 
      rand_forest(trees = {{trees_sel}}, mtry = tune(), min_n = tune()) %>% 
      set_engine("randomForest") %>% 
      set_mode("regression")
    
    # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except variables including treatment information, outcome, and group
    X_controls <- data_train %>% 
      select(-c(all_of(outcome), starts_with(treatment) & !ends_with("na"), 
                all_of(group))) %>% colnames()
    ## m(x) for each treatment category
    rf_recipe_m1 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual participates at least weekly in sports
      update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor") # controls
    rf_recipe_m2 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual participates monthly or less frequently in sports
      update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    rf_recipe_m3 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual does not participate in sports
      update_role("treatment_sport_freq_never", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    ## g(D, X) for each treatment category
    rf_recipe_g1 <- 
      data_train_g1 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    rf_recipe_g2 <- 
      data_train_g2 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    rf_recipe_g3 <- 
      data_train_g3 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    rf_workflow_m1 <- 
      workflow() %>%
      add_model(rf_spec_m) %>%
      add_recipe(rf_recipe_m1)
    
    rf_workflow_m2 <- 
      workflow() %>%
      add_model(rf_spec_m) %>%
      add_recipe(rf_recipe_m2)
    
    rf_workflow_m3 <- 
      workflow() %>%
      add_model(rf_spec_m) %>%
      add_recipe(rf_recipe_m3)
    
    rf_workflow_g1 <- 
      workflow() %>%
      add_model(rf_spec_g) %>%
      add_recipe(rf_recipe_g1)
    
    rf_workflow_g2 <- 
      workflow() %>%
      add_model(rf_spec_g) %>%
      add_recipe(rf_recipe_g2)
    
    rf_workflow_g3 <- 
      workflow() %>%
      add_model(rf_spec_g) %>%
      add_recipe(rf_recipe_g3)
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Parameter Tuning ####
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # parameter tuning via 5-fold CV
    # this means that training data is again partitioned into K-folds
    K_folds_inner_m1 <- rsample::group_vfold_cv(
      data = data_train, 
      v = K, group = group, strata = all_of(treatment_sport_freq_weekly_atleast), balance = "observations"
    )
    K_folds_inner_m2 <- rsample::group_vfold_cv(
      data = data_train, 
      v = K, group = group, strata = all_of(treatment_sport_freq_monthly_less), balance = "observations"
    )
    K_folds_inner_m3 <- rsample::group_vfold_cv(
      data = data_train, 
      v = K, group = group, strata = all_of(treatment_sport_freq_never), balance = "observations"
    )
    
    
    K_folds_inner_g1 <- rsample::group_vfold_cv(
      data = data_train_g1,  
      v = K, group = group, strata = all_of(outcome), balance = "observations"
    )
    K_folds_inner_g2 <- rsample::group_vfold_cv(
      data = data_train_g2,  
      v = K, group = group, strata = all_of(outcome), balance = "observations"
    )
    K_folds_inner_g3 <- rsample::group_vfold_cv(
      data = data_train_g3,  
      v = K, group = group, strata = all_of(outcome), balance = "observations"
    )
    
    # conduct parameter tuning
    ## m(X)
    rf_grid <- rf_grid %>% select(-trees)
    
    rf_grid_search_m1 <- 
      rf_workflow_m1 %>%
      tune_grid(resamples = K_folds_inner_m1, grid = rf_grid, metrics = metric_set(roc_auc))
    rf_grid_search_m2 <- 
      rf_workflow_m2 %>%
      tune_grid(resamples = K_folds_inner_m2, grid = rf_grid, metrics = metric_set(roc_auc))
    rf_grid_search_m3 <- 
      rf_workflow_m3 %>%
      tune_grid(resamples = K_folds_inner_m3, grid = rf_grid, metrics = metric_set(roc_auc))
    ## g(D, X)
    rf_grid_search_g1 <- 
      rf_workflow_g1 %>%
      tune_grid(resamples = K_folds_inner_g1, grid = rf_grid, metrics = metric_set(rmse))
    rf_grid_search_g2 <- 
      rf_workflow_g2 %>%
      tune_grid(resamples = K_folds_inner_g2, grid = rf_grid, metrics = metric_set(rmse))
    rf_grid_search_g3 <- 
      rf_workflow_g3 %>%
      tune_grid(resamples = K_folds_inner_g3, grid = rf_grid, metrics = metric_set(rmse))
    
    
    # select best penalty parameter: parameter with highest AUC
    rf_best_param_m1 <- rf_grid_search_m1 %>% select_best("roc_auc")
    rf_best_param_m2 <- rf_grid_search_m2 %>% select_best("roc_auc")
    rf_best_param_m3 <- rf_grid_search_m3 %>% select_best("roc_auc")
    rf_best_param_g1 <- rf_grid_search_g1 %>% select_best("rmse")
    rf_best_param_g2 <- rf_grid_search_g2 %>% select_best("rmse")
    rf_best_param_g3 <- rf_grid_search_g3 %>% select_best("rmse")
    
    
    df_best_param <- data.frame(
      "m1_trees" = trees_sel, "m1_mtry" = rf_best_param_m1$mtry,
      "m1_min_n" = rf_best_param_m1$min_n,
      "m2_trees" = trees_sel, "m2_mtry" = rf_best_param_m2$mtry,
      "m2_min_n" = rf_best_param_m2$min_n,
      "m3_trees" = trees_sel, "m3_mtry" = rf_best_param_m3$mtry,
      "m3_min_n" = rf_best_param_m3$min_n,
      "g1_trees" = trees_sel, "g1_mtry" = rf_best_param_g1$mtry,
      "g1_min_n" = rf_best_param_g1$min_n,
      "g2_trees" = trees_sel, "g2_mtry" = rf_best_param_g1$mtry,
      "g2_min_n" = rf_best_param_g1$min_n,
      "g3_trees" = trees_sel, "g3_mtry" = rf_best_param_g1$mtry,
      "g3_min_n" = rf_best_param_g1$min_n
    )
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Final Model Training ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # specify the models
    ## model for m(X) = E(D|X): prediction of treatment
    trees_m1 <- df_best_param$m1_trees
    mtry_m1 <- df_best_param$m1_mtry
    min_n_m1 <- df_best_param$m1_min_n
    
    rf_spec_final_m1 <- 
      rand_forest(trees = {{trees_m1}}, mtry = {{mtry_m1}}, min_n = {{min_n_m1}}) %>%
      set_engine("randomForest") %>% 
      set_mode("classification")
    
    trees_m2 <- df_best_param$m2_trees
    mtry_m2 <- df_best_param$m2_mtry
    min_n_m2 <- df_best_param$m2_min_n
    
    rf_spec_final_m2 <- 
      rand_forest(trees = {{trees_m2}}, mtry = {{mtry_m2}}, min_n = {{min_n_m2}}) %>%
      set_engine("randomForest") %>% 
      set_mode("classification")
    
    
    trees_m3 <- df_best_param$m3_trees
    mtry_m3 <- df_best_param$m3_mtry
    min_n_m3 <- df_best_param$m3_min_n
    
    rf_spec_final_m3 <- 
      rand_forest(trees = {{trees_m3}}, mtry = {{mtry_m3}}, min_n = {{min_n_m3}}) %>%
      set_engine("randomForest") %>% 
      set_mode("classification")

    
    ## model for g(D, X) 
    trees_g1 <- df_best_param$g1_trees
    mtry_g1 <- df_best_param$g1_mtry
    min_n_g1 <- df_best_param$g1_min_n
    
    rf_spec_final_g1 <- 
      rand_forest(trees = {{trees_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}
      ) %>%
      set_engine("randomForest") %>% 
      set_mode("regression")
    
    trees_g2 <- df_best_param$g2_trees
    mtry_g2 <- df_best_param$g2_mtry
    min_n_g2 <- df_best_param$g2_min_n
    
    rf_spec_final_g2 <- 
      rand_forest(trees = {{trees_g2}}, mtry = {{mtry_g2}}, min_n = {{min_n_g2}}
      ) %>%
      set_engine("randomForest") %>% 
      set_mode("regression")
    
    trees_g3 <- df_best_param$g3_trees
    mtry_g3 <- df_best_param$g3_mtry
    min_n_g3 <- df_best_param$g3_min_n
    
    rf_spec_final_g3 <- 
      rand_forest(trees = {{trees_g3}}, mtry = {{mtry_g3}}, min_n = {{min_n_g3}}
      ) %>%
      set_engine("randomForest") %>% 
      set_mode("regression")
    
    # generate workflow
    rf_workflow_final_m1 <- 
      workflow() %>%
      add_model(rf_spec_final_m1) %>%
      add_recipe(rf_recipe_m1)
    
    rf_workflow_final_m2 <- 
      workflow() %>%
      add_model(rf_spec_final_m2) %>%
      add_recipe(rf_recipe_m2)
    
    rf_workflow_final_m3 <- 
      workflow() %>%
      add_model(rf_spec_final_m3) %>%
      add_recipe(rf_recipe_m3)
    
    rf_workflow_final_g1 <- 
      workflow() %>%
      add_model(rf_spec_final_g1) %>%
      add_recipe(rf_recipe_g1)
    
    rf_workflow_final_g2 <- 
      workflow() %>%
      add_model(rf_spec_final_g2) %>%
      add_recipe(rf_recipe_g2)
    
    rf_workflow_final_g3 <- 
      workflow() %>%
      add_model(rf_spec_final_g3) %>%
      add_recipe(rf_recipe_g3)
    
    # fit the model
    ## m(X)
    rf_fit_final_m1 <- 
      rf_workflow_final_m1 %>%
      fit(data_train)
    rf_fit_final_m2 <- 
      rf_workflow_final_m2 %>%
      fit(data_train)
    rf_fit_final_m3 <- 
      rf_workflow_final_m3 %>%
      fit(data_train)
    ## g(D X)
    rf_fit_final_g1 <- 
      rf_workflow_final_g1 %>%
      fit(data_train_g1)
    rf_fit_final_g2 <- 
      rf_workflow_final_g2 %>%
      fit(data_train_g2)
    rf_fit_final_g3 <- 
      rf_workflow_final_g3 %>%
      fit(data_train_g3)
    
    
    #%%%%%%%%%%%%%%%%%%%#
    #### Predictions ####
    #%%%%%%%%%%%%%%%%%%%#
    
    # make predictions on test data
    rf_pred_m1 <- predict(rf_fit_final_m1, data_test, type = "prob")
    rf_pred_m1 <- rf_pred_m1$.pred_1 # probability for class 1
    
    rf_pred_m2 <- predict(rf_fit_final_m2, data_test, type = "prob")
    rf_pred_m2 <- rf_pred_m2$.pred_1 # probability for class 1
    
    rf_pred_m3 <- predict(rf_fit_final_m3, data_test, type = "prob")
    rf_pred_m3 <- rf_pred_m3$.pred_1 # probability for class 1
    
    rf_pred_g1 <- predict(rf_fit_final_g1, data_test)
    rf_pred_g1 <- rf_pred_g1$.pred
    
    rf_pred_g2 <- predict(rf_fit_final_g2, data_test)
    rf_pred_g2 <- rf_pred_g2$.pred
    
    rf_pred_g3 <- predict(rf_fit_final_g3, data_test)
    rf_pred_g3 <- rf_pred_g3$.pred
    
    
    # create prediction data frame
    df_pred <- data.frame(
      # predictions
      "m1" = rf_pred_m1, "m2" = rf_pred_m2, "m3" = rf_pred_m3,
      "g1" = rf_pred_g1, "g2" = rf_pred_g2, "g3" = rf_pred_g3,
      # true values
      "treatment" = data_test %>% pull(treatment), 
      "outcome" = data_test %>% pull(outcome),
      # number of predictors
      "num_pred_m1" = ncol(rf_fit_final_m1$pre$mold$predictors),
      "num_pred_m2" = ncol(rf_fit_final_m2$pre$mold$predictors),
      "num_pred_m3" = ncol(rf_fit_final_m3$pre$mold$predictors),
      "num_pred_g1" = ncol(rf_fit_final_g1$pre$mold$predictors),
      "num_pred_g2" = ncol(rf_fit_final_g2$pre$mold$predictors),
      "num_pred_g3" = ncol(rf_fit_final_g3$pre$mold$predictors)
    )
    
    
    # propensity scores are normalized to sum to 1 within an individual
    df_pred <- df_pred %>%
      mutate(m_sum = m1 + m2 + m3) %>%
      mutate(m1 = m1 / m_sum, m2 = m2 / m_sum, m3 = m3 / m_sum) %>%
      select(-m_sum)
    
    # return data frame with predictions
    return(list("pred" = df_pred, "param" = df_best_param))
  }
  
  
  
} # close function() 