#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses lasso to predict the nuisance parameters in both the binary
# and multivalued treatment setting. In the multivalued treatment setting
# separate logistic regression for each treatment level are performed.
#++++
# INPUT:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
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
# -> "coef":  non-zero coefficients (only for binary treatment setting)
#++++


func_ml_lasso <- function(treatment_setting, data_train, data_test, outcome, treatment, group, K, lambda_val) {
  
  if (!treatment_setting %in% c("binary", "multi")) {
    stop("Treatment setting: binary or multi")
  }
  
  
  # define a parameter grid with 1,000 random values for the penalty term
  lasso_grid <- grid_regular(penalty(), levels = lambda_val)

  
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
      # define outcome
      update_role({{treatment}}, new_role = "outcome") %>%
      # define predictors
      update_role(all_of(X_controls), new_role = "predictor")
    ## g(1, X)
    lasso_recipe_g1 <- 
      data_train_g1 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    ## g(0, X)
    lasso_recipe_g0 <- 
      data_train_g0 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_m <- 
      workflow() %>%
      add_model(lasso_spec_m) %>%
      add_recipe(lasso_recipe_m)
    
    lasso_workflow_g1 <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g1)
    
    lasso_workflow_g0 <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g0)
    
    
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
      lasso_workflow_g0 %>%
      tune_grid(resamples = K_folds_inner_g0, grid = lasso_grid, 
                metrics = metric_set(rmse))
    ## g(1, X)
    lasso_grid_search_g1 <- 
      lasso_workflow_g1 %>%
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
      logistic_reg(penalty = {{lasso_best_param_m}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
    lasso_spec_final_g0 <- 
      linear_reg(penalty = {{lasso_best_param_g0}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression") 
    ## model for g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
    lasso_spec_final_g1 <- 
      linear_reg(penalty = {{lasso_best_param_g1}}, mixture = 1) %>%  
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
      add_recipe(lasso_recipe_g0)
    
    lasso_workflow_final_g1 <- 
      workflow() %>%
      add_model(lasso_spec_final_g1) %>%
      add_recipe(lasso_recipe_g1)
    
    # fit the model
    ## m(X)
    lasso_fit_final_m <- 
      lasso_workflow_final_m %>%
      fit(data_train)
    ## g(0, X)
    lasso_fit_final_g0 <- 
      lasso_workflow_final_g0 %>%
      fit(data_train_g0)
    ## g(1, X)
    lasso_fit_final_g1 <- 
      lasso_workflow_final_g1 %>%
      fit(data_train_g1)
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### EXTRACT COEFFICIENTS ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # extract coefficients for treatment prediction
    lasso_coef_m <- tidy(lasso_fit_final_m) %>% as.data.frame()
    lasso_coef_m <- lasso_coef_m %>% filter(estimate > 0) %>% mutate(model = "m") %>% select(-penalty)
    
    # extract coefficients for outcome prediction
    lasso_coef_g0 <- tidy(lasso_fit_final_g0) %>% as.data.frame()
    lasso_coef_g0 <- lasso_coef_g0 %>% filter(estimate > 0) %>% mutate(model = "g0") %>% select(-penalty)
    
    lasso_coef_g1 <- tidy(lasso_fit_final_g1) %>% as.data.frame()
    lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    
    # append extracted coefficients to final data frame
    lasso_coef_all <- lasso_coef_m
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_g0)
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_g1)
    
    
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
      # predictions
      "m" = lasso_pred_m, "g0" = lasso_pred_g0, "g1" = lasso_pred_g1,
      # true values
      "treatment" = data_test %>% pull(treatment), 
      "outcome" = data_test %>% pull(outcome),
      # number of predictors
      "num_pred_m" = nrow(lasso_coef_m),
      "num_pred_g0" = nrow(lasso_coef_g0),
      "num_pred_g1" = nrow(lasso_coef_g1) #ncol(lasso_fit_final_g1$pre$mold$predictors)
    )
    
    # return data frame with predictions
    return(list("pred" = df_pred, "param" = df_best_param, "coef" = lasso_coef_all))
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Multivalued Treatment Setting ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    # ensure that treatment variables are factor
    data_train <- data_train %>% mutate("treatment_sport_freq" = as.factor(treatment_sport_freq)) 
    data_test <- data_test %>% mutate("treatment_sport_freq" = as.factor(treatment_sport_freq)) 
    
    data_train <- data_train %>% mutate("treatment_sport_freq_weekly_atleast" = as.factor(treatment_sport_freq_weekly_atleast))
    data_test <- data_test %>% mutate("treatment_sport_freq_weekly_atleast" = as.factor(treatment_sport_freq_weekly_atleast))
    
    data_train <- data_train %>% mutate("treatment_sport_freq_monthly_less" = as.factor(treatment_sport_freq_monthly_less))
    data_test <- data_test %>% mutate("treatment_sport_freq_monthly_less" = as.factor(treatment_sport_freq_monthly_less))
    
    data_train <- data_train %>% mutate("treatment_sport_freq_never" = as.factor(treatment_sport_freq_never)) 
    data_test <- data_test %>% mutate("treatment_sport_freq_never" = as.factor(treatment_sport_freq_never))
    
    # separate training data for g(1, X), g(2, X), and g(3, X) prediction
    data_train_g1 <- data_train %>% filter(treatment_sport_freq == 1)
    data_train_g2 <- data_train %>% filter(treatment_sport_freq == 2)
    data_train_g3 <- data_train %>% filter(treatment_sport_freq == 3)
    
    # specify the model: treatment is predicted via binary classification 
    # and outcome via regression
    lasso_spec_m <- 
      logistic_reg(penalty = tune(), mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification")  
    
    lasso_spec_g <- 
      linear_reg(penalty = tune(), mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")  
    
    # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except variables including treatment information, outcome, and group
    X_controls <- data_train %>% 
      select(-c(all_of(outcome), starts_with(treatment) & !ends_with("na"), all_of(group))) %>% colnames()
    ## m(x) for each treatment category
    lasso_recipe_m1 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual participates at least weekly in sports
      update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor") # controls
    lasso_recipe_m2 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual participates monthly or less frequently in sports
      update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    lasso_recipe_m3 <- 
      data_train %>%
      recipe(.) %>%
      # outcome: indicator if individual does not participate in sports
      update_role("treatment_sport_freq_never", new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    ## g(D, X) for each treatment category
    lasso_recipe_g1 <- 
      data_train_g1 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    lasso_recipe_g2 <- 
      data_train_g2 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    lasso_recipe_g3 <- 
      data_train_g3 %>%  
      recipe(.) %>%
      update_role({{outcome}}, new_role = "outcome") %>%
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_m1 <- 
      workflow() %>%
      add_model(lasso_spec_m) %>%
      add_recipe(lasso_recipe_m1)
    
    lasso_workflow_m2 <- 
      workflow() %>%
      add_model(lasso_spec_m) %>%
      add_recipe(lasso_recipe_m2)
    
    lasso_workflow_m3 <- 
      workflow() %>%
      add_model(lasso_spec_m) %>%
      add_recipe(lasso_recipe_m3)
    
    lasso_workflow_g1 <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g1)
    
    lasso_workflow_g2 <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g2)
    
    lasso_workflow_g3 <- 
      workflow() %>%
      add_model(lasso_spec_g) %>%
      add_recipe(lasso_recipe_g3)
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Parameter Tuning ####
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # parameter tuning via k-fold CV
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
    lasso_grid_search_m1 <- 
      lasso_workflow_m1 %>%
      tune_grid(resamples = K_folds_inner_m1, grid = lasso_grid, metrics = metric_set(roc_auc))
    lasso_grid_search_m2 <- 
      lasso_workflow_m2 %>%
      tune_grid(resamples = K_folds_inner_m2, grid = lasso_grid, metrics = metric_set(roc_auc))
    lasso_grid_search_m3 <- 
      lasso_workflow_m3 %>%
      tune_grid(resamples = K_folds_inner_m3, grid = lasso_grid, metrics = metric_set(roc_auc))
    ## g(D, X)
    lasso_grid_search_g1 <- 
      lasso_workflow_g1 %>%
      tune_grid(resamples = K_folds_inner_g1, grid = lasso_grid, metrics = metric_set(rmse))
    lasso_grid_search_g2 <- 
      lasso_workflow_g2 %>%
      tune_grid(resamples = K_folds_inner_g2, grid = lasso_grid, metrics = metric_set(rmse))
    lasso_grid_search_g3 <- 
      lasso_workflow_g3 %>%
      tune_grid(resamples = K_folds_inner_g3, grid = lasso_grid, metrics = metric_set(rmse))
    
    
    # select best penalty parameter: parameter with highest AUC
    lasso_best_param_m1 <- lasso_grid_search_m1 %>% select_best("roc_auc")
    lasso_best_param_m2 <- lasso_grid_search_m2 %>% select_best("roc_auc")
    lasso_best_param_m3 <- lasso_grid_search_m3 %>% select_best("roc_auc")
    lasso_best_param_g1 <- lasso_grid_search_g1 %>% select_best("rmse")
    lasso_best_param_g2 <- lasso_grid_search_g2 %>% select_best("rmse")
    lasso_best_param_g3 <- lasso_grid_search_g3 %>% select_best("rmse")
    
    
    df_best_param <- data.frame(
      "m1" = lasso_best_param_m1$penalty, "m2" = lasso_best_param_m2$penalty,
      "m3" = lasso_best_param_m3$penalty,
      "g1" = lasso_best_param_g1$penalty, "g2" = lasso_best_param_g2$penalty,
      "g3" = lasso_best_param_g3$penalty
    )
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Final Model Training ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # specify the models
    ## model for m(X) = E(D|X): prediction of treatment
    m1 <- df_best_param$m1
    lasso_spec_final_m1 <- 
      logistic_reg(penalty = {{m1}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    m2 <- df_best_param$m2
    lasso_spec_final_m2 <- 
      logistic_reg(penalty = {{m2}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    m3 <- df_best_param$m3
    lasso_spec_final_m3 <- 
      logistic_reg(penalty = {{m3}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    

    ## model for g(D, X) 
    g1 <- df_best_param$g1
    lasso_spec_final_g1 <- 
      linear_reg(penalty = {{g1}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression") 
    
    g2 <- df_best_param$g2
    lasso_spec_final_g2 <- 
      linear_reg(penalty = {{g2}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression") 
    
    g3 <- df_best_param$g3
    lasso_spec_final_g3 <- 
      linear_reg(penalty = {{g3}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression") 
    
    # generate workflow
    lasso_workflow_final_m1 <- 
      workflow() %>%
      add_model(lasso_spec_final_m1) %>%
      add_recipe(lasso_recipe_m1)
    
    lasso_workflow_final_m2 <- 
      workflow() %>%
      add_model(lasso_spec_final_m2) %>%
      add_recipe(lasso_recipe_m2)
    
    lasso_workflow_final_m3 <- 
      workflow() %>%
      add_model(lasso_spec_final_m3) %>%
      add_recipe(lasso_recipe_m3)
    
    lasso_workflow_final_g1 <- 
      workflow() %>%
      add_model(lasso_spec_final_g1) %>%
      add_recipe(lasso_recipe_g1)
    
    lasso_workflow_final_g2 <- 
      workflow() %>%
      add_model(lasso_spec_final_g2) %>%
      add_recipe(lasso_recipe_g2)
    
    lasso_workflow_final_g3 <- 
      workflow() %>%
      add_model(lasso_spec_final_g3) %>%
      add_recipe(lasso_recipe_g3)
    
    # fit the model
    ## m(X)
    lasso_fit_final_m1 <- 
      lasso_workflow_final_m1 %>%
      fit(data_train)
    lasso_fit_final_m2 <- 
      lasso_workflow_final_m2 %>%
      fit(data_train)
    lasso_fit_final_m3 <- 
      lasso_workflow_final_m3 %>%
      fit(data_train)
    ## g(D X)
    lasso_fit_final_g1 <- 
      lasso_workflow_final_g1 %>%
      fit(data_train_g1)
    lasso_fit_final_g2 <- 
      lasso_workflow_final_g2 %>%
      fit(data_train_g2)
    lasso_fit_final_g3 <- 
      lasso_workflow_final_g3 %>%
      fit(data_train_g3)
    
    
    #%%%%%%%%%%%%%%%%%%%%#
    #### Coefficients ####
    #%%%%%%%%%%%%%%%%%%%%#
    
    # extract coefficients for treatment prediction
    lasso_coef_m1 <- tidy(lasso_fit_final_m1) %>% as.data.frame()
    lasso_coef_m1 <- lasso_coef_m1 %>% filter(estimate > 0) %>% mutate(model = "m") %>% select(-penalty)
    
    lasso_coef_m2 <- tidy(lasso_fit_final_m2) %>% as.data.frame()
    lasso_coef_m2 <- lasso_coef_m2 %>% filter(estimate > 0) %>% mutate(model = "m") %>% select(-penalty)
    
    lasso_coef_m3 <- tidy(lasso_fit_final_m3) %>% as.data.frame()
    lasso_coef_m3 <- lasso_coef_m3 %>% filter(estimate > 0) %>% mutate(model = "m") %>% select(-penalty)
    
    # extract coefficients for outcome prediction
    lasso_coef_g1 <- tidy(lasso_fit_final_g1) %>% as.data.frame()
    lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    lasso_coef_g2 <- tidy(lasso_fit_final_g2) %>% as.data.frame()
    lasso_coef_g2 <- lasso_coef_g2 %>% filter(estimate > 0) %>% mutate(model = "g2") %>% select(-penalty)
    
    lasso_coef_g3 <- tidy(lasso_fit_final_g3) %>% as.data.frame()
    lasso_coef_g3 <- lasso_coef_g3 %>% filter(estimate > 0) %>% mutate(model = "g3") %>% select(-penalty)
    
    
    # append extracted coefficients to final data frame
    lasso_coef_all <- lasso_coef_m1
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_m2)
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_m3)
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_g1)
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_g2)
    lasso_coef_all <- rbind(lasso_coef_all, lasso_coef_g3)
    
    
    
    #%%%%%%%%%%%%%%%%%%%#
    #### Predictions ####
    #%%%%%%%%%%%%%%%%%%%#
    
    # make predictions on test data
    lasso_pred_m1 <- predict(lasso_fit_final_m1, data_test, type = "prob")
    lasso_pred_m1 <- lasso_pred_m1$.pred_1 # probability for class 1
    
    lasso_pred_m2 <- predict(lasso_fit_final_m2, data_test, type = "prob")
    lasso_pred_m2 <- lasso_pred_m2$.pred_1 # probability for class 1
    
    lasso_pred_m3 <- predict(lasso_fit_final_m3, data_test, type = "prob")
    lasso_pred_m3 <- lasso_pred_m3$.pred_1 # probability for class 1
    
    lasso_pred_g1 <- predict(lasso_fit_final_g1, data_test)
    lasso_pred_g1 <- lasso_pred_g1$.pred
    
    lasso_pred_g2 <- predict(lasso_fit_final_g2, data_test)
    lasso_pred_g2 <- lasso_pred_g2$.pred
    
    lasso_pred_g3 <- predict(lasso_fit_final_g3, data_test)
    lasso_pred_g3 <- lasso_pred_g3$.pred
    
    
    # create prediction data frame
    df_pred <- data.frame(
      # predictions
      "m1" = lasso_pred_m1, "m2" = lasso_pred_m2, "m3" = lasso_pred_m3,
      "g1" = lasso_pred_g1, "g2" = lasso_pred_g2, "g3" = lasso_pred_g3,
      # true values
      "treatment" = data_test %>% pull(treatment), 
      "outcome" = data_test %>% pull(outcome),
      # number of predictors
      "num_pred_m1" = nrow(lasso_coef_m1),
      "num_pred_m2" = nrow(lasso_coef_m2),
      "num_pred_m3" = nrow(lasso_coef_m3),
      "num_pred_g1" = nrow(lasso_coef_g1),
      "num_pred_g2" = nrow(lasso_coef_g2),
      "num_pred_g3" = nrow(lasso_coef_g3)
    )
    
    
    # propensity scores are normalized to sum to 1 within an individual
    df_pred <- df_pred %>%
      mutate(m_sum = m1 + m2 + m3) %>%
      mutate(m1 = m1 / m_sum, m2 = m2 / m_sum, m3 = m3 / m_sum) %>%
      select(-m_sum)
 
    
    # return data frame with predictions
    return(list("pred" = df_pred, "param" = df_best_param, "coef" = lasso_coef_all))
  } # close else-if from multi
  
} # close function() 
