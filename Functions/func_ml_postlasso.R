#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH POST-LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses post-lasso to predict the nuisance parameters. 
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
# -> "coef": non-zero coefficients
#++++


func_ml_postlasso <- function(data_train, data_test, outcome, treatment, group, K, lambda_val) {
  
  # ensure that treatment variable is factor
  data_train <- data_train %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  data_test <- data_test %>% mutate({{treatment}} := as.factor(!!sym(treatment))) 
  
  # generate extra training data sets for outcome predictions
  data_train_g1 <- data_train %>% filter(treatment_sport == 1)
  data_train_g0 <- data_train %>% filter(treatment_sport == 0)
  
  # generate vector with control variable names
  X_controls <- data_train %>% 
    select(-c(all_of(outcome), all_of(treatment), all_of(group))) %>% colnames()
  
  # specify the models: lasso regression with penalty as tuning parameter
  lasso_spec_m <- 
    logistic_reg(penalty = tune(), mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("classification") # treatment is classification
  
  lasso_spec_g <- 
    linear_reg(penalty = tune(), mixture = 1) %>%  
    set_engine("glmnet") %>%  
    set_mode("regression") # outcome is regression
  
  # generate recipe: define outcome and predictors
    ## m(x)
  lasso_recipe_m <- 
    data_train %>%
    recipe(.) %>%
    update_role({{treatment}}, new_role = "outcome") %>%
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
  
  # define a parameter grid with 1,000 random values for the penalty term
  lasso_grid <- grid_regular(penalty(), levels = lambda_val)
  
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
  

  df_tuning_all <- data.frame()
  
  for (lambda_sel in lasso_grid$penalty) {
    
    df_tuning_fold_all <- data.frame()
    
    for (fold_sel in 1:K) {
      
      
      # m(X) #
      #++++++#
      
      # extract training and test data for parameter tuning
      indices_fold_sel_m <- K_folds_inner_m$splits[[fold_sel]]$in_id
      data_tuning_train_m <- data_train[indices_fold_sel_m, ]
      data_tuning_test <- data_train[-indices_fold_sel_m, ] # same for m(X) and g(X)
      
      # specify the model
      lasso_spec_tuning_m <- 
        logistic_reg(penalty = {{lambda_sel}}, mixture = 1) %>%  
        set_engine("glmnet") %>%  
        set_mode("classification") 
      
      # define recipe
      lasso_recipe_tuning_m <- 
        data_tuning_train_m %>%
        recipe(.) %>%
        # price variable is outcome
        update_role({{treatment}}, new_role = "outcome") %>%
        # all other variables are predictors (drop outcome treatment)
        update_role(all_of(X_controls), new_role = "predictor")
      
      # generate workflow
      lasso_workflow_tuning_m <- 
        workflow() %>%
        add_model(lasso_spec_tuning_m) %>%
        add_recipe(lasso_recipe_tuning_m)
      
      # fit the model
      lasso_fit_tuning_m <- 
        lasso_workflow_tuning_m %>%
        fit(data_tuning_train_m)
      
      # extract coefficients
      lasso_coef_tuning_m <- tidy(lasso_fit_tuning_m) %>% as.data.frame()
      lasso_coef_tuning_m <- lasso_coef_tuning_m %>% filter(estimate > 0) %>% pull(term)
      
      
      
      # g(1, X) #
      #+++++++++#
      
      # extract training and test data for parameter tuning
      indices_fold_sel_g1 <- K_folds_inner_g1$splits[[fold_sel]]$in_id
      data_tuning_train_g1 <- data_train_g1[indices_fold_sel_g1, ]
      
      # specify the model
      lasso_spec_tuning_g1 <- 
        linear_reg(penalty = {{lambda_sel}}, mixture = 1) %>%  
        set_engine("glmnet") %>%  
        set_mode("regression")  
      
      # define recipe
      lasso_recipe_tuning_g1 <- 
        data_tuning_train_g1 %>%
        recipe(.) %>%
        # price variable is outcome
        update_role({{outcome}}, new_role = "outcome") %>%
        # all other variables are predictors (drop outcome treatment)
        update_role(all_of(X_controls), new_role = "predictor")
      
      # generate workflow
      lasso_workflow_tuning_g1 <- 
        workflow() %>%
        add_model(lasso_spec_tuning_g1) %>%
        add_recipe(lasso_recipe_tuning_g1)
      
      # fit the model
      lasso_fit_tuning_g1 <- 
        lasso_workflow_tuning_g1 %>%
        fit(data_tuning_train_g1)
      
      # extract coefficients
      lasso_coef_tuning_g1 <- tidy(lasso_fit_tuning_g1) %>% as.data.frame()
      lasso_coef_tuning_g1 <- lasso_coef_tuning_g1 %>% filter(estimate > 0) %>% pull(term)
      
      
      
      # g(0, X) #
      #+++++++++#
      
      # extract training and test data for parameter tuning
      indices_fold_sel_g0 <- K_folds_inner_g0$splits[[fold_sel]]$in_id
      data_tuning_train_g0 <- data_train_g0[indices_fold_sel_g0, ]
      
      # specify the model
      lasso_spec_tuning_g0 <- 
        linear_reg(penalty = {{lambda_sel}}, mixture = 1) %>%  
        set_engine("glmnet") %>%  
        set_mode("regression")  
      
      # define recipe
      lasso_recipe_tuning_g0 <- 
        data_tuning_train_g0 %>%
        recipe(.) %>%
        # price variable is outcome
        update_role({{outcome}}, new_role = "outcome") %>%
        # all other variables are predictors (drop outcome treatment)
        update_role(all_of(X_controls), new_role = "predictor")
      
      # generate workflow
      lasso_workflow_tuning_g0 <- 
        workflow() %>%
        add_model(lasso_spec_tuning_g0) %>%
        add_recipe(lasso_recipe_tuning_g0)
      
      # fit the model
      lasso_fit_tuning_g0 <- 
        lasso_workflow_tuning_g0 %>%
        fit(data_tuning_train_g0)
      
      # extract coefficients
      lasso_coef_tuning_g0 <- tidy(lasso_fit_tuning_g0) %>% as.data.frame()
      lasso_coef_tuning_g0 <- lasso_coef_tuning_g0 %>% filter(estimate > 0) %>% pull(term)
      
      
      
      # COEFFICIENTS #
      #++++++++++++++#
      
      # union of coefficients selected in treatment and both outcome equations
      coef_tuning_all <- union(lasso_coef_tuning_m, union(lasso_coef_tuning_g0, lasso_coef_tuning_g1))
      coef_tuning_all <- coef_tuning_all[!str_detect("(Intercept)", coef_tuning_all)]
      
      
      # POST-LASSO #
      #++++++++++++#
      
      # NOTE: SAME RESULT AS WITH TIDYMODELS FRAMEWORK BUT SHORTER
      data_train_final_m <- data_tuning_train_m %>% select(all_of(treatment), all_of(coef_tuning_all))
      data_train_final_g0 <-  data_tuning_train_g0 %>% select(all_of(outcome), all_of(coef_tuning_all))
      data_train_final_g1 <- data_tuning_train_g1 %>% select(all_of(outcome), all_of(coef_tuning_all))
      
      data_test_final_m <- data_tuning_test %>% select(all_of(treatment), all_of(coef_tuning_all))
      data_test_final_g <- data_tuning_test %>% select(all_of(outcome), all_of(coef_tuning_all))
      
      model_m <- glm(paste(treatment, "~ ."), family = binomial(link = "logit"), data = data_train_final_m)
      lasso_pred_m <- unname(predict(model_m, data_test_final_m, type = "response")) # return probability
      
      model_lm_0 <- lm(paste(outcome, "~ ."), data = data_train_final_g0)
      lasso_pred_g0 <- unname(predict(model_lm_0, data_test_final_g))
      
      model_lm_1 <- lm(paste(outcome, "~ ."), data = data_train_final_g1)
      lasso_pred_g1 <- unname(predict(model_lm_1, data_test_final_g))
      
      # create prediction data frame
      df_pred_tuning <- data.frame(
        # predictions
        "m" = 1 - lasso_pred_m, # to get probability for class 0
        "g0" = lasso_pred_g0, "g1" = lasso_pred_g1,
        # true values
        "treatment" = data_tuning_test %>% pull(treatment), 
        "outcome" = data_tuning_test %>% pull(outcome)
      )
      
      
      # Error Metrics #
      #+++++++++++++++#

      auc_tuning <- 
        yardstick::roc_auc(data = df_pred_tuning, truth = treatment, estimate = m) %>% 
        select(.estimate) %>% pull() 
      
      rmse_0_tuning <-
        yardstick::rmse(data = df_pred_tuning %>% filter(treatment == 0),
                        truth = outcome, estimate = g0) %>%
        select(.estimate) %>% pull() 
      
      rmse_1_tuning <-
        yardstick::rmse(data = df_pred_tuning %>% filter(treatment == 1),
                        truth = outcome, estimate = g1) %>%
        select(.estimate) %>% pull() 
      
      df_tuning_fold <- 
        data.frame(fold = fold_sel, AUC = auc_tuning, RMSE_0 = rmse_0_tuning, RMSE_1 = rmse_1_tuning)
      df_tuning_fold_all <- rbind(df_tuning_fold_all, df_tuning_fold)
      
    } # close iteration over folds
    
    # aggregate results over fold
    df_tuning_aggr <- df_tuning_fold_all %>%
      summarize(AUC = mean(AUC), RMSE_0 = mean(RMSE_0), RMSE_1 = mean(RMSE_1)) %>%
      mutate(lambda = lambda_sel)
    
    df_tuning_all <- rbind(df_tuning_all, df_tuning_aggr)
    
  } # close iteration over lambda

  # select lambda that yields highest AUC
  lasso_best_param_m <- df_tuning_all %>% filter(AUC == max(AUC)) %>% pull(lambda)
  lasso_best_param_g0 <- df_tuning_all %>% filter(RMSE_0 == min(RMSE_0)) %>% pull(lambda)
  lasso_best_param_g1 <- df_tuning_all %>% filter(RMSE_1 == min(RMSE_1)) %>% pull(lambda)

  # SAME RESULT
  # lasso_grid_search_m <- 
  #   lasso_workflow_m %>%
  #   tune_grid(resamples = K_folds_inner_m, grid = lasso_grid, metrics = metric_set(roc_auc))
  # lasso_best_param_m <- lasso_grid_search_m %>% select_best("roc_auc")
  # lasso_best_param_m <- lasso_best_param_m$penalty
  # 

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
  
  # generate workflow and fit model
  lasso_fit_final_m <- 
    workflow() %>%
    add_model(lasso_spec_final_m) %>%
    add_recipe(lasso_recipe_m) %>%
    fit(data_train)
  
  lasso_fit_final_g0 <- 
    workflow() %>%
    add_model(lasso_spec_final_g0) %>%
    add_recipe(lasso_recipe_g0) %>%
    fit(data_train_g0)
  
  lasso_fit_final_g1 <- 
    workflow() %>%
    add_model(lasso_spec_final_g1) %>%
    add_recipe(lasso_recipe_g1) %>% 
    fit(data_train_g1)
  

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### EXTRACT COEFFICIENTS ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # extract coefficients for treatment prediction
  lasso_coef_m <- tidy(lasso_fit_final_m) %>% as.data.frame()
  lasso_coef_m <- lasso_coef_m %>% filter(estimate > 0) %>% mutate(model = "m") %>% pull(term)
  
  # extract coefficients for outcome prediction
  lasso_coef_g0 <- tidy(lasso_fit_final_g0) %>% as.data.frame()
  lasso_coef_g0 <- lasso_coef_g0 %>% filter(estimate > 0) %>% mutate(model = "g0") %>% pull(term)
  
  lasso_coef_g1 <- tidy(lasso_fit_final_g1) %>% as.data.frame()
  lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% pull(term)
  
  lasso_coef_all <- union(union(lasso_coef_g0, lasso_coef_g1), lasso_coef_m)
  lasso_coef_all <- lasso_coef_all[!str_detect("(Intercept)", lasso_coef_all)]
  
  
  #%%%%%%%%%%%%%%%%%%%%%%#
  #### RUN POST-LASSO ####
  #%%%%%%%%%%%%%%%%%%%%%%#
  
  # NOTE: short version here yields exactly the same result as using the
  # tidymodels framework
  
  data_train_final <- data_train %>% select(all_of(outcome), all_of(treatment), all_of(lasso_coef_all))
  data_train_final_m <- data_train_final %>% select(-all_of(outcome))
  data_train_final_g0 <- data_train_final %>% filter(!!sym(treatment) == 0) %>% select(-all_of(treatment))
  data_train_final_g1 <- data_train_final %>% filter(!!sym(treatment) == 0) %>% select(-all_of(treatment))
  
  data_test_final_m <- data_test %>% select(all_of(treatment), all_of(lasso_coef_all))
  data_test_final_g <- data_test %>% select(all_of(outcome), all_of(lasso_coef_all))
  
  model_m <- glm(paste(treatment, "~ ."), family = binomial(link = "logit"), data = data_train_final_m)
  lasso_pred_m <- unname(predict(model_m, data_test_final_m, type = "response")) # return probability
  
  model_lm_0 <- lm(paste(outcome, "~ ."), data = data_train_final_g0)
  lasso_pred_g0 <- unname(predict(model_lm_0, data_test_final_g))
  
  model_lm_1 <- lm(paste(outcome, "~ ."), data = data_train_final_g1)
  lasso_pred_g1 <- unname(predict(model_lm_1, data_test_final_g))
  
  # create prediction data frame
  df_pred <- data.frame(
    # predictions
    "m" = lasso_pred_m, "g0" = lasso_pred_g0, "g1" = lasso_pred_g1,
    # true values
    "treatment" = data_test %>% pull(treatment), 
    "outcome" = data_test %>% pull(outcome),
    # number of predictors
    "num_pred_m" = ncol(data_train_final_m) - 1,
    "num_pred_g0" = ncol(data_train_final_g0) - 1,
    "num_pred_g1" = ncol(data_train_final_g1) - 1
  )
  
  # return data frame with predictions
  return(list("pred" = df_pred, "param" = df_best_param, "coef" = lasso_coef_all))
  
} # close function() 