func_ml_postlasso_tuning <- function(outcome, treatment, X_controls, 
                                     data_train, data_train_g0, data_train_g1, 
                                     K,  K_folds_inner_m, K_folds_inner_g0, K_folds_inner_g1, 
                                     lambda) {
  
  # generate empty data frame to store results
  df_tuning_fold_all <- data.frame()
  
  # iterate over folds
  for (fold_sel in 1:K) {
    
    
    # m(X) #
    #++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_m <- K_folds_inner_m$splits[[fold_sel]]$in_id
    data_tuning_train_m <- data_train[indices_fold_sel_m, ]
    data_tuning_test <- data_train[-indices_fold_sel_m, ] # same for m(X) and g(X)
    
    # specify the model
    lasso_spec_tuning_m <- 
      logistic_reg(penalty = {{lambda}}, mixture = 1) %>%  
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
      linear_reg(penalty = {{lambda}}, mixture = 1) %>%  
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
      linear_reg(penalty = {{lambda}}, mixture = 1) %>%  
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
    # SAME RESULT
    # lasso_grid_search_m <- 
    #   lasso_workflow_m %>%
    #   tune_grid(resamples = K_folds_inner_m, grid = lasso_grid, metrics = metric_set(roc_auc))
    # lasso_best_param_m <- lasso_grid_search_m %>% select_best("roc_auc")
    # lasso_best_param_m <- lasso_best_param_m$penalty
    # 
    
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
    mutate(lambda = lambda)
  
  return(df_tuning_aggr)
}