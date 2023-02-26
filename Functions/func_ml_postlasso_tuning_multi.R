#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: PARAMETER TUNING FOR POST-LASSO IN MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function conducts the parameter tuning for post-lasso in the multivalued
# treatment setting. This is hand-coded as no built-in function for post-lasso
# exists (at least for the setup needed here).
#++++
# INPUT:
# -> "outcome": name of outcome variable included in data_train and data_test
# -> "treatment": name of treatment variable included in data_train and data_test
# -> "X_controls": vector containing control variables
# -> "data_train, data_train_g1, data_train_g2, data_train_g3": training data overall, training
# data for prediction of g0 and training data for prediction of g1
# -> "K": number of folds generated for parameter tuning
# -> "K_folds_inner_m1, K_folds_inner_m2, K_folds_inner_m3, 
# K_folds_inner_g1, K_folds_inner_g2, K_folds_inner_g3": sample splits
# -> "lambda": value of lambda used in tuning process
#++++
# OUTPUT:
# -> "df_tuning_aggr": data frame with AUC and RMSE for the nuisance parameter
# predictions for selected lambda.
#++++

func_ml_postlasso_tuning_multi <- function(
    outcome, treatment, X_controls, data_train, 
    data_train_g1, data_train_g2, data_train_g3, K,  
    K_folds_inner_m1, K_folds_inner_m2, K_folds_inner_m3,
    K_folds_inner_g1, K_folds_inner_g2, K_folds_inner_g3, lambda) {
  
  # generate empty data frame to store results
  df_tuning_fold_all <- data.frame()
  
  # iterate over folds
  for (fold_sel in 1:K) {
    
    # m(1) #
    #++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_m1 <- K_folds_inner_m1$splits[[fold_sel]]$in_id
    data_tuning_train_m1 <- data_train[indices_fold_sel_m1, ]
    data_tuning_test_m1 <- data_train[-indices_fold_sel_m1, ] 
    
    # specify the model
    lasso_spec_tuning_m1 <- 
      logistic_reg(penalty = {{lambda}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    # define recipe
    lasso_recipe_tuning_m1 <- 
      data_tuning_train_m1 %>%
      recipe(.) %>%
      # price variable is outcome
      update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
      # all other variables are predictors (drop outcome treatment)
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_tuning_m1 <- 
      workflow() %>%
      add_model(lasso_spec_tuning_m1) %>%
      add_recipe(lasso_recipe_tuning_m1)
    
    # fit the model
    lasso_fit_tuning_m1 <- 
      lasso_workflow_tuning_m1 %>%
      fit(data_tuning_train_m1)
    
    # extract coefficients
    lasso_coef_tuning_m1 <- tidy(lasso_fit_tuning_m1) %>% as.data.frame()
    lasso_coef_tuning_m1 <- lasso_coef_tuning_m1 %>% filter(estimate > 0) %>% pull(term)
    
    
    
    # m(2) #
    #++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_m2 <- K_folds_inner_m2$splits[[fold_sel]]$in_id
    data_tuning_train_m2 <- data_train[indices_fold_sel_m2, ]
    data_tuning_test_m2 <- data_train[-indices_fold_sel_m2, ] 
    
    # specify the model
    lasso_spec_tuning_m2 <- 
      logistic_reg(penalty = {{lambda}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    # define recipe
    lasso_recipe_tuning_m2 <- 
      data_tuning_train_m2 %>%
      recipe(.) %>%
      # price variable is outcome
      update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
      # all other variables are predictors (drop outcome treatment)
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_tuning_m2 <- 
      workflow() %>%
      add_model(lasso_spec_tuning_m2) %>%
      add_recipe(lasso_recipe_tuning_m2)
    
    # fit the model
    lasso_fit_tuning_m2 <- 
      lasso_workflow_tuning_m2 %>%
      fit(data_tuning_train_m2)
    
    # extract coefficients
    lasso_coef_tuning_m2 <- tidy(lasso_fit_tuning_m2) %>% as.data.frame()
    lasso_coef_tuning_m2 <- lasso_coef_tuning_m2 %>% filter(estimate > 0) %>% pull(term)
    
    
    
    # m(3) #
    #++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_m3 <- K_folds_inner_m3$splits[[fold_sel]]$in_id
    data_tuning_train_m3 <- data_train[indices_fold_sel_m3, ]
    data_tuning_test_m3 <- data_train[-indices_fold_sel_m3, ] 
    
    # specify the model
    lasso_spec_tuning_m3 <- 
      logistic_reg(penalty = {{lambda}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    # define recipe
    lasso_recipe_tuning_m3 <- 
      data_tuning_train_m3 %>%
      recipe(.) %>%
      # price variable is outcome
      update_role("treatment_sport_freq_never", new_role = "outcome") %>%
      # all other variables are predictors (drop outcome treatment)
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_tuning_m3 <- 
      workflow() %>%
      add_model(lasso_spec_tuning_m3) %>%
      add_recipe(lasso_recipe_tuning_m3)
    
    # fit the model
    lasso_fit_tuning_m3 <- 
      lasso_workflow_tuning_m3 %>%
      fit(data_tuning_train_m3)
    
    # extract coefficients
    lasso_coef_tuning_m3 <- tidy(lasso_fit_tuning_m3) %>% as.data.frame()
    lasso_coef_tuning_m3 <- lasso_coef_tuning_m3 %>% filter(estimate > 0) %>% pull(term)
    
    
    # g(1, X) #
    #+++++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_g1 <- K_folds_inner_g1$splits[[fold_sel]]$in_id
    data_tuning_train_g1 <- data_train_g1[indices_fold_sel_g1, ]
    data_tuning_test_g1 <- data_train_g1[-indices_fold_sel_g1, ]
    
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
    
    
    
    # g(2, X) #
    #+++++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_g2 <- K_folds_inner_g2$splits[[fold_sel]]$in_id
    data_tuning_train_g2 <- data_train_g2[indices_fold_sel_g2, ]
    data_tuning_test_g2 <- data_train_g2[-indices_fold_sel_g2, ]
    
    # specify the model
    lasso_spec_tuning_g2 <- 
      linear_reg(penalty = {{lambda}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")  
    
    # define recipe
    lasso_recipe_tuning_g2 <- 
      data_tuning_train_g2 %>%
      recipe(.) %>%
      # price variable is outcome
      update_role({{outcome}}, new_role = "outcome") %>%
      # all other variables are predictors (drop outcome treatment)
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_tuning_g2 <- 
      workflow() %>%
      add_model(lasso_spec_tuning_g2) %>%
      add_recipe(lasso_recipe_tuning_g2)
    
    # fit the model
    lasso_fit_tuning_g2 <- 
      lasso_workflow_tuning_g2 %>%
      fit(data_tuning_train_g2)
    
    # extract coefficients
    lasso_coef_tuning_g2 <- tidy(lasso_fit_tuning_g2) %>% as.data.frame()
    lasso_coef_tuning_g2 <- lasso_coef_tuning_g2 %>% filter(estimate > 0) %>% pull(term)
    
    
    # g(3, X) #
    #+++++++++#
    
    # extract training and test data for parameter tuning
    indices_fold_sel_g3 <- K_folds_inner_g3$splits[[fold_sel]]$in_id
    data_tuning_train_g3 <- data_train_g3[indices_fold_sel_g3, ]
    data_tuning_test_g3 <- data_train_g3[-indices_fold_sel_g3, ]
    
    # specify the model
    lasso_spec_tuning_g3 <- 
      linear_reg(penalty = {{lambda}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")  
    
    # define recipe
    lasso_recipe_tuning_g3 <- 
      data_tuning_train_g3 %>%
      recipe(.) %>%
      # price variable is outcome
      update_role({{outcome}}, new_role = "outcome") %>%
      # all other variables are predictors (drop outcome treatment)
      update_role(all_of(X_controls), new_role = "predictor")
    
    # generate workflow
    lasso_workflow_tuning_g3 <- 
      workflow() %>%
      add_model(lasso_spec_tuning_g3) %>%
      add_recipe(lasso_recipe_tuning_g3)
    
    # fit the model
    lasso_fit_tuning_g3 <- 
      lasso_workflow_tuning_g3 %>%
      fit(data_tuning_train_g3)
    
    # extract coefficients
    lasso_coef_tuning_g3 <- tidy(lasso_fit_tuning_g3) %>% as.data.frame()
    lasso_coef_tuning_g3 <- lasso_coef_tuning_g3 %>% filter(estimate > 0) %>% pull(term)
    
    
    # COEFFICIENTS #
    #++++++++++++++#
    
    # union of coefficients selected in treatment and both outcome equations
    coef_tuning_all <- purrr::reduce(list(lasso_coef_tuning_m1, lasso_coef_tuning_m2, lasso_coef_tuning_m3,
                                          lasso_coef_tuning_g2, lasso_coef_tuning_g1, lasso_coef_tuning_g3),union)  
    coef_tuning_all <- unique(coef_tuning_all) # no duplicates
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
    
    data_train_final_m1 <- data_tuning_train_m1 %>% select(treatment_sport_freq_weekly_atleast, all_of(coef_tuning_all))
    data_train_final_m2 <- data_tuning_train_m2 %>% select(treatment_sport_freq_monthly_less, all_of(coef_tuning_all))
    data_train_final_m3 <- data_tuning_train_m3 %>% select(treatment_sport_freq_never, all_of(coef_tuning_all))
    data_train_final_g1 <- data_tuning_train_g1 %>% filter(treatment_sport_freq == 1) %>% select(all_of(outcome), all_of(coef_tuning_all)) 
    data_train_final_g2 <-  data_tuning_train_g2 %>% filter(treatment_sport_freq == 2) %>% select(all_of(outcome), all_of(coef_tuning_all))
    data_train_final_g3 <-  data_tuning_train_g3 %>% filter(treatment_sport_freq == 3) %>% select(all_of(outcome), all_of(coef_tuning_all))
    
    data_test_final_m1 <- data_tuning_test_m1 %>% select(treatment_sport_freq_weekly_atleast, all_of(coef_tuning_all))
    data_test_final_m2 <- data_tuning_test_m2 %>% select(treatment_sport_freq_monthly_less, all_of(coef_tuning_all))
    data_test_final_m3 <- data_tuning_test_m3 %>% select(treatment_sport_freq_never, all_of(coef_tuning_all))
    data_test_final_g1 <- data_tuning_test_g1 %>% select(all_of(outcome), all_of(coef_tuning_all))
    data_test_final_g2 <- data_tuning_test_g2 %>% select(all_of(outcome), all_of(coef_tuning_all))
    data_test_final_g3 <- data_tuning_test_g3 %>% select(all_of(outcome), all_of(coef_tuning_all))
    
    model_m1 <- glm(paste("treatment_sport_freq_weekly_atleast", "~ ."), family = binomial(link = "logit"), data = data_train_final_m1)
    lasso_pred_m1 <- unname(predict(model_m1, data_test_final_m1, type = "response")) # return probability
    
    model_m2 <- glm(paste("treatment_sport_freq_monthly_less", "~ ."), family = binomial(link = "logit"), data = data_train_final_m2)
    lasso_pred_m2 <- unname(predict(model_m2, data_test_final_m2, type = "response")) # return probability
    
    model_m3 <- glm(paste("treatment_sport_freq_never", "~ ."), family = binomial(link = "logit"), data = data_train_final_m3)
    lasso_pred_m3 <- unname(predict(model_m3, data_test_final_m3, type = "response")) # return probability
    
    model_lm_g1 <- lm(paste(outcome, "~ ."), data = data_train_final_g1)
    lasso_pred_g1 <- unname(predict(model_lm_g1, data_test_final_g1))
    
    model_lm_g2 <- lm(paste(outcome, "~ ."), data = data_train_final_g2)
    lasso_pred_g2 <- unname(predict(model_lm_g2, data_test_final_g2))
    
    model_lm_g3 <- lm(paste(outcome, "~ ."), data = data_train_final_g3)
    lasso_pred_g3 <- unname(predict(model_lm_g3, data_test_final_g3))
    
    
    df_pred_tuning_m1 <- data.frame(
      "pred" = 1 - lasso_pred_m1, 
      "true" = data_tuning_test_m1 %>% 
        mutate(treatment_sport_freq = as.factor(ifelse(treatment_sport_freq == 1, 1, 0))) %>% 
        pull(treatment_sport_freq)
    )
    
    df_pred_tuning_m2 <- data.frame(
      "pred" = 1 - lasso_pred_m2, 
      "true" = data_tuning_test_m2 %>% 
        mutate(treatment_sport_freq = as.factor(ifelse(treatment_sport_freq == 2, 1, 0))) %>% 
        pull(treatment_sport_freq)
    )
    
    
    df_pred_tuning_m3 <- data.frame(
      "pred" = 1 - lasso_pred_m3, 
      "true" = data_tuning_test_m3 %>% 
        mutate(treatment_sport_freq = as.factor(ifelse(treatment_sport_freq == 3, 1, 0))) %>% 
        pull(treatment_sport_freq)
    )
    
    df_pred_tuning_g1 <- data.frame(
      "pred" = as.numeric(lasso_pred_g1), "true" = data_tuning_test_g1 %>% pull(outcome)
    )
    
    df_pred_tuning_g2 <- data.frame(
      "pred" = lasso_pred_g2, "true" = data_tuning_test_g2 %>% pull(outcome)
    )
    
    df_pred_tuning_g3 <- data.frame(
      "pred" = lasso_pred_g3, "true" = data_tuning_test_g3 %>% pull(outcome)
    )
    
    
    
    # Error Metrics #
    #+++++++++++++++#
    
    auc_tuning_m1 <- 
      yardstick::roc_auc(data = df_pred_tuning_m1, truth = true, estimate = pred) %>% 
      select(.estimate) %>% pull() 
    
    auc_tuning_m2 <- 
      yardstick::roc_auc(data = df_pred_tuning_m2, truth = true, estimate = pred) %>% 
      select(.estimate) %>% pull() 
    
    auc_tuning_m3 <- 
      yardstick::roc_auc(data = df_pred_tuning_m3, truth = true, estimate = pred) %>% 
      select(.estimate) %>% pull() 
    
    
    rmse_1_tuning <-
      yardstick::rmse(data = df_pred_tuning_g1, truth = true, estimate = pred) %>%
      select(.estimate) %>% pull() 
    
    rmse_2_tuning <-
      yardstick::rmse(data = df_pred_tuning_g2, truth = true, estimate = pred) %>%
      select(.estimate) %>% pull() 
    
    rmse_3_tuning <-
      yardstick::rmse(data = df_pred_tuning_g3, truth = true, estimate = pred) %>%
      select(.estimate) %>% pull() 
    
    
    df_tuning_fold <- 
      data.frame(fold = fold_sel, 
                 AUC_1 = auc_tuning_m1, AUC_2 = auc_tuning_m2, AUC_3 = auc_tuning_m3, 
                 RMSE_1 = rmse_1_tuning, RMSE_2 = rmse_2_tuning, RMSE_3 = rmse_3_tuning,
                 lambda = lambda)
    df_tuning_fold_all <- rbind(df_tuning_fold_all, df_tuning_fold)
    
  } # close iteration over folds
  
  # aggregate results over fold
  df_tuning_aggr <- df_tuning_fold_all %>%
    summarize(AUC_1 = mean(AUC_1), AUC_2 = mean(AUC_2), AUC_3 = mean(AUC_3), 
              RMSE_1 = mean(RMSE_1), RMSE_2 = mean(RMSE_2), RMSE_3 = mean(RMSE_3)) %>%
    mutate(lambda = lambda)
  
  return(df_tuning_aggr)
  
}