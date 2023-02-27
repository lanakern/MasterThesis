#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH POST-LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses post-lasso to predict the nuisance parameters. 
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
# -> "coef": non-zero coefficients
#++++


func_ml_postlasso <- function(treatment_setting, data_train, data_test, outcome, treatment, group, K, lambda_val) {
  
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
  
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Parameter Tuning ####
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
    
  
    # parameter tuning
    list_param_tuning <- 
      # change lambda while keeping everything else constant
      sapply(lasso_grid$penalty, func_ml_postlasso_tuning_binary, 
             outcome = outcome, treatment = treatment, X_controls = X_controls, 
             data_train = data_train, data_train_g0 = data_train_g0, data_train_g1 = data_train_g1, 
             K = K, K_folds_inner_m = K_folds_inner_m, K_folds_inner_g0 = K_folds_inner_g0, 
             K_folds_inner_g1 = K_folds_inner_g1)
    
    df_tuning_all_list <- as.data.frame(t(list_param_tuning))
    df_tuning_all <- data.frame(
      "AUC" = unlist(df_tuning_all_list$AUC), 
      "RMSE_0" = unlist(df_tuning_all_list$RMSE_0),
      "RMSE_1" = unlist(df_tuning_all_list$RMSE_1),
      "lambda" = unlist(df_tuning_all_list$lambda)
    )
  
    # select lambda that yields highest AUC
    lasso_best_param_m <- df_tuning_all %>% filter(AUC == max(AUC)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_g0 <- df_tuning_all %>% filter(RMSE_0 == min(RMSE_0)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_g1 <- df_tuning_all %>% filter(RMSE_1 == min(RMSE_1)) %>% tail(1) %>% pull(lambda)
  
    df_best_param <- data.frame(
      "m" = lasso_best_param_m, "g0" = lasso_best_param_g0, "g1" = lasso_best_param_g1
    )
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Final Model Training ####
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
    #### Extract Coefficients ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # extract coefficients for treatment prediction
    lasso_coef_m <- tidy(lasso_fit_final_m) %>% as.data.frame()
    lasso_coef_m <- lasso_coef_m %>% filter(estimate > 0) %>% mutate(model = "m") %>% select(-penalty)
    
    # extract coefficients for outcome prediction
    lasso_coef_g0 <- tidy(lasso_fit_final_g0) %>% as.data.frame()
    lasso_coef_g0 <- lasso_coef_g0 %>% filter(estimate > 0) %>% mutate(model = "g0") %>% select(-penalty)
    
    lasso_coef_g1 <- tidy(lasso_fit_final_g1) %>% as.data.frame()
    lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    
    lasso_coef_all <- union(union(lasso_coef_g0$term, lasso_coef_g1$term), lasso_coef_m$term)
    lasso_coef_all <- lasso_coef_all[!str_detect("(Intercept)", lasso_coef_all)]
    
    # append extracted coefficients to final data frame
    lasso_coef_save <- lasso_coef_m
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_g0)
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_g1)
    
    
    #%%%%%%%%%%%%%%%%%%%%%%#
    #### Run Post-Lasso ####
    #%%%%%%%%%%%%%%%%%%%%%%#
    
    # NOTE: short version here yields exactly the same result as using the
    # tidymodels framework
    
    data_train_final <- data_train %>% select(all_of(outcome), all_of(treatment), all_of(lasso_coef_all))
    data_train_final_m <- data_train_final %>% select(-all_of(outcome))
    data_train_final_g0 <- data_train_final %>% filter(treatment_sport == 0) %>% select(-all_of(treatment))
    data_train_final_g1 <- data_train_final %>% filter(treatment_sport == 1) %>% select(-all_of(treatment))
    
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
    return(list("pred" = df_pred, "param" = df_best_param, "coef" = lasso_coef_save))
  
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
    

    #%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Parameter Tuning ####
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # parameter tuning via k-fold CV
    # this means that training data is again partitioned into K-folds for each
    # ML model
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
    
    
    # apply tuning
    list_param_tuning <- 
      # change lambda while keeping everything else constant
      sapply(lasso_grid$penalty, func_ml_postlasso_tuning_multi, 
             outcome = outcome, treatment = treatment, X_controls = X_controls, 
             data_train = data_train, data_train_g1 = data_train_g1, 
             data_train_g2 = data_train_g2, data_train_g3 = data_train_g3, 
             K = K, K_folds_inner_m1 = K_folds_inner_m1, K_folds_inner_m2 = K_folds_inner_m2,
             K_folds_inner_m3 = K_folds_inner_m3, K_folds_inner_g1 = K_folds_inner_g1,
             K_folds_inner_g2 = K_folds_inner_g2, K_folds_inner_g3 = K_folds_inner_g3
             )

    df_tuning_all_list <- as.data.frame(t(list_param_tuning))
    df_tuning_all <- data.frame(
      "AUC_1" = unlist(df_tuning_all_list$AUC_1), "AUC_2" = unlist(df_tuning_all_list$AUC_2),
      "AUC_3" = unlist(df_tuning_all_list$AUC_3),
      "RMSE_1" = unlist(df_tuning_all_list$RMSE_1), "RMSE_2" = unlist(df_tuning_all_list$RMSE_2),
      "RMSE_3" = unlist(df_tuning_all_list$RMSE_3),
      "lambda" = unlist(df_tuning_all_list$lambda)
    )
    
    # select lambda that yields highest AUC
    lasso_best_param_m1 <- df_tuning_all %>% filter(AUC_1 == max(AUC_1)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_m2 <- df_tuning_all %>% filter(AUC_2 == max(AUC_2)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_m3 <- df_tuning_all %>% filter(AUC_3 == max(AUC_3)) %>% tail(1) %>% pull(lambda)
    
    lasso_best_param_g1 <- df_tuning_all %>% filter(RMSE_1 == min(RMSE_1)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_g2 <- df_tuning_all %>% filter(RMSE_2 == min(RMSE_2)) %>% tail(1) %>% pull(lambda)
    lasso_best_param_g3 <- df_tuning_all %>% filter(RMSE_3 == min(RMSE_3)) %>% tail(1) %>% pull(lambda)
    
    df_best_param <- data.frame(
      "m1" = lasso_best_param_m1, "m2" = lasso_best_param_m2, "m3" = lasso_best_param_m3,
      "g1" = lasso_best_param_g1, "g2" = lasso_best_param_g2, "g3" = lasso_best_param_g3
    )
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Final Model Training ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # specify the models
    
    # m(1)
    lasso_best_param_m1 <- df_best_param$m1
    lasso_spec_final_m1 <- 
      logistic_reg(penalty = {{lasso_best_param_m1}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    
    # m(2)
    lasso_best_param_m2 <- df_best_param$m2
    lasso_spec_final_m2 <- 
      logistic_reg(penalty = {{lasso_best_param_m2}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification")
    
    # m(3)
    lasso_best_param_m3 <- df_best_param$m3
    lasso_spec_final_m3 <- 
      logistic_reg(penalty = {{lasso_best_param_m3}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("classification") 
    
    # g(1, X) = E(Y | D = 1, X): prediction of outcome for treated individuals
    lasso_best_param_g1 <- df_best_param$g1
    lasso_spec_final_g1 <- 
      linear_reg(penalty = {{lasso_best_param_g1}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")
    
    # g(2, X)
    lasso_best_param_g2 <- df_best_param$g2
    lasso_spec_final_g2 <- 
      linear_reg(penalty = {{lasso_best_param_g2}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")
    
    # g(3, X)
    lasso_best_param_g3 <- df_best_param$g3
    lasso_spec_final_g3 <- 
      linear_reg(penalty = {{lasso_best_param_g3}}, mixture = 1) %>%  
      set_engine("glmnet") %>%  
      set_mode("regression")
    
    
    # generate workflow and fit model
    lasso_fit_final_m1 <- 
      workflow() %>%
      add_model(lasso_spec_final_m1) %>%
      add_recipe(lasso_recipe_m1) %>%
      fit(data_train)
    
    lasso_fit_final_m2 <- 
      workflow() %>%
      add_model(lasso_spec_final_m2) %>%
      add_recipe(lasso_recipe_m2) %>%
      fit(data_train)
    
    lasso_fit_final_m3 <- 
      workflow() %>%
      add_model(lasso_spec_final_m3) %>%
      add_recipe(lasso_recipe_m3) %>%
      fit(data_train)
    
    lasso_fit_final_g1 <- 
      workflow() %>%
      add_model(lasso_spec_final_g1) %>%
      add_recipe(lasso_recipe_g1) %>% 
      fit(data_train_g1)
    
    lasso_fit_final_g2 <- 
      workflow() %>%
      add_model(lasso_spec_final_g2) %>%
      add_recipe(lasso_recipe_g2) %>% 
      fit(data_train_g2)
    
    lasso_fit_final_g3 <- 
      workflow() %>%
      add_model(lasso_spec_final_g3) %>%
      add_recipe(lasso_recipe_g3) %>% 
      fit(data_train_g3)
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Extract Coefficients ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # extract coefficients for treatment prediction
    lasso_coef_m1 <- tidy(lasso_fit_final_m1) %>% as.data.frame()
    lasso_coef_m1 <- lasso_coef_m1 %>% filter(estimate > 0) %>% mutate(model = "m1") %>% select(-penalty)
    
    lasso_coef_m2 <- tidy(lasso_fit_final_m2) %>% as.data.frame()
    lasso_coef_m2 <- lasso_coef_m2 %>% filter(estimate > 0) %>% mutate(model = "m2") %>% select(-penalty)
    
    lasso_coef_m3 <- tidy(lasso_fit_final_m3) %>% as.data.frame()
    lasso_coef_m3 <- lasso_coef_m3 %>% filter(estimate > 0) %>% mutate(model = "m3") %>% select(-penalty)
    
    # extract coefficients for outcome prediction
    lasso_coef_g1 <- tidy(lasso_fit_final_g1) %>% as.data.frame()
    lasso_coef_g1 <- lasso_coef_g1 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    lasso_coef_g2 <- tidy(lasso_fit_final_g2) %>% as.data.frame()
    lasso_coef_g2 <- lasso_coef_g2 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    lasso_coef_g3 <- tidy(lasso_fit_final_g3) %>% as.data.frame()
    lasso_coef_g3 <- lasso_coef_g3 %>% filter(estimate > 0) %>% mutate(model = "g1") %>% select(-penalty)
    
    
    # union of coefficients
    lasso_coef_all <- purrr::reduce(list(lasso_coef_m1, lasso_coef_m2, lasso_coef_m3,
                                          lasso_coef_g1, lasso_coef_g2, lasso_coef_g3),union)
    lasso_coef_all <- unique(lasso_coef_all$term)
    lasso_coef_all <- lasso_coef_all[!str_detect("(Intercept)", lasso_coef_all)]
    
    
    # append extracted coefficients to final data frame
    lasso_coef_save <- lasso_coef_m1
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_m2)
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_m3)
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_g1)
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_g2)
    lasso_coef_save <- rbind(lasso_coef_save, lasso_coef_g3)
    
    
    #%%%%%%%%%%%%%%%%%%%%%%#
    #### Run Post-Lasso ####
    #%%%%%%%%%%%%%%%%%%%%%%#
    
    # NOTE: short version here yields exactly the same result as using the
    # tidymodels framework
    
    data_train_final_m1 <- data_train %>% select(treatment_sport_freq_weekly_atleast, all_of(lasso_coef_all))
    data_train_final_m2 <- data_train %>% select(treatment_sport_freq_monthly_less, all_of(lasso_coef_all))
    data_train_final_m3 <- data_train %>% select(treatment_sport_freq_never, all_of(lasso_coef_all))
    data_train_final_g1 <- data_train %>% filter(treatment_sport_freq == 1) %>% select(all_of(outcome), all_of(lasso_coef_all))
    data_train_final_g2 <- data_train %>% filter(treatment_sport_freq == 2) %>% select(all_of(outcome), all_of(lasso_coef_all))
    data_train_final_g3 <- data_train %>% filter(treatment_sport_freq == 3) %>% select(all_of(outcome), all_of(lasso_coef_all))
    
    data_test_final_m1 <- data_test %>% select(treatment_sport_freq_weekly_atleast, all_of(lasso_coef_all))
    data_test_final_m2 <- data_test %>% select(treatment_sport_freq_monthly_less, all_of(lasso_coef_all))
    data_test_final_m3 <- data_test %>% select(treatment_sport_freq_never, all_of(lasso_coef_all))
    data_test_final_g <- data_test %>% select(all_of(outcome), all_of(lasso_coef_all))

    
    model_m1 <- glm(paste("treatment_sport_freq_weekly_atleast", "~ ."), family = binomial(link = "logit"), data = data_train_final_m1)
    lasso_pred_m1 <- unname(predict(model_m1, data_test_final_m1, type = "response")) # return probability
    
    model_m2 <- glm(paste("treatment_sport_freq_monthly_less", "~ ."), family = binomial(link = "logit"), data = data_train_final_m2)
    lasso_pred_m2 <- unname(predict(model_m2, data_test_final_m2, type = "response")) # return probability
    
    model_m3 <- glm(paste("treatment_sport_freq_never", "~ ."), family = binomial(link = "logit"), data = data_train_final_m3)
    lasso_pred_m3 <- unname(predict(model_m3, data_test_final_m3, type = "response")) # return probability
    
    model_lm_1 <- lm(paste(outcome, "~ ."), data = data_train_final_g1)
    lasso_pred_g1 <- unname(predict(model_lm_1, data_test_final_g))
    
    model_lm_2 <- lm(paste(outcome, "~ ."), data = data_train_final_g2)
    lasso_pred_g2 <- unname(predict(model_lm_2, data_test_final_g))
    
    model_lm_3 <- lm(paste(outcome, "~ ."), data = data_train_final_g3)
    lasso_pred_g3 <- unname(predict(model_lm_3, data_test_final_g))
    
    # create prediction data frame
    df_pred <- data.frame(
      # predictions
      "m1" = lasso_pred_m1, "m2" = lasso_pred_m2, "m3" = lasso_pred_m3, 
      "g1" = lasso_pred_g1, "g2" = lasso_pred_g2, "g3" = lasso_pred_g3,
      # true values
      "treatment" = data_test %>% pull(treatment), 
      "outcome" = data_test %>% pull(outcome),
      # number of predictors: union is used
      "num_pred_m1" = length(lasso_coef_all),
      "num_pred_m2" = length(lasso_coef_all),
      "num_pred_m3" = length(lasso_coef_all),
      "num_pred_g1" = length(lasso_coef_all),
      "num_pred_g2" = length(lasso_coef_all),
      "num_pred_g3" = length(lasso_coef_all)
    )
    
    # return data frame with predictions
    return(list("pred" = df_pred, "param" = df_best_param, "coef" = lasso_coef_save))
    
  }
  
} # close function() 