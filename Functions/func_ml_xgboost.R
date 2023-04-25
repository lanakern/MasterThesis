#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: MACHINE LEARNING PREDICTION WITH XGBOOST ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function uses xgboost to predict the nuisance parameters m(D) and g(D, X).
# This function can be applied in the binary and multivalued treatment setting.
# For the multivalued treatment setting one can distinguish between separate
# logistic regression for m(D) or one multinominal logistic regression.
#++++
# INPUT:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> "data_train": training data containing outcome, treatment, and all confounding factors
# -> "data_test": test data containing same variables as data_train
# -> "outcome": name of outcome variable included in data_train and data_test. Must start with "outcome_".
# -> "treatment": name of treatment variable included in data_train and data_test. Must start with "treatment_".
# -> "group": group variable included in data_train and data_test
# -> "K": number of folds generated for parameter tuning. Must be 2 or larger.
# -> "xgb_grid": hyperparameter grid consisting of tree_depth, trees, learn_rate, mtry, min_n
# All those parameters are tuned within this function. 
  # - "learn_rate": hyperparameter that is used to prevent overfitting by 
  # making the boosting process more conservative.  Low values makes the model more robust 
  # to overfitting but slower to compute. 0 < learn_rate < 1; default: 0.3.
  # - "trees": number of tress in the forest. Default: 15
  # - "tree_depth": higher values make the model more likely to overfit and complex. Default: 6. 
  # - "mtry": randomly selected predictors at each split. Default: p (all predictors)
  # - "min_n": minimal size of terminal node. Default: 1
  # Source: https://parsnip.tidymodels.org/reference/details_boost_tree_xgboost.html
# -> "probscore_separate": by default TRUE, i.e., propensity scores are estimated separately
# per treatment status (otherwise multinominal logistic regression)
# -> "probscore_normalize": by default TRUE, propensity score estimates are normalized
# to sum up to one within an individual (otherwise not normalized)
#++++
# OUTPUT:
# -> "pred": data frame with nuisance parameter predictions and true values
# -> "param": data frame including the hyperparameter values that are used for
# the final model training from which the predictions in "pred" result. 
#++++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_ml_xgboost <- function(treatment_setting, data_train, data_test, outcome, treatment, group, K, xgb_grid,
                            probscore_separate = TRUE, probscore_normalize = TRUE, hyperparam_sel) {
  
  # check inputs
  if (!treatment_setting %in% c("binary", "multi")) {
    stop("Treatment setting: binary or multi")
  }
  
  if (K < 2) {
    stop("For xgboost parameter tuning, K needs to be larger than 2.")
  }
  
  if (nrow(xgb_grid) == 0) {
    stop("Please specifcy xgboost parameter grid.")
  }
  
  if (!colnames(xgb_grid) %in% c("learn_rate", "trees", "tree_depth", "mtry", "min_n")) {
    stop("XGBoost parameter grid needs to include the following columsn: 
    'learn_rate', 'trees', 'tree_depth', 'mtry', 'min_n'")
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
    # treatment is predicted via binary classification and outcome via regression
    xgb_spec_m <- 
      boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                 mtry = tune(), min_n = tune()) %>% 
      set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
      set_mode("classification")
    
    xgb_spec_g <- 
      boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                 mtry = tune(), min_n = tune()) %>% 
      set_engine("xgboost") %>% 
      set_mode("regression")
    
    
    # generate recipe: define outcome and predictors
    ## confounding factors / predictors: all variables except treatment, outcome, and group
    X_controls <- data_train %>% 
      dplyr::select(-c(all_of(outcome), all_of(treatment), all_of(group))) %>% colnames()
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
    #### Parameter Tuning ####
    #%%%%%%%%%%%%%%%%%%%%%%%%#
    
    # parameter tuning via K-fold CV
    # this means that training data is again partitioned into K folds
    K_folds_inner_m <- rsample::group_vfold_cv(
      data = data_train %>%
        group_by(group) %>% 
        mutate(treatment_fold = mean(!!rlang::sym(treatment))) %>%
        ungroup(), 
      v = K, group = group, strata = treatment_fold, balance = "observations"
    )
    K_folds_inner_g0 <- rsample::group_vfold_cv(
      data = data_train_g0 %>%
        group_by(group) %>% 
        mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
        ungroup(),  
      v = K, group = group, strata = outcome_fold, balance = "observations"
    )
    K_folds_inner_g1 <- rsample::group_vfold_cv(
      data = data_train_g1 %>%
        group_by(group) %>% 
        mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
        ungroup(),  
      v = K, group = group, strata = outcome_fold, balance = "observations"
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
    
    
    ## SELECT HYPERPARAMETERS FOR BEST COMBINATION ##
    if (hyperparam_sel == "best") {
      xgb_best_param_m <- xgb_grid_search_m %>% select_best("roc_auc")
      xgb_best_param_g0 <- xgb_grid_search_g0 %>% select_best("rmse")
      xgb_best_param_g1 <- xgb_grid_search_g1 %>% select_best("rmse")
      
      
      df_best_param <- data.frame(
        "m_learn_rate" = xgb_best_param_m$learn_rate, "m_trees" = xgb_best_param_m$trees,
        "m_tree_depth" = xgb_best_param_m$tree_depth, "m_mtry" = xgb_best_param_m$mtry,
        "m_min_n" = xgb_best_param_m$min_n,
        "g0_learn_rate" = xgb_best_param_g0$learn_rate, "g0_trees" = xgb_best_param_g0$trees,
        "g0_tree_depth" = xgb_best_param_g0$tree_depth, "g0_mtry" = xgb_best_param_g0$mtry,
        "g0_min_n" = xgb_best_param_g0$min_n,
        "g1_learn_rate" = xgb_best_param_g1$learn_rate, "g1_trees" = xgb_best_param_g1$trees,
        "g1_tree_depth" = xgb_best_param_g1$tree_depth, "g1_mtry" = xgb_best_param_g1$mtry,
        "g1_min_n" = xgb_best_param_g1$min_n
      )
      ## Apply 1SE Rule ##
    } else if (hyperparam_sel == "1SE") {
      
      ## Treatment ##
      #+++++++++++++#
      
      # append results from error metrics
      m_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        m_error_metrics_all <- rbind(m_error_metrics_all, xgb_grid_search_m$.metrics[[K_sel]])
      }
      
      # aggregate across K
      m_error_metrics_all <- m_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        AUC = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      m_error_metrics_all <- m_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_m <- m_error_metrics_all %>% filter(AUC == max(AUC)) %>% head(1)
      
      # calculate standard error of best lambda
      xgb_best_m_se <- xgb_best_m %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      m_error_metrics_all_simpler <- m_error_metrics_all %>% filter(trees <= xgb_best_m$trees | tree_depth <= xgb_best_m$tree_depth)
      xgb_se_m <- m_error_metrics_all_simpler %>%
        filter(AUC > max(m_error_metrics_all %>% pull(AUC)) - xgb_best_m_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        head(1) 
      
      
      ## Outcome D = 0 ##
      #+++++++++++++++++#
      
      # append results from error metrics
      g0_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        g0_error_metrics_all <- rbind(g0_error_metrics_all, xgb_grid_search_g0$.metrics[[K_sel]])
      }
      
      # aggregate across K
      g0_error_metrics_all <- g0_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        RMSE = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      g0_error_metrics_all <- g0_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_g0 <- g0_error_metrics_all %>% filter(RMSE == min(RMSE)) 
      
      # calculate standard error of best lambda
      xgb_best_g0_se <- xgb_best_g0 %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      g0_error_metrics_all_simpler <- g0_error_metrics_all %>% filter(trees <= xgb_best_g0$trees | tree_depth <= xgb_best_g0$tree_depth)
      xgb_se_g0 <- g0_error_metrics_all_simpler %>%
        filter(RMSE < min(g0_error_metrics_all %>% pull(RMSE)) + xgb_best_g0_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        head(1) 
      
      
      ## Outcome D = 1 ##
      #+++++++++++++++++#
      
      g1_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        g1_error_metrics_all <- rbind(g1_error_metrics_all, xgb_grid_search_g1$.metrics[[K_sel]])
      }
      
      # aggregate across K
      g1_error_metrics_all <- g1_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        RMSE = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      g1_error_metrics_all <- g1_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_g1 <- g1_error_metrics_all %>% filter(RMSE == min(RMSE)) 
      
      # calculate standard error of best lambda
      xgb_best_g1_se <- xgb_best_g1 %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      g1_error_metrics_all_simpler <- g1_error_metrics_all %>% filter(trees <= xgb_best_g0$trees | tree_depth <= xgb_best_g0$tree_depth)
      xgb_se_g1 <- g1_error_metrics_all_simpler %>%
        filter(RMSE < min(g1_error_metrics_all %>% pull(RMSE)) + xgb_best_g1_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        head(1) 
      
      
      df_best_param <- data.frame(
        "m_learn_rate" = xgb_se_m$learn_rate, "m_trees" = xgb_se_m$trees,
        "m_tree_depth" = xgb_se_m$tree_depth, "m_mtry" = xgb_se_m$mtry,
        "m_min_n" = xgb_se_m$min_n,
        "g0_learn_rate" = xgb_se_g0$learn_rate, "g0_trees" = xgb_se_g0$trees,
        "g0_tree_depth" = xgb_se_g0$tree_depth, "g0_mtry" = xgb_se_g0$mtry,
        "g0_min_n" = xgb_se_g0$min_n,
        "g1_learn_rate" = xgb_se_g1$learn_rate, "g1_trees" = xgb_se_g1$trees,
        "g1_tree_depth" = xgb_se_g1$tree_depth, "g1_mtry" = xgb_se_g1$mtry,
        "g1_min_n" = xgb_se_g1$min_n
      )
      
      
      ## Apply 1SE Rule ##
      #++++++++++++++++++#
      
    } else if (hyperparam_sel == "1SE_plus") {
      
      ## Treatment ##
      #+++++++++++++#
      
      # append results from error metrics
      m_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        m_error_metrics_all <- rbind(m_error_metrics_all, xgb_grid_search_m$.metrics[[K_sel]])
      }
      
      # aggregate across K
      m_error_metrics_all <- m_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        AUC = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      m_error_metrics_all <- m_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_m <- m_error_metrics_all %>% filter(AUC == max(AUC)) %>% head(1)
      
      # calculate standard error of best lambda
      xgb_best_m_se <- xgb_best_m %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      m_error_metrics_all_complexer <- m_error_metrics_all %>% filter(trees >= xgb_best_m$trees | tree_depth >= xgb_best_m$tree_depth)
      xgb_se_m <- m_error_metrics_all_complexer %>%
        filter(AUC > max(m_error_metrics_all %>% pull(AUC)) - xgb_best_m_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        tail(1) 
      
      
      ## Outcome D = 0 ##
      #+++++++++++++++++#
      
      # append results from error metrics
      g0_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        g0_error_metrics_all <- rbind(g0_error_metrics_all, xgb_grid_search_g0$.metrics[[K_sel]])
      }
      
      # aggregate across K
      g0_error_metrics_all <- g0_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        RMSE = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      g0_error_metrics_all <- g0_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_g0 <- g0_error_metrics_all %>% filter(RMSE == min(RMSE)) 
      
      # calculate standard error of best lambda
      xgb_best_g0_se <- xgb_best_g0 %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      g0_error_metrics_all_complexer <- g0_error_metrics_all %>% filter(trees >= xgb_best_g0$trees | tree_depth >= xgb_best_g0$tree_depth)
      xgb_se_g0 <- g0_error_metrics_all_complexer %>%
        filter(RMSE < min(g0_error_metrics_all %>% pull(RMSE)) + xgb_best_g0_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        tail(1) 
      
      
      ## Outcome D = 1 ##
      #+++++++++++++++++#
      
      g1_error_metrics_all <- data.frame()
      for (K_sel in 1:K) {
        g1_error_metrics_all <- rbind(g1_error_metrics_all, xgb_grid_search_g1$.metrics[[K_sel]])
      }
      
      # aggregate across K
      g1_error_metrics_all <- g1_error_metrics_all %>% group_by(mtry, trees, min_n, tree_depth, learn_rate) %>% summarize(
        # average AUC
        RMSE = mean(`.estimate`), 
        # standard error: standard deviation divided by the number of folds
        se = sd(`.estimate`) / sqrt(K)
      ) %>% ungroup()
      g1_error_metrics_all <- g1_error_metrics_all %>% arrange(trees, tree_depth)
      
      # best: lambda which leads to best error metrics
      xgb_best_g1 <- g1_error_metrics_all %>% filter(RMSE == min(RMSE)) 
      
      # calculate standard error of best lambda
      xgb_best_g1_se <- xgb_best_g1 %>% pull(se) 
      
      # one-standard deviation rule: "largest value of lambda (-> simpler model) such that error is 
      # within 1 standard error of the cross-validated errors for best lambda".
      g1_error_metrics_all_complexer <- g1_error_metrics_all %>% filter(trees >= xgb_best_g0$trees | tree_depth >= xgb_best_g0$tree_depth)
      xgb_se_g1 <- g1_error_metrics_all_complexer %>%
        filter(RMSE < min(g1_error_metrics_all %>% pull(RMSE)) + xgb_best_g1_se) %>%
        arrange(trees, tree_depth, learn_rate, mtry, min_n) %>%
        tail(1) 
      
      
      df_best_param <- data.frame(
        "m_learn_rate" = xgb_se_m$learn_rate, "m_trees" = xgb_se_m$trees,
        "m_tree_depth" = xgb_se_m$tree_depth, "m_mtry" = xgb_se_m$mtry,
        "m_min_n" = xgb_se_m$min_n,
        "g0_learn_rate" = xgb_se_g0$learn_rate, "g0_trees" = xgb_se_g0$trees,
        "g0_tree_depth" = xgb_se_g0$tree_depth, "g0_mtry" = xgb_se_g0$mtry,
        "g0_min_n" = xgb_se_g0$min_n,
        "g1_learn_rate" = xgb_se_g1$learn_rate, "g1_trees" = xgb_se_g1$trees,
        "g1_tree_depth" = xgb_se_g1$tree_depth, "g1_mtry" = xgb_se_g1$mtry,
        "g1_min_n" = xgb_se_g1$min_n
      )
    }

    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### Final Model Training ####
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
      set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
      set_mode("classification")
    
    ## model for g(0,X) = E(Y | D = 0, X): prediction of outcome for untreated individuals
    tree_depth_g0 <- df_best_param$g0_tree_depth
    trees_g0 <- df_best_param$g0_trees
    learn_rate_g0 <- df_best_param$g0_learn_rate
    mtry_g0 <- df_best_param$g0_mtry
    min_n_g0 <- df_best_param$g0_min_n
    
    
    xgb_spec_final_g0 <- 
      boost_tree(tree_depth = {{tree_depth_g0}}, trees = {{trees_g0}}, 
                 learn_rate = {{learn_rate_g0}}, mtry = {{mtry_g0}}, min_n = {{min_n_g0}}
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
    #### Predictions ####
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
    
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### MULTIVALUED TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    
    #### TREATMENT: BINARY LOGISTIC REGRESSION ####
    #+++++++++++++++++++++++++++++++++++++++++++++#
    
    if (probscore_separate == TRUE) {
      
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
      xgb_spec_m <- 
        boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                   mtry = tune(), min_n = tune()) %>% 
        set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
        set_mode("classification")
      
      xgb_spec_g <- 
        boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                   mtry = tune(), min_n = tune()) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      # generate recipe: define outcome and predictors
        ## confounding factors / predictors: all variables except variables including treatment information, outcome, and group
      X_controls <- data_train %>% 
        dplyr::select(-c(all_of(outcome), starts_with(treatment), all_of(group))) %>% colnames()
      #X_controls <- c(X_controls, "treatment_sport_freq_na")
      if ("treatment_sport_freq_source_leisure" %in% colnames(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_source_leisure")
      }
      if ("treatment_sport_freq_source_uni" %in% colnames(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_source_uni")
      }
      if ("treatment_sport_freq_lag" %in% ncol(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_lag")
      }
        ## m(x) for each treatment category
      xgb_recipe_m1 <- 
        data_train %>%
        recipe(.) %>%
        # outcome: indicator if individual participates at least weekly in sports
        update_role("treatment_sport_freq_weekly_atleast", new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor") # controls
      xgb_recipe_m2 <- 
        data_train %>%
        recipe(.) %>%
        # outcome: indicator if individual participates monthly or less frequently in sports
        update_role("treatment_sport_freq_monthly_less", new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      xgb_recipe_m3 <- 
        data_train %>%
        recipe(.) %>%
        # outcome: indicator if individual does not participate in sports
        update_role("treatment_sport_freq_never", new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
        ## g(D, X) for each treatment category
      xgb_recipe_g1 <- 
        data_train_g1 %>%  
        recipe(.) %>%
        update_role({{outcome}}, new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      xgb_recipe_g2 <- 
        data_train_g2 %>%  
        recipe(.) %>%
        update_role({{outcome}}, new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      xgb_recipe_g3 <- 
        data_train_g3 %>%  
        recipe(.) %>%
        update_role({{outcome}}, new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      
      # generate workflow
      xgb_workflow_m1 <- 
        workflow() %>%
        add_model(xgb_spec_m) %>%
        add_recipe(xgb_recipe_m1)
      
      xgb_workflow_m2 <- 
        workflow() %>%
        add_model(xgb_spec_m) %>%
        add_recipe(xgb_recipe_m2)
      
      xgb_workflow_m3 <- 
        workflow() %>%
        add_model(xgb_spec_m) %>%
        add_recipe(xgb_recipe_m3)
      
      xgb_workflow_g1 <- 
        workflow() %>%
        add_model(xgb_spec_g) %>%
        add_recipe(xgb_recipe_g1)
      
      xgb_workflow_g2 <- 
        workflow() %>%
        add_model(xgb_spec_g) %>%
        add_recipe(xgb_recipe_g2)
      
      xgb_workflow_g3 <- 
        workflow() %>%
        add_model(xgb_spec_g) %>%
        add_recipe(xgb_recipe_g3)
      
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%#
      #### Parameter Tuning ####
      #%%%%%%%%%%%%%%%%%%%%%%%%#
      
      # parameter tuning via 5-fold CV
      # this means that training data is again partitioned into K-folds
      K_folds_inner_m1 <- rsample::group_vfold_cv(
        data = data_train %>%
          group_by(group) %>% 
          mutate(treatment_fold = mean(treatment_sport_freq_weekly_atleast)) %>%
          ungroup(), 
        v = K, group = group, strata = treatment_fold, balance = "observations"
      )
      K_folds_inner_m2 <- rsample::group_vfold_cv(
        data = data_train %>%
          group_by(group) %>% 
          mutate(treatment_fold = mean(treatment_sport_freq_monthly_less)) %>%
          ungroup(), 
        v = K, group = group, strata = treatment_fold, balance = "observations"
      )
      K_folds_inner_m3 <- rsample::group_vfold_cv(
        data = data_train %>%
          group_by(group) %>% 
          mutate(treatment_fold = mean(treatment_sport_freq_never)) %>%
          ungroup(), 
        v = K, group = group, strata = treatment_fold, balance = "observations"
      )
      
      K_folds_inner_g1 <- rsample::group_vfold_cv(
        data = data_train_g1 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      K_folds_inner_g2 <- rsample::group_vfold_cv(
        data = data_train_g2 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      K_folds_inner_g3 <- rsample::group_vfold_cv(
        data = data_train_g3 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      
      # conduct parameter tuning
        ## m(X)
      xgb_grid_search_m1 <- 
        xgb_workflow_m1 %>%
        tune_grid(resamples = K_folds_inner_m1, grid = xgb_grid, metrics = metric_set(roc_auc))
      xgb_grid_search_m2 <- 
        xgb_workflow_m2 %>%
        tune_grid(resamples = K_folds_inner_m2, grid = xgb_grid, metrics = metric_set(roc_auc))
      xgb_grid_search_m3 <- 
        xgb_workflow_m3 %>%
        tune_grid(resamples = K_folds_inner_m3, grid = xgb_grid, metrics = metric_set(roc_auc))
        ## g(D, X)
      xgb_grid_search_g1 <- 
        xgb_workflow_g1 %>%
        tune_grid(resamples = K_folds_inner_g1, grid = xgb_grid, metrics = metric_set(rmse))
      xgb_grid_search_g2 <- 
        xgb_workflow_g2 %>%
        tune_grid(resamples = K_folds_inner_g2, grid = xgb_grid, metrics = metric_set(rmse))
      xgb_grid_search_g3 <- 
        xgb_workflow_g3 %>%
        tune_grid(resamples = K_folds_inner_g3, grid = xgb_grid, metrics = metric_set(rmse))
      
      
      # select best tuning parameters: 
      xgb_best_param_m1 <- xgb_grid_search_m1 %>% select_best("roc_auc")
      xgb_best_param_m2 <- xgb_grid_search_m2 %>% select_best("roc_auc")
      xgb_best_param_m3 <- xgb_grid_search_m3 %>% select_best("roc_auc")
      xgb_best_param_g1 <- xgb_grid_search_g1 %>% select_best("rmse")
      xgb_best_param_g2 <- xgb_grid_search_g2 %>% select_best("rmse")
      xgb_best_param_g3 <- xgb_grid_search_g3 %>% select_best("rmse")
      
      
      df_best_param <- data.frame(
        "m1_learn_rate" = xgb_best_param_m1$learn_rate, "m1_trees" = xgb_best_param_m1$trees,
        "m1_tree_depth" = xgb_best_param_m1$tree_depth, "m1_mtry" = xgb_best_param_m1$mtry,
        "m1_min_n" = xgb_best_param_m1$min_n,
        
        "m2_learn_rate" = xgb_best_param_m2$learn_rate, "m2_trees" = xgb_best_param_m2$trees,
        "m2_tree_depth" = xgb_best_param_m2$tree_depth, "m2_mtry" = xgb_best_param_m2$mtry,
        "m2_min_n" = xgb_best_param_m2$min_n,
        
        "m3_learn_rate" = xgb_best_param_m3$learn_rate, "m3_trees" = xgb_best_param_m3$trees,
        "m3_tree_depth" = xgb_best_param_m3$tree_depth, "m3_mtry" = xgb_best_param_m3$mtry,
        "m3_min_n" = xgb_best_param_m3$min_n,
        
        "g1_learn_rate" = xgb_best_param_g1$learn_rate, "g1_trees" = xgb_best_param_g1$trees,
        "g1_tree_depth" = xgb_best_param_g1$tree_depth, "g1_mtry" = xgb_best_param_g1$mtry,
        "g1_min_n" = xgb_best_param_g1$min_n,
        
        "g2_learn_rate" = xgb_best_param_g2$learn_rate, "g2_trees" = xgb_best_param_g2$trees,
        "g2_tree_depth" = xgb_best_param_g2$tree_depth, "g2_mtry" = xgb_best_param_g2$mtry,
        "g2_min_n" = xgb_best_param_g2$min_n,
        
        "g3_learn_rate" = xgb_best_param_g3$learn_rate, "g3_trees" = xgb_best_param_g3$trees,
        "g3_tree_depth" = xgb_best_param_g3$tree_depth, "g3_mtry" = xgb_best_param_g3$mtry,
        "g3_min_n" = xgb_best_param_g3$min_n
      )
      
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      #### Final Model Training ####
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      # specify the models
        ## model for m(X) = E(D|X): prediction of treatment
      tree_depth_m1 <- df_best_param$m1_tree_depth
      trees_m1 <- df_best_param$m1_trees
      learn_rate_m1 <- df_best_param$m1_learn_rate
      mtry_m1 <- df_best_param$m1_mtry
      min_n_m1 <- df_best_param$m1_min_n
      
      xgb_spec_final_m1 <- 
        boost_tree(tree_depth = {{tree_depth_m1}}, trees = {{trees_m1}}, 
                   learn_rate = {{learn_rate_m1}}, mtry = {{mtry_m1}}, min_n = {{min_n_m1}}
        ) %>%
        set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
        set_mode("classification")
      
      tree_depth_m2 <- df_best_param$m2_tree_depth
      trees_m2 <- df_best_param$m2_trees
      learn_rate_m2 <- df_best_param$m2_learn_rate
      mtry_m2 <- df_best_param$m2_mtry
      min_n_m2 <- df_best_param$m2_min_n
      
      xgb_spec_final_m2 <- 
        boost_tree(tree_depth = {{tree_depth_m2}}, trees = {{trees_m2}}, 
                   learn_rate = {{learn_rate_m2}}, mtry = {{mtry_m2}}, min_n = {{min_n_m2}}
        ) %>%
        set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
        set_mode("classification")
      
      
      tree_depth_m3 <- df_best_param$m3_tree_depth
      trees_m3 <- df_best_param$m3_trees
      learn_rate_m3 <- df_best_param$m3_learn_rate
      mtry_m3 <- df_best_param$m3_mtry
      min_n_m3 <- df_best_param$m3_min_n
      
      xgb_spec_final_m3 <- 
        boost_tree(tree_depth = {{tree_depth_m3}}, trees = {{trees_m3}}, 
                   learn_rate = {{learn_rate_m3}}, mtry = {{mtry_m3}}, min_n = {{min_n_m3}}
        ) %>%
        set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
        set_mode("classification")
      
      
      
        ## model for g(D, X) 
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
      
      tree_depth_g2 <- df_best_param$g2_tree_depth
      trees_g2 <- df_best_param$g2_trees
      learn_rate_g2 <- df_best_param$g2_learn_rate
      mtry_g2 <- df_best_param$g2_mtry
      min_n_g2 <- df_best_param$g2_min_n
      
      xgb_spec_final_g2 <- 
        boost_tree(tree_depth = {{tree_depth_g2}}, trees = {{trees_g2}}, 
                   learn_rate = {{learn_rate_g2}}, mtry = {{mtry_g2}}, min_n = {{min_n_g2}}
        ) %>%
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      
      tree_depth_g3 <- df_best_param$g3_tree_depth
      trees_g3 <- df_best_param$g3_trees
      learn_rate_g3 <- df_best_param$g3_learn_rate
      mtry_g3 <- df_best_param$g3_mtry
      min_n_g3 <- df_best_param$g3_min_n
      
      xgb_spec_final_g3 <- 
        boost_tree(tree_depth = {{tree_depth_g3}}, trees = {{trees_g3}}, 
                   learn_rate = {{learn_rate_g3}}, mtry = {{mtry_g3}}, min_n = {{min_n_g3}}
        ) %>%
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      # generate workflow
      xgb_workflow_final_m1 <- 
        workflow() %>%
        add_model(xgb_spec_final_m1) %>%
        add_recipe(xgb_recipe_m1)
      
      xgb_workflow_final_m2 <- 
        workflow() %>%
        add_model(xgb_spec_final_m2) %>%
        add_recipe(xgb_recipe_m2)
      
      xgb_workflow_final_m3 <- 
        workflow() %>%
        add_model(xgb_spec_final_m3) %>%
        add_recipe(xgb_recipe_m3)
      
      xgb_workflow_final_g1 <- 
        workflow() %>%
        add_model(xgb_spec_final_g1) %>%
        add_recipe(xgb_recipe_g1)
      
      xgb_workflow_final_g2 <- 
        workflow() %>%
        add_model(xgb_spec_final_g2) %>%
        add_recipe(xgb_recipe_g2)
      
      xgb_workflow_final_g3 <- 
        workflow() %>%
        add_model(xgb_spec_final_g3) %>%
        add_recipe(xgb_recipe_g3)
      
      # fit the model
      ## m(X)
      xgb_fit_final_m1 <- 
        xgb_workflow_final_m1 %>%
        fit(data_train)
      xgb_fit_final_m2 <- 
        xgb_workflow_final_m2 %>%
        fit(data_train)
      xgb_fit_final_m3 <- 
        xgb_workflow_final_m3 %>%
        fit(data_train)
      ## g(D X)
      xgb_fit_final_g1 <- 
        xgb_workflow_final_g1 %>%
        fit(data_train_g1)
      xgb_fit_final_g2 <- 
        xgb_workflow_final_g2 %>%
        fit(data_train_g2)
      xgb_fit_final_g3 <- 
        xgb_workflow_final_g3 %>%
        fit(data_train_g3)
      
      
      #%%%%%%%%%%%%%%%%%%%#
      #### Predictions ####
      #%%%%%%%%%%%%%%%%%%%#
      
      # make predictions on test data
      xgb_pred_m1 <- predict(xgb_fit_final_m1, data_test, type = "prob")
      xgb_pred_m1 <- xgb_pred_m1$.pred_1 # probability for class 1
      
      xgb_pred_m2 <- predict(xgb_fit_final_m2, data_test, type = "prob")
      xgb_pred_m2 <- xgb_pred_m2$.pred_1 # probability for class 1
      
      xgb_pred_m3 <- predict(xgb_fit_final_m3, data_test, type = "prob")
      xgb_pred_m3 <- xgb_pred_m3$.pred_1 # probability for class 1
      
      xgb_pred_g1 <- predict(xgb_fit_final_g1, data_test)
      xgb_pred_g1 <- xgb_pred_g1$.pred
      
      xgb_pred_g2 <- predict(xgb_fit_final_g2, data_test)
      xgb_pred_g2 <- xgb_pred_g2$.pred
      
      xgb_pred_g3 <- predict(xgb_fit_final_g3, data_test)
      xgb_pred_g3 <- xgb_pred_g3$.pred
      
      
      # create prediction data frame
      df_pred <- data.frame(
        # predictions
        "m1" = xgb_pred_m1, "m2" = xgb_pred_m2, "m3" = xgb_pred_m3,
        "g1" = xgb_pred_g1, "g2" = xgb_pred_g2, "g3" = xgb_pred_g3,
        # true values
        "treatment" = data_test %>% pull(treatment), 
        "outcome" = data_test %>% pull(outcome),
        # number of predictors
        "num_pred_m1" = ncol(xgb_fit_final_m1$pre$mold$predictors),
        "num_pred_m2" = ncol(xgb_fit_final_m2$pre$mold$predictors),
        "num_pred_m3" = ncol(xgb_fit_final_m3$pre$mold$predictors),
        "num_pred_g1" = ncol(xgb_fit_final_g1$pre$mold$predictors),
        "num_pred_g2" = ncol(xgb_fit_final_g2$pre$mold$predictors),
        "num_pred_g3" = ncol(xgb_fit_final_g3$pre$mold$predictors)
      )
      
      
      # if selected propensity score are normalized to sum to 1 within an individual
      if (probscore_normalize == TRUE) {
        df_pred <- df_pred %>%
          mutate(m_sum = m1 + m2 + m3) %>%
          mutate(m1 = m1 / m_sum, m2 = m2 / m_sum, m3 = m3 / m_sum) %>%
          dplyr::select(-m_sum)
        # df_pred %>% mutate(m_sum = m1 + m2 + m3) %>% pull(m_sum) %>% unique() # check
      } else {
        df_pred <- df_pred
      }
      
      # return data frame with predictions
      return(list("pred" = df_pred, "param" = df_best_param))
      
    
    #### TREATMENT: MULTIONOMINAL LOGISTIC REGRESSION ####
    #++++++++++++++++++++++++++++++++++++++++++++++++++++#
    
    } else {
      
      # remove treatment dummys
      data_train <- data_train %>% 
        dplyr::select(-c(treatment_sport_freq_monthly_less, treatment_sport_freq_never, treatment_sport_freq_weekly_atleast)) 
      data_test <- data_test %>% 
        dplyr::select(-c(treatment_sport_freq_monthly_less, treatment_sport_freq_never, treatment_sport_freq_weekly_atleast)) 
      
      # ensure that treatment variable is factor
      data_train <- data_train %>% mutate(treatment_sport_freq = as.factor(treatment_sport_freq)) 
      data_test <- data_test %>% mutate(treatment_sport_freq = as.factor(treatment_sport_freq)) 
      
      # separate training data for g0 and g1 prediction
      data_train_g1 <- data_train %>% filter(treatment_sport_freq == 1)
      data_train_g2 <- data_train %>% filter(treatment_sport_freq == 2)
      data_train_g3 <- data_train %>% filter(treatment_sport_freq == 3)
      
      # specify the model: treatment is predicted via multinominal classification 
      # and outcome via regression
      xgb_spec_m <- 
        boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                   mtry = tune(), min_n = tune()) %>% 
        set_engine("xgboost", eval_metric = "merror") %>% # "merror" for classification
        set_mode("classification")
      
      xgb_spec_g <- 
        boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(),
                   mtry = tune(), min_n = tune()) %>% 
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      
      # generate recipe: define outcome and predictors
        ## confounding factors / predictors: all variables except treatment, outcome, and group
      X_controls <- data_train %>% 
        dplyr::select(-c(all_of(outcome), starts_with(treatment), all_of(group))) %>% colnames()
      #X_controls <- c(X_controls, "treatment_sport_freq_na")
      if ("treatment_sport_freq_source_leisure" %in% colnames(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_source_leisure")
      }
      if ("treatment_sport_freq_source_uni" %in% colnames(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_source_uni")
      }
      if ("treatment_sport_freq_lag" %in% ncol(data_train)) {
        X_controls <- c(X_controls, "treatment_sport_freq_lag")
      }
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
        update_role({{outcome}}, new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      xgb_recipe_g2 <- 
        data_train_g2 %>%  
        recipe(.) %>%
        update_role({{outcome}}, new_role = "outcome") %>%
        update_role(all_of(X_controls), new_role = "predictor")
      xgb_recipe_g3 <- 
        data_train_g3 %>%  
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
      
      xgb_workflow_g2 <- 
        workflow() %>%
        add_model(xgb_spec_g) %>%
        add_recipe(xgb_recipe_g2)
      
      xgb_workflow_g3 <- 
        workflow() %>%
        add_model(xgb_spec_g) %>%
        add_recipe(xgb_recipe_g3)
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%#
      #### Parameter Tuning ####
      #%%%%%%%%%%%%%%%%%%%%%%%%#
      
      # parameter tuning via 5-fold CV
      # this means that training data is again partitioned into 5 folds
      K_folds_inner_m<- rsample::group_vfold_cv(
        data = data_train %>%
          group_by(group) %>% 
          mutate(treatment_fold = mean(!!rlang::sym(treatment))) %>%
          ungroup(), 
        v = K, group = group, strata = treatment_fold, balance = "observations"
      )
      
      K_folds_inner_g1 <- rsample::group_vfold_cv(
        data = data_train_g1 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      K_folds_inner_g2 <- rsample::group_vfold_cv(
        data = data_train_g2 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      K_folds_inner_g3 <- rsample::group_vfold_cv(
        data = data_train_g3 %>%
          group_by(group) %>% 
          mutate(outcome_fold = mean(!!rlang::sym(outcome))) %>%
          ungroup(),  
        v = K, group = group, strata = outcome_fold, balance = "observations"
      )
      
      # conduct parameter tuning
        ## m(X)
      xgb_grid_search_m <- 
        xgb_workflow_m %>%
        tune_grid(resamples = K_folds_inner_m, grid = xgb_grid, metrics = metric_set(roc_auc))
      ## g(D, X)
      xgb_grid_search_g1 <- xgb_workflow_g1 %>%
        tune_grid(resamples = K_folds_inner_g1, grid = xgb_grid, metrics = metric_set(rmse))
      xgb_grid_search_g2 <- xgb_workflow_g2 %>%
        tune_grid(resamples = K_folds_inner_g2, grid = xgb_grid, metrics = metric_set(rmse))
      xgb_grid_search_g3 <- xgb_workflow_g3 %>%
        tune_grid(resamples = K_folds_inner_g3, grid = xgb_grid, metrics = metric_set(rmse))
      
      # select best penalty parameter: parameter with highest AUC
      xgb_best_param_m <- xgb_grid_search_m %>% select_best("roc_auc")
      xgb_best_param_g1 <- xgb_grid_search_g1 %>% select_best("rmse")
      xgb_best_param_g2 <- xgb_grid_search_g2 %>% select_best("rmse")
      xgb_best_param_g3 <- xgb_grid_search_g3 %>% select_best("rmse")
      
      
      df_best_param <- data.frame(
        "m_learn_rate" = xgb_best_param_m$learn_rate, "m_trees" = xgb_best_param_m$trees,
        "m_tree_depth" = xgb_best_param_m$tree_depth, "m_mtry" = xgb_best_param_m$mtry,
        "m_min_n" = xgb_best_param_m$min_n,
        "g1_learn_rate" = xgb_best_param_g1$learn_rate, "g1_trees" = xgb_best_param_g1$trees,
        "g1_tree_depth" = xgb_best_param_g1$tree_depth, "g1_mtry" = xgb_best_param_g1$mtry,
        "g1_min_n" = xgb_best_param_g1$min_n,
        "g2_learn_rate" = xgb_best_param_g2$learn_rate, "g2_trees" = xgb_best_param_g2$trees,
        "g2_tree_depth" = xgb_best_param_g2$tree_depth, "g2_mtry" = xgb_best_param_g2$mtry,
        "g2_min_n" = xgb_best_param_g2$min_n,
        "g3_learn_rate" = xgb_best_param_g3$learn_rate, "g3_trees" = xgb_best_param_g3$trees,
        "g3_tree_depth" = xgb_best_param_g3$tree_depth, "g3_mtry" = xgb_best_param_g3$mtry,
        "g3_min_n" = xgb_best_param_g3$min_n
      )
      
      
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      #### Final Model Training ####
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      # specify the models
        ## model for m(X) 
      tree_depth_m <- df_best_param$m_tree_depth
      trees_m <- df_best_param$m_trees
      learn_rate_m <- df_best_param$m_learn_rate
      mtry_m <- df_best_param$m_mtry
      min_n_m <- df_best_param$m_min_n
      
      xgb_spec_final_m <- 
        boost_tree(tree_depth = {{tree_depth_m}}, trees = {{trees_m}}, 
                   learn_rate = {{learn_rate_m}}, mtry = {{mtry_m}},
                   min_n = {{min_n_m}}) %>%
        set_engine("xgboost", eval_metric = "merror") %>% 
        set_mode("classification")
      
        ## models for g(D,X) 
      tree_depth_g1 <- df_best_param$g1_tree_depth
      trees_g1 <- df_best_param$g1_trees
      learn_rate_g1 <- df_best_param$g1_learn_rate
      mtry_g1 <- df_best_param$g1_mtry
      min_n_g1 <- df_best_param$g1_min_n
      
      xgb_spec_final_g1 <- 
        boost_tree(tree_depth = {{tree_depth_g1}}, trees = {{trees_g1}}, 
                   learn_rate = {{learn_rate_g1}}, mtry = {{mtry_g1}}, 
                   min_n = {{min_n_g1}}) %>%
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      
      tree_depth_g2 <- df_best_param$g2_tree_depth
      trees_g2 <- df_best_param$g2_trees
      learn_rate_g2 <- df_best_param$g2_learn_rate
      mtry_g2 <- df_best_param$g2_mtry
      min_n_g2 <- df_best_param$g2_min_n
      
      
      xgb_spec_final_g2 <- 
        boost_tree(tree_depth = {{tree_depth_g2}}, trees = {{trees_g2}}, 
                   learn_rate = {{learn_rate_g2}}, mtry = {{mtry_g2}},
                   min_n = {{min_n_g2}}) %>%
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      
      tree_depth_g3 <- df_best_param$g3_tree_depth
      trees_g3 <- df_best_param$g3_trees
      learn_rate_g3 <- df_best_param$g3_learn_rate
      mtry_g3 <- df_best_param$g3_mtry
      min_n_g3 <- df_best_param$g3_min_n
      
      
      xgb_spec_final_g3 <- 
        boost_tree(tree_depth = {{tree_depth_g3}}, trees = {{trees_g3}}, 
                   learn_rate = {{learn_rate_g3}}, mtry = {{mtry_g3}},
                   min_n = {{min_n_g3}}) %>%
        set_engine("xgboost") %>% 
        set_mode("regression")
      
      
      # generate workflow
      xgb_workflow_final_m <- 
        workflow() %>%
        add_model(xgb_spec_final_m) %>%
        add_recipe(xgb_recipe_m)
      
      xgb_workflow_final_g1 <- 
        workflow() %>%
        add_model(xgb_spec_final_g1) %>%
        add_recipe(xgb_recipe_g1)
      
      xgb_workflow_final_g2 <- 
        workflow() %>%
        add_model(xgb_spec_final_g2) %>%
        add_recipe(xgb_recipe_g2)
      
      xgb_workflow_final_g3 <- 
        workflow() %>%
        add_model(xgb_spec_final_g3) %>%
        add_recipe(xgb_recipe_g3)
      
      # fit the model
        ## m(X)
      xgb_fit_final_m <- 
        xgb_workflow_final_m %>%
        fit(data_train)
        ## g(D, X)
      xgb_fit_final_g1 <- 
        xgb_workflow_final_g1 %>%
        fit(data_train_g1)

      xgb_fit_final_g2 <- 
        xgb_workflow_final_g2 %>%
        fit(data_train_g2)
      
      xgb_fit_final_g3 <- 
        xgb_workflow_final_g3 %>%
        fit(data_train_g3)
      
      
      #%%%%%%%%%%%%%%%%%%%#
      #### Predictions ####
      #%%%%%%%%%%%%%%%%%%%#
      
      # make predictions on test data
      xgb_pred_m <- predict(xgb_fit_final_m, data_test, type = "prob")
      xgb_pred_m1 <- xgb_pred_m$.pred_1 # probability for class 1
      xgb_pred_m2 <- xgb_pred_m$.pred_2 # probability for class 2
      xgb_pred_m3 <- xgb_pred_m$.pred_3 # probability for class 3
      
      xgb_pred_g1 <- predict(xgb_fit_final_g1, data_test)
      xgb_pred_g1 <- xgb_pred_g1$.pred 
      
      xgb_pred_g2 <- predict(xgb_fit_final_g2, data_test)
      xgb_pred_g2 <- xgb_pred_g2$.pred
      
      xgb_pred_g3 <- predict(xgb_fit_final_g3, data_test)
      xgb_pred_g3 <- xgb_pred_g3$.pred
      
      # create prediction data frame
      df_pred <- data.frame(
        # predictions
        "m1" = xgb_pred_m1, "m2" = xgb_pred_m2, "m3" = xgb_pred_m3, 
        "g1" = xgb_pred_g1, "g2" = xgb_pred_g2, "g3" = xgb_pred_g3,
        # true values
        "treatment" = data_test %>% pull(treatment), 
        "outcome" = data_test %>% pull(outcome),
        # number of predictors (for m actually the same but needed to append data frame)
        "num_pred_m1" = ncol(xgb_fit_final_m$pre$mold$predictors),
        "num_pred_m2" = ncol(xgb_fit_final_m$pre$mold$predictors),
        "num_pred_m3" = ncol(xgb_fit_final_m$pre$mold$predictors),
        "num_pred_g1" = ncol(xgb_fit_final_g1$pre$mold$predictors),
        "num_pred_g2" = ncol(xgb_fit_final_g2$pre$mold$predictors),
        "num_pred_g3" = ncol(xgb_fit_final_g3$pre$mold$predictors)
      )
      
      # return data frame with predictions
      return(list("pred" = df_pred, "param" = df_best_param))
      
    } # close else for probscore_separate = FALSE
    
    
  } else {
    stop("Please specify treatment setting")
  }
  
} # close function() 