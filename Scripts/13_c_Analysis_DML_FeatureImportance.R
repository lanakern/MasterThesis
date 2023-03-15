#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FEATURE IMPORTANCE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, feature importance plots are created for all machine learning
# models and grades as outcome in both the binary and multivalued treatment setting. 
# However, this is only done for the main model (no robustness checks).
# To do so, final models are trained using the "best" hyperparameter combinations 
# and the full data set. This is done across all five mice data sets. 
# The feature importance scores are then again aggregated by taking the mean.
#+++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
for (mice_data_sel in 1:5) {
  # load data
  ## extract extracurricular activity ending
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  ## extract outcome
  if (str_detect(outcome_var, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
  ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
  }
  
  data_dml <- readRDS(load_data)
  data_dml <- data_dml %>% mutate(treatment_sport = as.factor(treatment_sport))
  assign(as.character(paste0("data_dml_mice", mice_data_sel)), data_dml)
}


#### LASSO ####
#+++++++++++++#


#### POST-LASSO ####
#++++++++++++++++++#


#### XGBoost ####
#+++++++++++++++#

# load all estimation results
xgb_binary_results_all <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_xgboost_all_controlssameoutcome_weekly_down_extradrop.rds")

# extract best hyperparameters and respective error metrics across folds, repetitions, and MICE
xgb_binary_param <- data.frame()
for (i in 1:5) {
  xgb_binary_param <- rbind(
    xgb_binary_param, 
    left_join(
      # parameters
      xgb_binary_results_all[[i]]$param,
      # error metrics
      xgb_binary_results_all[[i]]$error %>% 
        dplyr::select(Repetition, Fold, AUC_m, starts_with("RMSE")),
      by = c("Repetition", "Fold")
    ) %>% mutate(MICE = i)
  )
}

# best parameter combinations for each mice data set
  ## for classification: highest AUC
xgb_binary_best_param_m <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(AUC_m == max(AUC_m)) %>% 
  dplyr::select(MICE, starts_with("m"))
  ## for regression: smallest RMSE
xgb_binary_best_param_g0 <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g0 == min(RMSE_g0)) %>% 
  dplyr::select(MICE, starts_with("g0"))
xgb_binary_best_param_g1 <- 
  xgb_binary_param %>% 
  group_by(MICE) %>% 
  filter(RMSE_g1 == min(RMSE_g1)) %>% 
  dplyr::select(MICE, starts_with("g1"))


# retrain ML models
tree_depth_m <- xgb_binary_best_param_m$m_tree_depth
trees_m <- xgb_binary_best_param_m$m_trees
learn_rate_m <- xgb_binary_best_param_m$m_learn_rate
mtry_m <- xgb_binary_best_param_m$m_mtry
min_n_m <- xgb_binary_best_param_m$m_min_n

xgb_spec_m <- 
  boost_tree(tree_depth = {{tree_depth_m}}, trees = {{trees_m}}, 
             learn_rate = {{learn_rate_m}}, mtry = {{mtry_m}}, min_n = {{min_n_m}}
  ) %>%
  set_engine("xgboost", objective = "binary:logistic", eval_metric = "error") %>% 
  set_mode("classification")


tree_depth_g0 <- xgb_binary_best_param_g0$m_tree_depth
trees_g0 <- xgb_binary_best_param_g0$m_trees
learn_rate_g0 <- xgb_binary_best_param_g0$m_learn_rate
mtry_g0 <- xgb_binary_best_param_g0$m_mtry
min_n_g0 <- xgb_binary_best_param_g0$m_min_n

xgb_spec_g0 <- 
  boost_tree(tree_depth = {{tree_depth_g0}}, trees = {{trees_g0}}, 
             learn_rate = {{learn_rate_g0}}, mtry = {{mtry_g0}}, min_n = {{min_n_g0}}
  ) %>%
  set_engine("xgboost") %>% 
  set_mode("regression")


tree_depth_g1 <- xgb_binary_best_param_g1$m_tree_depth
trees_g1 <- xgb_binary_best_param_g1$m_trees
learn_rate_g1 <- xgb_binary_best_param_g1$m_learn_rate
mtry_g1 <- xgb_binary_best_param_g1$m_mtry
min_n_g1 <- xgb_binary_best_param_g1$m_min_n

xgb_spec_g1 <- 
  boost_tree(tree_depth = {{tree_depth_g1}}, trees = {{trees_g1}}, 
             learn_rate = {{learn_rate_g1}}, mtry = {{mtry_g1}}, min_n = {{min_n_g1}}
  ) %>%
  set_engine("xgboost") %>% 
  set_mode("regression")


# calculate importance score across data frames
xgb_scores_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  # generate name of data frame
  df_sel_name <- paste0("data_dml_mice", mice_data_sel)
  
  X_controls <- get(df_sel_name) %>% 
    dplyr::select(-c("outcome_grade", "treatment_sport", "group")) %>% colnames()
  
  xgb_recipe_m <- 
    get(df_sel_name) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("treatment_sport", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_m <- 
    workflow() %>%
    add_model(xgb_spec_m) %>%
    add_recipe(xgb_recipe_m)
  
  xgb_fit_m <- 
    xgb_workflow_m %>%
    fit(get(df_sel_name))
  
  
  xgb_recipe_g1 <- 
    get(df_sel_name) %>% filter(treatment_sport == 1) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g1 <- 
    workflow() %>%
    add_model(xgb_spec_g1) %>%
    add_recipe(xgb_recipe_g1)
  
  xgb_fit_g1 <- 
    xgb_workflow_g1 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 1))
  
  xgb_recipe_g0 <- 
    get(df_sel_name) %>% filter(treatment_sport == 0) %>%
    recipe(.) %>%
    # price variable is outcome
    update_role("outcome_grade", new_role = "outcome") %>%
    # all other variables are predictors (drop outcome treatment)
    update_role(all_of(X_controls), new_role = "predictor")
  
  xgb_workflow_g0 <- 
    workflow() %>%
    add_model(xgb_spec_g0) %>%
    add_recipe(xgb_recipe_g0)
  
  xgb_fit_g0 <- 
    xgb_workflow_g0 %>%
    fit(get(df_sel_name) %>% filter(treatment_sport == 0))
  
  
  # calculate feature importance score
  xgb_scores <- rbind(
    func_feature_importance_score(xgb_fit_m, 20) %>% mutate("Pred_Type" = "Treatment Prediction"),
    func_feature_importance_score(xgb_fit_g0, 20) %>% mutate("Pred_Type" = "Outcome 0 Prediction"),
    func_feature_importance_score(xgb_fit_g1, 20) %>% mutate("Pred_Type" = "Outcome 1 Prediction")
  ) %>% mutate(MICE = mice_data_sel)
  
  xgb_scores_all <- rbind(xgb_scores_all, xgb_scores)
}

xgb_scores_all <- 
  xgb_scores_all %>%
  group_by(Variable, Pred_Type) %>%
  summarize_all(mean) %>%
  arrange(Pred_Type, desc(Importance)) %>%
  dplyr::select(-MICE)

# generate plot
xgb_feature_imp_plot <- func_feature_importance_plot("binary", xgb_scores_all, "XGBoost", "separate")
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_m.png", xgb_feature_imp_plot$m)
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g0.png", xgb_feature_imp_plot$g0)
ggsave("Output/DML/Feature_Importance/xgboost_binary_feature_importance_g1.png", xgb_feature_imp_plot$g1)



#### Random Forests ####
#++++++++++++++++++++++#

