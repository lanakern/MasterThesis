#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML Analysis Results: Binary and Multivalued Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++ 
# by Lana Kern
#+++
# In this files, the DML results are analyzed and prepared so that they can
# be copied within the final paper.
#+++


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

## MAIN LOAD RESULTS ##
#+++++++++++++++++++++#

df_dml_main_binary <- 
  read.xlsx("Output/DML/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")

df_dml_main_multi <- 
  read.xlsx("Output/DML/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")


## LOAD RESULTS ACROSS FOLDS ##
#+++++++++++++++++++++++++++++#

if (main_extra_act == "yes") {
  main_extra_act_save <- "extradrop"
} else {
  main_extra_act_save <- ""
}

load_results_main <- paste(
  main_model_type, main_model_outcome, str_replace_all(main_cohort_prep, "_", ""),
  main_treatment_def, main_treatment_repl, main_extra_act_save, sep = "_"
)

## DETAIL ##

# BINARY TREATMENT SETTING #
df_dml_detail_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_detail_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_detail_sub <- df_dml_load_binary[[i]]$detail %>% filter(Type %in% c("ATE", "ATTE")) %>% mutate(MICE = i)
    df_dml_detail_sub_all <- rbind(df_dml_detail_sub_all, df_dml_detail_sub)
  }
  
  df_dml_detail_all_binary <- rbind(df_dml_detail_all_binary, df_dml_detail_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #



## ERROR METRICS ##

# BINARY TREATMENT SETTING #
df_dml_error_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_error_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_error_sub <- df_dml_load_binary[[i]]$error %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_error_sub_all <- rbind(df_dml_error_sub_all, df_dml_error_sub)
  }
  
  df_dml_error_all_binary <- rbind(df_dml_error_all_binary, df_dml_error_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #


## PREDICTIONS (FOR COMMON SUPPORT) ##

# FILE PATH ANPASSEN UND ALGOS

# BINARY TREATMENT SETTING #
df_dml_pred_all_binary <- data.frame()
for (model_algo_sel in c("lasso")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/TEST_binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_pred_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_pred_sub <- df_dml_load_binary[[i]]$pred %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_pred_sub_all <- rbind(df_dml_pred_sub_all, df_dml_pred_sub)
  }
  
  df_dml_pred_all_binary <- rbind(df_dml_pred_all_binary, df_dml_pred_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #


## TRIMMING ##

df_dml_trimming_all_binary <- data.frame()
for (model_algo_sel in c("lasso")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/TEST_binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_trimming_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_trimming_sub <- df_dml_load_binary[[i]]$trimming %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_trimming_sub_all <- rbind(df_dml_trimming_sub_all, df_dml_trimming_sub)
  }
  
  df_dml_trimming_all_binary <- rbind(df_dml_trimming_all_binary, df_dml_trimming_sub_all)
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TREATMENT EFFECTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act,
  starts_with("model"), Treatment, Type, starts_with("theta"),
  starts_with("se"), starts_with("tvalue"), starts_with("pvalue"), 
  starts_with("CI")
)


## Multivalued Treatment Setting ##


## FUNCTION RESULTS ##
readRDS("Output/DML/FUNCTION_KNAUS_BINARY.rds")
readRDS("Output/DML/FUNCTION_KNAUS_MULTI.rds")

readRDS("Output/DML/FUNCTION_DOUBLEML.rds")



#### Multivalued Treatment Setting ####
#+++++++++++++++++++++++++++++++++++++#

# ##++ results for separate models for m(D) ++##
# df_treatment_multi_sep <- read.xlsx("Output/ESTIMATION_RESULTS_MULTI.xlsx", sheetName = "Sheet1")
# 
# ## treatment effects
# df_treatment_multi_sep %>% select(Treatment, Type, ends_with("median")) %>% filter(Type == "ATE")
# df_treatment_multi_sep %>% select(Treatment, Type, ends_with("median")) %>% filter(Type == "ATTE")
# 
# ## error metrics
# df_treatment_multi_sep %>%
#   select(matches("_[mg][1-9]") & !starts_with("num")) %>% distinct()
# 
# 
# ##++ results for single model for m(D) ++##
# df_treatment_multi_one <- read.xlsx("Output/ESTIMATION_RESULTS_MULTI_ONE.xlsx", sheetName = "Sheet1")
# 
# ## treatment effects
# df_treatment_multi_one %>% select(Treatment, Type, ends_with("median")) %>% filter(Type == "ATE")
# df_treatment_multi_one %>% select(Treatment, Type, ends_with("median")) %>% filter(Type == "ATTE")
# 
# ## error metrics
# df_treatment_multi_one %>%
#   select(!starts_with("num")) %>% select(matches("_[g][1-9]"), ends_with("_m")) %>% distinct()


#%%%%%%%%%%%%%%%%%%%%%#
#### ERROR METRICS ####
#%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
  starts_with("ACC"), starts_with("KAPPA"), starts_with("BACC"), starts_with("AUC"),
  starts_with("MAE"), starts_with("MAPE"), starts_with("MSE"), starts_with("RMSE")
) %>% distinct()


## Multivalued Treatment Setting ##



#%%%%%%%%%%%%%%%%#
#### TRIMMING ####
#%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
  n_treats_min
) %>% distinct()


## Multivalued Treatment Setting ##



#%%%%%%%%%%%%%%%%%%#
#### PREDICTORS ####
#%%%%%%%%%%%%%%%%%%#

df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
  starts_with("num_predictors")
) %>% distinct()


#%%%%%%%%%%%%%%%%%%%%%%#
#### COMMON SUPPORT ####
#%%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##


# common support plot across all mice data frames and all repetitions
binary_plot_common_support <- 
  func_dml_common_support(treatment_setting, df_dml_pred_all_binary, 
                          df_dml_trimming_all_binary$min_trimming, 
                          df_dml_trimming_all_binary$max_trimming)
ggsave(paste0("Output/DML/binary_all_dml_plot_common_support_", treatment_setting, "_", mlalgo, ".png"), 
       binary_plot_common_support)


#%%%%%%%%%%%%%%%#
#### BOXPLOT ####
#%%%%%%%%%%%%%%%#

# The boxplot shows how variable the estimates are across the S repetitions and
# and 5 MICE data sets.


## BINARY TREATMENT SETTING ##
#++++++++++++++++++++++++++++#

# rename algorithms
df_dml_results_boxplot_binary <- df_dml_detail_all_binary %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "Lasso", ML_algo == "postlasso" ~ "Post-Lasso",
      ML_algo == "randomforests" ~ "Random Forests",
      ML_algo == "xgboost" ~ "XGBoost"
    )
  )

dml_boxplot_binary <- 
  ggplot(df_dml_results_boxplot_binary, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nTreatment Effect Estimates\n") +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(axis.text = element_text(size = 28), # size of x-axis tick labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # rotate x-axis ticks
        axis.title = element_text(size = 30, face = "bold"), # size of x-axis labels
        strip.text.x = element_text(size = 30) # size of facet text
        ) 

ggsave("Output/DML/binary_plot_dml_boxplot.png", dml_boxplot_binary)



## MULTIVALUED TREATMENT SETTING ##
#+++++++++++++++++++++++++++++++++#



#%%%%%%%%%%%%%%%%%%%%%%%#
#### HYPERPARAMETERS ####
#%%%%%%%%%%%%%%%%%%%%%%%#

## BINARY TREATMENT SETTING ##

# lasso
df_dml_hyperparam_all_binary_lasso <- data.frame()
for (model_algo_sel in c("lasso", "postlasso")) {
  df_dml_load <- readRDS(paste0("Output/DML/", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_hyperparam_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = model_algo_sel)
    df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
  }
  
  df_dml_hyperparam_all_binary_lasso <- rbind(df_dml_hyperparam_all_binary_lasso, df_dml_hyperparam_sub_all)
  
}


# random forests (no parameter tuning)
df_dml_hyperparam_all_binary_rf <- data.frame()

df_dml_load <- readRDS(paste0("Output/DML/", "randomforests", "_", load_results_main, ".rds"))
df_dml_hyperparam_sub_all <- data.frame()
for (i in 1:5) {
  df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = "randomforests")
  df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
}
df_dml_hyperparam_all_binary_rf <- rbind(df_dml_hyperparam_all_binary_rf, df_dml_hyperparam_sub_all)


# xgboost
df_dml_hyperparam_all_binary_xgb <- data.frame()
df_dml_load <- readRDS(paste0("Output/DML/", "xgboost", "_", load_results_main, ".rds"))
df_dml_hyperparam_sub_all <- data.frame()
for (i in 1:5) {
  df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = "xgboost")
  df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
}
df_dml_hyperparam_all_binary_xgb <- rbind(df_dml_hyperparam_all_binary_xgb, df_dml_hyperparam_sub_all)













