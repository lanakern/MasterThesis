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

#+++++++++++++++++++++#
## MAIN LOAD RESULTS ##
#+++++++++++++++++++++#

df_dml_main_binary <- 
  read.xlsx("Output/DML/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")

df_dml_main_multi <- 
  read.xlsx("Output/DML/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")


#+++++++++++++++++++++++++++++#
## LOAD RESULTS ACROSS FOLDS ##
#+++++++++++++++++++++++++++++#

if (main_extra_act == "yes") {
  main_extra_act_save <- "extradrop"
} else {
  main_extra_act_save <- ""
}

load_results_main <- paste(
  main_model_type, str_replace_all(main_cohort_prep, "_", ""),
  main_treatment_def, main_treatment_repl, main_extra_act_save, sep = "_"
)

## APE ##
#-------#

# the APE is identical across the MICE data sets and algorithms

# BINARY TREATMENT SETTING #

# MULTIVALUED TREATMENT SETTING #
df_dml_load_multi <- readRDS(paste0("Output/DML/multi_", "xgboost", "_", load_results_main, ".rds"))
df_dml_ape_multi <- df_dml_load_multi[[1]]$ape 


## DETAIL ##
#----------#

# BINARY TREATMENT SETTING #
df_dml_detail_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_detail_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_detail_sub <- df_dml_load_binary[[i]]$detail %>% filter(Type %in% c("ATE", "ATTE")) %>% mutate(MICE = i)
    df_dml_detail_sub_all <- rbind(df_dml_detail_sub_all, df_dml_detail_sub)
  }
  
  df_dml_detail_all_binary <- rbind(df_dml_detail_all_binary, df_dml_detail_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #
df_dml_detail_all_multi <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_multi <- readRDS(paste0("Output/DML/multi_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_detail_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_detail_sub <- df_dml_load_multi[[i]]$detail %>% filter(Type %in% c("ATE", "ATTE")) %>% mutate(MICE = i)
    df_dml_detail_sub_all <- rbind(df_dml_detail_sub_all, df_dml_detail_sub)
  }
  
  df_dml_detail_all_multi <- rbind(df_dml_detail_all_multi, df_dml_detail_sub_all)
  
}


## ERROR METRICS ##
#-----------------#

# BINARY TREATMENT SETTING #
df_dml_error_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_error_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_error_sub <- df_dml_load_binary[[i]]$error %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_error_sub_all <- rbind(df_dml_error_sub_all, df_dml_error_sub)
  }
  
  df_dml_error_all_binary <- rbind(df_dml_error_all_binary, df_dml_error_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #
df_dml_error_all_multi <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_multi <- readRDS(paste0("Output/DML/multi_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_error_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_error_sub <- df_dml_load_multi[[i]]$error %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_error_sub_all <- rbind(df_dml_error_sub_all, df_dml_error_sub)
  }
  
  df_dml_error_all_multi <- rbind(df_dml_error_all_multi, df_dml_error_sub_all)
}


## PREDICTIONS (FOR COMMON SUPPORT) ##
#------------------------------------#

# GEHT NOCH NICHT BEI BINARY

# BINARY TREATMENT SETTING #
df_dml_pred_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_pred_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_pred_sub <- df_dml_load_binary[[i]]$pred %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_pred_sub_all <- rbind(df_dml_pred_sub_all, df_dml_pred_sub)
  }
  
  df_dml_pred_all_binary <- rbind(df_dml_pred_all_binary, df_dml_pred_sub_all)
}


# MULTIVALUED TREATMENT SETTING #
df_dml_pred_all_multi <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_multi <- readRDS(paste0("Output/DML/multi_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_pred_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_pred_sub <- df_dml_load_multi[[i]]$pred %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_pred_sub_all <- rbind(df_dml_pred_sub_all, df_dml_pred_sub)
  }
  
  df_dml_pred_all_multi <- rbind(df_dml_pred_all_multi, df_dml_pred_sub_all)
}


## TRIMMING ##
#------------#

# BINARY TREATMENT SETTING #
df_dml_trimming_all_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_binary <- readRDS(paste0("Output/DML/binary_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_trimming_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_trimming_sub <- df_dml_load_binary[[i]]$trimming %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_trimming_sub_all <- rbind(df_dml_trimming_sub_all, df_dml_trimming_sub)
  }
  
  df_dml_trimming_all_binary <- rbind(df_dml_trimming_all_binary, df_dml_trimming_sub_all)
  
}


# MULTIVALUED TREATMENT SETTING #
df_dml_trimming_all_multi <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_load_multi <- readRDS(paste0("Output/DML/multi_", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_trimming_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_trimming_sub <- df_dml_load_multi[[i]]$trimming %>% mutate(MICE = i, modelalgo = model_algo_sel)
    df_dml_trimming_sub_all <- rbind(df_dml_trimming_sub_all, df_dml_trimming_sub)
  }
  
  df_dml_trimming_all_multi <- rbind(df_dml_trimming_all_multi, df_dml_trimming_sub_all)
  
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
) %>% filter(Type %in% c("ATE", "ATTE"))


## Multivalued Treatment Setting ##
df_dml_main_multi %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act,
  starts_with("model"), Treatment, Type, starts_with("theta"),
  starts_with("se"), starts_with("tvalue"), starts_with("pvalue"), 
  starts_with("CI")
) %>% filter(Type %in% c("ATE", "ATTE"))


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


#%%%%%%%%%%%%%%%%%#
#### APO & APE ####
#%%%%%%%%%%%%%%%%%#


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
df_dml_main_multi %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
  starts_with("ACC"), starts_with("KAPPA"), starts_with("BACC"), starts_with("AUC"),
  starts_with("MAE"), starts_with("MAPE"), starts_with("MSE"), starts_with("RMSE")
) %>% distinct()


#%%%%%%%%%%%%%%%%#
#### TRIMMING ####
#%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##
df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), n_treats_min
) %>% distinct()


## Multivalued Treatment Setting ##
df_dml_main_multi %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), n_treats_min
) %>% distinct()


#%%%%%%%%%%%%%%%%%%#
#### PREDICTORS ####
#%%%%%%%%%%%%%%%%%%#


## Binary Treatment Setting ##
df_dml_main_binary %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), starts_with("num_predictors")
) %>% distinct()


## Multivalued Treatment Setting ##
df_dml_main_multi %>% select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), starts_with("num_predictors")
) %>% distinct()



#%%%%%%%%%%%%%%%%%%%%%%#
#### COMMON SUPPORT ####
#%%%%%%%%%%%%%%%%%%%%%%#

## Binary Treatment Setting ##

# common support plot across all mice data frames and all repetitions
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_pred_all_binary_sub <- df_dml_pred_all_binary %>% 
    filter(modelalgo %in% model_algo_sel) %>%
    left_join(df_dml_trimming_all_binary, by = "modelalgo")

  binary_plot_common_support <- func_dml_common_support(
    "binary", df_dml_pred_all_binary_sub, df_dml_pred_all_binary_sub$min_trimming, 
    df_dml_pred_all_binary_sub$max_trimming)
  ggsave(paste0("Output/DML/binary_all_dml_plot_common_support_binary_", model_algo_sel, ".png"), 
         binary_plot_common_support)
}


## Multivalued Treatment Setting ##

# common support plot across all mice data frames and all repetitions
for (model_algo_sel in c("lasso", "postlasso", "randomforests", "xgboost")) {
  df_dml_pred_all_multi_sub <- df_dml_pred_all_multi %>% 
    filter(modelalgo %in% model_algo_sel) %>%
    left_join(
      df_dml_trimming_all_multi, by = "modelalgo"
    )
  multi_plot_common_support <- func_dml_common_support(
    "multi", df_dml_pred_all_multi_sub, df_dml_pred_all_multi_sub$min_trimming, 
    df_dml_pred_all_multi_sub$max_trimming)
  ggsave(paste0("Output/DML/binary_all_dml_plot_common_support_multi_", model_algo_sel, ".png"), 
         multi_plot_common_support)
}



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

df_dml_results_boxplot_multi <- df_dml_detail_all_multi %>%
  mutate(
    ML_algo = case_when(
      ML_algo == "lasso" ~ "Lasso", ML_algo == "postlasso" ~ "Post-Lasso",
      ML_algo == "randomforests" ~ "Random Forests",
      ML_algo == "xgboost" ~ "XGBoost"
    )
  )

# ATE
df_dml_results_ate_boxplot_multi <- df_dml_results_boxplot_multi %>% filter(Type == "ATE")
dml_ate_boxplot_multi <- 
  ggplot(df_dml_results_ate_boxplot_multi, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nATE\n") +
  facet_wrap(~ Treatment) + 
  theme_bw() +
  theme(axis.text = element_text(size = 28), # size of x-axis tick labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # rotate x-axis ticks
        axis.title = element_text(size = 30, face = "bold"), # size of x-axis labels
        strip.text.x = element_text(size = 25) # size of facet text
  ) 

ggsave("Output/DML/multi_plot_dml_boxplot_ate.png", dml_ate_boxplot_multi)


# ATTE
df_dml_results_ate_boxplot_multi <- df_dml_results_boxplot_multi %>% filter(Type == "ATTE")
dml_atte_boxplot_multi <- 
  ggplot(df_dml_results_ate_boxplot_multi, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  #xlab("\nMachine Learning Algorithms\n") +
  xlab("") + ylab("\nATTE\n") +
  facet_wrap(~ Treatment) + 
  theme_bw() +
  theme(axis.text = element_text(size = 28), # size of x-axis tick labels
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # rotate x-axis ticks
        axis.title = element_text(size = 30, face = "bold"), # size of x-axis labels
        strip.text.x = element_text(size = 25) # size of facet text
  ) 


ggsave("Output/DML/multi_plot_dml_boxplot_atte.png", dml_atte_boxplot_multi)



#%%%%%%%%%%%%%%%%%%%%%%%#
#### HYPERPARAMETERS ####
#%%%%%%%%%%%%%%%%%%%%%%%#

## BINARY TREATMENT SETTING ##

# (post-)lasso
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





## MULTIVALUED TREATMENT SETTING ##

# (post-)lasso
df_dml_hyperparam_all_multi_lasso <- data.frame()
for (model_algo_sel in c("lasso", "postlasso")) {
  df_dml_load <- readRDS(paste0("Output/DML/multi_", model_algo_sel, "_", load_results_main, ".rds"))
  df_dml_hyperparam_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = model_algo_sel)
    df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
  }
  
  df_dml_hyperparam_all_multi_lasso <- rbind(df_dml_hyperparam_all_multi_lasso, df_dml_hyperparam_sub_all)
  
}


# random forests (no parameter tuning)
df_dml_hyperparam_all_multi_rf <- data.frame()

df_dml_load <- readRDS(paste0("Output/DML/multi_", "randomforests", "_", load_results_main, ".rds"))
df_dml_hyperparam_sub_all <- data.frame()
for (i in 1:5) {
  df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = "randomforests")
  df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
}
df_dml_hyperparam_all_multi_rf <- rbind(df_dml_hyperparam_all_multi_rf, df_dml_hyperparam_sub_all)


# xgboost
df_dml_hyperparam_all_multi_xgb <- data.frame()
df_dml_load <- readRDS(paste0("Output/DML/multi_", "xgboost", "_", load_results_main, ".rds"))
df_dml_hyperparam_sub_all <- data.frame()
for (i in 1:5) {
  df_dml_hyperparam_sub <- df_dml_load[[i]]$param %>% mutate(MICE = i) %>% mutate(mlalgo = "xgboost")
  df_dml_hyperparam_sub_all <- rbind(df_dml_hyperparam_sub_all, df_dml_hyperparam_sub)
}
df_dml_hyperparam_all_multi_xgb <- rbind(df_dml_hyperparam_all_multi_xgb, df_dml_hyperparam_sub_all)











