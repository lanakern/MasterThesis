#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML Analysis Results: Binary and Multivalued Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++ 
# by Lana Kern
#+++
# In this files, the DML results are analyzed and prepared so that they can
# be copied within the final paper. For instance, error metrics are displayed,
# common support plots are created etc. 
#+++


#%%%%%%%%%%%%%%%%%%%%#
#### LOAD RESULTS ####
#%%%%%%%%%%%%%%%%%%%%#


## SUMMARY OF RESULTS ##
#++++++++++++++++++++++#

df_dml_main_binary <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
df_dml_main_binary

df_dml_main_multi <- 
  read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
df_dml_main_multi


## LASSO ##
#+++++++++#

# BINARY
lasso_grades <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_agree <- 
  readRDS("Output/DML/Estimation/Personality/binary_agreeableness_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_extra <- 
  readRDS("Output/DML/Estimation/Personality/binary_extraversion_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_open <- 
  readRDS("Output/DML/Estimation/Personality/binary_openness_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_consc <- 
  readRDS("Output/DML/Estimation/Personality/binary_conscientiousness_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_neuro <- 
  readRDS("Output/DML/Estimation/Personality/binary_neuroticism_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")


# MULTI
lasso_grades_multi <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")


## POST-LASSO ##
#++++++++++++++#

# BINARY
postlasso_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "postlasso", "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds"))

# MULTI
postlasso_grades_multi <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")


## XGBoosst ##
#++++++++++++#

# BINARY
xgb_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "xgboost", "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds"))

# MULTI
xgb_grades_multi <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_xgb_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")


## Random Forests ##
#++++++++++++++++++#

# BINARY
rf_grades <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_grades_", "randomforests", "_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-1_Rep5.rds"))


# MULTI
rf_grades_multi <- 
  readRDS("Output/DML/Estimation/Grades/multi_grades_randomforests_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-1_Rep5.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DETAILED LASSO ANALYSIS FOR GPA SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## MAIN RESULTS ##
#++++++++++++++++#

df_dml_main_binary %>% filter(model_algo == "lasso")


## RESULT ACROSS REPETITIONS ##
#+++++++++++++++++++++++++++++#

## S = 5 ##

lasso_theta_detail <- rbind(
  lasso_grades[[1]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 1),
  lasso_grades[[2]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 2)) %>%
  rbind(lasso_grades[[3]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 3)) %>%
  rbind(lasso_grades[[4]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 4)) %>%
  rbind(lasso_grades[[5]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 5)) 

# replicate overall treatment effect
lasso_theta_detail %>%
  group_by(MICE) %>%
  summarize(theta_median = median(Treatment_Effect)) %>%
  ungroup() %>% pull(theta_median) %>% median()

lasso_theta_detail %>%
  group_by(MICE) %>%
  summarize(theta_mean = mean(Treatment_Effect)) %>%
  ungroup() %>% pull(theta_mean) %>% mean()


# for paper: intermediate results
lasso_theta_detail %>%
  group_by(Rep) %>%
  summarize(theta_median = median(Treatment_Effect)) 



## S = 10 ##
lasso_grades_10 <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-4_Rep10.rds")

lasso_theta_detail_10 <- rbind(
  lasso_grades_10[[1]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 1),
  lasso_grades_10[[2]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 2)) %>%
  rbind(lasso_grades_10[[3]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 3)) %>%
  rbind(lasso_grades_10[[4]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 4)) %>%
  rbind(lasso_grades_10[[5]]$detail %>% filter(Type == "ATE") %>% mutate(MICE = 5)) 

lasso_theta_detail_10 %>%
  group_by(Rep) %>%
  summarize(theta_median = median(Treatment_Effect)) 



## COMMON SUPPORT ##
#++++++++++++++++++#

# MAIN MODEL: No treatment and outcome lags #
lasso_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_sub <- left_join(lasso_grades[[mice_sel]]$pred, lasso_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred <- rbind(lasso_grades_pred, lasso_grades_pred_sub)
}

lasso_grades_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred, unique(lasso_grades_pred$min_trimming), 
  unique(lasso_grades_pred$max_trimming), "lasso")

lasso_grades_plot_common_support <- lasso_grades_plot_common_support + 
  ggtitle(bquote(paste(atop(bold(.("LASSO")), "Propensity Score Overlap without Treatment and Outcome Lags")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# RC: Treatment and outcome lags #
lasso_rc_treatoutlags <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_all_endogyes_trimming0.01_K4-2_Rep5.rds")


lasso_rc_treatoutlags_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_lasso_rc_treatoutlags_pred_sub <- left_join(lasso_rc_treatoutlags[[mice_sel]]$pred, lasso_rc_treatoutlags[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_rc_treatoutlags_pred <- rbind(lasso_rc_treatoutlags_pred, lasso_lasso_rc_treatoutlags_pred_sub)
}

lasso_rc_treatoutlags_plot_common_support <- func_dml_common_support(
  "binary", lasso_rc_treatoutlags_pred, unique(lasso_rc_treatoutlags_pred$min_trimming), 
  unique(lasso_rc_treatoutlags_pred$max_trimming), "lasso")


lasso_rc_treatoutlags_plot_common_support <- lasso_rc_treatoutlags_plot_common_support + 
  ggtitle("Propensity Score Overlap with Treatment and Outcome Lags") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# RC: No Lags at all #
lasso_rc_nolags <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_nolags_endogyes_trimming0.01_K4-2_Rep5.rds")

lasso_rc_nolags_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_lasso_rc_nolags_pred_sub <- left_join(lasso_rc_nolags[[mice_sel]]$pred, lasso_rc_nolags[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_rc_nolags_pred <- rbind(lasso_rc_nolags_pred, lasso_lasso_rc_nolags_pred_sub)
}

lasso_rc_nolags_plot_common_support <- func_dml_common_support(
  "binary", lasso_rc_nolags_pred, unique(lasso_rc_nolags_pred$min_trimming), 
  unique(lasso_rc_nolags_pred$max_trimming), "lasso")


lasso_rc_nolags_plot_common_support <- lasso_rc_nolags_plot_common_support + 
  ggtitle("Propensity Score Overlap without any Lags") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

ggarrange(lasso_grades_plot_common_support, lasso_rc_nolags_plot_common_support,
          lasso_rc_treatoutlags_plot_common_support,
          nrow = 3)


# MAIN BUT DIFFERENT TRIMMING #
lasso_grades_01 <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.1_K4-2_Rep5.rds")

lasso_grades_pred_01 <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_01_sub <- left_join(lasso_grades_01[[mice_sel]]$pred, lasso_grades_01[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred_01 <- rbind(lasso_grades_pred_01, lasso_grades_pred_01_sub)
}

lasso_grades_01_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred_01, unique(lasso_grades_pred_01$min_trimming), 
  unique(lasso_grades_pred_01$max_trimming), "lasso")

lasso_grades_01_plot_common_support <- lasso_grades_01_plot_common_support + 
  ggtitle("Propensity Score Overlap with Trimming Threshold of 0.1") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))



lasso_grades_minmax <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_lasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimmingmin-max_K4-2_Rep5.rds")

lasso_grades_pred_minmax <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_minmax_sub <- left_join(lasso_grades_minmax[[mice_sel]]$pred, lasso_grades_minmax[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred_minmax <- rbind(lasso_grades_pred_minmax, lasso_grades_pred_minmax_sub)
}

lasso_grades_minmax_plot_common_support <- func_dml_common_support(
  "binary", lasso_grades_pred_minmax, unique(lasso_grades_pred_minmax$min_trimming), 
  unique(lasso_grades_pred_minmax$max_trimming), "lasso")

lasso_grades_minmax_plot_common_support <- lasso_grades_minmax_plot_common_support + 
  ggtitle("Propensity Score Overlap with Min-Max Trimming") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))


# subplot
ggarrange(lasso_grades_plot_common_support + rremove("xlab") +
            ggtitle(bquote(paste(atop(bold(.("LASSO")), "Propensity Score Overlap with Trimming Threshold of 0.01")))) +
            theme(plot.title = element_text(hjust = 0.5, size = 10)),
          lasso_grades_01_plot_common_support + rremove("xlab"),
          lasso_grades_minmax_plot_common_support, 
          nrow = 3, common.legend = TRUE, legend = "bottom")


lasso_grades_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct()
lasso_grades_pred_01 %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)
lasso_grades_pred_minmax %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)


ggarrange(lasso_grades_plot_common_support + rremove("xlab"),
          lasso_rc_treatoutlags_plot_common_support, 
          nrow = 2, common.legend = TRUE, legend = "bottom")

lasso_grades_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct()
lasso_rc_treatoutlags_pred %>% dplyr::select(starts_with("n_treats")) %>% distinct() %>% arrange(n_treats_after)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DETAILED XGBoost ANALYSIS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#



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


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++ERROR METRICS++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# The error metrics obtained in the Excel files are calculated based on the
# standardized outcome variables. For better interpretability, the standardization
# is reversed.
# https://support.numxl.com/hc/en-us/articles/207841883-Transforming-standardized-values-to-non-standardized-values

# mean and standard deviation used for standardization (same across MICE data sets
# as outcome vars are not replaced as they do not contain any missing values)
data_stand_grade <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice1.rds")
data_stand_grade <- data_stand_grade %>%
  summarize(mean = mean(outcome_grade), sd = sd(outcome_grade)) 

data_stand_pers <- readRDS("Data/Personality/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice1_personality.rds")
data_stand_pers <- data_stand_pers %>%
  summarize(agree_mean = mean(bigfive_agreeableness), agree_sd = sd(bigfive_agreeableness),
            consc_mean = mean(bigfive_conscientiousness), consc_sd = sd(bigfive_conscientiousness),
            extra_mean = mean(bigfive_extraversion), extra_sd = sd(bigfive_extraversion),
            open_mean = mean(bigfive_openness), open_sd = sd(bigfive_openness),
            neuro_mean = mean(bigfive_neuroticism), neuro_sd = sd(bigfive_neuroticism)) 


#### GPA ####
#+++++++++++#

## BINARY TREATMENT SETTING ##

# errors from final data frame
df_error_grade <- df_dml_main_binary %>% filter(
  cohort_prep == "controls_same_outcome", treatment_repl == "down", treatment_def == "weekly",
  extra_act == "yes", model_type == "all", model_k == 4, model_k_tuning %in% c(1,2), model_s_rep == 5,
  model_trimming %in% c("0.01", 0.01), model_controls_lag == "no_treatment_outcome_lags", model_controls_endog == "yes",
) %>% dplyr::select(outcome, model_algo, ends_with("m"), ends_with("g0"), ends_with("g1")) %>%
  dplyr::select(-starts_with("num_pred")) %>% distinct()

# load predictions
lasso_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_pred_sub <- left_join(lasso_grades[[mice_sel]]$pred, lasso_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_pred <- rbind(lasso_grades_pred, lasso_grades_pred_sub)
}

postlasso_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  postlasso_grades_pred_sub <- left_join(postlasso_grades[[mice_sel]]$pred, postlasso_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  postlasso_grades_pred <- rbind(postlasso_grades_pred, postlasso_grades_pred_sub)
}

xgb_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  xgb_grades_pred_sub <- left_join(xgb_grades[[mice_sel]]$pred, xgb_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  xgb_grades_pred <- rbind(xgb_grades_pred, xgb_grades_pred_sub)
}

rf_grades_pred <- data.frame()
for (mice_sel in 1:5) {
  rf_grades_pred_sub <- left_join(rf_grades[[mice_sel]]$pred, rf_grades[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  rf_grades_pred <- rbind(rf_grades_pred, rf_grades_pred_sub)
}

# generate un-standardized outcome and the corresponding predictions
lasso_grades_pred <- lasso_grades_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g0_orig = g0*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean
  )

postlasso_grades_pred <- postlasso_grades_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g0_orig = g0*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean
  )

xgb_grades_pred <- xgb_grades_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g0_orig = g0*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean
  )

rf_grades_pred <- rf_grades_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g0_orig = g0*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean
  )

# calculate error metrics for paper
df_error_main_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
  "MAPE_g0" = yardstick::mape(lasso_grades_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
    dplyr::select(.estimate) %>% pull(), 
  "MAPE_g1" = yardstick::mape(lasso_grades_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
    dplyr::select(.estimate) %>% pull(),
  "RMSE_g0" = lasso_grades_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
  "RMSE_g1" = lasso_grades_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))

df_error_main_postlasso <- cbind(
  df_error_grade %>% filter(model_algo == "postlasso" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
  "MAPE_g0" = yardstick::mape(postlasso_grades_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
    dplyr::select(.estimate) %>% pull(), 
  "MAPE_g1" = yardstick::mape(postlasso_grades_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
    dplyr::select(.estimate) %>% pull(),
  "RMSE_g0" = postlasso_grades_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
  "RMSE_g1" = postlasso_grades_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
))

df_error_main_rf <- cbind(
  df_error_grade %>% filter(model_algo == "randomforests" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
  "MAPE_g0" = yardstick::mape(rf_grades_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
    dplyr::select(.estimate) %>% pull(), 
  "MAPE_g1" = yardstick::mape(rf_grades_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
    dplyr::select(.estimate) %>% pull(),
  "RMSE_g0" = rf_grades_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
  "RMSE_g1" = rf_grades_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
))

df_error_main_xgb <- cbind(
  df_error_grade %>% filter(model_algo == "xgboost" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
  "MAPE_g0" = yardstick::mape(xgb_grades_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
    dplyr::select(.estimate) %>% pull(), 
  "MAPE_g1" = yardstick::mape(xgb_grades_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
    dplyr::select(.estimate) %>% pull(),
  "RMSE_g0" = xgb_grades_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
  "RMSE_g1" = xgb_grades_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
))



## MULTIVALUED TREATMENT SETTING ##

# errors from final data frame
df_error_grade_multi <- df_dml_main_multi %>% filter(
  cohort_prep == "controls_same_outcome", treatment_repl == "down", treatment_def == "weekly",
  extra_act == "yes", model_type == "all", model_k == 4, model_k_tuning %in% c(1,2), model_s_rep == 5,
  model_trimming %in% c("0.01", 0.01), model_controls_lag == "no_treatment_outcome_lags", model_controls_endog == "yes",
) %>% dplyr::select(outcome, model_algo, ends_with("m1"), ends_with("m2"), ends_with("m3"), ends_with("g1"), ends_with("g2"), ends_with("g3")) %>%
  dplyr::select(-starts_with("num_pred")) %>% distinct()

# load predictions
lasso_grades_multi_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_grades_multi_pred_sub <- left_join(lasso_grades_multi[[mice_sel]]$pred, lasso_grades_multi[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_grades_multi_pred <- rbind(lasso_grades_multi_pred, lasso_grades_multi_pred_sub)
}

postlasso_grades_multi_pred <- data.frame()
for (mice_sel in 1:5) {
  postlasso_grades_multi_pred_sub <- left_join(postlasso_grades_multi[[mice_sel]]$pred, postlasso_grades_multi[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  postlasso_grades_multi_pred <- rbind(postlasso_grades_multi_pred, postlasso_grades_multi_pred_sub)
}

xgb_grades_multi_pred <- data.frame()
for (mice_sel in 1:5) {
  xgb_grades_multi_pred_sub <- left_join(xgb_grades_multi[[mice_sel]]$pred, xgb_grades_multi[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  xgb_grades_multi_pred <- rbind(xgb_grades_multi_pred, xgb_grades_multi_pred_sub)
}

rf_grades_multi_pred <- data.frame()
for (mice_sel in 1:5) {
  rf_grades_multi_pred_sub <- left_join(rf_grades_multi[[mice_sel]]$pred, rf_grades_multi[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  rf_grades_multi_pred <- rbind(rf_grades_multi_pred, rf_grades_multi_pred_sub)
}

# generate un-standardized outcome and the corresponding predictions
lasso_grades_multi_pred <- lasso_grades_multi_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean,
    g2_orig = g2*data_stand_grade$sd + data_stand_grade$mean,
    g3_orig = g3*data_stand_grade$sd + data_stand_grade$mean,
  )

postlasso_grades_multi_pred <- postlasso_grades_multi_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean,
    g2_orig = g2*data_stand_grade$sd + data_stand_grade$mean,
    g3_orig = g3*data_stand_grade$sd + data_stand_grade$mean,
  )

xgb_grades_multi_pred <- xgb_grades_multi_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean,
    g2_orig = g2*data_stand_grade$sd + data_stand_grade$mean,
    g3_orig = g3*data_stand_grade$sd + data_stand_grade$mean,
  )

rf_grades_multi_pred <- rf_grades_multi_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_grade$sd + data_stand_grade$mean,
    g1_orig = g1*data_stand_grade$sd + data_stand_grade$mean,
    g2_orig = g2*data_stand_grade$sd + data_stand_grade$mean,
    g3_orig = g3*data_stand_grade$sd + data_stand_grade$mean,
  )

# calculate error metrics for paper
df_error_grade_multis_multi_lasso <- cbind(
  df_error_grade_multi %>% filter(model_algo == "lasso" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g1" = yardstick::mape(lasso_grades_multi_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g2" = yardstick::mape(lasso_grades_multi_pred %>% filter(treatment == 2), truth = outcome_orig, estimate = g2_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g3" = yardstick::mape(lasso_grades_multi_pred %>% filter(treatment == 3), truth = outcome_orig, estimate = g3_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g1" = lasso_grades_multi_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g2" = lasso_grades_multi_pred %>% filter(treatment == 2) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g3" = lasso_grades_multi_pred %>% filter(treatment == 3) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  )) %>%
  mutate(
    AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
    MAPE = mean(MAPE_g1, MAPE_g2, MAPE_g3),  RMSE = mean(RMSE_g1, RMSE_g2, RMSE_g3)
  ) %>% dplyr::select(model_algo, outcome, AUC, BACC, RMSE, MAPE)

df_error_grade_multis_multi_postlasso <- cbind(
  df_error_grade_multi %>% filter(model_algo == "postlasso" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g1" = yardstick::mape(postlasso_grades_multi_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g2" = yardstick::mape(postlasso_grades_multi_pred %>% filter(treatment == 2), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g3" = yardstick::mape(postlasso_grades_multi_pred %>% filter(treatment == 3), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g1" = postlasso_grades_multi_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g2" = postlasso_grades_multi_pred %>% filter(treatment == 2) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g3" = postlasso_grades_multi_pred %>% filter(treatment == 3) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  )) %>%
  mutate(
    AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
    MAPE = mean(MAPE_g1, MAPE_g2, MAPE_g3),  RMSE = mean(RMSE_g1, RMSE_g2, RMSE_g3)
  ) %>% dplyr::select(model_algo, outcome, AUC, BACC, RMSE, MAPE)

df_error_grade_multis_multi_rf <- cbind(
  df_error_grade_multi %>% filter(model_algo == "randomforests" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g1" = yardstick::mape(rf_grades_multi_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g2" = yardstick::mape(rf_grades_multi_pred %>% filter(treatment == 2), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g3" = yardstick::mape(rf_grades_multi_pred %>% filter(treatment == 3), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g1" = rf_grades_multi_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g2" = rf_grades_multi_pred %>% filter(treatment == 2) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g3" = rf_grades_multi_pred %>% filter(treatment == 3) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  )) %>%
  mutate(
    AUC = mean(c(AUC_m1, AUC_m2, AUC_m3)), BACC = mean(c(BACC_m1, BACC_m2, BACC_m3)),
    MAPE = mean(MAPE_g1, MAPE_g2, MAPE_g3),  RMSE = mean(RMSE_g1, RMSE_g2, RMSE_g3)
  ) %>% dplyr::select(model_algo, outcome, AUC, BACC, RMSE, MAPE)

df_error_grade_multis_multi_xgb <- cbind(
  df_error_grade_multi %>% filter(model_algo == "xgboost" & outcome == "grade") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g1" = yardstick::mape(xgb_grades_multi_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g2" = yardstick::mape(xgb_grades_multi_pred %>% filter(treatment == 2), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "MAPE_g3" = yardstick::mape(xgb_grades_multi_pred %>% filter(treatment == 3), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g1" = xgb_grades_multi_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g2" = xgb_grades_multi_pred %>% filter(treatment == 2) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt(),
    "RMSE_g3" = xgb_grades_multi_pred %>% filter(treatment == 3) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))


#### Agreeableness ####
#+++++++++++++++++++++#

# load predictions
lasso_agree_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_agree_pred_sub <- left_join(lasso_agree[[mice_sel]]$pred, lasso_agree[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_agree_pred <- rbind(lasso_agree_pred, lasso_agree_pred_sub)
}


# generate un-standardized outcome and the corresponding predictions
lasso_agree_pred <- lasso_agree_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_pers$agree_sd + data_stand_pers$agree_mean,
    g0_orig = g0*data_stand_pers$agree_sd + data_stand_pers$agree_mean,
    g1_orig = g1*data_stand_pers$agree_sd + data_stand_pers$agree_mean
  )

# calculate error metrics for paper
df_error_agree_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "bigfive_agreeableness") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g0" = yardstick::mape(lasso_agree_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
      dplyr::select(.estimate) %>% pull(), 
    "MAPE_g1" = yardstick::mape(lasso_agree_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g0" = lasso_agree_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
    "RMSE_g1" = lasso_agree_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))


#### Conscientiousness ####
#+++++++++++++++++++++++++#

lasso_consc_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_consc_pred_sub <- left_join(lasso_consc[[mice_sel]]$pred, lasso_consc[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_consc_pred <- rbind(lasso_consc_pred, lasso_consc_pred_sub)
}


# generate un-standardized outcome and the corresponding predictions
lasso_consc_pred <- lasso_consc_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_pers$consc_sd + data_stand_pers$consc_mean,
    g0_orig = g0*data_stand_pers$consc_sd + data_stand_pers$consc_mean,
    g1_orig = g1*data_stand_pers$consc_sd + data_stand_pers$consc_mean
  )

# calculate error metrics for paper
df_error_consc_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "bigfive_conscientiousness") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g0" = yardstick::mape(lasso_consc_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
      dplyr::select(.estimate) %>% pull(), 
    "MAPE_g1" = yardstick::mape(lasso_consc_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g0" = lasso_consc_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
    "RMSE_g1" = lasso_consc_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))


#### Extraversion ####
#++++++++++++++++++++#

lasso_extra_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_extra_pred_sub <- left_join(lasso_extra[[mice_sel]]$pred, lasso_extra[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_extra_pred <- rbind(lasso_extra_pred, lasso_extra_pred_sub)
}


# generate un-standardized outcome and the corresponding predictions
lasso_extra_pred <- lasso_extra_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_pers$extra_sd + data_stand_pers$extra_mean,
    g0_orig = g0*data_stand_pers$extra_sd + data_stand_pers$extra_mean,
    g1_orig = g1*data_stand_pers$extra_sd + data_stand_pers$extra_mean
  )

# calculate error metrics for paper
df_error_extra_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "bigfive_extraversion") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g0" = yardstick::mape(lasso_extra_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
      dplyr::select(.estimate) %>% pull(), 
    "MAPE_g1" = yardstick::mape(lasso_extra_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g0" = lasso_extra_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
    "RMSE_g1" = lasso_extra_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))

#### Neuroticism ####
#+++++++++++++++++++#

lasso_neuro_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_neuro_pred_sub <- left_join(lasso_neuro[[mice_sel]]$pred, lasso_neuro[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_neuro_pred <- rbind(lasso_neuro_pred, lasso_neuro_pred_sub)
}


# generate un-standardized outcome and the corresponding predictions
lasso_neuro_pred <- lasso_neuro_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_pers$neuro_sd + data_stand_pers$neuro_mean,
    g0_orig = g0*data_stand_pers$neuro_sd + data_stand_pers$neuro_mean,
    g1_orig = g1*data_stand_pers$neuro_sd + data_stand_pers$neuro_mean
  )

# calculate error metrics for paper
df_error_neuro_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "bigfive_neuroticism") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g0" = yardstick::mape(lasso_neuro_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
      dplyr::select(.estimate) %>% pull(), 
    "MAPE_g1" = yardstick::mape(lasso_neuro_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g0" = lasso_neuro_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
    "RMSE_g1" = lasso_neuro_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))


#### Openness ####
#++++++++++++++++#

lasso_open_pred <- data.frame()
for (mice_sel in 1:5) {
  lasso_open_pred_sub <- left_join(lasso_open[[mice_sel]]$pred, lasso_open[[mice_sel]]$trimming, by = "Repetition") %>%
    mutate(MICE = mice_sel)
  lasso_open_pred <- rbind(lasso_open_pred, lasso_open_pred_sub)
}


# generate un-standardized outcome and the corresponding predictions
lasso_open_pred <- lasso_open_pred %>%
  mutate(
    outcome_orig = outcome*data_stand_pers$open_sd + data_stand_pers$open_mean,
    g0_orig = g0*data_stand_pers$open_sd + data_stand_pers$open_mean,
    g1_orig = g1*data_stand_pers$open_sd + data_stand_pers$open_mean
  )

# calculate error metrics for paper
df_error_open_lasso <- cbind(
  df_error_grade %>% filter(model_algo == "lasso" & outcome == "bigfive_openness") %>% dplyr::select(-starts_with("RMSE"), -starts_with("MAPE")),
  data.frame(
    "MAPE_g0" = yardstick::mape(lasso_open_pred %>% filter(treatment == 0), truth = outcome_orig, estimate = g0_orig) %>%
      dplyr::select(.estimate) %>% pull(), 
    "MAPE_g1" = yardstick::mape(lasso_open_pred %>% filter(treatment == 1), truth = outcome_orig, estimate = g1_orig) %>%
      dplyr::select(.estimate) %>% pull(),
    "RMSE_g0" = lasso_open_pred %>% filter(treatment == 0) %>% summarize(mean((outcome_orig - g0_orig)^2)) %>% pull() %>% sqrt(), 
    "RMSE_g1" = lasso_open_pred %>% filter(treatment == 1) %>% summarize(mean((outcome_orig - g1_orig)^2)) %>% pull() %>% sqrt()
  ))


#%%%%%%%%%%%%%%%%%%#
#### PREDICTORS ####
#%%%%%%%%%%%%%%%%%%#

#### Grades ####
#++++++++++++++#

## Binary Treatment Setting ##
df_dml_main_binary %>% 
  filter(cohort_prep == main_cohort_prep, treatment_repl == main_treatment_repl, treatment_def == main_treatment_def,
         extra_act == main_extra_act, model_type == main_model_type, str_detect(model_algo, "lasso"), 
         model_k == 4, model_k_tuning %in% c(1,2), model_s_rep == 5, model_trimming == 0.01,
         model_controls_lag == "no_treatment_outcome_lags", model_controls_endog == "yes") %>%
  dplyr::select(outcome, starts_with("model"), starts_with("num_predictors")) %>% distinct()

df_lasso_predictors <- 
  rbind(lasso_grades[[1]]$predictors, lasso_grades[[2]]$predictors) %>%
  rbind(lasso_grades[[3]]$predictors) %>%
  rbind(lasso_grades[[4]]$predictors) %>%
  rbind(lasso_grades[[5]]$predictors)
summary(df_lasso_predictors$num_pred_m)
summary(c(df_lasso_predictors$num_pred_g0, df_lasso_predictors$num_pred_g1))

df_postlasso_predictors <- 
  rbind(postlasso_grades[[1]]$predictors, postlasso_grades[[2]]$predictors) %>%
  rbind(postlasso_grades[[3]]$predictors) %>%
  rbind(postlasso_grades[[4]]$predictors) %>%
  rbind(postlasso_grades[[5]]$predictors)
summary(c(df_postlasso_predictors$num_pred_m, df_postlasso_predictors$num_pred_g0, df_postlasso_predictors$num_pred_g1))

## Multivalued Treatment Setting ##
df_dml_main_multi %>% dplyr::select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), starts_with("num_predictors")
) %>% distinct()


df_lasso_predictors_multi <- 
  rbind(lasso_grades_multi[[1]]$predictors, lasso_grades_multi[[2]]$predictors) %>%
  rbind(lasso_grades_multi[[3]]$predictors) %>%
  rbind(lasso_grades_multi[[4]]$predictors) %>%
  rbind(lasso_grades_multi[[5]]$predictors)
summary(c(df_lasso_predictors_multi$num_pred_m1, df_lasso_predictors_multi$num_pred_m2, df_lasso_predictors_multi$num_pred_m3))
summary(c(df_lasso_predictors_multi$num_pred_g1, df_lasso_predictors_multi$num_pred_g2, df_lasso_predictors_multi$num_pred_g3))

df_postlasso_predictors_multi <- 
  rbind(postlasso_grades_multi[[1]]$predictors, postlasso_grades_multi[[2]]$predictors) %>%
  rbind(postlasso_grades_multi[[3]]$predictors) %>%
  rbind(postlasso_grades_multi[[4]]$predictors) %>%
  rbind(postlasso_grades_multi[[5]]$predictors)
summary(c(df_postlasso_predictors_multi$num_pred_m1, df_postlasso_predictors_multi$num_pred_m2, df_postlasso_predictors_multi$num_pred_m3,
          df_postlasso_predictors_multi$num_pred_g1, df_postlasso_predictors_multi$num_pred_g2, df_postlasso_predictors_multi$num_pred_g3))


#### Agreeableness ####
#+++++++++++++++++++++#

## Binary ##
df_lasso_predictors_agree <- 
  rbind(lasso_agree[[1]]$predictors, lasso_agree[[2]]$predictors) %>%
  rbind(lasso_agree[[3]]$predictors) %>%
  rbind(lasso_agree[[4]]$predictors) %>%
  rbind(lasso_agree[[5]]$predictors)
summary(df_lasso_predictors_agree$num_pred_m)
summary(c(df_lasso_predictors_agree$num_pred_g0, df_lasso_predictors_agree$num_pred_g1))

df_postlasso_predictors_agree <- 
  rbind(postlasso_agree[[1]]$predictors, postlasso_agree[[2]]$predictors) %>%
  rbind(postlasso_agree[[3]]$predictors) %>%
  rbind(postlasso_agree[[4]]$predictors) %>%
  rbind(postlasso_agree[[5]]$predictors)
summary(df_postlasso_predictors_agree$num_pred_m)
summary(c(df_postlasso_predictors_agree$num_pred_g0, df_postlasso_predictors_agree$num_pred_g1))


## Multivalued ##


#### Extraversion ####
#++++++++++++++++++++#

## Binary ##

df_lasso_predictors_extra <- 
  rbind(lasso_extra[[1]]$predictors, lasso_extra[[2]]$predictors) %>%
  rbind(lasso_extra[[3]]$predictors) %>%
  rbind(lasso_extra[[4]]$predictors) %>%
  rbind(lasso_extra[[5]]$predictors)
summary(df_lasso_predictors_extra$num_pred_m)
summary(c(df_lasso_predictors_extra$num_pred_g0, df_lasso_predictors_extra$num_pred_g1))

df_postlasso_predictors_extra <- 
  rbind(postlasso_extra[[1]]$predictors, postlasso_extra[[2]]$predictors) %>%
  rbind(postlasso_extra[[3]]$predictors) %>%
  rbind(postlasso_extra[[4]]$predictors) %>%
  rbind(postlasso_extra[[5]]$predictors)
summary(df_postlasso_predictors_extra$num_pred_m)
summary(c(df_postlasso_predictors_extra$num_pred_g0, df_postlasso_predictors_extra$num_pred_g1))


## Multivalued ##


#### Openness ####
#++++++++++++++++#


## Binary ##
df_lasso_predictors_open <- 
  rbind(lasso_open[[1]]$predictors, lasso_open[[2]]$predictors) %>%
  rbind(lasso_open[[3]]$predictors) %>%
  rbind(lasso_open[[4]]$predictors) %>%
  rbind(lasso_open[[5]]$predictors)
summary(df_lasso_predictors_open$num_pred_m)
summary(c(df_lasso_predictors_open$num_pred_g0, df_lasso_predictors_open$num_pred_g1))

df_postlasso_predictors_open <- 
  rbind(postlasso_open[[1]]$predictors, postlasso_open[[2]]$predictors) %>%
  rbind(postlasso_open[[3]]$predictors) %>%
  rbind(postlasso_open[[4]]$predictors) %>%
  rbind(postlasso_open[[5]]$predictors)
summary(df_postlasso_predictors_open$num_pred_m)
summary(c(df_postlasso_predictors_open$num_pred_g0, postlasso_open$num_pred_g1))


## Multivalued ##



#### Conscientiousness ####
#++++++++++++++++++++++++#

## Binary ##
df_lasso_predictors_consc <- 
  rbind(lasso_consc[[1]]$predictors, lasso_consc[[2]]$predictors) %>%
  rbind(lasso_consc[[3]]$predictors) %>%
  rbind(lasso_consc[[4]]$predictors) %>%
  rbind(lasso_consc[[5]]$predictors)
summary(df_lasso_predictors_consc$num_pred_m)
summary(c(df_lasso_predictors_consc$num_pred_g0, df_lasso_predictors_consc$num_pred_g1))

df_postlasso_predictors_consc_multi <- 
  rbind(postlasso_consc_multi[[1]]$predictors, postlasso_consc_multi[[2]]$predictors) %>%
  rbind(postlasso_consc_multi[[3]]$predictors) %>%
  rbind(postlasso_consc_multi[[4]]$predictors) %>%
  rbind(postlasso_consc_multi[[5]]$predictors)
summary(df_postlasso_predictors_consc_multi$num_pred_m)
summary(c(df_postlasso_predictors_consc_multi$num_pred_g0, df_postlasso_predictors_consc_multi$num_pred_g1))


## Multivalued ##



#### Neuroticism ####
#+++++++++++++++++++#

## Binary ##
df_lasso_predictors_neuro <- 
  rbind(lasso_neuro[[1]]$predictors, lasso_neuro[[2]]$predictors) %>%
  rbind(lasso_neuro[[3]]$predictors) %>%
  rbind(lasso_neuro[[4]]$predictors) %>%
  rbind(lasso_neuro[[5]]$predictors)
summary(df_lasso_predictors_neuro$num_pred_m)
summary(c(df_lasso_predictors_neuro$num_pred_g0, df_lasso_predictors_neuro$num_pred_g1))

df_postlasso_predictors_neuro_multi <- 
  rbind(postlasso_neuro_multi[[1]]$predictors, postlasso_neuro_multi[[2]]$predictors) %>%
  rbind(postlasso_neuro_multi[[3]]$predictors) %>%
  rbind(postlasso_neuro_multi[[4]]$predictors) %>%
  rbind(postlasso_neuro_multi[[5]]$predictors)
summary(df_postlasso_predictors_neuro_multi$num_pred_m)
summary(c(df_postlasso_predictors_neuro_multi$num_pred_g0, df_postlasso_predictors_neuro_multi$num_pred_g1))


## Multivalued ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ++COMMON SUPPORT++ ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Check for dropped observations ####
#++++++++++++++++++++++++++++++++++++++#

## Binary Treatment Setting ##
df_dml_main_binary %>% dplyr::select(
  outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
  n_treats_before, n_treats_after
) %>% distinct() %>%
  filter(
    cohort_prep == "controls_same_outcome", treatment_repl == "down", treatment_def == "weekly", extra_act == "yes") %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after, n_treats_diff_perf = (n_treats_before - n_treats_after) / n_treats_before)


## Multivalued Treatment Setting ##
df_dml_main_multi %>% dplyr::select(
  cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), n_treats_before, n_treats_after
) %>% distinct() %>%
  mutate(n_treats_diff = n_treats_before - n_treats_after)


#### Create Plots ####
#++++++++++++++++++++#

## Binary Treatment Setting ##

list_binary_plot_common_support <- list()
# common support plot across all mice data frames and all repetitions
# All machine learning models and outcome are considered
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      # extract predictions
      df_dml_pred_all_binary <- data.frame()
      for (mice_sel in 1:5) {
        df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$pred %>% mutate(MICE = mice_sel) 
        df_dml_pred_all_binary <- rbind(df_dml_pred_all_binary, df_dml_pred_sub)
      }
      
      # extract trimming information
      df_dml_trimming_all_binary <- data.frame()
      for (mice_sel in 1:5) {
        df_dml_pred_sub <- get(load_pred_algo)[[mice_sel]]$trimming %>% mutate(MICE = mice_sel) 
        df_dml_trimming_all_binary <- rbind(df_dml_trimming_all_binary, df_dml_pred_sub)
      }
      
      df_dml_pred_all_binary_sub <- df_dml_pred_all_binary %>% 
        left_join(df_dml_trimming_all_binary, by = c("Repetition", "MICE"))
      
      binary_plot_common_support_sub <- func_dml_common_support(
        "binary", df_dml_pred_all_binary_sub, 
        unique(df_dml_pred_all_binary_sub$min_trimming), unique(df_dml_pred_all_binary_sub$max_trimming), 
        model_algo_sel)
      list_binary_plot_common_support[[outcome_var_sel]][[model_algo_sel]] <- binary_plot_common_support_sub
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
    
}

# Grades
binary_plot_common_support <- ggarrange(
  list_binary_plot_common_support$lasso + xlab("") + ggtitle("LASSO"), 
  list_binary_plot_common_support$postlasso + xlab("") + ggtitle("POST-LASSO"),
  list_binary_plot_common_support$rf + ggtitle("RANDOM FORESTS"), 
  list_binary_plot_common_support$xgb + ggtitle("XGBOOST"),
  nrow = 2, ncol = 2, common.legend = T, legend = "bottom"
) 
ggsave(paste0("Output/DML/Common_Support/dml_plot_common_support_binary_allalgos.png"), 
       binary_plot_common_support,
       width = 20, height = 15, dpi = 300, units = "in", device = 'png')


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


#### Check Means ####
#+++++++++++++++++++#

## Binary Treatment Setting ##
df_quantiles_binary <- data.frame()
for (model_algo_sel in c("lasso", "postlasso", "rf", "xgb")) {
  
  for (outcome_var_sel in c("grades", "agree", "extra", "consc", "open", "neuro")) {
    # name
    load_pred_algo <- paste0(model_algo_sel, "_", outcome_var_sel)
    
    tryCatch({
      print(load_pred_algo)
      if (exists(load_pred_algo) == FALSE) stop("Does not exist")
      
      df_quantiles_aggr <- data.frame()
      for (micel_sel in 1:5) {
        df_pred_m <- get(load_pred_algo)[[1]]$pred %>% dplyr::select(m, treatment) %>% arrange(m)
        df_quantiles <- df_pred_m %>% mutate(quint = ntile(m, 20))
        df_quantiles <- df_quantiles %>% 
          group_by(quint, treatment) %>% 
          summarize(mean_prob = mean(m)) %>%
          mutate(MICE = micel_sel)
        df_quantiles_aggr <- rbind(df_quantiles_aggr, df_quantiles) 
      }
      df_quantiles_aggr <- df_quantiles_aggr %>%
        group_by(quint, treatment) %>%
        summarize(mean_prob = mean(mean_prob)) %>%
        mutate(model_algo = model_algo_sel, outcome = outcome_var_sel) %>%
        dplyr::select(outcome, model_algo, everything())
      
      df_quantiles_binary <- rbind(df_quantiles_binary, df_quantiles_aggr)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
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











