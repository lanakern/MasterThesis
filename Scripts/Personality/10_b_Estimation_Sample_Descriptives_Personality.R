#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE DESCRIPTIVES FOR PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the different estimation samples are analyzed according
# to their number of students, observations, treatment and control group sizes
# and number of variables. To do so, only one MICE data set is loaded as these
# characteristics are identical across those (MICE data sets only differ
# across control variables).
#+++


# user selection
if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}

if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### NUMBER OF OBS AND COLS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

c
print(paste("Number of respondents:", length(unique(data_main$group))))
print(paste("Number of observations:", nrow(data_main)))
print(paste("Number of observations for sport participations:", nrow(data_main %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_main %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_main)))


#%%%%%%%%%%%%%%%%%%%%%%%#
#### MEAN COMPARISON ####
#%%%%%%%%%%%%%%%%%%%%%%%#


## BINARY ##
#++++++++++#

# calculate mean and p-values
data_outcome_descr <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_mean_comp(df = data_main, y_variables = outcome_var_sel, treatment_setting = "binary")
  data_outcome_descr_sub <- data_outcome_descr_sub %>% dplyr::select(-median)
  data_outcome_descr <- rbind(data_outcome_descr, data_outcome_descr_sub)
}


# calculate minimum, maximum, median etc.
data_outcome_descr_2 <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_summary_stats(data_main, "treatment_sport", outcome_var_sel)
  data_outcome_descr_2 <- rbind(data_outcome_descr_2, data_outcome_descr_sub)
}


# merge
data_outcome_descr <- left_join(
  data_outcome_descr, data_outcome_descr_2, by = c("variable", "treatment_sport")
) %>%
  dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                treatment_sport, num_obs, min, max, median, mean, everything())

# save
if (file.exists("Output/Descriptives/Grades/MEAN_COMPARISON_BINARY.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/Grades/MEAN_COMPARISON_BINARY.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr) %>%
    group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/Grades/MEAN_COMPARISON_BINARY.rds")
} else {
  saveRDS(data_outcome_descr, "Output/Descriptives/Grades/MEAN_COMPARISON_BINARY.rds")
}



## MULTI ##
#+++++++++#

data_descr <- readRDS("Data/Personality/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop_covbal_mice1_personality.rds")

data_outcome_descr <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_mean_comp(df = data_descr, y_variables = outcome_var_sel, treatment_setting = "multi")
  data_outcome_descr_sub <- data_outcome_descr_sub %>% dplyr::select(-median)
  data_outcome_descr <- rbind(data_outcome_descr, data_outcome_descr_sub)
}

data_outcome_descr_2 <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_summary_stats(data_descr, "treatment_sport_freq", outcome_var_sel) 
  data_outcome_descr_2 <- rbind(data_outcome_descr_2, data_outcome_descr_sub)
}

# merge
data_outcome_descr <- left_join(
  data_outcome_descr, data_outcome_descr_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                    treatment_sport_freq, num_obs, min, max, median, mean, everything())

if (file.exists("Output/Descriptives/Grades/MEAN_COMPARISON_MULTI.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/Grades/MEAN_COMPARISON_MULTI.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr) %>%
    group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/Grades/MEAN_COMPARISON_MULTI.rds")
} else {
  saveRDS(data_outcome_descr, "Output/Descriptives/Grades/MEAN_COMPARISON_MULTI.rds")
} 