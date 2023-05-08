#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE DESCRIPTIVES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the mean comparison across sport participants and non-participants
# is conducted.
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


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

data_main <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_down_extradrop_covbal_mice1.rds"))
print(paste("Number of respondents:", length(unique(data_main$group))))
print(paste("Number of observations:", nrow(data_main)))
print(paste("Number of observations for sport participations:", nrow(data_main %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_main %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_main)))



#%%%%%%%%%%%%%%%%%%%%%%%#
#### MEAN COMPARISON ####
#%%%%%%%%%%%%%%%%%%%%%%%#


#### BINARY ####
#%%%%%%%%%%%%%%#

# load data
if (cohort_prep == "controls_same_outcome") {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1.rds")
} else {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice1.rds")
}
  
data_descr <- readRDS(load_data)


## LEVEL ##

# calculate mean and p-values
data_outcome_descr <- 
  func_mean_comp(df = data_descr, y_variables = "outcome_grade", treatment_setting = "binary")
data_outcome_descr <- data_outcome_descr %>% dplyr::select(-median)

# calculate minimum, maximum, median etc.
data_outcome_descr_2 <- func_summary_stats(data_descr, "treatment_sport", "outcome_grade")

# merge
data_outcome_descr <- left_join(
  data_outcome_descr, data_outcome_descr_2, by = c("variable", "treatment_sport")
) %>%
  dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                treatment_sport, num_obs, min, max, median, mean, everything())


## STANDARDIZED ##
data_descr_stand <- data_descr %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_stand <- func_mean_comp(df = data_descr_stand, y_variables = "outcome_grade", treatment_setting = "binary") %>% dplyr::select(-median)

data_outcome_descr_stand_2 <- func_summary_stats(data_descr_stand, "treatment_sport", "outcome_grade")

data_outcome_descr_stand <- left_join(
  data_outcome_descr_stand, data_outcome_descr_stand_2, by = c("variable", "treatment_sport")
) %>%
  dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                treatment_sport, num_obs, min, max, median, mean, everything())


## COMBINE ##
data_outcome_descr_all <- rbind(
  data_outcome_descr %>% mutate(variable_measure = "level"), 
  data_outcome_descr_stand %>% mutate(variable_measure = "stand")
) %>% 
  mutate(treatment_setting = "binary") %>%
  rename(treatment = treatment_sport) %>%
  dplyr::select(treatment_setting, variable, variable_measure, everything())


# save
if (file.exists("Output/Descriptives/BINARY_MEAN_COMPARISON.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/BINARY_MEAN_COMPARISON.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_all) %>%
    group_by(treatment_setting, variable, variable_measure, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/BINARY_MEAN_COMPARISON.rds")
} else {
  saveRDS(data_outcome_descr_all, "Output/Descriptives/BINARY_MEAN_COMPARISON.rds")
}



#### MULTI ####
#%%%%%%%%%%%%%#


# load data 
if (cohort_prep == "controls_same_outcome") {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1.rds")
} else {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice1.rds")
}

data_descr_multi <- readRDS(load_data)


## LEVEL ##
data_outcome_descr_multi <- 
  func_mean_comp(df = data_descr_multi, y_variables = "outcome_grade", treatment_setting = "multi")
data_outcome_descr_multi <- data_outcome_descr_multi %>% dplyr::select(-median)

data_outcome_descr_multi_2 <- func_summary_stats(data_descr_multi, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

data_outcome_descr_multi <- left_join(
  data_outcome_descr_multi, data_outcome_descr_multi_2, by = c("variable", "treatment_sport_freq")
  ) %>% dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                      treatment_sport_freq, num_obs, min, max, median, mean, everything())
                                           
                                        
## Standardized ###
data_descr_multi_stand <- data_descr_multi %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_multi_stand <- func_mean_comp(df = data_descr_multi_stand, y_variables = "outcome_grade", treatment_setting = "multi") %>% 
  dplyr::select(-median)

data_outcome_descr_multi_stand_2 <- func_summary_stats(data_descr_multi_stand, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

data_outcome_descr_multi_stand <- left_join(
  data_outcome_descr_multi_stand, data_outcome_descr_multi_stand_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                    treatment_sport_freq, num_obs, min, max, median, mean, everything())


## Combine ##
data_outcome_descr_all_multi <- rbind(
  data_outcome_descr_multi %>% mutate(variable_measure = "level"), 
  data_outcome_descr_multi_stand %>% mutate(variable_measure = "stand")
) %>% 
  mutate(treatment_setting = "multi") %>%
  rename(treatment = treatment_sport_freq) %>%
  dplyr::select(treatment_setting, variable, variable_measure, everything())


# save
if (file.exists("Output/Descriptives/MULTI_MEAN_COMPARISON.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_all_multi) %>%
    group_by(treatment_setting, variable, variable_measure, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
} else {
  saveRDS(data_outcome_descr_all_multi, "Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
}