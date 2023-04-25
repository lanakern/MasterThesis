#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE DESCRIPTIVES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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

## Main Model ##
#++++++++++++++#

data_main <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_covbal_mice1.rds")
print(paste("Number of respondents:", length(unique(data_main$group))))
print(paste("Number of observations:", nrow(data_main)))
print(paste("Number of observations for sport participations:", nrow(data_main %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_main %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_main)))


## RC 1: All Sport Participants ##
#++++++++++++++++++++++++++++++++#

data_rc_1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_all_down_extradrop_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_1$group))))
print(paste("Number of observations:", nrow(data_rc_1)))
print(paste("Number of observations for sport participations:", nrow(data_rc_1 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_1 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_1)))


## RC 2: No LVCF ##
#+++++++++++++++++#

data_rc_2 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_no_extradrop_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_2$group))))
print(paste("Number of observations:", nrow(data_rc_2)))
print(paste("Number of observations for sport participations:", nrow(data_rc_2 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_2 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_2)))


## RC 3: No ExtraActivity drop ##
#+++++++++++++++++++++++++++++++#

data_rc_3 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_3$group))))
print(paste("Number of observations:", nrow(data_rc_3)))
print(paste("Number of observations for sport participations:", nrow(data_rc_3 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_3 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_3)))


## RC 5: Controls-Treatment Before Outcome ##
#+++++++++++++++++++++++++++++++++++++++++++#

data_rc_4 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_robustcheck_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_4$group))))
print(paste("Number of observations:", nrow(data_rc_4)))
print(paste("Number of observations for sport participations:", nrow(data_rc_4 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_4 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_4)))



#%%%%%%%%%%%%%%%%%%%%%%%#
#### MEAN COMPARISON ####
#%%%%%%%%%%%%%%%%%%%%%%%#


## BINARY ##
#++++++++++#

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

if (cohort_prep == "controls_same_outcome") {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_mice1.rds")
} else {
  load_data <- 
    paste0("Data/Grades/Prep_10/prep_10_dml_multi_", model_type, "_", treatment_def, 
           "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice1.rds")
}

data_descr <- readRDS(load_data)

data_outcome_descr <- 
  func_mean_comp(df = data_descr, y_variables = "outcome_grade", treatment_setting = "multi")
data_outcome_descr <- data_outcome_descr %>% dplyr::select(-median)

# calculate minimum, maximum, median etc.
data_outcome_descr_2 <- func_summary_stats(data_descr, "treatment_sport_freq", "outcome_grade") 

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


