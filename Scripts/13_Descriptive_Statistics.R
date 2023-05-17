#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, mean values are compared across sport participation frequency
# levels for the most useful variables for the prediction determined by the
# feature importance as well as the main drivers for selection.
#+++


#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}

# load main drivers of selection
data_main_drivers <- readRDS("Output/DML/Covariate_Balancing/main_drivers.rds") 

# load ASDM before DML
data_asdm_all <- readRDS("Output/DML/Covariate_Balancing/covariate_balancing_asdm_multi.rds") %>%
  filter(outcome == "grades") %>% dplyr::select(-outcome) %>%
  arrange(-SD_before) %>%
  group_by(control_var) %>% 
  summarize_all(mean)

# load most important variables according to feature importance
# keep only those which are not included in main drivers of selection and those
# which are important in at least two models
descr_vars <- readRDS("Output/Descriptives/descr_vars.rds") 
descr_vars <- descr_vars[!descr_vars %in% unique(data_main_drivers$control_var)]
descr_vars <- names(table(descr_vars)[table(descr_vars) >= 2])
length(descr_vars)
descr_vars

# load data
## main GPA
# data_all_mice_grades_multi <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop_covbal",
#                       "_mice", mice_data_sel, ".rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
#   data_all_mice_grades_multi <- rbind(data_all_mice_grades_multi, data_all_mice_sub)
# }
## no extra curricular activity GPA
data_all_mice_grades_multi_noextra <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down",
                      cov_balance_save, "_mice", mice_data_sel, ".rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_grades_multi_noextra <- rbind(data_all_mice_grades_multi_noextra, data_all_mice_sub)
}
data_all_mice_grades_multi_noextra <- data_all_mice_grades_multi_noextra %>%
  mutate(treatment_sport_freq = case_when(extracurricular_num == 0 & treatment_sport_freq == 3 ~ 4, 
                                          TRUE ~ treatment_sport_freq))
# filter(extracurricular_num == 0 & treatment_sport_freq == 3)

## main personality
# data_all_mice_pers_multi <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_all_down_extradrop_covbal",
#                       "_mice", mice_data_sel, "_personality.rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel) %>%
#     dplyr::select(treatment_sport_freq, extracurricular_num, starts_with("bigfive"), MICE)
#   data_all_mice_pers_multi <- rbind(data_all_mice_pers_multi, data_all_mice_sub)
# }
## no extra curricular activity personality
data_all_mice_pers_multi_noextra <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_all_down",
                      cov_balance_save, "_mice", mice_data_sel, "_personality.rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel) %>%
    dplyr::select(treatment_sport_freq, extracurricular_num, starts_with("bigfive"), MICE)
  data_all_mice_pers_multi_noextra <- rbind(data_all_mice_pers_multi_noextra, data_all_mice_sub)
}
data_all_mice_pers_multi_noextra <- data_all_mice_pers_multi_noextra %>%
  filter(extracurricular_num == 0 & treatment_sport_freq == 3)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Feature Importance Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# variables exist
descr_vars <- descr_vars[descr_vars %in% colnames(data_all_mice_grades_multi_noextra)]

# calculate descriptives
# df_descr_imp <- data_all_mice_grades_multi %>%
#     dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
#     group_by(treatment_sport_freq) %>%
#     summarize_all(mean) %>%
#     gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
#     spread(key = treatment_sport_freq, value = mean) %>%
#     rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3") %>%
#     mutate(control_var = case_when(
#       control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
#       TRUE ~ control_var
#     ))

df_descr_imp_all <- data_all_mice_grades_multi_noextra %>%
  dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
  group_by(treatment_sport_freq) %>%
  summarize_all(mean) %>%
  gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
  spread(key = treatment_sport_freq, value = mean) %>%
  rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3", mean_noextra = "4") %>%
  mutate(control_var = case_when(
    control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
    TRUE ~ control_var
  ))

#df_descr_imp_all <- left_join(df_descr_imp, df_descr_imp_noextra, by = "control_var")

# add ASDM before dml
df_descr_imp_all <- left_join(df_descr_imp_all, data_asdm_all, by = "control_var") %>%
  filter(control_var %in% descr_vars) %>%
  dplyr::select(control_var, SD_before, starts_with("mean")) %>%
  arrange(-SD_before)

# save
saveRDS(df_descr_imp_all %>% as.data.frame(), "Output/Descriptives/feature_imp_descr.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Main Drivers of Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# cols_multi <- colnames(data_all_mice_grades_multi)[colnames(data_all_mice_grades_multi) %in% data_main_drivers$control_var]
# cols_multi <- c(cols_multi, "friends_study_share_(almost)half")
# 
# df_descr <- data_all_mice_grades_multi %>% dplyr::select(treatment_sport_freq, all_of(cols_multi))
# 
# df_descr <- left_join(
#   data_main_drivers, 
#   df_descr %>%
#     group_by(treatment_sport_freq) %>%
#     summarize_all(mean) %>%
#     gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
#     spread(key = treatment_sport_freq, value = mean) %>%
#     rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3") %>%
#     mutate(control_var = case_when(
#       control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
#       TRUE ~ control_var
#     )),
#   by = "control_var"
# )


# add descriptives for students who are not physically active
cols_multi <- colnames(data_all_mice_grades_multi_noextra)[colnames(data_all_mice_grades_multi_noextra) %in% data_main_drivers$control_var]
cols_multi <- c(cols_multi, "friends_study_share_(almost)half")

df_descr_noextra <- data_all_mice_grades_multi_noextra %>% dplyr::select(treatment_sport_freq, all_of(cols_multi))

df_descr_all <- left_join(
  data_main_drivers, 
  df_descr_noextra %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3", mean_noextra = "4") %>%
    mutate(control_var = case_when(
      control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
      TRUE ~ control_var
    )),
  by = "control_var"
)

# df_descr_all <- left_join(
#   df_descr %>% dplyr::select(-outcome), 
#   df_descr_noextra %>% dplyr::select(-c("outcome", "SD_before", "SD_after")), 
#   by = "control_var")

# save
saveRDS(df_descr_all %>% as.data.frame(), "Output/Descriptives/main_drivers_descr.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Outcome Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### GPA ####
#+++++++++++#

left_join(
  data_all_mice_grades_multi_noextra %>%
    filter(MICE == 1) %>%
    mutate(treatment_sport_freq = case_when(treatment_sport_freq == 3 & extracurricular_num == 0 ~ 4, 
                                            TRUE ~ treatment_sport_freq)) %>%
    dplyr::select(treatment_sport_freq, outcome_grade) %>% 
    group_by(treatment_sport_freq) %>%
    summarize_all(mean), 
  data_all_mice_grades_multi_noextra %>%
    filter(MICE == 1) %>%
    mutate(treatment_sport_freq = case_when(treatment_sport_freq == 3 & extracurricular_num == 0 ~ 4, 
                                            TRUE ~ treatment_sport_freq)) %>%
    group_by(treatment_sport_freq) %>% count(),
  by = "treatment_sport_freq"
) %>% as.data.frame()


#### PERSONALITY ####
#+++++++++++++++++++#

df_descr_imp_pers <- data_all_mice_pers_multi %>%
  dplyr::select(treatment_sport_freq, starts_with("bigfive")) %>%
  group_by(treatment_sport_freq) %>%
  summarize_all(mean) %>%
  gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
  spread(key = treatment_sport_freq, value = mean) %>%
  rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3") %>% left_join(
    data_all_mice_pers_multi_noextra %>%
      dplyr::select(treatment_sport_freq, starts_with("bigfive")) %>%
      group_by(treatment_sport_freq) %>%
      summarize_all(mean) %>%
      gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
      spread(key = treatment_sport_freq, value = mean) %>%
      rename(mean_extra = "3"),
    by = "control_var"
  )


data_all_mice_pers_multi %>% filter(MICE == 1) %>% count(treatment_sport_freq)
data_all_mice_pers_multi_noextra %>% filter(MICE == 1) %>% count(treatment_sport_freq)


