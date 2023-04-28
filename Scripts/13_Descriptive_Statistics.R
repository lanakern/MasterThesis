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

# load most important variables according to feature importance
descr_vars <- readRDS("Output/Descriptives/descr_vars.rds") 

# load main drivers of selection
data_main_drivers <- readRDS("Output/DML/Covariate_Balancing/main_drivers.rds") 

# load data
## main
data_all_mice_grades_multi <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop_covbal",
                      "_mice", mice_data_sel, ".rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_grades_multi <- rbind(data_all_mice_grades_multi, data_all_mice_sub)
}
## no extra curricular activity
data_all_mice_grades_multi_noextra <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_weekly_down",
                      cov_balance_save, "_mice", mice_data_sel, ".rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_grades_multi_noextra <- rbind(data_all_mice_grades_multi_noextra, data_all_mice_sub)
}
data_all_mice_grades_multi_noextra <- data_all_mice_grades_multi_noextra %>%
  filter(extracurricular_num == 0 & treatment_sport_freq == 3)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Feature Importance Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# drop main drivers for selection
descr_vars <- descr_vars[!descr_vars %in% unique(data_main_drivers$control_var)]

# variables exist
descr_vars <- descr_vars[descr_vars %in% colnames(data_all_mice_grades_multi)]

# calculate descriptives
df_descr_imp <- data_all_mice_grades_multi %>%
    dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3") %>%
    mutate(control_var = case_when(
      control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
      TRUE ~ control_var
    ))

df_descr_imp_noextra <- data_all_mice_grades_multi_noextra %>%
  dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
  group_by(treatment_sport_freq) %>%
  summarize_all(mean) %>%
  gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
  spread(key = treatment_sport_freq, value = mean) %>%
  rename(mean_noextra = "3") %>%
  mutate(control_var = case_when(
    control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
    TRUE ~ control_var
  ))

df_descr_imp_all <- left_join(df_descr_imp, df_descr_imp_noextra, by = "control_var")

# save
saveRDS(df_descr_imp_all %>% as.data.frame(), "Output/Descriptives/feature_imp_descr.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Main Drivers of Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

cols_multi <- colnames(data_all_mice_grades_multi)[colnames(data_all_mice_grades_multi) %in% data_main_drivers$control_var]
cols_multi <- c(cols_multi, "friends_study_share_(almost)half")

df_descr <- data_all_mice_grades_multi %>% dplyr::select(treatment_sport_freq, all_of(cols_multi))

df_descr <- left_join(
  data_main_drivers, 
  df_descr %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3") %>%
    mutate(control_var = case_when(
      control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
      TRUE ~ control_var
    )),
  by = "control_var"
)


# add descriptives for students who are not physically active
cols_multi <- colnames(data_all_mice_grades_multi_noextra)[colnames(data_all_mice_grades_multi_noextra) %in% data_main_drivers$control_var]
cols_multi <- c(cols_multi, "friends_study_share_(almost)half")

df_descr_noextra <- data_all_mice_grades_multi_noextra %>% dplyr::select(treatment_sport_freq, all_of(cols_multi))

df_descr_noextra <- left_join(
  data_main_drivers, 
  df_descr_noextra %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename(mean_noextra = "3") %>%
    mutate(control_var = case_when(
      control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
      TRUE ~ control_var
    )),
  by = "control_var"
)

df_descr_all <- left_join(
  df_descr %>% dplyr::select(-outcome), 
  df_descr_noextra %>% dplyr::select(-c("outcome", "SD_before", "SD_after")), 
  by = "control_var")

# save
saveRDS(df_descr_all %>% as.data.frame(), "Output/Descriptives/main_drivers_descr.rds")





