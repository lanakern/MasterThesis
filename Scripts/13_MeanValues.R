#%%%%%%%%%%%%%%%%%%%#
#### Mean Values ####
#%%%%%%%%%%%%%%%%%%%#

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
  mutate(treatment_sport_freq = case_when(extracurricular_num == 0 & treatment_sport_freq == 3 ~ 4, 
                                          TRUE ~ treatment_sport_freq))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Feature Importance Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# variables exist
descr_vars <- descr_vars[descr_vars %in% colnames(data_all_mice_grades_multi_noextra)]
descr_vars <- c(descr_vars, "extracurricular_music", "interest_art_works", "interest_music_classic",
                "partner_educ_years", "partner_living_ger", "partner_current",
                "bigfive_agreeableness", "bigfive_openness", "bigfive_conscientiousness",
                "bigfive_neuroticism", "bigfive_extraversion", "interest_reading_num_books_more_than_500_books") %>% unique()


df_descr_imp_all <- data_all_mice_grades_multi_noextra %>%
  dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
  group_by(treatment_sport_freq) %>%
  summarize_all(mean) %>%
  gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
  spread(key = treatment_sport_freq, value = mean) %>%
  rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3", mean_inactive = "4") %>%
  mutate(control_var = case_when(
    control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
    TRUE ~ control_var
  )) %>% 
  left_join(
    data_all_mice_grades_multi_noextra %>%
      dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
      filter(treatment_sport_freq %in% c(1, 2)) %>%
      mutate(treatment_sport_freq = "any") %>%
      group_by(treatment_sport_freq) %>%
      summarize_all(mean) %>%
      gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
      spread(key = treatment_sport_freq, value = mean) %>%
      rename("mean_any" = "any") %>%
      mutate(control_var = case_when(
        control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
        TRUE ~ control_var
      )),
    by = "control_var"
  ) %>%
  left_join(
    data_all_mice_grades_multi_noextra %>%
      dplyr::select(treatment_sport_freq, all_of(descr_vars)) %>%
      filter(treatment_sport_freq %in% c(1, 2, 3)) %>%
      mutate(treatment_sport_freq = "active") %>%
      group_by(treatment_sport_freq) %>%
      summarize_all(mean) %>%
      gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
      spread(key = treatment_sport_freq, value = mean) %>%
      rename("mean_active" = "active") %>%
      mutate(control_var = case_when(
        control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
        TRUE ~ control_var
      )),
    by = "control_var"
  )

#df_descr_imp_all <- left_join(df_descr_imp, df_descr_imp_noextra, by = "control_var")

# add ASDM before dml
df_descr_imp_all <- left_join(df_descr_imp_all, data_asdm_all, by = "control_var") %>%
  filter(control_var %in% descr_vars) %>%
  dplyr::select(control_var, SD_before, starts_with("mean")) %>%
  arrange(-SD_before) %>%
  dplyr::select(control_var, SD_before, mean_inactive, mean_active, mean_never, mean_any, mean_monthly, mean_weekly) %>%
  as.data.frame()

# save
saveRDS(df_descr_imp_all, "Output/Descriptives/feature_imp_descr.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptives for Main Drivers of Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
    rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3", mean_inactive = "4") %>%
    mutate(control_var = case_when(
      control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
      TRUE ~ control_var
    )),
  by = "control_var"
) %>%
  left_join(
    df_descr_noextra %>%
      filter(treatment_sport_freq %in% c(1,2)) %>%
      mutate(treatment_sport_freq = "any") %>%
      summarize_all(mean) %>%
      gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
      spread(key = treatment_sport_freq, value = mean) %>%
      rename("mean_any" = "<NA>") %>%
      mutate(control_var = case_when(
        control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
        TRUE ~ control_var
      )),
    by = "control_var"
  ) %>%
  left_join(
    df_descr_noextra %>%
      filter(treatment_sport_freq %in% c(1,2,3)) %>%
      mutate(treatment_sport_freq = "active") %>%
      summarize_all(mean) %>%
      gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
      spread(key = treatment_sport_freq, value = mean) %>%
      rename("mean_active" = "<NA>") %>%
      mutate(control_var = case_when(
        control_var == "friends_study_share_(almost)half" ~ "friends_study_share_.almost.half",
        TRUE ~ control_var
      )),
    by = "control_var"
  ) %>%
  dplyr::select(control_var, SD_before, mean_inactive, mean_active, mean_never, 
                mean_any, mean_monthly, mean_weekly) %>%
  as.data.frame()

# save
saveRDS(df_descr_all, "Output/Descriptives/main_drivers_descr.rds")



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
) %>% rbind(
  left_join(
    data_all_mice_grades_multi_noextra %>%
      filter(MICE == 1, treatment_sport_freq %in% c(1, 2)) %>%
      mutate(treatment_sport_freq = case_when(treatment_sport_freq == 1 | treatment_sport_freq == 2 ~ "any", TRUE ~ "NA")) %>%
      dplyr::select(treatment_sport_freq, outcome_grade) %>% 
      group_by(treatment_sport_freq) %>%
      summarize_all(mean), 
    data_all_mice_grades_multi_noextra %>%
      filter(MICE == 1, treatment_sport_freq %in% c(1, 2)) %>%
      mutate(treatment_sport_freq = case_when(treatment_sport_freq == 1 | treatment_sport_freq == 2 ~ "any", TRUE ~ "NA")) %>%
      dplyr::select(treatment_sport_freq, outcome_grade) %>% 
      group_by(treatment_sport_freq) %>% count(),
    by = "treatment_sport_freq"
  ) 
) %>% rbind(
  left_join(
    data_all_mice_grades_multi_noextra %>%
      filter(MICE == 1, treatment_sport_freq %in% c(1, 2, 3)) %>%
      mutate(treatment_sport_freq = case_when(treatment_sport_freq == 1 | treatment_sport_freq == 2 | treatment_sport_freq == 3 ~ "active", 
                                              TRUE ~ "NA")) %>%
      dplyr::select(treatment_sport_freq, outcome_grade) %>% 
      group_by(treatment_sport_freq) %>%
      summarize_all(mean), 
    data_all_mice_grades_multi_noextra %>%
      filter(MICE == 1, treatment_sport_freq %in% c(1, 2, 3)) %>%
      mutate(treatment_sport_freq = case_when(treatment_sport_freq == 1 | treatment_sport_freq == 2 | treatment_sport_freq == 3 ~ "active", 
                                              TRUE ~ "NA")) %>%
      dplyr::select(treatment_sport_freq, outcome_grade) %>% 
      group_by(treatment_sport_freq) %>% count(),
    by = "treatment_sport_freq"
  ) 
) %>%
  mutate(treatment_sport_freq = case_when(
    treatment_sport_freq == 1 ~ "weekly", treatment_sport_freq == 2 ~ "monthly",
    treatment_sport_freq == 3 ~ "never", treatment_sport_freq == 4 ~ "inactive",
    treatment_sport_freq == "any" ~ "any", treatment_sport_freq == "active" ~ "active", 
    TRUE ~ "NA"
  )) %>%
  as.data.frame()


#### PERSONALITY ####
#+++++++++++++++++++#

df_descr_imp_pers <- left_join(
  data_all_mice_pers_multi_noextra %>%
  dplyr::select(treatment_sport_freq, starts_with("bigfive")) %>%
  group_by(treatment_sport_freq) %>%
  summarize_all(mean) %>%
  gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
  spread(key = treatment_sport_freq, value = mean) %>%
  rename("weekly" = "1", "monthly" = "2", "never" = "3", "inactive" = "4") %>%
  as.data.frame(),
  data_all_mice_pers_multi_noextra %>%
    dplyr::select(treatment_sport_freq, starts_with("bigfive")) %>%
    filter(treatment_sport_freq %in% c(1, 2)) %>%
    mutate(treatment_sport_freq = "any") %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename("any" = `<NA>`) %>%
    as.data.frame(),
  by = "control_var"
) %>% left_join(
  data_all_mice_pers_multi_noextra %>%
    dplyr::select(treatment_sport_freq, starts_with("bigfive")) %>%
    filter(treatment_sport_freq %in% c(1, 2, 3)) %>%
    mutate(treatment_sport_freq = "active") %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename("active" = `<NA>`) %>%
    as.data.frame(),
  by = "control_var"
) %>%
  dplyr::select(control_var, inactive, active, never, any, monthly, weekly)


data_all_mice_pers_multi %>% filter(MICE == 1) %>% count(treatment_sport_freq)
data_all_mice_pers_multi_noextra %>% filter(MICE == 1) %>% count(treatment_sport_freq)


