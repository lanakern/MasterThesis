#%%%%%%%%%%%%%%%%%%%%%%%%%%##
#### MEAN VALUES FOR RCs ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the mean comparison across sport participants and non-participants
# is conducted for the robustness checks wrt the sample.
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

## Inactive ###
data_rc_inactive <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_covbal_mice1.rds"))
data_descr_multi_stand <- data_rc_inactive %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_multi_stand <- func_mean_comp(df = data_descr_multi_stand, y_variables = "outcome_grade", treatment_setting = "multi") %>% 
  dplyr::select(-c(median, cohort_prep, treatment_repl, treatment_def, extra_act_save)) %>%
  mutate("RC" = "Inactive")

data_outcome_descr_multi_stand_2 <- func_summary_stats(data_descr_multi_stand, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

df_rc_all <- left_join(
  data_outcome_descr_multi_stand, data_outcome_descr_multi_stand_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, treatment_sport_freq, num_obs, min, max, median, mean, everything())

df_rc_se <- func_mean_comp_se(data_descr_multi_stand) %>% mutate("RC" = "Inactive")


## Extra act. within uni ##
data_rc_extrauni <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extrauni_covbal_mice1.rds"))
data_descr_multi_stand <- data_rc_extrauni %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_multi_stand <- func_mean_comp(df = data_descr_multi_stand, y_variables = "outcome_grade", treatment_setting = "multi") %>% 
  dplyr::select(-c(median, cohort_prep, treatment_repl, treatment_def, extra_act_save)) %>%
  mutate("RC" = "ExtraUni")

data_outcome_descr_multi_stand_2 <- func_summary_stats(data_descr_multi_stand, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

df_rc_all <- rbind(df_rc_all, left_join(
  data_outcome_descr_multi_stand, data_outcome_descr_multi_stand_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, treatment_sport_freq, num_obs, min, max, median, mean, everything()))

df_rc_se <- rbind(df_rc_se, func_mean_comp_se(data_descr_multi_stand) %>% mutate("RC" = "ExtraUni"))


## Controls before outcome ##
data_rc_controls_bef_outcome <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop_covbal_robustcheck_mice1.rds"))
data_descr_multi_stand <- data_rc_controls_bef_outcome %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_multi_stand <- func_mean_comp(df = data_descr_multi_stand, y_variables = "outcome_grade", treatment_setting = "multi") %>% 
  dplyr::select(-c(median, cohort_prep, treatment_repl, treatment_def, extra_act_save)) %>%
  mutate("RC" = "Controls_bef_Outcome")

data_outcome_descr_multi_stand_2 <- func_summary_stats(data_descr_multi_stand, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

df_rc_all <- rbind(df_rc_all, left_join(
  data_outcome_descr_multi_stand, data_outcome_descr_multi_stand_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, treatment_sport_freq, num_obs, min, max, median, mean, everything()))

df_rc_se <- rbind(df_rc_se, func_mean_comp_se(data_descr_multi_stand) %>% mutate("RC" = "Controls_bef_Outcome"))

## Controls before all ##
data_rc_controls_bef_all <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop_covbal_robustcheck_controls_bef_all_mice1.rds"))
data_descr_multi_stand <- data_rc_controls_bef_all %>%
  recipe(.) %>%
  step_normalize(outcome_grade) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()
data_outcome_descr_multi_stand <- func_mean_comp(df = data_descr_multi_stand, y_variables = "outcome_grade", treatment_setting = "multi") %>% 
  dplyr::select(-c(median, cohort_prep, treatment_repl, treatment_def, extra_act_save)) %>%
  mutate("RC" = "Controls_bef_all")

data_outcome_descr_multi_stand_2 <- func_summary_stats(data_descr_multi_stand, "treatment_sport_freq", "outcome_grade") %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

df_rc_all <- rbind(df_rc_all, left_join(
  data_outcome_descr_multi_stand, data_outcome_descr_multi_stand_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, treatment_sport_freq, num_obs, min, max, median, mean, everything()))

df_rc_se <- rbind(df_rc_se, func_mean_comp_se(data_descr_multi_stand) %>% mutate("RC" = "Controls_bef_all"))

# save
saveRDS(df_rc_all, "Output/Descriptives/MULTI_MEAN_COMPARISON_RC.rds")
saveRDS(df_rc_se, "Output/Descriptives/MULTI_MEAN_COMPARISON_RC_SE.rds")