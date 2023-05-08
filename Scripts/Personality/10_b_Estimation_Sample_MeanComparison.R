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


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

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

data_main <- readRDS("Data/Personality/Prep_10/prep_10_dml_binary_all_all_down_extradrop_covbal_mice1_personality.rds")

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

## LEVEL ##

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


## Standardized ##
data_descr_stand <- data_main %>%
  recipe(.) %>%
  step_normalize(c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                   "bigfive_openness", "bigfive_neuroticism")) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()

data_outcome_descr_stand <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_mean_comp(df = data_descr_stand, y_variables = outcome_var_sel, treatment_setting = "binary")
  data_outcome_descr_stand <- rbind(data_outcome_descr_stand, data_outcome_descr_sub)
}

data_outcome_descr_stand_2 <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_summary_stats(data_descr_stand, "treatment_sport", outcome_var_sel)
  data_outcome_descr_stand_2 <- rbind(data_outcome_descr_stand_2, data_outcome_descr_sub)
}

data_outcome_descr_stand <- left_join(
  data_outcome_descr_stand %>% dplyr::select(-median), 
  data_outcome_descr_stand_2, by = c("variable", "treatment_sport")
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


## MULTI ##
#+++++++++#

data_descr_multi <- readRDS("Data/Personality/Prep_10/prep_10_dml_multi_all_all_down_extradrop_covbal_mice1_personality.rds")

## LEVEL ##
data_outcome_descr_multi <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_mean_comp(df = data_descr_multi, y_variables = outcome_var_sel, treatment_setting = "multi")
  data_outcome_descr_sub <- data_outcome_descr_sub %>% dplyr::select(-median)
  data_outcome_descr_multi <- rbind(data_outcome_descr_multi, data_outcome_descr_sub)
}

data_outcome_descr_multi_2 <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_summary_stats(data_descr_multi, "treatment_sport_freq", outcome_var_sel) 
  data_outcome_descr_multi_2 <- rbind(data_outcome_descr_multi_2, data_outcome_descr_sub)
}
data_outcome_descr_multi_2 <- data_outcome_descr_multi_2 %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )

data_outcome_descr_multi <- left_join(
  data_outcome_descr_multi, data_outcome_descr_multi_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                    treatment_sport_freq, num_obs, min, max, median, mean, everything())


## Standardized ##
data_descr_stand_multi <- data_descr_multi %>%
  recipe(.) %>%
  step_normalize(c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                   "bigfive_openness", "bigfive_neuroticism")) %>%
  prep() %>%
  bake(new_data = NULL) %>%
  as.data.frame()

data_outcome_descr_stand_multi <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_mean_comp(df = data_descr_stand_multi, y_variables = outcome_var_sel, treatment_setting = "multi")
  data_outcome_descr_stand_multi <- rbind(data_outcome_descr_stand_multi, data_outcome_descr_sub)
}

data_outcome_descr_stand_multi_2 <- data.frame()
for (outcome_var_sel in c("bigfive_agreeableness", "bigfive_conscientiousness", "bigfive_extraversion",
                          "bigfive_openness", "bigfive_neuroticism")) {
  data_outcome_descr_sub <- 
    func_summary_stats(data_descr_stand_multi, "treatment_sport_freq", outcome_var_sel) 
  data_outcome_descr_stand_multi_2 <- rbind(data_outcome_descr_stand_multi_2, data_outcome_descr_sub)
}
data_outcome_descr_stand_multi_2 <- data_outcome_descr_stand_multi_2 %>%
  mutate(
    treatment_sport_freq = case_when(treatment_sport_freq == 1 ~ "weekly_atleast", 
                                     treatment_sport_freq == 2 ~ "monthly_less",
                                     treatment_sport_freq == 3 ~ "never", TRUE ~ treatment_sport_freq)
  )


data_outcome_descr_multi_stand <- left_join(
  data_outcome_descr_stand_multi %>% dplyr::select(-median), 
  data_outcome_descr_stand_multi_2, by = c("variable", "treatment_sport_freq")
) %>% dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                    treatment_sport_freq, num_obs, min, max, median, mean, everything())


## COMBINE ##
data_outcome_descr_multi_all <- rbind(
  data_outcome_descr_multi %>% mutate(variable_measure = "level"), 
  data_outcome_descr_multi_stand %>% mutate(variable_measure = "stand")
) %>% 
  mutate(treatment_setting = "multi") %>%
  rename(treatment = treatment_sport_freq) %>%
  dplyr::select(treatment_setting, variable, variable_measure, everything())


# save
if (file.exists("Output/Descriptives/MULTI_MEAN_COMPARISON.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_multi_all) %>%
    group_by(treatment_setting, variable, variable_measure, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
} else {
  saveRDS(data_outcome_descr_multi_all, "Output/Descriptives/MULTI_MEAN_COMPARISON.rds")
}
