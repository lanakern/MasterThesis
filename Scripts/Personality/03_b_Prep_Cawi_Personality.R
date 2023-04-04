#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCAWI DATA FOR PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Load data 
# -> CAWI and cohort profile data set are loaded. Only respondents who are 
# in both data sets are kept.
#++++
# 2.) Data Preparation (same for all cohort profiles)
# -> NA Indicator for treatment and outcome
# -> Replace missing values according to selection
# -> Join with cohort profile
# -> Generate treatment period variable
#++++
# --> Resulting data frame is panel data frame
#++++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
data_target_cawi <- readRDS("Data/Personality/Prep_1/prep_1_target_cawi_personality.rds")
  ## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality_robustcheck.rds")
} else {
  stop("specify which cohort data preparation should be used")
}


# number of respondents
id_num_cawi <- length(unique(data_target_cawi$ID_t)) # 15,239 respondents
id_num_cohort <- length(unique(data_cohort_profile$ID_t)) 


# check if number of respondents are the same in both data sets
id_cawi <- unique(data_target_cawi$ID_t)
id_cohort <- unique(data_cohort_profile$ID_t)
setdiff(id_cohort, id_cawi) # should be "integer(0)"
sum(id_cohort %in% id_cawi) == length(id_cohort) # should be TRUE


# CAWI: keep only respondents who are also in data cohort
data_target_cawi <- data_target_cawi %>% filter(ID_t %in% id_cohort)
id_num_cawi_adj_1 <- length(unique(data_target_cawi$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# indicator if treatment and outcome are NA
data_target_cawi <- data_target_cawi %>%
  mutate(
    sport_uni_NA = ifelse(is.na(sport_uni), 1, 0),
    sport_uni_freq_NA = ifelse(is.na(sport_uni_freq), 1, 0)
  )

# create lags for selected variables (including outcome and treatment)
# sort(names(colSums(is.na(data_target_cawi))[colSums(is.na(data_target_cawi)) > nrow(data_target_cawi)/2]))
var_sel_lag <- c("sport_uni", "sport_uni_freq", "grade_current", "uni_degree_import", 
                 "uni_ects_degree", "uni_ects_total", "uni_degree_complete_prob",
                 data_target_cawi %>% dplyr::select(
                   starts_with("academic"), starts_with("extracurricular"), starts_with("friends"),
                   starts_with("personality"), starts_with("stress"),
                   starts_with("uni_best_student"), starts_with("uni_learn_group"),
                   starts_with("satisfaction_life"), starts_with("social_integr"),
                   starts_with("uni_commitment"), starts_with("uni_termination"), starts_with("uni_time"), 
                   starts_with("uni_cost_giving_up"), starts_with("uni_courses_num"), starts_with("uni_anxiety"),
                   starts_with("uni_fear"), starts_with("uni_perf_satisfied"), starts_with("uni_achievement_expect"),
                   starts_with("living")
                 ) %>% colnames())
data_target_cawi_lags <- data_target_cawi %>%
  mutate(wave_2 = as.numeric(str_sub(wave, 1, 4))) %>%
  arrange(ID_t, wave_2) %>%
  dplyr::select(ID_t, wave, var_sel_lag) %>% 
  group_by(ID_t) %>%
  dplyr::mutate(across(var_sel_lag, ~lag(., default = NA))) %>%
  rename_at(.vars = vars(var_sel_lag), ~paste0(., "_lag")) %>%
  ungroup()

# generate NA dummies for lagged variables
# var_sel_lag_na <- colnames(data_target_cawi_lags %>% dplyr::select(-c("ID_t", "wave")))
var_sel_lag_na <- c("sport_uni_lag", "sport_uni_freq_lag")
for (var_sel in var_sel_lag_na) {
  data_target_cawi_lags <- func_na_dummy(data_target_cawi_lags, var_sel)
}

data_target_cawi <- left_join(data_target_cawi, data_target_cawi_lags, 
                              by = c("ID_t", "wave"))

# fill missing values of CAWI: down here because cohort profile does not
# contain all waves anymore
# depending on selection missing values in treatment variable may also be
# replaced upwards
if (treatment_repl == "down") {
  # replace missing values of controls, treatment and outcome variables downward
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cawi), .direction = "down")
} else if (treatment_repl == "no") {
  # only replace missing values for control variables downward
  repl_controls <- 
    names(data_target_cawi)[!names(data_target_cawi) %in% c("sport_uni", "sport_uni_freq")]
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
} else if (treatment_repl == "onelag") {
  # control variables
  repl_controls <- 
    names(data_target_cawi)[!names(data_target_cawi) %in% c("sport_uni", "sport_uni_freq")]
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
  # one lag for treatment and outcome
  data_target_cawi <- data_target_cawi %>% 
    arrange(ID_t, wave) %>% 
    group_by(ID_t) %>% 
    mutate(
      sport_uni = ifelse(is.na(sport_uni), lag(sport_uni), sport_uni),
      sport_uni_freq = ifelse(is.na(sport_uni_freq), lag(sport_uni_freq), sport_uni_freq)
    )
} else {
  stop("Please select treatment and outcome replacement strategy.")
}

# merge cohort data to cawi data via inner_join:
# only keep respondents who are in both data sets (also done above)
data_cawi <- inner_join(
  data_target_cawi, 
  data_cohort_profile %>% 
    dplyr::select(-c(starts_with("competence"))), 
  by = c("ID_t", "wave")
)


# generate treatment_period variable and rename interview_date
## this differs (only for "controls_same_outcome" this is interview start)
if (cohort_prep == "controls_same_outcome") {
  data_cawi <- data_cawi %>% rename(treatment_period = treatment_starts, 
                                    interview_date_start = interview_date)
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(-c(treatment_ends, starts_with("wave"))) %>%
    dplyr::select(ID_t, treatment_period, interview_date_start, 
                  starts_with("sport_"), everything())
  
} else if (cohort_prep == "controls_bef_outcome") {
  data_cawi <- data_cawi %>% rename(treatment_period = treatment_starts)
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(-c(treatment_ends, starts_with("wave"))) %>%
    dplyr::select(ID_t, treatment_period, interview_date, 
                  starts_with("sport_"), everything())
} 

# extract number of respondents
id <- unique(data_cawi$ID_t)
id_num <- length(unique(data_cawi$ID_t))


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# check for duplicates
sum(duplicated(data_cawi))

# check for missing values
colSums(is.na(data_cawi))

# number of rows, columns and respondents
print(paste("Number of respondents before data preparation:", id_num_cawi))
print(paste("Number of respondents after merge with cohort profile:", id_num_cawi_adj_1))
print(paste("Number of rows:", nrow(data_cawi)))
print(paste("Number of columns:", ncol(data_cawi)))

# save
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile_save <- 
    paste0("Data/Personality/Prep_3/prep_3_cawi_treat", treatment_repl, "_personality.rds")
} else {
  data_cohort_profile_save <- 
    paste0("Data/Personality/Prep_3/prep_3_cawi_treat", treatment_repl, "_personality_robustcheck.rds") 
}

saveRDS(data_cawi, data_cohort_profile_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "cawi",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = NA, 
  "num_id" = length(unique(data_cawi$ID_t)), 
  "num_rows" = nrow(data_cawi),
  "num_cols" = ncol(data_cawi),
  "time_stamp" = Sys.time()
)
  ## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "personality")