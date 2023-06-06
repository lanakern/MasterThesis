#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCAWI DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++
# by Lana Kern
#++++
# In this file the CAWI data is prepared. Most importantly, missing values are
# replaced downwards.
#++++
# 1.) Load data and join with cohort profile
# -> CAWI and Cohort Profile are merged so that only respondents who are in 
# both data sets are kept.
# -> Missing values are replaced downwards
#++++
# 2.) Data Preparation (also depends on selected cohort profile step)
# -> "controls_same_outcome": only treatment_starts variable is dropped as it
# is always NA because treatment period starts with CATI and ends with CAWI
# -> "controls_bef_outcome": outcome and treatment+control variables are
# prepared separately as they are measured in different survey. Then, they
# are merged via the treatment period indicator.
# -> "controls_bef_all": same idea than for "controls_bef_outcome", but treatment
# is measured in same interview than outcome.
# -> "controls_treatment_outcome"; controls, treatment, and outcome are
# measured in different surveys. 
#++++
# --> Resulting data frame is panel data frame
#++++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# load data
data_target_cawi <- readRDS("Data/Grades/Prep_1/prep_1_target_cawi.rds")
  ## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile.rds")
} else if (cohort_prep %in% c("controls_bef_outcome", "controls_bef_all")) {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck.rds")
} else if (cohort_prep == "controls_treatment_outcome") {
  data_cohort_profile <- readRDS(paste0("Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck_", cohort_prep, ".rds"))
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

# indicator if treatment and outcome are NA
data_target_cawi <- data_target_cawi %>%
  mutate(
    sport_uni_NA = ifelse(is.na(sport_uni), 1, 0),
    sport_uni_freq_NA = ifelse(is.na(sport_uni_freq), 1, 0),
    grade_current_NA = ifelse(is.na(grade_current), 1, 0) 
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
var_sel_lag_na <- c("sport_uni_lag", "sport_uni_freq_lag", "grade_current_lag")
for (var_sel in var_sel_lag_na) {
  data_target_cawi_lags <- func_na_dummy(data_target_cawi_lags, var_sel)
}

# add to data frame
data_target_cawi <- left_join(data_target_cawi, data_target_cawi_lags, 
                              by = c("ID_t", "wave"))

# fill missing values of CAWI: down here because cohort profile does not
# contain all waves anymore
# depending on selection missing values in treatment variable may also be
# replaced upwards
if (treatment_repl == "downup") { # NOT USED ANYMORE BECAUSE UNREALISTIC
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cawi), .direction = "down")
  
  # also upward replacement of treatment
  data_target_cawi <- data_target_cawi %>%
    group_by(ID_t) %>%
    fill(c(sport_uni, sport_uni_freq), .direction = "downup") %>% ungroup()

} else if (treatment_repl == "down") {
  # replace missing values of controls, treatment and outcome variables downward
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cawi), .direction = "down")
} else if (treatment_repl == "no") {
  # only replace missing values for control variables downward
  repl_controls <- names(data_target_cawi)[!names(data_target_cawi) %in% c("sport_uni", "sport_uni_freq", "grade_current")]
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
} else if (treatment_repl == "onelag") {
  # control variables
  repl_controls <- names(data_target_cawi)[!names(data_target_cawi) %in% c("sport_uni", "sport_uni_freq", "grade_current")]
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
      sport_uni_freq = ifelse(is.na(sport_uni_freq), lag(sport_uni_freq), sport_uni_freq),
      grade_current = ifelse(is.na(grade_current), lag(grade_current), grade_current)
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
  ## examples
data_target_cawi %>% subset(ID_t == 7003857) %>% dplyr::select(ID_t, wave)
data_cohort_profile %>% subset(ID_t == 7003857) %>% dplyr::select(ID_t, wave, treatment_starts, treatment_ends)
data_cawi %>% subset(ID_t == 7003857) %>% dplyr::select(ID_t, wave)

data_target_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave)
data_cohort_profile %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave)
data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave)


# extract number of respondents
id <- unique(data_cawi$ID_t)
id_num <- length(unique(data_cawi$ID_t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Further data preparation depends on the selection of cohort profile preparation


if (cohort_prep == "controls_same_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 1; Outcome at same time than Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # drop unnecessary columns and order
    ## treatment_starts is missing for all rows because treatment always starts
    ## with CATI interview (hence variable not necessary)
  data_cawi <- data_cawi %>% dplyr::select(-treatment_starts) %>%
    dplyr::select(ID_t, wave, wave_2, interview_date, treatment_ends, everything())
  
  # generate treatment_period variable
  data_cawi <- data_cawi %>% rename(treatment_period = treatment_ends, 
                                    interview_date_end = interview_date)
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(ID_t, treatment_period, interview_date_end, 
                  starts_with("sport_"), grade_current, everything())
  
  
} else if (cohort_prep == "controls_bef_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 2: Outcome after Controls and Treatment ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # because control variables are taken from an earlier interview than the
  # outcome and treatment variable, they need to be prepared separately
  
  ## Outcome ##
  
  # select treatment and outcome variables
  data_outcome <- data_cawi %>%
    dplyr::select(ID_t, interview_date, treatment_ends, grade_current) %>%
    subset(!is.na(treatment_ends)) %>% 
    rename(treatment_period = treatment_ends, interview_date_end = interview_date)

  
  length(unique(data_outcome$ID_t))
  
  
  
  ## Control Variables and Treatment ##
  
  # keep only variables which are used for start of treatment period
  # drop outcome and treatment variables
  data_controls <- data_cawi %>%
    dplyr::select(-c(grade_current, treatment_ends, wave)) %>%
    subset(!is.na(treatment_starts)) %>%
    rename(treatment_period = treatment_starts, interview_date_start = interview_date)
  
  
  length(unique(data_controls$ID_t))
  
  ## Merge ##
  
  # treatment-outcome and control variable data frames are merged via treatment_period
  # data_outcome has less observations because data_controls contains
  # treatment starts for which no treatment ends exists
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  
  
  data_cawi <- inner_join(
    data_outcome, data_controls, by = c("ID_t", "treatment_period")
  )
  
  # examples
  data_outcome %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  data_outcome %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(ID_t, treatment_period, interview_date_start, interview_date_end, 
                  starts_with("sport_"), grade_current, everything())
  
  length(unique(data_controls$ID_t))
  
  
} else if (cohort_prep == "controls_bef_all") {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 3: Outcome and Treatment after Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## Outcome and Treatment ##
  
  # select treatment and outcome variables
  data_outcome <- data_cawi %>%
    dplyr::select(ID_t, interview_date, treatment_ends, grade_current, starts_with("sport")) %>%
    subset(!is.na(treatment_ends)) %>% 
    rename(treatment_period = treatment_ends, interview_date_end = interview_date)
  
  
  length(unique(data_outcome$ID_t))
  
  
  
  ## Control Variables ##
  
  # keep only variables which are used for start of treatment period
  # drop outcome and treatment variables
  data_controls <- data_cawi %>%
    dplyr::select(-c(grade_current, treatment_ends, wave, starts_with("sport"))) %>%
    subset(!is.na(treatment_starts)) %>%
    rename(treatment_period = treatment_starts, interview_date_start = interview_date)
  
  
  length(unique(data_controls$ID_t))
  
  ## Merge ##
  
  # treatment-outcome and control variable data frames are merged via treatment_period
  # data_outcome has less observations because data_controls contains
  # treatment starts for which no treatment ends exists
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  
  
  data_cawi <- inner_join(
    data_outcome, data_controls, by = c("ID_t", "treatment_period")
  )
  
  # examples
  data_outcome %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  data_outcome %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(ID_t, treatment_period, interview_date_start, interview_date_end, 
                  starts_with("sport_"), grade_current, everything())
  
  length(unique(data_controls$ID_t))
} else if (cohort_prep == "controls_treatment_outcome") {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 4: Controls - Treatment - Outcome ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  ## Outcome ##
  data_outcome <- data_cawi %>%
    dplyr::select(ID_t, interview_date, treatment_ends, grade_current, outcome) %>%
    filter(outcome == 1) %>% dplyr::select(-outcome) %>% 
    rename(interview_date_end = interview_date) %>%
    mutate(treatment_period = 1)
  length(unique(data_outcome$ID_t))
  
  
  ## Treatment ##
  data_treatment <- data_cawi %>%
    dplyr::select(ID_t, interview_date, starts_with("sport"), treatment) %>%
    filter(treatment == 1) %>% dplyr::select(-treatment) %>% 
    rename(interview_date_treatment = interview_date) %>%
    mutate(treatment_period = 1)
  length(unique(data_treatment$ID_t))
  
  ## Control Variables ##
  
  # keep only variables which are used for start of treatment period
  # drop outcome and treatment variables
  data_controls <- data_cawi %>%
    dplyr::select(-c(grade_current, treatment_ends, wave, starts_with("sport"))) %>%
    filter(controls == 1) %>% dplyr::select(-controls) %>% 
    rename(interview_date_start = interview_date) %>%
    mutate(treatment_period = 1)
  
  
  length(unique(data_controls$ID_t))
  
  ## Merge ##
  
  # treatment-outcome and control variable data frames are merged via treatment_period
  # data_outcome has less observations because data_controls contains
  # treatment starts for which no treatment ends exists
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, wave, starts_with("treatment"))
  
  
  data_cawi <- inner_join(
    data_outcome, data_treatment, by = c("ID_t", "treatment_period")
  ) %>% inner_join(
    data_controls, by = c("ID_t", "treatment_period")
  )
  
  # examples
  data_outcome %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7036384) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  data_outcome %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% dplyr::select(ID_t, starts_with("treatment"))
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    dplyr::select(ID_t, treatment_period, interview_date_start, interview_date_end, 
                  starts_with("sport_"), grade_current, everything())
  
  length(unique(data_controls$ID_t))
} else {
  stop("specify which cohort data preparation should be used")
}



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
  data_cohort_profile_save <- paste0("Data/Grades/Prep_3/prep_3_cawi_treat", 
                                     treatment_repl, ".rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile_save <- paste0("Data/Grades/Prep_3/prep_3_cawi_treat", 
                                     treatment_repl, "_robustcheck.rds") 
} else {
  data_cohort_profile_save <- paste0("Data/Grades/Prep_3/prep_3_cawi_treat", 
                                     treatment_repl, "_robustcheck_", cohort_prep, 
                                     ".rds") 
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
func_save_sample_reduction(df_excel_save, "grade")