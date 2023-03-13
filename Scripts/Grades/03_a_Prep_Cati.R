#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCATI DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Load data and join with cohort profile
# -> CATI data set is loaded
# -> Cohort Profile data set is loaded based on selection 
# -> CATI and Cohort Profile are merged so that only respondents who are in 
# both data sets are kept.
# -> Missing values are replaced downwards
#++++
# 2.) Final CATI data set is created which includes control variables and
# treatment information.
# -> Missing values in treatment variable are also upward replaced (if selected by user)
#++++
# --> Resulting data frame is panel data frame.



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
data_target_cati <- readRDS("Data/Grades/Prep_1/prep_1_target_cati.rds")
## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck.rds")
} else {
  stop("specify which cohort data preparation should be used")
}

# number of respondents
length(unique(data_target_cati$ID_t)) # 17,909 respondents
length(unique(data_cohort_profile$ID_t)) 

# check if number of respondents are the same in both data sets
id_cati <- unique(data_target_cati$ID_t)
id_cohort <- unique(data_cohort_profile$ID_t)
setdiff(id_cohort, id_cati) # should be "integer(0)"
sum(id_cohort %in% id_cati) == length(id_cohort) # should be TRUE


# CAWI: keep only respondents who are also in data cohort
data_target_cati <- data_target_cati %>% filter(ID_t %in% id_cohort)
id_num_cati_adj_1 <- length(unique(data_target_cati$ID_t))

# indicator if treatment and outcome are NA
data_target_cati <- data_target_cati %>%
  mutate(
    sport_leisure_freq_NA = ifelse(is.na(sport_leisure_freq), 1, 0),
    grade_final_NA = ifelse(is.na(grade_final), 1, 0) 
  )

# handle many missing values in CATI: unless a new value has been reported,
# value is copied downwards, i.e., to later waves. 
# depending on selection missing values in treatment variable may also be
# replaced upwards
if (treatment_repl == "downup") { # NOT USED ANYMORE BECAUSE UNREALISTIC
  # downward replacement for all
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cati), .direction = "down")
  
  # additional also upward replacement of treatment
  data_target_cati <- data_target_cati %>%
    group_by(ID_t) %>%
    fill(sport_leisure_freq, .direction = "downup") %>% ungroup()
} else if (treatment_repl == "down") {
  # replace missing values of controls, treatment and outcome variables downward
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cati), .direction = "down")
} else if (treatment_repl == "no") {
  # only replace missing values for control variables downward
  repl_controls <- names(data_target_cati)[!names(data_target_cati) %in% c("sport_leisure_freq", "grade_final")]
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
} else if (treatment_repl == "onelag") {
  # control variables
  repl_controls <- names(data_target_cati)[!names(data_target_cati) %in% c("sport_leisure_freq", "grade_final")]
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
  # one lag for treatment and outcome
  data_target_cati <- data_target_cati %>% 
    arrange(ID_t, wave) %>% 
    group_by(ID_t) %>% 
    mutate(
      sport_leisure_freq = ifelse(is.na(sport_leisure_freq), lag(sport_leisure_freq), sport_leisure_freq),
      grade_final = ifelse(is.na(grade_final), lag(grade_final), grade_final)
      )
} else {
  stop("Please select treatment and outcome replacement strategy.")
}

# merge cati data to cohort date -> only respondents which are 
# also in cohort data are kept (also ensured above)
data_cati <- inner_join(
  data_target_cati, data_cohort_profile %>% dplyr::select(-c(starts_with("competence"))),
  by = c("ID_t", "wave")
) 


# extract number of respondents
  ## some respondents are dropped who have valid survey participation in cohort
  ## profile but no data in CATI
id_cati_adj_2 <- unique(data_cati$ID_t)
id_num_cati_adj_2 <- length(unique(data_cati$ID_t))

setdiff(id_cohort, id_cati_adj_2)
id_drop_num <- length(setdiff(id_cohort, id_cati_adj_2))
  ## examples
data_cohort_profile %>% dplyr::select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7011450)
data_target_cati %>% dplyr::select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7011450)

data_cohort_profile %>% dplyr::select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7016646)
data_target_cati %>% dplyr::select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7016646)



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# same for both cohort profile data because from CATI interview always 
# control variables and treatment information is taken.

# generate treatment_period variable and rename interview_date
  ## this differs (only for "controls_same_outcome" this is interview start)
if (cohort_prep == "controls_same_outcome") {
  data_cati <- data_cati %>% rename(treatment_period = treatment_starts, 
                                    interview_date_start = interview_date)
  
  # order columns
  data_cati <- data_cati %>% 
    dplyr::select(-c(treatment_ends, starts_with("wave"))) %>%
    dplyr::select(ID_t, treatment_period, interview_date_start, 
                  starts_with("sport_"), grade_final, everything())
  
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati <- data_cati %>% rename(treatment_period = treatment_starts)
  
  # order columns
  data_cati <- data_cati %>% 
    dplyr::select(-c(treatment_ends, starts_with("wave"))) %>%
    dplyr::select(ID_t, treatment_period, interview_date, 
                  starts_with("sport_"), grade_final, everything())
} 

# big five personality variables as numeric
data_cati <- data_cati %>% mutate_at(vars(starts_with("bigfive")),funs(as.numeric))



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# check for duplicates
sum(duplicated(data_cati))

# check for missing values
colSums(is.na(data_cati))

# number of rows, columns and respondents
print(paste("Number of respondents before data preparation:", length(id_cati)))
print(paste("Number of respondents after merge with cohort profile:", id_num_cati_adj_2))
print(paste("Number of rows:", nrow(data_cati)))
print(paste("Number of columns:", ncol(data_cati)))

# save
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile_save <- paste0("Data/Grades/Prep_3/prep_3_cati_treat", treatment_repl, ".rds")
} else {
  data_cohort_profile_save <- paste0("Data/Grades/Prep_3/prep_3_cati_treat", treatment_repl, "_robustcheck.rds")
}

saveRDS(data_cati, data_cohort_profile_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "cati",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = NA, 
  "num_id" = length(unique(data_cati$ID_t)), 
  "num_rows" = nrow(data_cati),
  "num_cols" = ncol(data_cati),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "grade")

