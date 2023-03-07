#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCATI DATA FOR PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Load data and join with cohort profile
# -> CATI data set is loaded
# -> Cohort Profile data set is loaded based on selection 
# -> CATI and Cohort Profile are merged so that only respondents who are in 
# both data sets are kept.
# -> Missing values are replaced according to selection.
#++++
# 2.) Generation of treatment period variables
# -> 1.) Outcome at sime time than controls: CAWI CATI
# -> 2.) Outcome after controls & treatment: CATI CAWI CATI
#++++
# --> Resulting data frame is panel data frame.
#++++


#%%%%%%%%%%%%%#
#### Setup ####
#%%%%%%%%%%%%%#

# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # load and install (if necessary) packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to fill missing values
# 
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate) # to work with dates
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx) # for saving as xlsx
# 
# # set language for dates and times to German, since the NEPS month names
# # are written in German; otherwise date/time functions are not working
# # for German language
# Sys.setlocale("LC_TIME", "German")
# 
# # treatment replacement
# treatment_repl <- "downup" # with any other selection, only downward replacement is made
# 
# # selection on cohort prepration
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
data_target_cati <- readRDS("Data/Personality/Prep_1/prep_1_target_cati_personality.rds")
## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality_robustcheck.rds")
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

# extract names of all big five variables (used often in this file)
cols_bigfive <- data_target_cati %>% dplyr::select(starts_with("bigfive")) %>% colnames()

# big five personality traits as numeric variables
data_target_cati <- data_target_cati %>%
  mutate(across(starts_with("bigfive"), ~ as.numeric(.)))

# indicator if treatment and outcomea are NA
data_target_cati <- data_target_cati %>%
  mutate(
    sport_leisure_freq_NA = ifelse(is.na(sport_leisure_freq), 1, 0),
    bigfive_extraversion_NA = ifelse(is.na(bigfive_extraversion), 1, 0),
    bigfive_agreeableness_NA = ifelse(is.na(bigfive_agreeableness), 1, 0),
    bigfive_conscientiousness_NA = ifelse(is.na(bigfive_conscientiousness), 1, 0),
    bigfive_neuroticism_NA = ifelse(is.na(bigfive_neuroticism), 1, 0),
    bigfive_openness_NA = ifelse(is.na(bigfive_openness), 1, 0) 
  )

# handle many missing values in CATI: unless a new value has been reported,
# value is copied downwards, i.e., to later waves. 
# depending on selection missing values in treatment variable may also be
# replaced upwards
if (treatment_repl == "down") {
  # replace missing values of controls, treatment and outcome variables downward
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cati), .direction = "down")
} else if (treatment_repl == "no") {
  # only replace missing values for control variables downward
  repl_controls <- names(data_target_cati)[!names(data_target_cati) %in% c("sport_leisure_freq", cols_bigfive)]
  data_target_cati <- data_target_cati %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
} else if (treatment_repl == "onelag") {
  # control variables
  repl_controls <- names(data_target_cati)[!names(data_target_cati) %in% c("sport_leisure_freq", cols_bigfive)]
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
      bigfive_extraversion = ifelse(is.na(bigfive_extraversion), lag(bigfive_extraversion), bigfive_extraversion),
      bigfive_agreeableness = ifelse(is.na(bigfive_agreeableness), lag(bigfive_agreeableness), bigfive_agreeableness),
      bigfive_conscientiousness = ifelse(is.na(bigfive_conscientiousness), lag(bigfive_conscientiousness), bigfive_conscientiousness),
      bigfive_neuroticism = ifelse(is.na(bigfive_neuroticism), lag(bigfive_neuroticism), bigfive_neuroticism),
      bigfive_openness = ifelse(is.na(bigfive_openness), lag(bigfive_openness), bigfive_openness)
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


#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


# Data preparation depends on the selection of cohort profile preparation
if (cohort_prep == "controls_same_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 1: Outcome at same time than Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # drop unnecessary columns and order
  ## treatment_starts is missing for all rows because treatment always starts
  ## with CATI interview (hence variable not necessary)
  data_cati <- data_cati %>% dplyr::select(-treatment_starts) %>%
    dplyr::select(ID_t, wave, wave_2, interview_date, treatment_ends, everything())
  
  # generate treatment_period variable
  data_cati <- data_cati %>% rename(treatment_period = treatment_ends, 
                                    interview_date_end = interview_date)
  
  
  # order columns
  data_cati <- data_cati %>% 
    dplyr::select(ID_t, treatment_period, interview_date_end, 
                  starts_with("sport_"), all_of(cols_bigfive), everything())
  
  
} else if (cohort_prep == "controls_bef_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 2: Outcome after Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # because control and treatment variables are taken from an earlier interview than the
  # outcome variable, they need to be prepared separately
  
  ## Outcome and Treatment ##
  
  # select treatment and outcome variables
  data_outcome <- data_cati %>%
    dplyr::select(ID_t, interview_date, treatment_ends, all_of(cols_bigfive)) %>%
    subset(!is.na(treatment_ends)) %>% 
    rename(treatment_period = treatment_ends, interview_date_end = interview_date)
  
  
  length(unique(data_outcome$ID_t))
  
  
  
  ## Control Variables ##
  
  # keep only variables which are used for start of treatment period
  # drop outcome and treatment variables
  data_controls <- data_cati %>%
    dplyr::select(-c(all_of(cols_bigfive), treatment_ends, wave)) %>%
    subset(!is.na(treatment_starts)) %>%
    rename(treatment_period = treatment_starts, interview_date_start = interview_date)
  
  
  length(unique(data_controls$ID_t))
  
  ## Merge ##
  
  # treatment-outcome and control variable data frames are merged via treatment_period
  # data_outcome has less observations because data_controls contains
  # treatment starts for which no treatment ends exists
  data_cati <- inner_join(
    data_outcome, data_controls, by = c("ID_t", "treatment_period")
  )
  
  # order columns
  data_cati <- data_cati %>% 
    dplyr::select(ID_t, treatment_period, interview_date_start, interview_date_end, 
                  starts_with("sport_"), everything())
  
  length(unique(data_controls$ID_t))
  
  
} else {
  stop("specify which cohort data preparation should be used")
}


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
  data_cohort_profile_save <- paste0("Data/Personality/Prep_3/prep_3_cati_treat", 
                                     treatment_repl, "_personality.rds")
} else {
  data_cohort_profile_save <- paste0("Data/Personality/Prep_3/prep_3_cati_treat", 
                                     treatment_repl, "_personality_robustcheck.rds")
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
func_save_sample_reduction(df_excel_save, "personality")


