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


#%%%%%%%%%%%%%#
#### Setup ####
#%%%%%%%%%%%%%#

# clear workspace
rm(list = ls())

# load and install (if necessary) packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to fill missing values

if (!require("lubridate")) install.packages("lubridate")
library(lubridate) # to work with dates

if (!require("xlsx")) install.packages("xlsx")
library(xlsx) # for saving as xlsx

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")

# treatment replacement
treatment_repl <- "downup" # with any other selection, only downward replacement is made

# selection on cohort prepration
cohort_prep <- "controls_bef_outcome" 
#cohort_prep <- "controls_same_outcome"



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
data_target_cati <- readRDS("Data/Prep_1/prep_1_target_cati.rds")
## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile_robustcheck.rds")
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


# handle many missing values in CATI: unless a new value has been reported,
# value is copied downwards, i.e., to later waves. 
data_target_cati <- data_target_cati %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  fill(names(data_target_cati), .direction = "down")

# merge cati data to cohort date -> only respondents which are 
# also in cohort data are kept (also ensured above)
data_cati <- inner_join(
  data_target_cati, data_cohort_profile %>% select(-c(starts_with("competence"))),
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
data_cohort_profile %>% select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7011450)
data_target_cati %>% select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7011450)

data_cohort_profile %>% select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7016646)
data_target_cati %>% select(ID_t, wave, starts_with("treatment")) %>% subset(ID_t == 7016646)



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# same for both cohort profile data because from CATI interview always 
# control variables and treatment information is taken.

# depending on selection you may replace missing values in the treatment
# variable also upwards
if (treatment_repl == "downup") {
  # because there are so many missing values in treatment, information is
  # also copied upwards
  data_cati <- data_cati %>%
    group_by(ID_t) %>%
    fill(sport_leisure_freq, .direction = "downup") %>% ungroup()
  # otherwise only downward which is done above
} else {
  data_cati <- data_cati
}

# generate treatment_period variable and rename interview_date
  ## this differs (only for "controls_same_outcome" this is interview start)
if (cohort_prep == "controls_same_outcome") {
  data_cati <- data_cati %>% rename(treatment_period = treatment_starts, 
                                    interview_date_start = interview_date)
  
  # order columns
  data_cati <- data_cati %>% 
    select(-c(treatment_ends, starts_with("wave"))) %>%
    select(ID_t, treatment_period, interview_date_start, 
           starts_with("sport_"), grade_final, everything())
  
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati <- data_cati %>% rename(treatment_period = treatment_starts)
  
  # order columns
  data_cati <- data_cati %>% 
    select(-c(treatment_ends, starts_with("wave"))) %>%
    select(ID_t, treatment_period, interview_date, 
           starts_with("sport_"), grade_final, everything())
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
  data_cohort_profile_save <- "Data/Prep_3/prep_3_cati.rds"
} else {
  data_cohort_profile_save <- "Data/Prep_3/prep_3_cati_robustcheck.rds"
}

saveRDS(data_cati, data_cohort_profile_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "cati",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl, 
  "num_id" = length(unique(data_cati$ID_t)), 
  "num_rows" = nrow(data_cati),
  "num_cols" = ncol(data_cati),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save)


