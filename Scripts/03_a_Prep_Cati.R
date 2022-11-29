#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCATI DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Missing values in cati data are replaced downwards
#++++
# 2.) CATI and Cohort Profile are merged via an inner join to add the interview date
# and keep only waves which are used for the treatment periods. Doing so,
# only respondents who are in both CATI and CohortProfile are kept.
#++++
# 2.) Preparation of Outcome and Treatment: Information about grade and sport
# is kept from every CATI interview which is used to create treatment
# periods. This is done because later treatment and outcome information from CATI
# is merged to CAWI based on the closest time difference (minimum difference in days
# between CATI and CAWI interviews). 
# Leisure sport can either be downward or downward & upward replaced (based on selection)
#++++
# 3.) Preparation of Control variables: Control variables are taken from
# the beginning of the treatment period. 
#++++
# --> Resulting data frame is panel data frame


#### Setup ####
#+++++++++++++#

# clear workspace
rm(list = ls())

# load and install (if necessary) packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to fill missing values

if (!require("lubridate")) install.packages("lubridate")
library(lubridate) # to work with dates

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")

# treatment replacement
treatment_repl <- "downup" # with any other selection, only downward replacement is made


#+++++++++++++++++#
#### Load Data ####
#+++++++++++++++++#

# load data
data_target_cati <- readRDS("Data/Prep_1/prep_1_target_cati.rds")
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")

# number of respondents
length(unique(data_target_cati$ID_t)) # 17,909 respondents
length(unique(data_cohort_profile$ID_t)) # 12,670 respondents

# handle many missing values in CATI: unless a new value has been reported,
# value is copied downwards, i.e., to later waves. 
data_target_cati <- data_target_cati %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  fill(names(data_target_cati), .direction = "down")

# merge cati data to cohort date -> only respondents which are 
# also in cohort data are kept. Moreover, only information for waves used
# are kept -> inner_join
data_cati <- inner_join(
  data_target_cati, 
  # drop treatment_ends from cohort profile because it will be NA for
  # any observation as treatment period only ends in CAWI wave
  data_cohort_profile %>% select(-treatment_ends), 
  by = c("ID_t", "wave")
) 


# extract number of respondents
id <- unique(data_cati$ID_t)
id_num <- length(unique(data_cati$ID_t))

# SAMPLE REDUCTION
  ## there are 154 respondents who are in data_cohort_profile but not
  ## in CATI 
length(setdiff(unique(data_cohort_profile$ID_t), unique(data_cati$ID_t)))


print("After Merge:")
print(paste("Number of respondents:", length(unique(data_cati$ID_t))))
print(paste("Number of rows:", nrow(data_cati)))
print(paste("Number of columns:", ncol(data_cati)))



#++++++++++++++++++++++++++++++#
#### Treatment and Outcome #####
#++++++++++++++++++++++++++++++#

# treatment and outcome from CATI is kept for every wave
# later it is merged to CAWI treatment and outcome based on the CATI
# interview closest to the CAWI interview. 


# select outcome and treatment variables
data_cati_treatment <- data_cati %>%
  ungroup() %>%
  select(ID_t, wave, interview_date, treatment_starts, grade_final, sport_leisure_freq)

# factor variables as character
data_cati_treatment <- data_cati_treatment %>% mutate_if(is.factor, as.character)

# treatment replacement based on selection
if (treatment_repl == "downup") {
  # because there are so many missing values, sport information is also copied
  # upwards
  colSums(is.na(data_cati_treatment))
  data_cati_treatment <- data_cati_treatment %>%
    group_by(ID_t) %>%
    fill(sport_leisure_freq, .direction = "downup") %>% ungroup()
  # otherwise only down replacement (which is already down above)
} else {
  data_cati_treatment <- data_cati_treatment 
}


# count number of individuals for who no treatment and/or outcome info is available
data_cati_treatment %>%
  group_by(ID_t) %>%
  summarize(
    count_na_sport_freq = all(is.na(sport_leisure_freq)),
    count_na_grade = all(is.na(grade_final))
  ) %>%
  summarize(
    num_indiv_na_sport_freq = sum(count_na_sport_freq),
    num_indiv_na_grade = sum(count_na_grade)
  )


# final steps
print("Treatment and Outcome Data Set")
print(paste("Number of respondents:", length(unique(data_cati_treatment$ID_t))))
print(paste("Number of rows:", nrow(data_cati_treatment)))
print(paste("Number of columns:", ncol(data_cati_treatment)))

saveRDS(data_cati_treatment, "Data/Prep_3/prep_3_outcome_treatment_cati.rds")




#+++++++++++++++++++++++++#
#### Control Variables ####
#+++++++++++++++++++++++++#


# time-variant and time-invariant control variables are in one data set
data_controls_cati <- data_cati %>%
  # select all control variables
  select(-c(grade_final, sport_leisure_freq)) %>% 
  # keep only variables which are used for start of treatment period
  subset(!is.na(treatment_starts)) 

# convert all factor variables as characters
data_controls_cati <- data_controls_cati %>%
  mutate_if(is.factor, as.character) 

# number of missing values in each column
colSums(is.na(data_controls_cati))

# final: number of respondents, rows. and columns
print("Control Variable Data Set")
print(paste("Number of respondents:", length(unique(data_controls_cati$ID_t))))
print(paste("Number of rows:", nrow(data_controls_cati)))
print(paste("Number of columns:", ncol(data_controls_cati)))

saveRDS(data_controls_cati, "Data/Prep_3/prep_3_controls_cati.rds")





