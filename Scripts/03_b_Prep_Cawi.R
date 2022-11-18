#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCAWI DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++
# by Lana Kern
#++++
# 1.) Missing values in cawi data are replaced downwards
#++++
# 2.) CAWI and Cohort Profile are merged via an inner join to add the interview date
# and keep only waves which are used for the treatment periods. Doing so,
# only respondents who are in both CAWI and CohortProfile are kept.
#++++
# 2.) Preparation of Outcome and Treatment: Outcome and treatment are taken
# at the interview at the end of the treatment period. 
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
library(tidyr)  # to manipulate data



#+++++++++++++++++#
#### Load Data ####
#+++++++++++++++++#

# load data
data_target_cawi <- readRDS("Data/Prep_1/prep_1_target_cawi.rds")
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")

# number of respondents
length(unique(data_target_cawi$ID_t)) # 15,239 respondents
length(unique(data_cohort_profile$ID_t)) # 12,670 respondents

# fill missing values of CAWI: down here because cohort profile does not
# contain all waves anymore
data_target_cawi <- data_target_cawi %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  fill(names(data_target_cawi), .direction = "down")

# merge cohort data to cawi data via inner_join:
# only keep respondents who are in both data sets
data_cawi <- inner_join(
  data_target_cawi, 
  data_cohort_profile %>% 
    select(-c(controls_invariant, starts_with("competence"))), 
  by = c("ID_t", "wave")
)

# extract number of respondents
id <- unique(data_cawi$ID_t)
id_num <- length(unique(data_cawi$ID_t))

# keep only CAWI waves
# unique(data_cawi$wave)
# data_cawi <- data_cawi %>%
#   filter(grepl("CAWI", wave)) %>%
#   mutate(wave = as.character(wave))
unique(data_cawi$wave)

# respondents, rows, and columns
print(paste("Number of respondents:", length(unique(data_cawi$ID_t))))
print(paste("Number of rows:", nrow(data_cawi)))
print(paste("Number of columns:", ncol(data_cawi)))



#++++++++++++++++++++++++++++++#
#### Outcomes and Treatment ####
#++++++++++++++++++++++++++++++#

# here the outcome variable (current average grade) and treatment variable
# (participation in university sports and their frequency) are extracted

# select variables of interest
# keep only variables which are used for ending of treatment period
data_treatment <- data_cawi %>%
  select(ID_t, wave, interview_date, treatment_ends,
         sport_uni, sport_uni_freq, grade_current) %>%
  subset(!is.na(treatment_ends))

# CURRENTLY AT BEGINNING
# # because there are many missing values, replace values downward, i.e.,
# # I assume that if individual reported doing sports in CAWI wave 1 and has
# # a missing value in CAWI wave 2, he or she still participates in sports.
# sapply(data_treatment, function(y) sum(length(which(is.na(y)))))
# data_treatment <- data_treatment %>%
#   group_by(ID_t) %>%
#   fill(c(sport_uni, sport_uni_freq, grade_current), .direction = "down") %>%
#   ungroup()

# factor variables as character
data_treatment <- data_treatment %>% mutate_if(is.factor, as.character)


# because there are so many missing values in treatment, information is
# also copied upwards
colSums(is.na(data_treatment))
data_treatment <- data_treatment %>%
  group_by(ID_t) %>%
  fill(c(sport_uni, sport_uni_freq), .direction = "downup") %>% ungroup()
colSums(is.na(data_treatment)) # DID NOT HELP


# count number of individuals for who no treatment and/or outcome info is available
data_treatment %>%
  group_by(ID_t) %>%
  summarize(
    count_na_sport = all(is.na(sport_uni)),
    count_na_sport_freq = all(is.na(sport_uni_freq)),
    count_na_grade = all(is.na(grade_current))
    ) %>%
  summarize(
    num_indiv_na_sport = sum(count_na_sport),
    num_indiv_na_sport_freq = sum(count_na_sport_freq),
    num_indiv_na_grade = sum(count_na_grade)
    )


# final steps
print("Treatment and Outcome Data Set")
print(paste("Number of respondents:", length(unique(data_treatment$ID_t))))
print(paste("Number of rows:", nrow(data_treatment)))
print(paste("Number of columns:", ncol(data_treatment)))

saveRDS(data_treatment, "Data/Prep_3/prep_3_outcome_treatment_cawi.rds")



#++++++++++++++++++++++++++++++++++++++#
#### Time-variant Control Variables ####
#++++++++++++++++++++++++++++++++++++++#

# keep only variables which are used for start of treatment period
# drop outcome and treatment
data_controls_cawi <- data_cawi %>%
  select(-c(sport_uni, sport_uni_freq, grade_current)) %>%
  subset(!is.na(treatment_starts)) 


# CURRENTLY AT BEGINNING
# # fill missings
# data_controls_cawi <- data_controls_cawi %>%
#   group_by(ID_t) %>%
#   fill(names(data_controls_cawi), .direction = "down")


# CURRENTLY IN COHORT PROFILE (MAY BE CHANGED LATER DUE TO MISSINGS)
# # if two interviews were conducted before the end of the treatment period
# # keep the information from the second interview as this is more up-to-date
# data_controls_cawi <- data_controls_cawi %>%
#   group_by(ID_t, treatment_starts) %>% 
#   filter(row_number() == n())
# 
# # drop rows with treatment_starts NA
# data_controls_cawi <- data_controls_cawi %>%
#   filter(!is.na(treatment_starts))

# convert all factor variables as characters
data_controls_cawi <- data_controls_cawi %>%
  mutate_if(is.factor, as.character) 


# number of missing values in each column
colSums(is.na(data_controls_cawi))

# final: number of respondents, rows. and columns
print("Control Variable Data Set")
print(paste("Number of respondents:", length(unique(data_controls_cawi$ID_t))))
print(paste("Number of rows:", nrow(data_controls_cawi)))
print(paste("Number of columns:", ncol(data_controls_cawi)))

saveRDS(data_controls_cawi, "Data/Prep_3/prep_3_controls_cawi.rds")

