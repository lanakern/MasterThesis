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
# Because there are many missing values in leisure sport, missing values are
# also upward replaced.
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


#+++++++++++++++++#
#### Load Data ####
#+++++++++++++++++#

# load data
data_target_cati <- readRDS("Data/Prep_1/prep_1_target_cati.rds")
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")

# number of respondents
length(unique(data_target_cati$ID_t)) # 17,909 respondents
length(unique(data_cohort_profile$ID_t)) # 12,670 respondents

# handle many missing values: unless a new value has been reported,
# value is copied downwards, i.e., to later waves. 
data_target_cati <- data_target_cati %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  fill(names(data_target_cati), .direction = "down")


# merge cati data to cohort date -> only respondents which are 
# also in cohort data are kept. Moreover, only information for waves used
# are kept -> inner_join
data_cati <- inner_join(
  data_target_cati, data_cohort_profile, by = c("ID_t", "wave")
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



# keep only CATI waves
# data_cati <- data_cati %>%
#   filter(grepl("CATI", wave))

# # drop all variables not needed for the upcoming analysis
# # this is all variables which are not renamed, i.e., starting with t#, ts, tf, th, tx, and tg
# # and the ID_i variable
# data_cati <- data_cati %>%
#   select(-c(starts_with("tg"), starts_with("ts"), starts_with("tf"), 
#             starts_with("th"), starts_with("tx"), starts_with("Version"))) %>%
#   select(-(matches("^t[0-9].*"))) %>%
#   select(-c(ID_i))

# # define vector for time-invariant variables
# vars_constant <- c(
#   # time-constant information on student
#   "gender", "birth_year", "birth_month", "birth_country", "birth_ger_eastwest",
#   "birth_nationality_ger", "degree_uentrance_ger",
#   "bilingual", "kindergarden", "educ_school_rep_grade", "childhood_parents",
#   # select time-invariant information about parents
#   "mother_country_ger", "mother_country_all", "mother_language_first", "mother_educ_school_degree",
#   "mother_educ_school_degree_isced", "mother_educ_school_degree_casmin", "mother_educ_years",
#   "mother_educ_degree", "mother_educ_degree_inst", "mother_educ_doctorate", "mother_emp_15y",
#   "mother_emp_bef_15y", "mother_language_target", "father_country_ger", "father_country_all",
#   "father_language_first", "father_educ_school_degree", "father_educ_school_degree_isced",
#   "father_educ_school_degree_casmin", "father_educ_years", "father_educ_degree",
#   "father_educ_degree_inst", "father_educ_doctorate", "father_emp_15y", "father_emp_bef_15y",
#   "father_language_first"
# )


#++++++++++++++++++++++++++++++#
#### Treatment and Outcome #####
#++++++++++++++++++++++++++++++#

# treatment and outcome from CATI is kept for every wave
# later it is merged to CAWI treatment and outcome based on the CATI
# interview closest to the CAWI interview. 


# select outcome and treatment variables
data_cati_treatment <- data_cati %>%
  ungroup() %>%
  select(ID_t, wave, treatment_starts, grade_final, sport_leisure_freq)

# factor variables as character
data_cati_treatment <- data_cati_treatment %>% mutate_if(is.factor, as.character)

# because there are so many missing values, sport information is also copied
# upwards
colSums(is.na(data_cati_treatment))
data_cati_treatment <- data_cati_treatment %>%
  group_by(ID_t) %>%
  fill(sport_leisure_freq, .direction = "downup") %>% ungroup()

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













# #+++++++++++++++++++++++++++++++++++++++#
# #### Time-variant Control Variables #####
# #+++++++++++++++++++++++++++++++++++++++#
# 
# # drop time-invariant variables, i.e., keep only time-variant variables
# data_controls_cati <- data_cati %>%
#   select(-all_of(vars_constant)) %>%
#   # select(ID_t, wave, interview_date, treatment_starts,
#   #        current_family_status, educ_uni_type, educ_uni_degree_aspire) %>%
#   arrange(ID_t, wave)
# 
# # handle many missing values: unless a new value has been reported,
# # value is copied downwards, i.e., to later waves / interview dates
# # to do so, all factor variables are converted as character
# data_controls_cati <- data_controls_cati %>%
#   group_by(ID_t) %>%
#   fill(names(data_controls_cati), .direction = "down")
# 
# # if two interviews were conducted before the end of the treatment period
# # keep the information from the first interview
# data_controls_cati <- data_controls_cati %>%
#   group_by(ID_t, treatment_starts) %>% 
#   filter(row_number() == 1)
# 
# # drop rows with treatment_starts NA
# data_controls_cati <- data_controls_cati %>%
#   filter(!is.na(treatment_starts))
# 
# # convert all factor variables as characters
# data_controls_cati <- data_controls_cati %>%
#   mutate_if(is.factor, as.character) 
# 
# # number of missing values in each column
# colSums(is.na(data_controls_cati))
# 
# saveRDS(data_controls_cati, "Data/Prep_3/prep_3_controls_cati.rds")


# #+++++++++++++++++++++++++++++++++++++++++#
# #### Time-invariant Control Variables #####
# #+++++++++++++++++++++++++++++++++++++++++#
# 
# 
# # select variables which do not change over time
# # convert all factor variables as character
# data_cross_cati <- data_target_cati %>%
#   select(ID_t, wave, vars_constant) %>% 
#   distinct() %>%
#   mutate_if(is.factor, as.character)
# 
# 
# # ensure that all individuals have started their participation in the first wave
# data_cross_cati %>% select(ID_t, wave) %>% distinct() %>% arrange(ID_t, wave) %>%
#   group_by(ID_t) %>% filter(row_number() == 1) %>% ungroup() %>% 
#   select(wave) %>% unique()
# 
# # replace missing values per group for all columns in the data frame
#   ## replacement is made in both directions (up and down) because 
#   ## time-constant variables are sometimes asked later than in wave 1
# data_cross_cati <- data_cross_cati %>%
#   group_by(ID_t) %>%
#   fill(names(data_cross_cati), .direction = "downup")
# 
# # check if any missing values are left
#   ## missing values result from non-responses
# sum(is.na(data_cross_cati))
# sapply(data_cross_cati, function(y) sum(length(which(is.na(y)))))
# 
# # for all respondents information from first row is kept
# data_cross_cati <- data_cross_cati %>%
#   arrange(ID_t, wave) %>%
#   group_by(ID_t) %>%
#   filter(row_number() == 1)
# 
# # keep only respondents who are also in data cohort file
# data_cross_cati <- data_cross_cati %>%
#   subset(ID_t %in% unique(data_cohort_profile$ID_t))
# 
# # check if we have one row per respondent
# length(unique(data_cross_cati$ID_t)) == nrow(data_cross_cati)
# 
# # save data frame
# saveRDS(data_cross_cati, "Data/Prep_3/prep_3_controls_constant_cati.rds")





## OLD ##

# 
# # in case of duplicates across all columns except wave, keep only the first
# # column - it should be that all values are duplicated
# data_cross_cati <- data_cross_cati %>%
#   arrange(ID_t, wave) %>%
#   group_by(across(c(-wave))) %>%
#   filter(row_number() <= 1)
#   ## check if there are duplicates; it is the case if number of respondents
#   ## are not equal, i.e., not one row per respondent. 
#   ## In this case expression yields FALSE
# nrow(data_cross_cati) == id_num
#   ## look at duplicates
#   ## duplicates exist probably due to self-reported measures which
#   ## differ between waves, especially for birth month and mother language.
#   ## Thus, results from first wave are kept 
#   ## Then, I have one row per respondent
# id_dups <- data_cross_cati[duplicated(data_cross_cati$ID_t), "ID_t"] %>% 
#   select(ID_t) %>% pull() %>% unique()
# data_cross_cati <- data_cross_cati %>%
#   arrange(ID_t, wave) %>%
#   group_by(ID_t) %>%
#   filter(row_number() <= 1) %>%
#   ungroup()
# 
# # all observations are from first wave
# unique(data_cross_cati$wave)
# 
# # create variables
# data_cross_cati_adj <- data_cross_cati %>%
#   mutate(
#     # gender: dummy variable which equals 1 if respondent is male
#     male = ifelse(gender == "[m] männlich", 1, 0),
#     # birth date (needed later to calculate current age of respondent)
#     birth_date = mdy(paste(paste(birth_month, 1), birth_year, sep = ",")),
#     # born in Germany
#     birth_ger = ifelse(birth_country == "Deutschland", 1, 0),
#     # born in West Germany
#     birth_ger_west = ifelse(birth_country == "Westdeutschland", 1, 0),
#     # german nationality since birth
#     birth_ger_nationality = ifelse(birth_nationality_ger == "ja", 1, 0),
#     # being bilingual
#     bilingual = ifelse(bilingual == "ja", 1, 0),
#     # if respondent ever went to kindergarden
#     kindergarden = ifelse(kindergarden == "ja", 1, 0),
#     # language respondent speeks with mother
#     mother_tongue_ger = ifelse(mother_language_target %in% c("nur Deutsch", "meistens Deutsch"), 1, 0),
#     # mother: country of birth is germany
#     mother_birth_ger = ifelse(mother_country_all == "Deutschland", 1, 0) ,
#     # mother: language learned as a child
#     mother_language_first = ifelse(mother_language_first == "Deutsch", 1, 0),
#     # mother: has mother a university degree
#     mother_highest_degree_uni = ifelse(
#       mother_educ_school_degree_casmin %in% c("Universitätsabschluss", "Fachhochschulabschluss"), 1, 0
#     ),
#     # mother employment at respondents age of 15 and before
#     mother_emp_15y = ifelse(mother_emp_15y == "ja", 1, 0),
#     mother_emp_bef_15y = ifelse(mother_emp_bef_15y == "ja", 1, 0),
#     # same for father
#     father_birth_ger = ifelse(father_country_all == "Deutschland", 1, 0) ,
#     father_language_first = ifelse(father_language_first == "Deutsch", 1, 0),
#     father_highest_degree_uni = ifelse(
#       father_educ_school_degree_casmin %in% c("Universitätsabschluss", "Fachhochschulabschluss"), 1, 0
#     ),
#     father_emp_15y = ifelse(father_emp_15y == "ja", 1, 0),
#     father_emp_bef_15y = ifelse(father_emp_bef_15y == "ja", 1, 0)
#   ) %>%
#   # drop variables not needed anymore
#   select(
#     -c(gender, birth_year, birth_month, birth_country, birth_ger_eastwest, 
#        nationality_other, degree_uentrance_ger, 
#        birth_nationality_ger, mother_country_all, 
#        mother_educ_school_degree, mother_educ_school_degree_casmin,
#        mother_educ_school_degree_isced, mother_educ_degree, mother_country_ger,
#        mother_educ_degree_inst, mother_educ_doctorate, mother_language_target, 
#        father_country_all, father_educ_school_degree_casmin, father_educ_school_degree, 
#        father_educ_school_degree_isced, father_educ_degree, father_educ_degree_inst, 
#        father_educ_doctorate, father_country_ger,
#        wave)
#   ) %>%
#   # all numeric columns as integer
#   mutate_if(is.numeric, as.integer) %>%
#   # replace all missing values with zero, except for years of education and birth date
#   mutate(
#     across(-c(mother_educ_years, father_educ_years, birth_date), 
#            ~replace_na(.x, 0))
#   )
# 
# 
# # final steps/checks for cross-sectional data set
#   ## check if all individuals are kept
# length(unique(data_cross_cati_adj$ID_t)) == id_num
# setdiff(unique(data_cross_cati_adj$ID_t), id)
# setdiff(id, unique(data_cross_cati_adj$ID_t))
#   ## check for NAs: only for years of education of parents
# sum(is.na(data_cross_cati_adj))
# if (sum(is.na(data_cross_cati_adj)) != 0) {
#   sapply(data_cross_cati_adj, function(y) sum(length(which(is.na(y)))))
# }
#   ## save data frame
# saveRDS(data_cross_cati_adj, "Data/Prep_2/socio_parents_prep.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#





# #### Outcomes and Treatment ####
# #++++++++++++++++++++++++++++++#
# 
# # for simplicity, I create a data set only including the outcome and 
# # treatment variable
# data_treatment <- data_target_cati %>%
#   select(ID_t, wave, sport_leisure_freq, grade_final)
# 
# # merge this with cohort profile (includes date of interview)
#   ## right join to keep all values in data_cohort_profile
# # data_treatment <- right_join(
# #   data_treatment, data_cohort_profile, by = c("ID_t", "wave")
# # )
# data_treatment <- left_join(
#   data_treatment, data_cohort_profile %>% select("ID_t", "wave", "interview_date"), 
#   by = c("ID_t", "wave")
# )
# 
# # drop rows where all three relevant variables are missing
# data_treatment  <- data_treatment %>%
#   filter(!if_all(c(sport_leisure_freq, grade_final), is.na))
# 
# 
# #test <- data_treatment %>% subset(ID_t == 7002013)
# 
# 
# saveRDS(data_treatment, "Data/Prep_2/cati_outcome_treatment_prep.rds")



