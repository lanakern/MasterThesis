#%%%%%%%%%%%%%%%%%%%%%%%%#
#### MERGE DATA FILES ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Merge CATI and CAWI
# -> Prepare treatment: university sport and leisure sport variables 
# are united in a single treatment variable (binary and frequency)
# -> Prepare outcome: current grade is used most of the time; for 8
# individuals final grade is chosen as current grade is NA
# -> Control variables: merged from CATI and CAWI based on the treatment
# period (treatment_starts); outcome and treatment are also merged to the
# respective treatment period via treatment_ends.
#++++
# 2.) Add Episodes (includes school degree, past employment etc.)
# ->
# ->
#++++
# 3.) Add Sibling
#++++
# 4.) Add Child
#++++
# 5.) Add Partner
#++++
# 6.) Add Competencies
#++++
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to manipulate data, e.g. replace_na, spread() etc.

if (!require("sqldf")) install.packages("sqldf")
library(sqldf)  # for sql syntax

# define inputs
  ## approach to generate treatment: "period" and "distance" (explanation see below)
treatment_cati <- "period"


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# CATI: outcome/treatment and control variables
data_outcome_cati <- readRDS("Data/Prep_3/prep_3_outcome_treatment_cati.rds")
data_controls_cati <- readRDS("Data/Prep_3/prep_3_controls_cati.rds")

# CAWI: outcome/treatment and control variables
data_controls_cawi <- readRDS("Data/Prep_3/prep_3_controls_cawi.rds")
data_outcome_cawi <- readRDS("Data/Prep_3/prep_3_outcome_treatment_cawi.rds")

# Episode data
data_life_course <- readRDS("Data/Prep_2/prep_2_life_course.rds")

# Sibling information (time-invariant)
data_sibling <- readRDS("Data/Prep_3/prep_3_sibling.rds")

# child data (time-variant)
data_child <- readRDS("Data/Prep_3/prep_3_child.rds")

# partner information (time-variant)
data_partner <- readRDS("Data/Prep_3/prep_3_partner.rds")

# competencies
data_competencies <- readRDS("Data/Prep_3/prep_3_competencies.rds")

# number of respondents in different data sets
  ## CATI: minimum from treatment and outcome
num_id_cati <- min(c(length(unique(data_controls_cati$ID_t)), length(unique(data_outcome_cati$ID_t))))
  ## CAWI
num_id_cawi <- min(c(length(unique(data_controls_cawi$ID_t)), length(unique(data_outcome_cawi$ID_t))))
  ## episode data
num_id_eps <- length(unique(data_life_course$ID_t))
  ## sibling
num_id_sib <- length(unique(data_sibling$ID_t))
  ## child
num_id_child <- length(unique(data_child$ID_t))
  ## partner
num_id_partner <- length(unique(data_partner$ID_t))
  ## competencies
num_id_comp <- length(unique(data_competencies$ID_t))



#%%%%%%%%%%%%%%%%%%%%#
#### CATI &  CAWI ####
#%%%%%%%%%%%%%%%%%%%%#

# pre-processing
  ## rename interview dates to know later interview dates for CATI and CAWI
data_controls_cati <- data_controls_cati %>%
  rename(interview_date_cati = interview_date) 
data_outcome_cati <- data_outcome_cati %>%
  rename(interview_date_outcome_cati = interview_date) 

data_controls_cawi <- data_controls_cawi %>%
  rename(interview_date_cawi = interview_date)
data_outcome_cawi <- data_outcome_cawi %>%
  rename(interview_date_outcome_cawi = interview_date)


#### Treatment ####
#+++++++++++++++++#

# The CATI data frame includes information about leisure sport and the final
# grade. This treatment and outcome information is merged to the CAWI data set
# using the interview dates.

# good test example
# test_cati <- data_outcome_cati %>% subset(ID_t == 7001969) 
# test_cawi <- data_outcome_cawi %>% subset(ID_t == 7001969) 

if (treatment_cati == "period") {
  ## FIRST APPROACH: PERIOD ##
  # 1.) CATI information is taken from CATI survey of respective treatment period
  # 2.) This information is compared with info from next treatment period. If
  # they differ, closest info is taken (this is the idea but never the case)
  data_outcome_cawi_cati <- 
    # do not join by wave because they may differ
    left_join(data_outcome_cawi %>% select(-wave), 
              data_outcome_cati %>% select(-wave), 
              by = c("ID_t", "treatment_ends" = "treatment_starts")) %>%
    group_by(ID_t) %>%
    mutate(sport_diff = if_else(sport_leisure_freq == lag(sport_leisure_freq), 1, 0))
  
  #table(data_outcome_cawi_cati$sport_diff) # never a difference
  
  data_outcome_cawi_cati <- data_outcome_cawi_cati %>% select(-sport_diff)
} else if (treatment_cati == "distance") {
  ## SECOND APPROACH: DISTANCE ##
  # Take CATI treatment and outcome information from closest interview date to CAWI
  data_outcome_cawi_cati <-
    left_join(data_outcome_cawi %>% select(-wave), 
              data_outcome_cati %>% select(-wave),
              by = c("ID_t", "treatment_ends" = "treatment_starts")) %>%
    group_by(ID_t) %>%
    mutate(date_diff = abs(as.numeric(difftime(interview_date_outcome_cawi, interview_date_outcome_cati)))) %>%
    group_by(ID_t, treatment_ends) %>%
    filter(date_diff == min(date_diff))
} else {
  print("Select how treatment from CATI survey should be determined: period or distance")
}


# length(unique(data_outcome_cawi_cati$ID_t)) # 12010 (CAWI due to left join)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## CREATE ONE SPORT FREQUENCY VARIABLE ##

# display labels
unique(data_outcome_cawi_cati$sport_uni_freq)
unique(data_outcome_cawi_cati$sport_leisure_freq)

# recode: in original data set the order is reversed so it does not make
# sense to unlabel them
## 0 = never 
## 1 = once a month or less frequently
## 2 = several times a month or once a week
## 3 = at least once per week
## 4 = (almost) daily
table(data_outcome_cawi_cati$sport_uni_freq, useNA = "always")
table(data_outcome_cawi_cati$sport_leisure_freq, useNA = "always")

data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    sport_uni_freq = recode(
      sport_uni_freq, 
      "less frequently" = 1, "once a month" = 1, "several times a month" = 2,
      "several times a week" = 2, "once a week" = 3, "daily" = 4
      ),
    sport_leisure_freq = recode(
      sport_leisure_freq, 
      "never" = 0, "once a month or less" = 1, "several times a month or once a week" = 2,
      "several times a week" = 3, "almost daily or daily" = 4
    )
  ) %>%
  # also if: sport_uni == "not involved" then sport_uni_freq = 0 ("never")
  mutate(
    sport_uni_freq = ifelse(sport_uni == "not involved" & is.na(sport_uni_freq), 0, sport_uni_freq)
  )

# sport_uni_freq has less NA due to not involved variable
table(data_outcome_cawi_cati$sport_uni, useNA = "always")
table(data_outcome_cawi_cati$sport_uni_freq, useNA = "always")
table(data_outcome_cawi_cati$sport_leisure_freq, useNA = "always")

# if info for both is given, higher value is taken
# leisure sport DOES NOT explicitly exclude university sport
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    # uni sport is base (hence last value in ifelse construction)
    # if uni sport is missing use info on leisure sport
    treatment_sport_freq = ifelse(
      is.na(sport_uni_freq), sport_leisure_freq,
      # always use highest frequency value: if value on leisure sport is higher use this value
      ifelse(sport_leisure_freq > sport_uni_freq & !is.na(sport_leisure_freq), sport_leisure_freq, 
             # otherwise use always uni sport value
             sport_uni_freq))
  )

table(data_outcome_cawi_cati$treatment_sport_freq, useNA = "always")

# create dummy to know if I used uni or leisure sport information
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(sport_freq_source = 
           case_when(
          # if frequency of university sport equals frequency of sport in treatment variable
          # AND frequency of uni sport does not equal leisure sport or leisure sport is NA
          # then the source is uni
          treatment_sport_freq == sport_uni_freq & 
            (sport_uni_freq != sport_leisure_freq | is.na(sport_leisure_freq)) ~ "uni",
          # same idea for leisure than for uni
          treatment_sport_freq == sport_leisure_freq  & 
            (sport_uni_freq != sport_leisure_freq | is.na(sport_uni_freq)) ~ "leisure",
          # "both" is selected if frequency of leisure and university sport is the same
          sport_uni_freq == sport_leisure_freq & !is.na(sport_uni_freq) & !is.na(sport_leisure_freq) ~ "both",
          # otherwise NA because no info about sport participation is given
          TRUE ~ as.character(NA)
          )
         )
  
table(data_outcome_cawi_cati$sport_freq_source, useNA = "always")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## CREATE INDICATOR FOR UNIVERSITY AND LEISURE SPORT ##

# sport_uni = 1 if respondent participates in university sport
# sport_leisure = 1 if respondent participates in leisure sport
# -> also both variables can take on the value 1
table(data_outcome_cawi_cati$sport_uni, useNA = "always")
table(data_outcome_cawi_cati$sport_uni_freq, useNA = "always")
table(data_outcome_cawi_cati$sport_leisure_freq, useNA = "always")

data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  # rename original sport_uni variable
  rename(sport_uni_orig = sport_uni) %>%
  # create variables
  mutate(
    # respondent participates in university sport if sport_uni_orig is "involved"
    # or sport_uni_freq NOT NA (never the case that frequency is given but
    # involved variable not but vice verca)
    sport_uni = ifelse(sport_uni_orig == "not involved", 0, 
                       ifelse(is.na(sport_uni_orig), NA, 1)),
    # respondent participates in leisure sport if sport_leisure_freq is NOT NA and not 0
    sport_leisure = ifelse(sport_leisure_freq > 0, 1, 
                           ifelse(is.na(sport_leisure_freq), NA, 0))
  ) 

table(data_outcome_cawi_cati$sport_uni, useNA = "always")
table(data_outcome_cawi_cati$sport_leisure, useNA = "always")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## CREATE BINARY TREATMENT VARIABLE ##

# create general dummy for sport-participation (=1) and non-participation (=0)
# keep NA as later individuals and treatment periods without sport information are dropped
table(data_outcome_cawi_cati$treatment_sport_freq, useNA = "always")
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    treatment_sport = ifelse(treatment_sport_freq != 0 & !is.na(treatment_sport_freq), 1, 
                             ifelse(is.na(treatment_sport_freq), NA, 0))
  ) %>%
  # create indicator for source
  mutate(
    sport_source = 
      case_when(
        is.na(treatment_sport) ~ as.character(NA),
        !is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "both",
        !is.na(sport_uni_orig) & is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "uni",
        is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "leisure",
        TRUE ~ as.character(NA)
      )
  )

table(data_outcome_cawi_cati$treatment_sport, useNA = "always")
table(data_outcome_cawi_cati$sport_source, useNA = "always")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## CREATE DATE FOR INTERVIEW ##

# can be from CAWI or CATI depending on the source
# if same info is in both sources, CAWI date is chosen.
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(interview_date_treatment = case_when(
    sport_source == "uni" ~ interview_date_outcome_cawi,
    sport_source == "leisure" ~ interview_date_outcome_cati,
    sport_source == "both" ~ interview_date_outcome_cawi,
    TRUE ~ as.Date(NA)
  ))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## CHECK ##

# check
data_outcome_cawi_cati %>% subset(ID_t == 7001968) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7002010) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7001969) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7002074) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7001975) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7002011) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))
data_outcome_cawi_cati %>% subset(ID_t == 7002166) %>% 
  select(ID_t, starts_with("sport"), starts_with("treatment"))

# number of respondents
# length(unique(data_outcome_cawi_cati$ID_t)) # 12010 (unchanged)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Outcome ####
#+++++++++++++++#

# better outcome is "grade_current" because it determines the grade for the
# academic achievements so far
# however, if it is missing, the final grade is used (does not happen often)
summary(data_outcome_cawi_cati$grade_current)

data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(outcome_grade = ifelse(is.na(grade_current), grade_final, grade_current))

summary(data_outcome_cawi_cati$outcome_grade)


# plausible values: every value above 5.0 is implausible
# hence those are set NA; missing values are downward replaced; remaining
# missing values are deleted
length(unique(data_outcome_cawi_cati$ID_t)) # 12010
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(outcome_grade = replace(outcome_grade, outcome_grade > 5, NA)) %>%
  fill(outcome_grade, .direction = "down") %>%
  filter(outcome_grade <= 5)
summary(data_outcome_cawi_cati$outcome_grade)
length(unique(data_outcome_cawi_cati$ID_t)) # 10684


# interview date of outcome
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    interview_date_outcome = case_when(
      # if grade from grade_current is used, the interview date is taken from CAWI
      outcome_grade == grade_current & !is.na(outcome_grade) ~ interview_date_outcome_cawi,
      # if final grade is used, the interview date is taken from CATI
      outcome_grade == grade_final & !is.na(outcome_grade) ~ interview_date_outcome_cati,
      # for downward replaced grades, interview date is CAWI
      outcome_grade != grade_current & !is.na(outcome_grade) ~ interview_date_outcome_cawi, 
      # for missing grades, interview date would be NA (this is never the case)
      TRUE ~ as.Date(NA)
    )
  )

# check missings
sum(is.na(data_outcome_cawi_cati$grade_current))
sum(is.na(data_outcome_cawi_cati$grade_final))
sum(is.na(data_outcome_cawi_cati$outcome_grade)) # 0
sum(is.na(data_outcome_cawi_cati$interview_date_outcome)) # 0 -> must coincide with outcome_grade

# number of respondents
length(unique(data_outcome_cawi_cati$ID_t)) # 10684 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



## COUNT NUMBER RESPONDENTS ##

# Binary treatment indicator 
## sport participation, non-participation and NA overall
data_outcome_cawi_cati %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(Count = n())
## sport participation, non-participation and NA per treatment period
data_outcome_cawi_cati %>%
  ungroup() %>% group_by(treatment_ends, treatment_sport) %>%
  summarize(Count = n())


# Frequency
data_outcome_cawi_cati %>%
  ungroup() %>% group_by(treatment_sport_freq) %>%
  summarize(Count = n())  

data_outcome_cawi_cati %>%
  ungroup() %>% group_by(treatment_ends, treatment_sport_freq) %>%
  summarize(Count = n())


## DROP VARIABLES NOT NEEDED ANYMORE

str(data_outcome_cawi_cati)
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  select(ID_t, starts_with("interview_date"), treatment_ends,
         treatment_sport, treatment_sport_freq, outcome_grade, starts_with("sport"))


## Number of rows and respondents
print(paste("Number of respondents after outcome and treatment preparation:", 
            length(unique(data_outcome_cawi_cati$ID_t))))
print(paste("Number of rows:", nrow(data_outcome_cawi_cati)))
print(paste("Number of rows:", ncol(data_outcome_cawi_cati)))

num_id_cati_cawi_out_treat <- length(unique(data_outcome_cawi_cati$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Control Variables ####
#+++++++++++++++++++++++++#


# merge control variables
  ## happens over treatment_starts
  ## wave is not necessary anymore
  ## final control variable data set should have one row per treatment period
data_controls <- inner_join(
  data_controls_cati %>% select(-c(wave, treatment_ends)), 
  data_controls_cawi %>% select(-c(wave)),
  by = c("ID_t", "treatment_starts")
)

# merge outcome: observations without outcome or controls are useless -> inner join
# rows from data_controls are left; data_outcome_cawi_cati contains rows with
# treatment ends which do not have a start
data_cati_cawi <- inner_join(
  data_controls, data_outcome_cawi_cati,
  by = c("ID_t", "treatment_starts" = "treatment_ends")
)

# sort rows and column order
data_cati_cawi <- data_cati_cawi %>%
  arrange(ID_t, treatment_starts) %>%
  select(ID_t, interview_date_treatment, interview_date_outcome, interview_date_cati, interview_date_cawi, 
         treatment_starts, treatment_ends, everything())

# ensure that interview date of outcome is always after CATI and CAWI interview
  ## CATI
sum(data_cati_cawi$interview_date_outcome > data_cati_cawi$interview_date_cati) == nrow(data_cati_cawi)
id_wrong <- data_cati_cawi[data_cati_cawi$interview_date_outcome <= data_cati_cawi$interview_date_cati, "ID_t"] %>% 
  pull(ID_t) %>% unique()
data_cati_cawi %>% subset(ID_t == id_wrong[1])
  ## there are 8 individuals where this is a problem
  ## however, this is because I replacd the missing values in the day and month in file 02_b
  ## thus, I just take those dates and subtract 3 months (= 90 days)
data_cati_cawi[data_cati_cawi$interview_date_outcome <= 
                 data_cati_cawi$interview_date_cati, "interview_date_cati"] <-
  data_cati_cawi[data_cati_cawi$interview_date_outcome <= 
                   data_cati_cawi$interview_date_cati, "interview_date_cati"] - 90
  ## check again
sum(data_cati_cawi$interview_date_outcome > data_cati_cawi$interview_date_cati) == nrow(data_cati_cawi)
  ## do the same for CAWI: everything is alright
sum(data_cati_cawi$interview_date_outcome > data_cati_cawi$interview_date_cawi) == nrow(data_cati_cawi)
  ## do the same for treatment
  ## CAWI controls are always after
sum(data_cati_cawi$interview_date_treatment > data_cati_cawi$interview_date_cawi, na.rm = TRUE) == 
  data_cati_cawi %>% ungroup() %>% select(interview_date_treatment, interview_date_cawi) %>% na.omit() %>% nrow() 
  ## CATI controls may be at same interview, if treatment is taken from leisure sport
sum(data_cati_cawi$interview_date_treatment >= data_cati_cawi$interview_date_cati, na.rm = TRUE) == 
  data_cati_cawi %>% ungroup() %>% select(interview_date_treatment, interview_date_cati) %>% na.omit() %>% nrow() 

# check number of respondents
num_id_cati_cawi_all <- length(unique(data_cati_cawi$ID_t)) # 10439

print(paste("Number of respondents after adding control variables from CATI & CAWI:", 
            length(unique(data_cati_cawi$ID_t))))
print(paste("Number of rows:", nrow(data_cati_cawi)))
print(paste("Number of rows:", ncol(data_cati_cawi)))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%#
#### EPISODES ####
#%%%%%%%%%%%%%%%%#

# now the episode data is added to gain knowledge about each respondent's
# education and employment history. 

# keep only individuals who are also in CAWI and CATI data
data_life_course <- data_life_course %>%
  subset(ID_t %in% unique(data_cati_cawi$ID_t))

# new sample size: there are 7 individuals who are in CATI-CAWI data frame
# but not in episode data. Those are 7 of the 18 students who state that they 
# start their study before WT 10/11 
num_id_cati_cawi_eps <- length(unique(data_life_course$ID_t)) # 10432
setdiff(unique(data_cati_cawi$ID_t), unique(data_life_course$ID_t)) # dropped individuals


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## UNI SPELLS ##
#++++++++++++++#

# from data_cati_cawi,, I keep only rows for which the interview dates are in the uni spell
# to do so, a data frame with only the university spells is generated
# so only observations are kept for which the respondent currently studies
data_uni <- data_life_course %>%
  filter(uni_spell == 1)
length(unique(data_uni$ID_t)) # all individuals are kept (10432)

  # subset data for faster computation
  ## DELETE LATER ##
# data_uni <- data_uni %>%
#   subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))
# data_cati_cawi <- data_cati_cawi %>%
#   subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))

  ## show example for merging
data_uni %>% select(ID_t, start_date, end_date) %>% 
  subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))
data_cati_cawi %>% 
  select(ID_t, treatment_starts, treatment_ends, starts_with("interview_date")) %>% 
  subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))
  ## create data frame with ID_t and treatment_starts where interview_date_outcome
  ## and interview_date_treatment is inside the uni spell
  ## this data set is used to keep only those observations
data_check_outcome <- 
  sqldf(
    "select d1.ID_t, treatment_starts
    from data_cati_cawi AS d1
    inner join data_uni AS d2 on d1.ID_t = d1.ID_t and 
    d1.interview_date_outcome between d2.start_date and d2.end_date and
    d1.interview_date_treatment between d2.start_date and d2.end_date"
  ) %>%
  distinct()
  ## drop rows where outcome interview is not inside uni spell
  ## this is done via an inner join
data_cati_cawi_eps <- data_cati_cawi %>%
  inner_join(data_check_outcome, by = c("ID_t", "treatment_starts"))
length(unique(data_cati_cawi_eps$ID_t)) # 9053
data_cati_cawi_eps %>%
  select(ID_t, treatment_starts, treatment_ends, starts_with("interview_date")) %>% 
  subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))

# perform the same check of CAWI and CATI interval
# the interview date for the control variables must be within the uni spell
# subset calculation because of memory problems
ids_sub <- data_cati_cawi %>% pull(ID_t) %>% unique()

# loop to iterate over IDs -> necessary because of memory issues
loop_index_end <- seq(1000, length(unique(data_cati_cawi$ID_t)), by = 1000)
loop_index_end[length(loop_index_end)] <- length(unique(data_cati_cawi$ID_t))
loop_index_start <- 1

data_check_controls <- data.frame()

for (i in loop_index_end) {
  
  # garbage collector
  gc() 
  
  # subset data 
  data_cati_cawi_sub <- data_cati_cawi %>% 
    subset(ID_t %in% ids_sub[loop_index_start:i]) %>% 
    select(ID_t, treatment_starts, interview_date_cati, interview_date_cawi)
  
  data_life_course_sub <- data_life_course %>%
    subset(ID_t %in% ids_sub[loop_index_start:i]) %>%
    select(ID_t, start_date, end_date)
  
  # adjust loop start index, by last used value + 1
  loop_index_start <- i + 1
  
  # SQL: keep only treatment periods where interview dates are in uni spell
  data_check_controls_sub <- 
    sqldf(
      "select d1.ID_t, treatment_starts
    from data_cati_cawi_sub AS d1
    inner join data_life_course_sub AS d2 on d1.ID_t = d1.ID_t and 
    d1.interview_date_cati between d2.start_date and d2.end_date and
    d1.interview_date_cawi between d2.start_date and d2.end_date
    "
    ) %>%
    distinct()
  
  # final data frame
  data_check_controls <- rbind(data_check_controls, data_check_controls_sub)
  
}

length(unique(data_check_controls$ID_t)) # 10439

# keep only respondents with CATI and CAWI survey inside uni spell
data_cati_cawi_eps <- data_cati_cawi_eps %>%
  inner_join(data_check_controls, by = c("ID_t", "treatment_starts")) 

# check number of respondents
length(unique(data_cati_cawi_eps$ID_t)) # 9053
  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Spell length ##
#++++++++++++++++#

# spell length is calculated here, that is for example, the years of schooling,
# previous years of employment and previous years of study

# first, the date closest to the interview outcome date is identified
# this date is used to calculate (or rather adjust) the spell length
data_cati_cawi_eps <- 
  data_cati_cawi_eps %>%
  # create interview date for outcome-treatment: largest of outcome and treatment
  mutate(interview_date_out_treat_spell = pmax(interview_date_outcome, interview_date_treatment)) %>%
  # identify spell date
  mutate(
    # generate variable with decision which date I use: this is based on the smallest
    # difference between the outcome interview date and the control variable interview date 
    decision = ifelse(
      as.numeric(difftime(interview_date_out_treat_spell, interview_date_cati)) < 
        as.numeric(difftime(interview_date_out_treat_spell, interview_date_cawi)), 
      "CATI", "CAWI"
      ),
    # generate date variable which includes the date which is used to adjust the spell length
    interview_date_spell = if_else(decision == "CATI", interview_date_cati, interview_date_cawi)
  ) %>%
  # drop decision variable as this is not needed anymore
  select(-decision)

# life course data is duplicated for each treatment period
# thus, number of rows for each respondent is nrow(data_life_course)*num_treatment_periods
data_cati_cawi_eps %>%
  select(ID_t, treatment_starts, interview_date_spell, interview_date_out_treat_spell, interview_date_cati, interview_date_cawi)

data_cati_cawi_eps_all <- left_join(data_cati_cawi_eps, data_life_course, by = "ID_t")

data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, sptype_2, start_date, end_date, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi) %>%
  head(20)

# adjust date to calculate spell length
  ## if end_date of spell is larger than the interview date AND the interview
  ## date inside the episode, the spell length needs to be adjusted because spell 
  ## is not finished
  ## to simplify this process, I create a new date variable
  ## this variable equals the real end date if the interview date takes place
  ## after the end of the period, otherwise, the interview date
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(end_date_adj = case_when(
    end_date > interview_date_spell & interview_date_spell > start_date ~ interview_date_spell,
    TRUE ~ end_date
  ))


data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, sptype_2, start_date, end_date, end_date_adj, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi) %>%
  head(20)


## CALCULATE SPELL LENGTH ##

# spell length is only calculated if it is relevant for the treatment period,
# i.e. only for current and previous episodes but not for future one
# indicator = 1 if episode is relevant for treatment, i.e., if interview date
# is larger than start_date of episode
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>% 
  mutate(eps_rel = if_else(interview_date_spell > start_date, 1, 0))

data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, sptype_2, start_date, end_date, end_date_adj, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi, eps_rel) %>%
  head(20)


# spell length in years: start_date - end_date
# is only calculated if episode is relevant for the respective treatment period
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(
    spell_length_years = case_when(
      eps_rel == 1 ~ as.numeric(difftime(end_date_adj, start_date, units = "weeks")) / 52.5, 
      TRUE ~ as.double(NA)
    )
  ) 


data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, sptype_2, start_date, end_date, end_date_adj, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi, eps_rel, spell_length_years) %>%
  head(20)


# cumulated years spent in education type
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  # group by ID_t and education type
  group_by(ID_t, treatment_starts, sptype_2) %>%
  # calculate cumulative sum 
  mutate(spell_length_cum_years = cumsum(spell_length_years))


data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, sptype_2, start_date, end_date, end_date_adj, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi, eps_rel, spell_length_years, spell_length_cum_years) %>%
  head(20)


# restructure so that one variable for each education type
# fill missing values 
data_cati_cawi_eps_all <- 
  data_cati_cawi_eps_all %>%
  group_by(ID_t) %>%
  mutate(row = row_number()) %>% # to avoid error message
  mutate(uni_spell = ifelse(sptype_2 == "Uni", 1, 0)) %>%
  pivot_wider(names_from = sptype_2, values_from = spell_length_cum_years,
              names_prefix = "spell_length_cum_") %>%
  select(-row) %>%
  fill(starts_with("spell_length_cum_"), .direction = "down")


data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, start_date, end_date, end_date_adj, 
         interview_date_spell, interview_date_out_treat_spell, interview_date_cati, 
         interview_date_cawi, eps_rel, starts_with("spell_length_cum_")) %>%
  head(20)


# all missing values in spell_length variables are replaced by 0
# missings indicate that person has no observation for spell type, i.e., length is 0
# data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
#   mutate(across(starts_with("spell_"),  ~replace_na(., 0)))

# keep only uni spells
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>% filter(uni_spell == 1 & eps_rel == 1)

# there may be duplicates as in this example because during interviews
# respondent has two uni spells within one treatment period
data_cati_cawi_eps_all %>%
  subset(ID_t == 7017362) %>%
  select(ID_t, treatment_starts, start_date, end_date, end_date_adj, 
         interview_date_spell, spell_length_cum_Emp, spell_length_cum_Uni)

# in this case, the last (most recent) uni spell is kept
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  arrange(ID_t, treatment_starts, start_date) %>%
  group_by(ID_t, treatment_starts) %>%
  slice(n())
  

# calculate length of current study: subtracting interview_spell with start_date
# note: the cumulated variable is the cumulated sum of previous study time (
# including the current one
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(spell_length_current_Uni = as.numeric(difftime(
    interview_date_spell, start_date, units = "weeks")) / 52.5
  )
  ## from this variable one can infer if it is the first study period or not
  ## this is if the cumulated sum equals the current time
  ## create dummy for this
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(uni_first_eps = 
           case_when(spell_length_current_Uni == spell_length_cum_Uni ~ 1,
                     TRUE ~ 0)
         )


# check operations
check <- data_cati_cawi_eps_all %>% 
  select(
    ID_t, start_date, end_date, end_date_adj, treatment_starts, interview_date_spell, 
    interview_date_out_treat_spell, starts_with("spell_length_cum_"), 
    spell_length_current_Uni, uni_first_eps
    )

# drop variables which are not of interest anymore
# order data frame
data_merge_1 <- data_cati_cawi_eps_all %>%
  select(-c(spell_length_years, uni_spell, eps_rel, sptype, spms, splast, 
            start_date, end_date, interview_date_outcome_cati, 
            interview_date_outcome_cawi)) %>%
  select(ID_t, treatment_starts, starts_with("interview_"), 
         treatment_sport, treatment_sport_freq, outcome_grade, 
         starts_with("spell_"), starts_with("educ_"),
         everything())



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## CURRENT EMPLOYMENT ##
#++++++++++++++++++++++#


# of interest is if student is working during study
# create data frame
# to identify this, I need to come back to the previous data frame
data_emp <- data_life_course %>% 
  select(ID_t, sptype_2, start_date, end_date, matches("emp")) %>%
  filter(sptype_2 == "Emp")

# employment start needs to be smaller than interview date
# employment ends needs to be larger than the interview date
# to determine this the CATI interview date is used as this is the interview for
# control variables which is usually the closest to the outcome+treatnent interview
# the created data frame identifies the employment spells during the university study
test_1 <- data_emp %>% subset(ID_t %in% c(7001969, 7017362, 7019370)) %>% 
  select(ID_t, sptype_2, start_date, end_date, starts_with("emp")) %>%
  filter(sptype_2 == "Emp")
test_2 <- data_cati_cawi %>% 
  subset(ID_t %in% c(7001969, 7017362, 7019370)) %>% 
  select(ID_t, interview_date_cati)
test_3 <- data_life_course %>% 
  subset(ID_t %in% c(7001969, 7017362, 7019370) & sptype_2 == "Uni") %>% 
  select(ID_t, start_date, end_date)

data_check_emp <-
  sqldf(
    "SELECT * 
    FROM test_1 AS e
    INNER JOIN test_2 AS c ON e.ID_t = c.ID_t AND
    e.start_date < c.interview_date_cati AND e.end_date > c.interview_date_cati
    ")
# # drop duplicated ID column and duplicates in general
# data_check_emp <- data_check_emp[!duplicated(as.list(data_check_emp))]
# data_check_emp <- data_check_emp %>% distinct()
# # if duplicated interview_date, start_date, and end_date, keep only first
# data_check_emp <- data_check_emp %>%
#   distinct(interview_date_cati, start_date, end_date, .keep_all = TRUE)

# drop duplicated ID resulting from join in sqldf
data_check_emp <- data_check_emp %>%
  subset(select = which(!duplicated(names(.)))) 

# calculate length of current employment at CATI interview
data_check_emp <- data_check_emp %>%
 mutate(
    spell_length_current_Emp = as.numeric(difftime(interview_date_cati, start_date, units = "weeks")) / 52.5
  )

# select only variables needed for analysis
data_check_emp <- data_check_emp %>%
  select(ID_t, interview_date_cati, starts_with("emp"), spell_length_current_Emp) 

# add prefix for emp_ variables
data_check_emp <- cbind(
  data_check_emp %>%
    select(starts_with("emp")) %>%
    rename_with(~ paste("current",.x,  sep = "_")),
  data_check_emp %>%
    select(-starts_with("emp"))
)

# add employment to respective treatment period
data_merge_2 <- left_join(
  data_merge_1, data_check_emp, by = c("ID_t", "interview_date_cati")
)

# drop the employment variables not needed anymore
data_merge_2 <- data_merge_2 %>%
  select(-starts_with("emp"))

# number of respondents, rows and columns
print(paste("Number of respondents after outcome and treatment preparation:", 
            length(unique(data_merge_2$ID_t))))
print(paste("Number of rows:", nrow(data_merge_2)))
print(paste("Number of columns:", ncol(data_merge_2)))

num_id_merge_2 <- length(unique(data_merge_2$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%#
#### SIBLING ####
#%%%%%%%%%%%%%%%#


# merge sibling information
# as this information is cross-sectional, only the ID is needed for the merge
data_merge_3 <- left_join(
  data_merge_2, data_sibling, by = "ID_t"
)
  ## for respondents with no siblings all variables are set to 0
col_sibling <- colnames(data_sibling %>% select(-ID_t))
data_merge_3 <- data_merge_3 %>%
  mutate_at(all_of(col_sibling), ~replace_na(.,0))


# number of respondents, rows and columns
print(paste("Number of respondents after outcome and treatment preparation:", 
            length(unique(data_merge_3$ID_t))))
print(paste("Number of rows:", nrow(data_merge_3)))
print(paste("Number of columns:", ncol(data_merge_3)))

num_id_merge_3 <- length(unique(data_merge_3$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
#### CHILD ####
#%%%%%%%%%%%%%#

# information about each respondent's children is appended.
# this information is only collected in the CATI surveys
# Hence, as merge variable the CATI interview date is used additional to the ID
data_merge_4 <- left_join(data_merge_3, data_child %>% select(-wave), 
                          by = c("ID_t", "interview_date_cati" = "interview_date"))
  
  
# for respondents with no children all variables are set to zero
col_child <- data_child %>% select(-c(ID_t, wave, interview_date)) %>% colnames()
data_merge_4 <- data_merge_4 %>%
  mutate_at(all_of(col_child), ~replace_na(.,0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%#
#### Partner ####
#%%%%%%%%%%%%%%%#


# information about each respondent's partner history is appended.
# this information is only collected in the CATI surveys
# Hence, as merge variable the CATI interview date is used additional to the ID
data_merge_5 <- left_join(data_merge_4, data_partner %>% select(-wave), 
                          by = c("ID_t", "interview_date_cati" = "interview_date"))


# for respondents with no partner all variables are set to zero
col_partner <- data_partner %>% select(-c(ID_t, wave, interview_date)) %>% colnames()
data_merge_5 <- data_merge_5 %>%
  mutate_at(all_of(col_partner), ~replace_na(.,0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%#
#### Competencies ####
#%%%%%%%%%%%%%%%%%%%%#

# data competencies is merged via the treatment_starts indicator
# to do so, the duplicates are removed, so that every individual has one
data_merge_6 <- left_join(data_merge_5, data_competencies, 
                          by = c("ID_t", "treatment_starts"))
 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# number of respondents, rows and columns
print(paste("Number of respondents after merge process:", length(unique(data_merge_6$ID_t))))
print(paste("Number of rows after merge process:", nrow(data_merge_6)))
print(paste("Number of columns after merge process:", ncol(data_merge_6)))


# save
saveRDS(data_merge_6, "Data/prep_4_merge.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

