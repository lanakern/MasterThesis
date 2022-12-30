#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### GENERATE TREATMENT AND OUTCOME VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the treatment and outcome variables are generated.
# -> outcome: current average grade
# -> treatment: sport participation as binary variable (0,1) and multiple
# treatment variable considering the frequency (daily, weekly, monthly, never).
#++++
# 1.) Treatment
#++++
# 2.) Outcome
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#

# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("xlsx")) install.packages("xlsx")
library(xlsx)  # for excel file

# define inputs
  ## selection on cohort preparation
#cohort_prep <- "controls_bef_outcome" 
cohort_prep <- "controls_same_outcome"
  ## only for saving
treatment_repl <- "downup" 


# load data
if (cohort_prep == "controls_same_outcome") {
  data_raw <- readRDS("Data/Prep_4/prep_4_merge_all.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_raw <- readRDS("Data/Prep_4/prep_4_merge_all_robustcheck.rds")
}


#%%%%%%%%%%%%%%%%%#
#### Treatment ####
#%%%%%%%%%%%%%%%%%#

# count missing values in treatment variables
colSums(is.na(data_raw %>% select(starts_with("sport"))))


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

# recode: in original data set the order is reversed AND the meaning of the
# labels is not identical. Thus, it does not make sense to unlabel them
## 0 = never 
## 1 = once a month or less frequently
## 2 = several times a month or once a week
## 3 = at least once per week
## 4 = (almost) daily
table(data_outcome_cawi_cati$sport_uni, useNA = "always")
table(data_outcome_cawi_cati$sport_uni_freq, useNA = "always")
table(data_outcome_cawi_cati$sport_leisure_freq, useNA = "always")

data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    sport_uni_freq = recode(
      sport_uni_freq,
      "less frequently" = 1, "once a month" = 1, "several times a month" = 2,
      "once a week" = 2, "several times a week" = 3, "daily" = 4
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

# generate one treatment frequency variable (for both university and leisure sport)
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    treatment_sport_freq = case_when(
      # if uni sport is missing use info on leisure sport
      is.na(sport_uni_freq) ~ sport_leisure_freq,
      # if leisure sport frequency is higher, however, use info on leisure sport
      sport_leisure_freq > sport_uni_freq & !is.na(sport_leisure_freq) & !is.na(sport_uni_freq) ~ sport_leisure_freq,
      # in all other cases use uni sport
      TRUE ~ sport_uni_freq
    )
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
summary(data_outcome_cawi_cati$grade_current) # 3,661 NAs

data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(outcome_grade = ifelse(is.na(grade_current), grade_final, grade_current))

summary(data_outcome_cawi_cati$outcome_grade) # 3654 NAs


# plausible values: every value above 5.0 is implausible
# hence those are set NA; missing values are downward replaced; remaining
# missing values are deleted
# length(unique(data_outcome_cawi_cati$ID_t)) # 12010
# data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
#   mutate(outcome_grade = replace(outcome_grade, outcome_grade > 5, NA)) %>%
#   fill(outcome_grade, .direction = "down") %>%
#   filter(outcome_grade <= 5)
# summary(data_outcome_cawi_cati$outcome_grade)
# length(unique(data_outcome_cawi_cati$ID_t)) # 10684


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
sum(is.na(data_outcome_cawi_cati$outcome_grade)) # 0
sum(is.na(data_outcome_cawi_cati$interview_date_outcome)) # 0 -> must coincide with outcome_grade

# number of respondents
# length(unique(data_outcome_cawi_cati$ID_t)) # 12,010 (unchanged)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



## GENERATE ONE INTERVIEW DATE END VARIABLE

# this variable includes the date where the treatment period ends, that is
# the last interview in the respective treatment period.
# always highest date is taken from interview_date_outcome and interview_date_treatment

# # For example, treatment may be taken from CATI but outcome from CAWI
# sum(data_outcome_cawi_cati$interview_date_outcome != data_outcome_cawi_cati$interview_date_treatment, na.rm = TRUE)
# data_outcome_cawi_cati %>% subset(ID_t == 7002011) %>% 
#   select(ID_t, starts_with("sport"), starts_with("treatment"), starts_with("grade"), 
#          starts_with("outcome"), interview_date_outcome, interview_date_treatment)
# # Or grade is taken from CATI and sport from CAWI
# data_outcome_cawi_cati %>% subset(ID_t == 7006217) %>%
#   select(ID_t, starts_with("sport"), starts_with("treatment"), starts_with("grade"), starts_with("outcome"),
#          interview_date_outcome, interview_date_treatment)


data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  mutate(
    interview_date_end = case_when(
      interview_date_outcome > interview_date_treatment ~ interview_date_outcome,
      is.na(interview_date_outcome) ~ as.Date(NA), is.na(interview_date_treatment) ~ as.Date(NA), 
      TRUE ~ interview_date_treatment
    )
  )

sum(is.na(data_outcome_cawi_cati$interview_date_treatment))
sum(is.na(data_outcome_cawi_cati$interview_date_outcome))
sum(is.na(data_outcome_cawi_cati$interview_date_end))


## DROP VARIABLES NOT NEEDED ANYMORE

str(data_outcome_cawi_cati)
data_outcome_cawi_cati <- data_outcome_cawi_cati %>%
  select(ID_t, starts_with("interview_date"), treatment_ends,
         treatment_sport, treatment_sport_freq, outcome_grade, starts_with("sport"))


## Number of rows and respondents
num_id_cati_cawi_out_treat <- length(unique(data_outcome_cawi_cati$ID_t)) # 12010

print(paste("Number of respondents after outcome and treatment preparation:", 
            num_id_cati_cawi_out_treat))
print(paste("Number of rows:", nrow(data_outcome_cawi_cati)))
print(paste("Number of rows:", ncol(data_outcome_cawi_cati)))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Merge Control Variables ####
#+++++++++++++++++++++++++#

# merge control variables
## happens over treatment_starts
## wave is not necessary anymore
## final control variable data set should have one row per treatment period
data_controls <- inner_join(
  data_controls_cati %>% select(-c(wave)), 
  data_controls_cawi %>% select(-c(wave)),
  by = c("ID_t", "treatment_starts")
)

length(unique(data_controls_cati$ID_t)) # 11,726
length(unique(data_controls_cawi$ID_t)) # 12,010
length(unique(data_controls$ID_t)) # 11,726

# merge outcome: observations without outcome or controls are useless -> inner join
# rows from data_controls are left; data_outcome_cawi_cati contains rows with
# treatment ends which do not have a start
data_cati_cawi <- inner_join(
  data_controls, data_outcome_cawi_cati,
  by = c("ID_t", "treatment_starts" = "treatment_ends")
)

length(unique(data_cati_cawi$ID_t)) # 11,726

# sort rows and column order
data_cati_cawi <- data_cati_cawi %>%
  arrange(ID_t, treatment_starts) %>%
  select(ID_t, interview_date_treatment, interview_date_outcome, interview_date_cati, interview_date_cawi, 
         treatment_starts, treatment_ends, everything())

# generate one variable for interview_date_start
# this variable is the smallest interview date between interview_date_cati and interview_date_cawi
# typically it is CAWI (see below "never the case")
sum(is.na(data_cati_cawi$interview_date_cati)) # 0 
sum(is.na(data_cati_cawi$interview_date_cawi)) # 0
data_cati_cawi %>% filter(interview_date_cawi > interview_date_cati) %>% nrow() # never the case
data_cati_cawi <- data_cati_cawi %>%
  mutate(
    interview_date_start = case_when(
      # only if CATI interview comes after CAWI use this as interview_date_start (however, never the case)
      interview_date_cawi > interview_date_cati ~ interview_date_cati, TRUE ~ interview_date_cawi
    )
  )
## check that interview_date_start always equals interview_date_cawi
sum(data_cati_cawi$interview_date_start == data_cati_cawi$interview_date_cawi) == nrow(data_cati_cawi)

# ensure that interview end date is always after interview start date
data_cati_cawi %>%
  ungroup() %>% 
  filter(!is.na(interview_date_end)) %>%
  summarise(sum_wrong = sum(interview_date_start >= interview_date_end)) 

# check number of respondents
## is again reduced due to missing observations in CATI
num_id_cati_cawi_out_treat_contr <- length(unique(data_cati_cawi$ID_t))
print(paste("Number of respondents after adding control variables from CATI & CAWI:", 
            num_id_cati_cawi_out_treat_contr)) # 11,726
print(paste("Number of rows:", nrow(data_cati_cawi)))
print(paste("Number of rows:", ncol(data_cati_cawi)))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Sample Selection: Treatment and Outcome ####
#+++++++++++++++++++++++++++++++++++++++++++++++#

# subset: keep only respondents who do not have any missing in treatment and
# grade 
data_cati_cawi <- 
  data_cati_cawi %>%
  filter(!is.na(treatment_sport) & !is.na(outcome_grade))

num_id_treatment_outcome_no_na <- length(unique(data_cati_cawi$ID_t)) # 9,069


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#