#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#

# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

# load data
data <- readRDS("Data/prep_5_sample_selection.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


## AGE ##

# 1.) Create birth date from month and year with day = 1
source("Functions/func_generate_date.R")
data <- 
  func_generate_date(data, month = "birth_month", year = "birth_year", 
                     varname = "birth_date")

# 2.) Age = interview date used for spell - birth date 
data <- data %>%
  mutate(age = as.numeric(difftime(interview_date_spell, birth_date, units = "weeks") / 52.5))
summary(data$age)



## YEARS OF EDUCATION ##

# summary of years of schooling and university studies
data <- data %>%
  mutate(educ_years = rowSums(across(c("spell_length_cum_School", "spell_length_cum_Uni")))) 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Lagged Outcome and Treatment variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# THINK ABOUT NA IN LAGS

# generate lags for the two treatment and one outcome variable
data <- data %>%
  group_by(ID_t) %>%
  mutate(
    treatment_sport_lag = lag(treatment_sport), 
    treatment_sport_freq_lag = lag(treatment_sport_freq),
    outcome_grade_lag = lag(outcome_grade)
  ) 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Aggregate variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## REVERSE SCORE ##
#+++++++++++++++++#


# recode positive /negative values: some variables measure the same thing but
# in a different direction. For instance "stress" is measured negatively 
# "I often feel lonely" and positively "I do meaningful work". Those variables
# need to be changed manually in one direction.

# load function
source("Functions/func_reverse_score.R")


# create data frame with variable name and highest variable value number
# for those variables for which the order of the scale needs to be reversed.
df_reverse_vars <- data.frame(
  "vars_reverse" = c("uni_termination_4", paste0("stress_", c(3, 5:7, 11)),
                     "uni_commitment_1", "uni_commitment_4",
                     paste0("personality_selfesteem_", c(2, 5, 6, 8, 9)),
                     paste0("opinion_educ_", c(2,4,8,9,14,15)),
                     paste0("satisfaction_study_", c(2,3,5,6,8,9))
                     ),
  "num_scores" = c(4, rep(5, 5), 5, 5, rep(5, 5), rep(5,6), rep(10, 6))
)


# apply function and check
data %>% select(uni_termination_4, stress_3, satisfaction_study_2)
data <- func_reverse_score(data, df_reverse_vars)
data %>% select(uni_termination_4, stress_3, satisfaction_study_2)



## AGGREGATE VARIABLES ##
#+++++++++++++++++++++++#

# create vector including all variables that should be aggregated
# in the upcoming function variables are identified using prefixes.
  ## vector with variables aggregated as mean
vars_aggregated_mean <- c(
  ## CAWI
  "personality_goal_pers", "personality_goal_flex", "parents_importance_success",
  "uni_counsel_quality", "uni_quality", "uni_best_student", "uni_fear",
  "uni_anxiety", "uni_termination", "uni_commitment", "uni_prep",
  "academic", "helpless", "social_integr", "stress", 
  ## CATI
  "personality_assertiveness", "personality_conflicts", 
  "personality_selfesteem", "parents_opinion_degree", "opinion_educ",
  "motivation_degree", "satisfaction_study", "satisfaction_life",
  "interest_math", "interest_german", "risk",
  ## CATI & CAWI
  "friends_opinion_degree"
)
  ## vector with variables aggregated as sum
vars_aggregated_sum <- c(
  ## CAWI
  "uni_counsel_.*_offer", "uni_counsel_.*_use"
)


# load function 
source("Functions/func_aggregate_vars.R")

# ungroup data frame
data <- data %>% ungroup()

# show example
data %>% select(ID_t, matches("uni_counsel_.*_offer$"), starts_with("risk"))

# apply aggregation
  ## mean
for (vars_aggr in vars_aggregated_mean) {
  # ensure that all variables are numeric
  data <- data %>% mutate(across(starts_with(vars_aggr), as.numeric))
  # then apply aggregation
  data <- func_aggregate_vars(data, vars_aggr, "no", "mean")
}
  ## sum
for (vars_aggr in vars_aggregated_sum) {
  # ensure that all variables are numeric
  data <- data %>% mutate(across(starts_with(vars_aggr), as.numeric))
  # then apply aggregation
  data <- func_aggregate_vars(data, vars_aggr, "no", "sum")
}


data %>% select(ID_t, starts_with("uni_counsel_offer"), starts_with("risk"))


## POLITICS ##

# special variable: politics with different scales
# just create dummy if one is interest in politics
data <- data %>%
  # create dummy
  mutate(interest_politics = case_when(
    interest_politics_signatures == "yes" ~ 1,
    interest_politics_demo == "yes" ~ 1,
    interest_politics_vote == "yes" ~ 1,
    interest_politics_general %in% c("very interested", "rather interested", "little interested") ~ 1,
    interest_politics_discussion %in% c("daily", "several times a week", "once a week", "several times a month") ~ 1,
    TRUE ~ 0
  )) %>%
  # drop other variables
  select(-starts_with("interest_politics_"))

table(data$interest_politics, useNA = "always")



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#


# save data frame
saveRDS(data, "Data/prep_6_variables.rds")
