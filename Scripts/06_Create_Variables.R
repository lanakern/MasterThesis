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

# define parameters
  ## how to replace missing values
na_replace <- "constant" # "regression"



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

# summary of years of schooling, university studies, vocational training, and
# vocational preparation
data <- data %>%
  mutate(educ_years = rowSums(across(
    c("spell_length_cum_School", "spell_length_cum_Uni", "spell_length_cum_VocTrain", "spell_length_cum_VocPrep")
    ))) 
summary(data$educ_years)



## BMI ##

# BMI = weight / height^2
data <- data %>%
  mutate(
    BMI = health_weight / (health_height^2)
  )

sum(is.na(data$health_weight)) # number of NA in health_weight
sum(is.na(data$health_height)) # number of NA  in health_height
sum(rowSums(is.na(data[, c("health_weight", "health_height")])) > 0) # number of NA in both
sum(is.na(data$BMI)) # number of NA in generated variable


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
  "uni_counsel_.*_offer", "uni_counsel_.*_use"
)
  ## vector with variables aggregated as one binary variables
vars_aggregate_binary <- c(
  "drugs_[^m]" # starts with drugs_ but not follows m for motive
)


# load function 
source("Functions/func_aggregate_vars.R")

# ungroup data frame
data <- data %>% ungroup()

# show example
data %>% select(ID_t, matches("uni_counsel_.*_offer"))
data %>% select(ID_t, starts_with("risk_"))
data %>% select(ID_t, matches("drugs_[^m]"))

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
  ## binary
for (vars_aggr in vars_aggregate_binary) {
  # ensure that all variables are numeric
  data <- data %>% mutate(across(starts_with(vars_aggr), as.numeric))
  # then apply aggregation
  data <- func_aggregate_vars(data, vars_aggr, "no", "binary")
}


data %>% select(ID_t, matches("uni_counsel_offer"))
data %>% select(ID_t, starts_with("risk"))
data %>% select(ID_t, starts_with("drugs"))



## POLITICS ##
#++++++++++++#

# special variable: politics with different scales
# just create dummy if one is (highly) interest in politics
# vote is exlucing because also individuals with no interest vote and
# this increases group with no interest

data <- data %>%
  # create dummy
  mutate(interest_politics = case_when(
    # if there is any sign of political interest, variable takes on value 1
    interest_politics_signatures == 1 ~ 1,
    interest_politics_demo == 1 ~ 1,
    #interest_politics_vote == 1 ~ 1,
    interest_politics_general %in% c(1:2) ~ 1,
    interest_politics_discussion %in% c(1:2) ~ 1,
    interest_politics_understanding %in% c(1:2) ~ 1,
    # if all variables are NA then dummy variable is also NA
    data %>% select(starts_with("interest_politics")) %>% 
      is.na() %>% rowSums() == 
      data %>% select(starts_with("interest_politics")) %>% 
      colnames() %>% length() ~ as.double(NA), 
    # otherwise 0
    TRUE ~ 0
  )) %>%
  # drop other variables
  select(-starts_with("interest_politics_"))

table(data$interest_politics, useNA = "always")


## SMOKING ##
#+++++++++++# 

# check NAs
table(data$health_smoking, useNA = "always")
table(data$health_smoking_v1, useNA = "always") # less missing values

#%%
# DROP LATER
data <- data %>%
  mutate(health_smoking = case_when(
    health_smoking == "yes, daily" ~ 1,
    health_smoking == "yes, occasionally" ~ 2,
    health_smoking == "no, not anymore" ~ 3,
    health_smoking == "have never smoked" ~ 4,
    TRUE ~ as.double(NA)
  )) %>%
  mutate(
    health_smoking_v1 = case_when(
      health_smoking_v1 == "have never smoked" ~ 1,
      health_smoking_v1 == "did smoke before" ~ 2,
      health_smoking_v1 == "currently smoke occasionally" ~ 3,
      health_smoking_v1 == "currently smoke every day" ~ 4,
      TRUE ~ as.double(NA)
    )
  )
#%%


# reverse score of health_smoking_v1
data <- func_reverse_score(data, data.frame(
  "vars_reverse" = "health_smoking_v1", "num_scores" = 4
))


# replace missings in new variable "health_smoking" with values from
# previous version
data <- data %>%
  mutate(health_smoking = case_when(
    is.na(health_smoking) ~ health_smoking_v1,
    TRUE ~ health_smoking
  ))

# generate dummy for smoking
data <- data %>%
  mutate(health_smoking_current = case_when(
    health_smoking %in% c(1:2) ~ 1,
    health_smoking %in% c(3:4) ~ 0,
    TRUE ~ as.double(NA)
  ))


# drop other smoking variables
data <- data %>%
  select(-c("health_smoking_v1", "health_smoking","health_smoking_number"
  ))

table(data$health_smoking_current, useNA = "always")


## ALCOHOL ##
#+++++++++++#

table(data$health_alcohol, useNA = "always")
table(data$health_alcohol_v1, useNA = "always")

# only minor differences in labels
data <- data %>%
  mutate(
    health_alcohol_v1 = recode(health_alcohol_v1, 
       "(almost) every day" = "daily",
       "(almost) never" = "never"
    )
  )

# replace new variable by previous if NA and generate the dummy for 
# alcohol consumption
data <- data %>%
  # replacement
  mutate(health_alcohol = case_when(
    is.na(health_alcohol) ~ health_alcohol_v1,
    TRUE ~ health_alcohol
  )) %>%
  # dummy generation
  mutate(
    health_alcohol = case_when(
      health_alcohol %in% c("daily", "once a week", "several times a week") ~ 1,
      is.na(health_alcohol) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  # drop v1 variable
  select(-health_alcohol_v1)


table(data$health_alcohol, useNA = "always")



## HEALTH ##
#++++++++++#


# dummy for good general health
table(data$health_general, useNA = "always")

data <- data %>%
  mutate(
    health_general_good = case_when(
      health_general %in% c("good", "very good") ~ 1,
      is.na(health_general) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  select(-health_general)

table(data$health_general_good, useNA = "always")


# dummy for good physical and mental health (asked for last 30 days)
# not good health can be extracted if this is true for at least 7 days
summary(data$health_physical)
sum(data$health_physical > 6, na.rm = TRUE)

summary(data$health_mental)
sum(data$health_mental > 6, na.rm = TRUE)

data <- data %>%
  mutate(
    health_physical_good = case_when(
      health_physical < 7 ~ 1,
      is.na(health_physical) ~ as.double(NA),
      TRUE ~ 0
    ),
    health_mental_good = case_when(
      health_mental < 7 ~ 1,
      is.na(health_mental) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  select(-c(health_physical, health_mental))

table(data$health_physical_good, useNA = "always")
table(data$health_mental_good, useNA = "always")



# dummy for chronic health issues: disability, allergic, neurodermitis
table(data$health_disability, useNA = "always")
table(data$health_allergic, useNA = "always")
table(data$health_neuro, useNA = "always")

data <- data %>%
  mutate(health_chronic = case_when(
    health_disability == "yes" | health_allergic == "yes" | health_neuro == "yes" ~ 1,
    is.na(health_disability) & is.na(health_allergic) & is.na(health_neuro) ~ as.double(NA),
    TRUE ~ 0
  ))

table(data$health_chronic, useNA = "always")




## EXTRACURRICULAR ACTIVITY ##

# generate binary variables for each activity 
# to do so replace "involved" = 1 and "not involved" = 0
vars_recode <- data %>%
  ungroup() %>%
  select(starts_with("extracurricular_") & !ends_with("freq")) %>%
  colnames()

data <- data %>% 
  mutate_at(
    all_of(vars_recode), 
    list(
      ~recode(., `involved` = 1, `not involved` = 0, 
              .default = NaN) # NA does not work
    ) 
  ) %>%
  # replace NaN with NA
  mutate_all(~ifelse(is.nan(.), NA, .))


#%%%%%%%%%%%%%%%%%%%%%%%#
#### Dummy Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%#

# automatically generate dummy variables
# LASSO will select which are important
data <- dummy_cols(
  data, remove_selected_columns = TRUE, remove_first_dummy = TRUE, 
  select_columns = c("interest_art_musuem", "interest_reading_num_books")
  )



#%%%%%%%%%%%%%%%%%%%%%%#
#### Drop Variables ####
#%%%%%%%%%%%%%%%%%%%%%%#

# create new data frame for dropping
data_sub <- data

## DROP VARIABLES NOT NEEDED ANYMORE ##
#+++++++++++++++++++++++++++++++++++++#


# data_sub <- data %>%
#   select(-c(
#     "current_emp", "current_emp_2", "current_volontariat", "current_ref",
#     "current_pracyear", "current_intern", "current_voctrain", "current_study",
#     "current_study_academy", "current_study_college", "current_study_coopuni",
#     "gap_type", "educ_uni_break_term_off", "educ_uni_break_deregist_temp",
#     "educ_uni_break_deregist_nform",
#     
#     
#   ))


#### Drop Variables with too many Missing Values ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++#

# variables with too many missing values are dropped
# that is variables with more than 70% of missing values
perc_drop_na <- nrow(data_sub) * 0.7
  ## identify those variables
col_sums_na <- colSums(is.na(data_sub))
col_sums_na_drop <- col_sums_na[col_sums_na > perc_drop_na]


## SPELL LENGTH ##

# many missing values in spell length
# this is because not many individuals take part in military, vocational
# training etc.
# in this case, I generate dummy variables if respondent takes part
# I only do so for military and vocational training and current
# employment (other variables are uninteresting and have too many missings)
data_sub <- data_sub %>%
  mutate(
    educ_military = case_when(spell_length_cum_Military > 0 ~ 1, TRUE ~ 0),
    educ_voctrain = case_when(spell_length_cum_VocTrain > 0 ~ 1, TRUE ~ 0),
    educ_emp = case_when(spell_length_current_Emp > 0 ~ 1, TRUE ~ 0)
  ) %>%
  select(
    -c(all_of(names(col_sums_na_drop)[stringr::str_starts(names(col_sums_na_drop), "spell_length")]))
  )



## DROP REMAINING VARIABLES ##
col_sums_na <- colSums(is.na(data_sub))
col_sums_na_drop <- col_sums_na[col_sums_na > perc_drop_na]



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### NA Dummies and Replacement ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# replacements and NA dummies are already made for:
  ## competencies, child, partner, and sibling -> CHECK
  ## maybe this makes sense because why mean, regression for those?
  ## for competencies: average and plausible values -> makes sense

# for other variables like BMI, other strategies:


## "CONSTANT" ##

# for numeric values median is replaced (median instead of mean due to outliers)
# for character variables most often value is replaced
# for binary variables 0
# -> In any case NA variable is generated indicating that this value is originally
# missing and replaced.


## "REGRESSION" ##



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#


# save data frame
saveRDS(data, "Data/prep_6_variables.rds")
