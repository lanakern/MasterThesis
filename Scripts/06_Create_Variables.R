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

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # for replace_na() function

if (!require("fastDummies")) install.packages("fastDummies")
library(fastDummies)  # to generate dummy variables

# load data
data <- readRDS("Data/prep_5_sample_selection.rds")

# define parameters
  ## how to replace missing values
na_replace <- "constant" # "mice", "forest"



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


## YEARS OF EDUCATION ##

# summary of years of schooling, university studies, vocational training, and
# vocational preparation
data <- data %>%
  mutate(educ_years = rowSums(
    across(c("spell_length_cum_School", "spell_length_cum_Uni")), # , "spell_length_cum_VocTrain", "spell_length_cum_VocPrep")),
    na.rm = TRUE
    )) 
summary(data$educ_years)

# summary across age and education years
table(round(data$age), round(data$educ_years))


## BMI ##

# check weight and height for unrealistic values
summary(data$health_weight)
summary(data$health_height)

# BMI = weight / height^2
data <- data %>%
  mutate(
    BMI = health_weight / ((health_height/100)^2)
  )

sum(is.na(data$health_weight)) # number of NA in health_weight
sum(is.na(data$health_height)) # number of NA  in health_height
sum(rowSums(is.na(data[, c("health_weight", "health_height")])) > 0) # number of NA in both
sum(is.na(data$BMI)) # number of NA in generated variable


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Outcome and Treatment variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## GENERATE LAGS ##
#+++++++++++++++++#

# generate lags for the two treatment and one outcome variable
data <- data %>%
  group_by(ID_t) %>%
  mutate(
    treatment_sport_lag = lag(treatment_sport), 
    treatment_sport_freq_lag = lag(treatment_sport_freq),
    outcome_grade_lag = lag(outcome_grade)
  )

# if lag is NA (that is only for first treatment period) current value is used
data <- data %>%
  arrange(ID_t, treatment_period) %>%
  group_by(ID_t) %>%
  mutate(
    treatment_sport_lag = case_when(is.na(treatment_sport_lag) ~ treatment_sport, TRUE ~ treatment_sport_lag),
    treatment_sport_freq_lag = case_when(is.na(treatment_sport_freq_lag) ~ treatment_sport_freq, TRUE ~ treatment_sport_freq_lag),
    outcome_grade_lag = case_when(is.na(outcome_grade_lag) ~ outcome_grade, TRUE ~ outcome_grade_lag)
  )

# ensure that no missing values in lagged variables are left
sum(is.na(data %>% ungroup() %>% select(ends_with("_lag"))))



## STANDARDIZE OUTCOME ##
#+++++++++++++++++++++++#

# standardize outcome variable to have mean zero and standard deviation of 1
data$outcome_grade_stand <- scale(data$outcome_grade)[, 1]
summary(data$outcome_grade_stand) # mean zero
sd(data$outcome_grade_stand) # standard deviation of 1



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
  "uni_counsel_.*_quality", "uni_quality", "uni_best_student", "uni_fear",
  "uni_anxiety", "uni_termination", "uni_commitment", "uni_prep",
  "academic", "helpless", "social_integr", "stress", 
  ## CATI
  "personality_assertiveness", "personality_conflicts", 
  "personality_selfesteem", "parents_opinion_degree", "opinion_educ",
  "motivation_degree", "satisfaction_study", "satisfaction_life",
  "interest_math", "interest_german", "risk", #"uni_offers_.*_helpful",
  ## CATI & CAWI
  "friends_opinion_degree"
)
  ## vector with variables aggregated as sum
vars_aggregated_sum <- c(
  "uni_counsel_.*_offer", "uni_counsel_.*_use", "uni_offers_.*_partic"
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

# both smoking variables as integer (so that they have same data type)
# this avoids mistakes
data <- data %>%
  mutate(health_smoking = as.integer(health_smoking), 
         health_smoking_v1 = as.integer(health_smoking_v1))

# check NAs
table(data$health_smoking, useNA = "always")
table(data$health_smoking_v1, useNA = "always") # less missing values

# reverse score of health_smoking_v1
data <- func_reverse_score(data, data.frame(
  "vars_reverse" = "health_smoking_v1", "num_scores" = 4
))


# replace missings in new variable "health_smoking" with values from
# previous version
data <- data %>%
  mutate(
    health_smoking = ifelse(is.na(health_smoking_v1), health_smoking, health_smoking_v1)
  ) %>%
  select(-health_smoking_v1)

# generate dummy for smoking
  ## 1&2: smoke daily or occasionally
data <- data %>%
  mutate(health_smoking_current = case_when(
    health_smoking %in% c(1:2) ~ 1,
    health_smoking %in% c(3:4) ~ 0,
    TRUE ~ as.double(NA)
  )) %>%
  ## for missings use also health_smoking_number
  mutate(health_smoking_current = ifelse(
    is.na(health_smoking_current) & health_smoking_current > 0, 1, health_smoking_current)
    ) %>%
  ## drop other smoking variables
  select(-c("health_smoking","health_smoking_number"))
  
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


## Spell Length ##
#++++++++++++++++#

# spell length variables are NA for individuals who never experienced this spell,
# for example for individuals who never went to the military, never undertook
# vocational training etc. # In this case, the values are set to zero.
# I only do so for military, vocational training, gap and current
# employment (other variables are uninteresting and have too many missings)
data <- data %>%
  mutate(
    spell_length_cum_Military = case_when(spell_length_cum_Military > 0 ~ 1, TRUE ~ 0),
    spell_length_cum_VocTrain = case_when(spell_length_cum_VocTrain > 0 ~ 1, TRUE ~ 0),
    spell_length_current_Emp = case_when(spell_length_current_Emp > 0 ~ 1, TRUE ~ 0),
    spell_length_cum_Gap = case_when(spell_length_cum_Gap > 0 ~ 1, TRUE ~ 0)
  )
  # ) %>%
  # select(
  #   -c(all_of(names(col_sums_na_drop)[stringr::str_starts(names(col_sums_na_drop), "spell_length")]))
  # )


## Child ##
#+++++++++#

# generate indicator for having a child
data <- data %>%
  mutate(
    child = case_when(!is.na(child_total_num) ~ 1, TRUE ~ 0)
  )


## Partner ##
#+++++++++++#

# generate indicator if respondent currently has a partner
data <- data %>%
  mutate(
    partner = case_when(partner_current_no == 0 ~ 1, TRUE ~ 0)
  )


## School Information ##
#++++++++++++++++++++++#

data <- data %>%
  mutate(
    # school country: only dummy for Germany or not
    educ_school_germany = case_when(
      educ_school_country == "Germany" ~ 1, is.na(educ_school_country) ~ as.double(NA), TRUE ~ 0
      ),
    # school type: only dummy for regular gymnasium or not
    educ_school_type_regular = case_when(
      educ_school_type == "regular Gymnasium (scientific/modern language/humanistic/musical)" ~ 1,
      is.na(educ_school_type) ~ as.double(NA), TRUE ~ 0
    ), 
    # school degree: dummy for ihigh degree (university of applied science and general university)
    educ_school_quali_high_applied = case_when(
      grepl("Fachhochschulreife", educ_school_quali) ~ 1, is.na(educ_school_quali) ~ as.double(NA), TRUE ~ 0
    ),
    educ_school_quali_high_general = case_when(
      grepl("Abitur", educ_school_quali) ~ 1, is.na(educ_school_quali) ~ as.double(NA), TRUE ~ 0
    )
  ) %>%
  select(-c("educ_school_germany", "educ_school_type", "educ_school_quali"))


## Birth Country ##
#+++++++++++++++++#

data <- data %>% mutate(
  birth_country_germany = case_when(
    birth_country == "Germany" ~ 1, is.na(birth_country) ~ as.double(NA), TRUE ~ 0),
  birth_country_germany_west = case_when(
    birth_ger_eastwest == "West Germany" ~ 1, is.na(birth_ger_eastwest) ~ as.double(NA), TRUE ~ 0)
  ) %>% 
  select(-c(birth_country, birth_ger_eastwest))


## Childhood ##
#+++++++++++++#


data <- data %>% mutate(childhood_biological_parents = case_when(
  childhood_parents == "with your biological parents" ~ 1, is.na(childhood_parents) ~ as.double(NA), TRUE ~ 0
)) %>% select(-childhood_parents)


## Parents ##
#++++++++++#

# books
data <- data %>%
  mutate(
    parents_number_books_100plus = case_when(
      is.na(parents_number_books) ~ as.character(NA),
      parents_number_books != "0 to 10 books" & parents_number_books != "11 to 25 books" & parents_number_books != "26 to 100 books" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(-parents_number_books)


# mother
data <- data %>%
  mutate(
    # birth country of mother
    mother_birth_country_germany = case_when(
      mother_country_ger == "abroad /in another country" ~ 0, is.na(mother_country_ger) ~ as.double(NA), TRUE ~ 1
    ),
    # first language of mother
    mother_first_language_german = case_when(
      mother_language_first == "German" ~ 1,  is.na(mother_language_first) ~ as.double(NA), TRUE ~ 0
    ),
    # highest degree of mother
      ## 1.) Make replacements
    mother_highest_degree = case_when(
      is.na(mother_educ_school_degree_casmin) ~ mother_educ_school_degree_isced,
      is.na(mother_educ_school_degree_casmin) & is.na(mother_educ_school_degree_isced) ~ mother_educ_school_degree,
      is.na(mother_educ_school_degree_casmin) & is.na(mother_educ_school_degree_isced) & is.na(mother_educ_school_degree) ~ as.character(NA), 
      TRUE ~ mother_educ_school_degree_casmin
    ), 
      ## 2.) Dummies for low, intermediate, high, and university degree
    mother_school_degree_low = case_when(
      grepl("Hauptschule", mother_highest_degree) ~ 1, is.na(mother_highest_degree) ~ as.double(NA), TRUE ~ 0
    ), 
    mother_school_degree_intermediate = case_when(
      grepl("Mittlere Reife", mother_highest_degree) ~ 1, is.na(mother_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    mother_school_degree_high = case_when(
      grepl("education entrance qualification", mother_highest_degree) ~ 1, 
      is.na(mother_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    mother_university_degree = case_when(
      grepl("university", mother_highest_degree) ~ 1, is.na(mother_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    # employed 
      ## at age of 15
    mother_emp_15y = case_when(
      mother_emp_15y == "yes" ~ 1, is.na(mother_emp_15y) ~ as.double(NA), TRUE ~ 0
    ),
      ## employed now
    mother_emp = case_when(
      !is.na(mother_emp_prof_pos) | !is.na(mother_emp_prof_egp) | !is.na(mother_emp_prof_blk) ~ 1, TRUE ~ 0
    ),
    # mother profession
      ## self-employed
    mother_emp_selfemployed = case_when(
      grepl("Self-employed", mother_emp_prof_pos) | grepl("Freelancer", mother_emp_prof_pos) | grepl("Self-employed", mother_emp_prof_egp) ~ 1, 
      is.na(mother_emp_prof_pos) & is.na(mother_emp_prof_egp) ~ as.double(NA), TRUE ~ 0
    ),
      ## manager
    mother_emp_manager = case_when(
      grepl("Manager", mother_emp_prof_blk) | grepl("Supervisors", mother_emp_prof_egp) ~ 1,
      is.na(mother_emp_prof_blk) & is.na(mother_emp_prof_egp) ~ as.double(NA), TRUE ~ 0
    )
  ) %>%
  select(-c(starts_with("mother_country"), mother_living_ger, starts_with("mother_educ_school_degree"),
            mother_educ_degree, mother_educ_degree_inst, mother_emp_prof_pos, mother_emp_prof_egp, 
            mother_emp_prof_blk, mother_emp_number))


# father 
data <- data %>%
  mutate(
    # birth country of mother
    father_birth_country_germany = case_when(
      father_country_ger == "abroad /in another country" ~ 0, is.na(father_country_ger) ~ as.double(NA), TRUE ~ 1
    ),
    # first language of mother
    father_first_language_german = case_when(
      father_language_first == "German" ~ 1,  is.na(father_language_first) ~ as.double(NA), TRUE ~ 0
    ),
    # highest degree of mother
    ## 1.) Make replacements
    father_highest_degree = case_when(
      is.na(father_educ_school_degree_casmin) ~ father_educ_school_degree_isced,
      is.na(father_educ_school_degree_casmin) & is.na(father_educ_school_degree_isced) ~ father_educ_school_degree,
      is.na(father_educ_school_degree_casmin) & is.na(father_educ_school_degree_isced) & is.na(father_educ_school_degree) ~ as.character(NA), 
      TRUE ~ father_educ_school_degree_casmin
    ), 
    ## 2.) Dummies for low, intermediate, high, and university degree
    father_school_degree_low = case_when(
      grepl("Hauptschule", father_highest_degree) ~ 1, is.na(father_highest_degree) ~ as.double(NA), TRUE ~ 0
    ), 
    father_school_degree_intermediate = case_when(
      grepl("Mittlere Reife", father_highest_degree) ~ 1, is.na(father_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    father_school_degree_high = case_when(
      grepl("education entrance qualification", father_highest_degree) ~ 1, 
      is.na(father_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    father_university_degree = case_when(
      grepl("university", father_highest_degree) ~ 1, is.na(father_highest_degree) ~ as.double(NA), TRUE ~ 0
    ),
    # employed 
    ## at age of 15
    father_emp_15y = case_when(
      father__emp_15y == "yes" ~ 1, is.na(father_emp_15y) ~ as.double(NA), TRUE ~ 0
    ),
    ## employed now
    father__emp = case_when(
      !is.na(father_emp_prof_pos) | !is.na(father_emp_prof_egp) | !is.na(father_emp_prof_blk) ~ 1, TRUE ~ 0
    ),
    # mother profession
    ## self-employed
    father_emp_selfemployed = case_when(
      grepl("Self-employed", father_emp_prof_pos) | grepl("Freelancer", father_emp_prof_pos) | grepl("Self-employed", father_emp_prof_egp) ~ 1, 
      is.na(father_emp_prof_pos) & is.na(father_emp_prof_egp) ~ as.double(NA), TRUE ~ 0
    ),
    ## manager
    father_emp_manager = case_when(
      grepl("Manager", father_emp_prof_blk) | grepl("Supervisors", father_emp_prof_egp) ~ 1,
      is.na(father_emp_prof_blk) & is.na(father_emp_prof_egp) ~ as.double(NA), TRUE ~ 0
    )
  ) %>%
  select(-c(starts_with("father_country"), father_living_ger, starts_with("father_educ_school_degree"),
            father_educ_degree, father_educ_degree_inst, father_emp_prof_pos, father_emp_prof_egp, 
            father_emp_prof_blk, father_emp_number))



## Uni ##
#+++++++#

# probability to graduate
table(data$uni_prob_graduation, useNA = "always")
table(data$uni_degree_complete_prob, useNA = "always")

sum(is.na(data$uni_prob_graduation))
sum(is.na(data$uni_degree_complete_prob))

data <- data %>%
  ## recode 50-50
  mutate(uni_degree_complete_prob = recode(uni_degree_complete_prob,
                                           "approx. 50:50" = "about 50-50")) %>%
  ## make replacements
  mutate(uni_prob_graduation = case_when(
    is.na(uni_prob_graduation) ~ uni_degree_complete_prob,
    is.na(uni_prob_graduation) & is.na(uni_degree_complete_prob) ~ as.character(NA),
    TRUE ~ uni_prob_graduation
  )) %>%
  select(-uni_degree_complete_prob)

table(data$uni_prob_graduation, useNA = "always")


# uni successful compared to others (not possible with function due to different naming)


# uni own expectations fulfilled (not possible with function due to different naming)


# internship
data <- data %>%
  mutate(
    internship_voluntary = case_when(
      grepl("voluntary", intern_type) ~ 1, is.na(intern_type) ~ as.double(NA), TRUE ~ 0
    )
  ) %>% select(-intern_type)



## Employment ##
#++++++++++++++#

data <- data %>%
  mutate(
    current_emp = case_when(
      !is.na(current_emp_prof_pos) | !is.na(current_emp_student_job_type) ~ 1,
      TRUE ~ current_emp
    ), 
    current_emp_type_selfemployed = case_when(
      grepl("freelancer", current_emp_prof_pos) | grepl("self-employed", current_emp_prof_pos) | 
        grepl("freelance", current_emp_student_job_type) | grepl("self-employment", current_emp_student_job_type) ~ 1,
      is.na(current_emp_student_job_type) & is.na(current_emp_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    ),
    current_emp_type_student = case_when(
      grepl("student", current_emp_student_job_type) | grepl("tutoring", current_emp_student_job_type) |
        grepl("student", current_emp_prof_pos) ~ 1,
      is.na(current_emp_student_job_type) & is.na(current_emp_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  select(-c(current_emp_student_job_type, current_emp_prof_pos))


## Religion ##
#++++++++++++#

data <- data %>%
  mutate(religion_christian = case_when(
    religion_deno == "Christian" ~ 1, is.na(religion_deno) ~ as.double(NA), TRUE ~ 0
  )) %>% select(-religion_deno)



#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Rename Categories ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#


# Preferred institution #
#+++++++++++++++++++++++#

data <- data %>%
  mutate(uni_institution_choice = recode(uni_institution_choice,
    "I didn't really have a preferred higher education institution" = "no_choice"
  ))


# Major #
#+++++++#

# Too long names
table(data$educ_uni_major)

data <- data %>%
  mutate(educ_uni_major = recode(
    educ_uni_major, "Arts, fine arts" = "arts",  
     "Human medicine/health sciences" = "healthsciences",  "Mathematics/sciences" = "naturalsciences",
     "Social sciences" = "socialsciences",  "Sport, language/cultural studies" = "sportculture",
     "Veterinary medicine, Agricultural/wood/nutritional sciences" = "healthsciences"
    )
  )

table(data$educ_uni_major)


# Aspired and achieved degree #
#+++++++++++++++++++++++++++++#

data <- data %>%
  mutate(
    educ_uni_degree_aspire = recode(educ_uni_degree_aspire, 
                                    "Master, Diploma, Magister, state examination" = "Master",
                                    "no completed degree" = "No", "doctorate/habilitation" = "PHD"),
    educ_uni_degree_achieve = recode(educ_uni_degree_achieve, 
                                     "Master, Diploma, Magister, state examination" = "Master",
                                     "no completed degree" = "No", "doctorate/habilitation" = "PHD")
    )


## Highest degree ##
#++++++++++++++++++#

# always both information is available 
sum(is.na(data$educ_highest_degree_casmin))
sum(is.na(data$educ_highest_degree_isced))

# keep only casmin
table(data$educ_highest_degree_casmin)

# rename categories
data <- data %>%
  mutate(educ_highest_degree = case_when(
    grepl("higher education entrance qualification", educ_highest_degree_casmin) ~ "high_degree",
    grepl("3a", educ_highest_degree_casmin) ~ "uni_applied",
    grepl("3b", educ_highest_degree_casmin) ~ "uni_general",
  )) %>%  
  select(-c(educ_highest_degree_casmin, educ_highest_degree_isced))

table(data$educ_highest_degree)


## Parents ##
#+++++++++++#

# Importance profession: set NA everything not indicating importance
data <- data %>%
  mutate(
    status_prof_mother = recode(status_prof_mother, "has never been employed" = as.character(NA)),
    status_prof_father = recode(status_prof_father, "Father deceased/no contact" = as.character(NA))
  )

# parents degree wish
data <- data %>%
  mutate(
    parents_degree_wish = recode(parents_degree_wish, 
                                 "Master, Diploma, Magister, state examination" = "master",
                                 "no completed degree" = "no_degree", "doctorate/habilitation" = "phd",
                                 "My parents have no opinion on that." = "no_opinion")
  )


## Current residence ##
#+++++++++++++++++++++#

# location
data <- data %>%
  mutate(
    current_residence_germany_west = case_when(
      current_residence_eastwest == "West Germany" ~ 1, is.na(current_residence_eastwest) ~ as.double(NA), TRUE ~ 0
    ), 
    current_residence_germany_east = case_when(
      current_residence_eastwest == "East Germany incl. Berlin" ~ 1, is.na(current_residence_eastwest) ~ as.double(NA), TRUE ~ 0
    )
  )

# living type
data <- data %>%
  mutate(
    living_type = recode(living_type, "in a dormitory?" = "dormitory", "in an apartment/house that you own?" = "own",
                         "in some other rental accommodation?" = "rent", "with parents or relatives?" = "parents",
                         "with private individuals for subtenancy?" = "subtenancy")
  )


## Gender ##
#++++++++++#

data <- data %>%
  mutate(gender = recode(gender, "[m] male" = "male", "[w] female" = "female"))


## Uni ##
#+++++++#

# learning group
data <- data %>%
  mutate(
    uni_learn_group_partic = recode(uni_learn_group_partic, "yes, namely:" = 1, "no" = 0)
  )


## General ##
#+++++++++++#



# "apply" variables
## extract variables
vars_recode_apply <- data %>%
  select_if(~ is.character(.)) %>%
  select_if(~ any(. == "does completely apply")) %>%
  colnames()
## recode variables
data <- data %>% 
  mutate_at(
    all_of(vars_recode_apply), 
    list(
      ~recode(., `does completely apply` = "completely", `does rather apply` = "rather",
              `does partly apply` = "partly", `does rather not apply` = "rather_not",
              `does not apply at all`= "not", .default = as.character(NA)) 
    ) 
  ) 

table(data$bigfive_extraversion, useNA = "always")


# variables not occuring often
  ## extract variables
vars_recode_good <- data %>%
  select_if(~ is.character(.)) %>%
  select_if(~ any(. == "very bad") | any(. == "very high") | any(. == "very important") | any(. == "slightly less")) %>%
  colnames()
  ## only put "_" in between
data <- data %>% 
  mutate_at(all_of(vars_recode_good), list(~ str_replace(., " ", "_")))




#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Drop Observations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# drop observations with unmeaningful categories
data <- data %>%
  filter(educ_uni_type != "no higher education institution/higher education institution abroad") 
  
  
  
  
#%%%%%%%%%%%%%%%%%%%%%%#
#### Drop Variables ####
#%%%%%%%%%%%%%%%%%%%%%%#

# create new data frame for dropping
data_sub_1 <- data


#### Drop Variables with too many Missing Values ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++#

# variables with too many missing values are dropped
# that is variables with more than 40% of missing values
perc_drop_na <- nrow(data_sub_1) * 0.4
## identify those variables
col_names_na <- colSums(is.na(data_sub_1))
col_names_na_drop <- col_names_na[col_names_na > perc_drop_na]

# generate vector containing columns which I keep anyway
# KEEP mother_ and father_v variables, parents_
col_keep_emp <- data_sub_1 %>% select(starts_with("current_emp")) %>% select(-current_emp_2) %>% colnames()
col_keep_parents <- data_sub_1 %>% select(starts_with("mother") | starts_with("father_") | starts_with("parents")) %>% colnames()
col_keep <- c("educ_uni_master_current", "current_emp", "satisfaction_life", 
              "risk", "child", "extracurricular_freq", "health_physical_good", "health_mental_good",
              "health_smoking_current")
col_keep_all <- c(col_keep_emp, col_keep_parents, col_keep)

# adjust vector with colnames to drop
col_names_na_drop <- names(col_names_na_drop)
col_names_na_drop <- col_names_na_drop[!col_names_na_drop %in% col_keep_all]

# drop those column names
data_sub_1 <- data_sub_1 %>%
  select(-all_of(col_names_na_drop))


#### Drop variables not needed anymore ####
#+++++++++++++++++++++++++++++++++++++++++#

# there are some variables which are just not useful anymore
vars_drop <- c(
  "educ_uni_start", "uni_first_eps", "current_emp_2", 
  "spell_length_cum_Data edition gap", "spell_length_cum_Unemp", "spell_length_cum_ParLeave", 
  "spell_length_cum_Gap", "educ_profession_aspired", "current_family_status",
  "mother_language_target", "father_language_target", "degree_uentrance_ger"
  )
data_sub_1 <- data_sub_1 %>%
  select(-all_of(vars_drop))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### NA Dummies and Missing Value Replacement ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++++++++++++++++#
#### NA DUMMIES ####
#++++++++++++++++++#

data_sub_2 <- data_sub_1

# THINK ABOUT NO NA DUMMY FOR VARIABLES WITH E.G. ONLY 100 MISSING VALUES OR LESS
# OR DROP OBSERVATIONS WITH LESS THAN 100 MISSING VALUES

# extract all columns containing any missing values
colnames_any_missing <- names(colSums(is.na(data_sub_2)))
colnames_any_missing <- colnames_any_missing[colnames_any_missing > 0]

# for every variable containing at least one missing value, a dummy variable
# is generated determining that the value was initially missing (-> replaced
# in next step)
func_generate_NA_dummies <- function(data, variable_NA_dummy) {
  varname_NA <- paste0(variable_NA_dummy, "_NA")
  data <- data %>%
    mutate(
      {{varname_NA}} := case_when(is.na(!!!rlang::syms(variable_NA_dummy)) ~ 1, TRUE ~ 0)
    )
  return(data)
}

i <- 0
for (col_sel in colnames_any_missing) {
  i <- i + 1
  data_sub_2 <- func_generate_NA_dummies(data_sub_2, col_sel)
  
  # if (i %% 100 == 0) {
  #   print(paste("Iteration", i, "from", length(colnames_any_missing)))
  # }
}



#+++++++++++++++++++#
#### REPLACEMENT ####
#+++++++++++++++++++#

data_sub_3 <- data_sub_2

# replacements and NA dummies are already made for:
  ## competencies, child, partner, and sibling -> CHECK
  ## maybe this makes sense because why mean, regression for those?
  ## for competencies: average and plausible values -> makes sense
# for other variables like BMI, other strategies, as selected by user

## CONSTANT ##
#++++++++++++#
# for numeric values median is replaced (median instead of mean due to outliers)
# for character variables most frequent category is replaced
# for binary variables, all are set to 0
# -> In any case NA variable is generated indicating that this value is originally
# missing and replaced.

if (na_replace == "constant") {
  
  ## IDENTIFY VARIABLES ##
  
  # identify all 0-1 indicators
  vars_dummy <- 
    data_sub_3 %>%
    select(all_of(colnames_any_missing)) %>%
    select_if(~ all(. %in% (0:1) | is.na(.))) %>%
    colnames()
  # unique(unlist(test)) #-> to check if really only variables with 0,1, and NA are includes
    ## all as integer
  data_sub_3 <- data_sub_3 %>%
    mutate_at(vars(all_of(vars_dummy)), ~ as.integer(.))
    
  # identify all numeric variables (but not 0-1 indicators)
  vars_numeric <- 
    data_sub_3 %>%
    select(all_of(colnames_any_missing)) %>%
    select_if(~ is.numeric(.)) %>%
    colnames()
  vars_numeric <- vars_numeric[!vars_numeric %in% vars_dummy]
  
  # identify all categorical variables
  # to find everyone, ensure that every factor variable is a character variables
  data_sub_3 <- data_sub_3 %>%
    ungroup() %>% 
    mutate_if(is.factor, as.character)
  vars_categoric <- 
    data_sub_3 %>%
    select(all_of(colnames_any_missing)) %>%
    select_if(~ is.character(.)) %>%
    colnames()
  vars_categoric <- vars_categoric[!vars_categoric %in% vars_dummy]
  
  
  ## REPLACE MISSING VALUES IN BINARY INDICATORS WITH 0 ##
  
  data_sub_3 <- data_sub_3 %>%
    mutate_at(vars(all_of(vars_dummy)), ~replace_na(., 0))
  
  
  ## REPLACE MISSING VALUES IN NUMERIC VARIABLES WITH MEDIAN ##
  
  # median is taken from group (treatment and control group)
  data_sub_3 <- 
    data_sub_3 %>%
    ungroup() %>% 
    group_by(treatment_sport) %>%
    mutate_at(vars(all_of(vars_numeric)), ~ ifelse(is.na(.), round(median(., na.rm = TRUE)), .))
    
  
  
  ## REPLACE MISSING VALUES IN CATEGORICAL VARIABLES WITH MOST FREQUENT CATEGORY ##
  for (var_categoric_sel in vars_categoric) {
    
    # identify most frequent category
    df_most_freq <- 
      data_sub_3 %>% 
      group_by(treatment_sport) %>% 
      count(!!!rlang::syms(var_categoric_sel)) %>% 
      na.omit() %>% 
      filter(n == max(n)) %>%
      # in case there are two most frequent values, just keep the first one
      distinct(treatment_sport, .keep_all = TRUE)
    
    # replace NAs by most frequent category
    data_sub_3 <- data_sub_3 %>%
      mutate(
        {{var_categoric_sel}} := case_when(
          treatment_sport == 0 & is.na(!!!rlang::syms(var_categoric_sel)) ~ df_most_freq %>% filter(treatment_sport == 0) %>% pull(!!!rlang::syms(var_categoric_sel)),
          treatment_sport == 1 & is.na(!!!rlang::syms(var_categoric_sel)) ~ df_most_freq %>% filter(treatment_sport == 1) %>% pull(!!!rlang::syms(var_categoric_sel)),
          TRUE ~ !!!rlang::syms(var_categoric_sel)
        )
      )
  }

} else if (na_replace == "mice") {

## MICE ##
#++++++++#

  data_sub_3 <- data_sub_3
} else if (na_replace == "forest") {
  
## FOREST ##
#++++++++++#
  
  data_sub_3 <- data_sub_3
} else {
  stop("Pleade select missing value replacement strategy")
}


# ensure that no missing values are left
sum(is.na(data_sub_3))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create Variables: Dummy and Category ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Dummy Variables ##
#+++++++++++++++++++#

data_sub_4 <- data_sub_3

# automatically generate dummy variables for categorical variables
# LASSO will select which are important
  ## identify all categorical columns
vars_categoric <- data_sub_4 %>% select_if(~ is.character(.)) %>% colnames()
data_sub_4 <- dummy_cols(
  # selected column is removed, and base category is omitted 
  data_sub_4, remove_selected_columns = TRUE, remove_first_dummy = TRUE, 
  select_columns = vars_categoric
  )



## Categories ##
#++++++++++++++#

## AGE ##

# create age categories
summary(data$age)
data <- data %>%
  mutate(
    age_18_21 = case_when(age >= 18 & age <= 21 ~ 1, TRUE ~ 0),
    age_22_23 = case_when(age > 21 & age <= 23 ~ 1, TRUE ~ 0),
    age_24_25 = case_when(age > 23 ~ 1, TRUE ~ 0)
  )
table(data$age_18_21, useNA = "always")
table(data$age_22_23, useNA = "always")
table(data$age_24_25, useNA = "always")


## BMI ##

# generate categories
# https://www.cdc.gov/obesity/basics/adult-defining.html and Flegal et al.
# normal weight: BMI of 18.5-25
# underweight: BMI < 18.5
# overweight: 25-30
# obesity: > 30
summary(data$BMI)
data <- data %>%
  mutate(
    BMI_normal = case_when(BMI >= 18.5 & BMI <= 25 ~ 1, TRUE ~ 0),
    BMI_over = case_when(BMI > 25 ~ 1, TRUE ~ 0)
  )
table(data$BMI_normal, useNA = "always")
table(data$BMI_over, useNA = "always")



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#


# save data frame
saveRDS(data, "Data/prep_6_variables.rds")
