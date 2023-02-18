#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


#+++
# by Lana Kern
#+++
# In this file the control variables are created. More precisely, variables
# are generated (totally new, dummy variables etc.) and aggregated, and missing
# values are replaced.
#+++
# 1.) Generate Variables: Years of education, BMI, cohort & period effects, ...
#+++
# 2.) Treatment and outcome: generate lagged variables, NA dummies
#+++
# 3.) Rename categories: To simplify the generating of dummy variables, and let
# the dummy variables have more appealing names, categories are re-named, 
# for instance "[m] male" to "male"
#+++
# -> The final data frame is a panel data frame containing the final number
# of variables.


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#

# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", 
#                           "df_inputs", "prep_sel_num", ls()[str_starts(ls(), "func_")])))

# # install packages if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # for replace_na() function
# 
# if (!require("fastDummies")) install.packages("fastDummies")
# library(fastDummies)  # to generate dummy variables
# 
# if (!require("readstata13")) install.packages("readstata13")
# library(readstata13)  # to re-label aggregated variables
# 
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate)  # to work with dates
# 
# # now set language for dates and times to English
# Sys.setlocale("LC_TIME", "English")


# load data
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}

if (cohort_prep == "controls_same_outcome") {
  load_data <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, extra_act_save, ".rds")
} else {
  load_data <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, extra_act_save, "_robustcheck.rds")
}


data_raw <- readRDS(load_data)

# number of students
id_num <- length(unique(data_raw$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%
#### Generate new Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%

data_prep_1 <- data_raw


## YEARS OF EDUCATION ##
#++++++++++++++++++++++#

# summary of years of schooling, university studies, vocational training, and
# vocational preparation
data_prep_1 <- data_prep_1 %>%
  mutate(educ_years_total = rowSums(
    across(c("spell_length_cum_School", "spell_length_cum_Uni", "spell_length_cum_VocTrain", "spell_length_cum_VocPrep")),
    na.rm = TRUE
    )) 
summary(data_prep_1$educ_years_total)

# summary across age and education years
table(round(data_prep_1$age), round(data_prep_1$educ_years_total))

# rename all spell variables
data_prep_1 <- data_prep_1 %>%
  rename_all(list(~ stringr::str_replace_all(., 'spell_length_cum_', 'educ_years_'))) %>%
  rename_all(list(~ stringr::str_replace_all(., 'spell_length_current_', 'educ_years_current_')))


## BMI ##
#+++++++#

# check weight and height for unrealistic values
summary(data_prep_1$health_weight)
summary(data_prep_1$health_height)
sum(rowSums(is.na(data_prep_1[, c("health_weight", "health_height")])) > 0) # number of NA in both

# BMI = weight / height^2
data_prep_1 <- data_prep_1 %>%
  mutate(health_bmi = as.numeric(health_weight / ((health_height/100)^2))) %>%
  select(-c(health_weight, health_height))

summary(data_prep_1$health_bmi) # number of NA in generated variable



## Cohort & Period ##
#+++++++++++++++++++#

# cohort and period effects may interact with age
# hence, they are considered separately

# cohort is measured by birth year (below dummies are created but also
# numeric variable is considered)
# create numeric variable starting at 0
table(data_prep_1$birth_year)
cohort_recode_names <- c(min(unique(data_prep_1$birth_year)):max(unique(data_prep_1$birth_year)))
cohort_recode <- c(1:length(cohort_recode_names))
names(cohort_recode) <- cohort_recode_names

data_prep_1 <- data_prep_1 %>% 
  mutate(birth_year_num = recode(birth_year, !!!cohort_recode)) %>%
  mutate(birth_year_num = birth_year_num - 1) # to start with 0

table(data_prep_1$birth_year_num)


# period effects are considered by using the survey interview date
# I use both from controls and outcome
years_recode <- c(1, 2, 3, 4, 5, 6, 7, 8)
names(years_recode) <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

table(year(data_prep_1$interview_date_start))
table(year(data_prep_1$interview_date_end))

data_prep_1 <- data_prep_1 %>%
  # extract years
  mutate(
    interview_start_year = year(data_prep_1$interview_date_start),
    interview_end_year = year(data_prep_1$interview_date_end)
  ) %>%
  # recode years
  mutate(
    interview_start_year_num = recode(interview_start_year, !!!years_recode),
    interview_end_year_num = recode(interview_end_year, !!!years_recode)
  )

table(data_prep_1$interview_start_year_num)
table(data_prep_1$interview_end_year_num)


## POLITICS ##
#++++++++++++#

# special variable: politics with different scales
# just create dummy if one is (highly) interest in politics
# vote is exlucing because also individuals with no interest vote and
# this increases group with no interest

data_prep_1 <- data_prep_1 %>%
  # create dummy
  mutate(interest_politics = case_when(
    # if there is any sign of political interest, variable takes on value 1
    interest_politics_signatures == 1 ~ 1,
    interest_politics_demo == 1 ~ 1,
    interest_politics_general %in% c(1) ~ 1,
    interest_politics_discussion %in% c(1:2) ~ 1,
    interest_politics_understanding %in% c(1) ~ 1,
    # if all variables are NA then dummy variable is also NA
    data_prep_1 %>% select(starts_with("interest_politics")) %>% 
      is.na() %>% rowSums() == 
      data_prep_1 %>% select(starts_with("interest_politics")) %>% 
      colnames() %>% length() ~ as.double(NA), 
    # otherwise 0
    TRUE ~ 0
  )) %>%
  # drop other variables
  select(-starts_with("interest_politics_"))

table(data_prep_1$interest_politics, useNA = "always")



## SMOKING ##
#+++++++++++# 

# both smoking variables as integer (so that they have same data type)
# this avoids mistakes
data_prep_1 <- data_prep_1 %>%
  mutate(health_smoking = as.integer(health_smoking), 
         health_smoking_v1 = as.integer(health_smoking_v1))

# check NAs
table(data_prep_1$health_smoking, useNA = "always")
table(data_prep_1$health_smoking_v1, useNA = "always") # less missing values

# reverse score of health_smoking_v1
data_prep_1 <- func_reverse_score(data_prep_1, data.frame(
  "vars_reverse" = "health_smoking_v1", "num_scores" = 4
))


# replace missings in new variable "health_smoking" with values from
# previous version
data_prep_1 <- data_prep_1 %>%
  mutate(
    health_smoking = ifelse(is.na(health_smoking_v1), health_smoking, health_smoking_v1)
  ) %>%
  select(-health_smoking_v1)

# generate dummy for smoking
## 1&2: smoke daily or occasionally
data_prep_1 <- data_prep_1 %>%
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

table(data_prep_1$health_smoking_current, useNA = "always")



## ALCOHOL ##
#+++++++++++#

table(data_prep_1$health_alcohol, useNA = "always")
table(data_prep_1$health_alcohol_v1, useNA = "always")

# only minor differences in labels
data_prep_1 <- data_prep_1 %>%
  mutate(
    health_alcohol_v1 = recode(health_alcohol_v1, 
                               "(almost) every day" = "daily",
                               "(almost) never" = "never"
    )
  )

# replace new variable by previous if NA and generate the dummy for 
# alcohol consumption
data_prep_1 <- data_prep_1 %>%
  # replacement
  mutate(health_alcohol = case_when(
    is.na(health_alcohol) ~ health_alcohol_v1,
    TRUE ~ health_alcohol
  )) %>%
  # dummy generation
  mutate(
    health_alcohol_current = case_when(
      health_alcohol %in% c("daily", "several times a week") ~ 1,
      is.na(health_alcohol) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  # drop v1 variable
  select(-c(health_alcohol_v1, health_alcohol))


table(data_prep_1$health_alcohol_current, useNA = "always")


## DRUGS ##
#+++++++++#

data_prep_1 <- data_prep_1 %>%
  mutate(health_drugs_ever = ifelse(drugs_never == 0, 1, 0)) %>%
  select(-starts_with("drugs_"))


table(data_prep_1$health_drugs_ever, useNA = "always")



## Spell Length ##
#++++++++++++++++#

# spell length variables are NA for individuals who never experienced this spell,
# for example for individuals who never went to the military, never undertook
# vocational training etc. In this case, the values are set to zero.
# for all variables already done except educ_years_current_Emp
colSums(is.na(data_prep_1 %>% select(starts_with("educ_year"))))

summary(data_prep_1$educ_years_current_Emp)
data_prep_1 <- data_prep_1 %>%
  mutate(educ_years_current_Emp = case_when(is.na(educ_years_current_Emp) ~ 0, 
                                            TRUE ~ educ_years_current_Emp))
summary(data_prep_1$educ_years_current_Emp)
sum(is.na(data_prep_1 %>% select(starts_with("spell_length"))))


# problem with dummies (always zero) -> one if educ_year > 0
data_prep_1 <- data_prep_1 %>%
  mutate(
    educ_military = ifelse(educ_years_Military > 0, 1, 0),
    educ_vocprep = ifelse(educ_years_VocPrep > 0, 1, 0),
    educ_voctrain = ifelse(educ_years_VocTrain > 0, 1, 0),
    educ_intern = ifelse(educ_years_Internship > 0, 1, 0)
    )

table(data_prep_1$educ_military)
table(data_prep_1$educ_vocprep)
table(data_prep_1$educ_voctrain)
table(data_prep_1$educ_intern)


## Child ##
#+++++++++#

# generate indicator for having a child
data_prep_1 <- data_prep_1 %>%
  mutate(
    child = case_when(child_total_num > 0 ~ 1, TRUE ~ 0)
  )



## School Information ##
#++++++++++++++++++++++#

# since those variables have many categories, I generate dummys myself

data_prep_1 <- data_prep_1 %>%
  mutate(
    # school country: only dummy for Germany or not
    educ_school_germany = case_when(
      educ_school_country == "Germany" ~ 1, is.na(educ_school_country) ~ as.double(NA), TRUE ~ 0
    ),
    # school type: only dummy for regular gymnasium or not
    educ_school_gymn = case_when(
      educ_school_type == "regular Gymnasium (scientific/modern language/humanistic/musical)" ~ 1,
      is.na(educ_school_type) ~ as.double(NA), TRUE ~ 0
    ), 
    # school degree: dummy for high degree (university of applied science and general university)
    educ_school_degree_applied = case_when(
      grepl("Fachhochschulreife", educ_school_quali) ~ 1, is.na(educ_school_quali) ~ as.double(NA), TRUE ~ 0
    ),
    educ_school_degree_general = case_when(
      grepl("Abitur", educ_school_quali) ~ 1, is.na(educ_school_quali) ~ as.double(NA), TRUE ~ 0
    )
  ) %>%
  select(-c("educ_school_country", "educ_school_type", "educ_school_quali"))



## Migration / Nationality ##
#+++++++++++++++++++++++++++#


table(data_prep_1$birth_country, useNA = "always")
table(data_prep_1$mother_country_ger, useNA = "always")
table(data_prep_1$father_country_ger, useNA = "always")

# dummy for born in germany
data_prep_1 <- data_prep_1 %>% mutate(
  birth_country_ger = case_when(
    birth_country == "Germany" ~ 1, is.na(birth_country) ~ as.double(NA), TRUE ~ 0)
) %>% 
  select(-c(birth_country))


# migration variable: respondent or parents are born in another country
data_prep_1 <- data_prep_1 %>% mutate(
  migration = case_when(
    birth_country_ger == 0 | mother_country_ger == "abroad /in another country" | father_country_ger == "abroad /in another country" ~  1,
    is.na(birth_country_ger) & is.na(mother_country_ger) & is.na(father_country_ger) ~ as.double(NA),
    TRUE ~ 0
  )
) %>% select(-c(starts_with("mother_country"), starts_with("father_country")))


table(data_prep_1$birth_country_ger, useNA = "always")
table(data_prep_1$migration, useNA = "always")



## Childhood ##
#+++++++++++++#


table(data_prep_1$childhood_parents, useNA = "always")

data_prep_1 <- data_prep_1 %>% mutate(childhood_biological_parents = case_when(
  childhood_parents == "with your biological parents" ~ 1, is.na(childhood_parents) ~ as.double(NA), TRUE ~ 0
)) %>% select(-childhood_parents)

table(data_prep_1$childhood_biological_parents, useNA = "always")


## Parents ##
#++++++++++#

# mother
data_prep_1 <- data_prep_1 %>%
  mutate(
    # first language of mother
    mother_first_lang_ger = case_when(
      mother_language_first == "German" ~ 1,  is.na(mother_language_first) ~ as.double(NA), TRUE ~ 0
    ),
    # highest degree of mother
    ## 1.) Make replacements
    mother_degree_highest = case_when(
      is.na(mother_educ_school_degree_casmin) ~ mother_educ_school_degree_isced,
      is.na(mother_educ_school_degree_casmin) & is.na(mother_educ_school_degree_isced) ~ mother_educ_school_degree,
      is.na(mother_educ_school_degree_casmin) & is.na(mother_educ_school_degree_isced) & is.na(mother_educ_school_degree) ~ as.character(NA), 
      TRUE ~ mother_educ_school_degree_casmin
    ), 
    # employed before age of 15
    mother_emp_bef_15y = case_when(
      is.na(mother_emp_bef_15y) & mother_emp_15y == "yes" ~ 1, 
      is.na(mother_emp_bef_15y) & is.na(mother_emp_15y) ~ as.double(NA), 
      TRUE ~ 0
    )
  ) %>%
  select(-c(mother_living_ger, starts_with("mother_educ_school_degree"), mother_language_first, 
            mother_educ_degree, mother_educ_degree_inst, mother_emp_number, mother_emp_15y))


# father 
data_prep_1 <- data_prep_1 %>%
  mutate(
    # first language of mother
    father_first_lang_ger = case_when(
      father_language_first == "German" ~ 1,  is.na(father_language_first) ~ as.double(NA), TRUE ~ 0
    ),
    # highest degree of mother
    ## 1.) Make replacements
    father_degree_highest = case_when(
      is.na(father_educ_school_degree_casmin) ~ father_educ_school_degree_isced,
      is.na(father_educ_school_degree_casmin) & is.na(father_educ_school_degree_isced) ~ father_educ_school_degree,
      is.na(father_educ_school_degree_casmin) & is.na(father_educ_school_degree_isced) & is.na(father_educ_school_degree) ~ as.character(NA), 
      TRUE ~ father_educ_school_degree_casmin
    ), 
    # employed before age of 15
    father_emp_bef_15y = case_when(
      is.na(father_emp_bef_15y) & father_emp_15y == "yes" ~ 1, 
      is.na(father_emp_bef_15y) & is.na(father_emp_15y) ~ as.double(NA), 
      TRUE ~ 0
    )
  ) %>%
  select(-c(father_living_ger, starts_with("father_educ_school_degree"), father_language_first, 
            father_educ_degree, father_educ_degree_inst, father_emp_number, father_emp_15y))



## Uni ##
#+++++++#

# probability to graduate
table(data_prep_1$uni_prob_graduation, useNA = "always")
table(data_prep_1$uni_degree_complete_prob, useNA = "always")

sum(is.na(data_prep_1$uni_prob_graduation))
sum(is.na(data_prep_1$uni_degree_complete_prob))

data_prep_1 <- data_prep_1 %>%
  ## recode 50-50
  mutate(uni_degree_complete_prob = recode(uni_degree_complete_prob,
                                           "approx. 50:50" = "about 50-50")) %>%
  ## make replacements
  mutate(uni_prob_graduation = case_when(
    is.na(uni_prob_graduation) ~ uni_degree_complete_prob,
    is.na(uni_prob_graduation) & is.na(uni_degree_complete_prob) ~ as.character(NA),
    TRUE ~ uni_prob_graduation
  )) %>%
  ## recode
  mutate(uni_prob_graduation = case_when(
    grepl("unlikely", uni_prob_graduation) ~ "unlikely",
    grepl("likely", uni_prob_graduation) ~ "likely",
    TRUE ~ uni_prob_graduation)) %>% 
  select(-uni_degree_complete_prob)

table(data_prep_1$uni_prob_graduation, useNA = "always")



## Employment ##
#++++++++++++++#

table(data_prep_1$emp_current, useNA = "always")

# 1.) If any employment variable has a value, emp_current must equal 1
data_prep_1 <- data_prep_1 %>%
  ungroup() %>%
  mutate(emp_current = ifelse(
    rowSums(!is.na(data_prep_1 %>% select(starts_with("emp_current_")))) > 0, 1, emp_current)
    )

table(data_prep_1$emp_current, useNA = "always")


# 2.) Dummies
data_prep_1 <- data_prep_1 %>%
  mutate(
    emp_current_self = case_when(
      grepl("freelancer", emp_current_prof_pos) | grepl("self-employed", emp_current_prof_pos) | 
        grepl("freelance", emp_current_student_job_type) | grepl("self-employment", emp_current_student_job_type) ~ 1,
      is.na(emp_current_student_job_type) & is.na(emp_current_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    ),
    emp_current_student = case_when(
      grepl("student", emp_current_student_job_type) | grepl("tutoring", emp_current_student_job_type) |
        grepl("student", emp_current_prof_pos) ~ 1,
      emp_current_student_job == 1 ~ 1,
      is.na(emp_current_student_job_type) & is.na(emp_current_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  select(-c(emp_current_student_job, emp_current_student_job_type, emp_current_prof_pos, emp_current_student_job_rel))



## Religion ##
#++++++++++++#

table(data_prep_1$religion_deno, useNA = "always")

data_prep_1 <- data_prep_1 %>%
  mutate(religion_christian = case_when(
    religion_deno == "Christian" ~ 1, is.na(religion_deno) ~ as.double(NA), TRUE ~ 0
  )) %>% select(-religion_deno)


table(data_prep_1$religion_christian, useNA = "always")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Outcome and Treatment Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## STANDARDIZE OUTCOME ##
#+++++++++++++++++++++++#

# standardize outcome variable to have mean zero and standard deviation of 1
data_prep_1$outcome_grade_stand <- scale(data_prep_1$outcome_grade)[, 1]
summary(data_prep_1$outcome_grade_stand) # mean zero
sd(data_prep_1$outcome_grade_stand) # standard deviation of 1


## GENERATE LAGS ##
#+++++++++++++++++#

# generate lags for the two treatment and one outcome variable
data_prep_1 <- data_prep_1 %>%
  group_by(ID_t) %>%
  mutate(
    treatment_sport_lag = lag(treatment_sport), 
    treatment_sport_freq_lag = lag(treatment_sport_freq),
    outcome_grade_lag = lag(outcome_grade),
    outcome_grade_stand_lag = lag(outcome_grade_stand)
  )

# if lag is NA (that is only for first treatment period) current value is used
data_prep_1 <- data_prep_1 %>%
  arrange(ID_t, treatment_period) %>%
  group_by(ID_t) %>%
  mutate(
    treatment_sport_lag = case_when(is.na(treatment_sport_lag) ~ treatment_sport, TRUE ~ treatment_sport_lag),
    treatment_sport_freq_lag = case_when(is.na(treatment_sport_freq_lag) ~ treatment_sport_freq, TRUE ~ treatment_sport_freq_lag),
    outcome_grade_lag = case_when(is.na(outcome_grade_lag) ~ outcome_grade, TRUE ~ outcome_grade_lag),
    outcome_grade_stand_lag = case_when(is.na(outcome_grade_stand_lag) ~ outcome_grade_stand, TRUE ~ outcome_grade_stand_lag)
  )

# ensure that no missing values in lagged variables are left
sum(is.na(data_prep_1 %>% ungroup() %>% select(ends_with("_lag"))))

# differences in variables
sum(data_prep_1$treatment_sport != data_prep_1$treatment_sport_lag)
sum(data_prep_1$treatment_sport_freq != data_prep_1$treatment_sport_freq_lag)
sum(data_prep_1$outcome_grade != data_prep_1$outcome_grade_lag)


## NA DUMMIES ##
#++++++++++++++#

# NA dummies equal 1 if value is used from previous interview
table(data_prep_1$treatment_sport_NA)
table(data_prep_1$treatment_sport_freq_NA)
table(data_prep_1$outcome_grade_NA)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Rename Categories ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#


# Uni #
#+++++#

# degrees have too long names
table(data_prep_1$uni_institution_choice, useNA = "always")
table(data_prep_1$educ_uni_major, useNA = "always")
table(data_prep_1$educ_uni_degree_aspire, useNA = "always")
table(data_prep_1$educ_uni_degree_achieve, useNA = "always")
table(data_prep_1$educ_highest_degree_casmin, useNA = "always")
table(data_prep_1$educ_uni_type, useNA = "always")
table(data_prep_1$uni_learn_group_partic, useNA = "always")

data_prep_1 <- data_prep_1 %>%
  mutate(educ_uni_major = recode(
    educ_uni_major, "Arts, fine arts" = "arts",  
    "Human medicine/health sciences" = "healthsciences",  "Mathematics/sciences" = "naturalsciences",
    "Social sciences" = "socialsciences",  "Sport, language/cultural studies" = "sportculture",
    "Veterinary medicine, Agricultural/wood/nutritional sciences" = "healthsciences"),
    educ_uni_degree_aspire = recode(educ_uni_degree_aspire, 
                                    "Master, Diploma, Magister, state examination" = "master",
                                    "no completed degree" = "no", "doctorate/habilitation" = "phd"),
    educ_uni_degree_achieve = recode(educ_uni_degree_achieve, 
                                     "Master, Diploma, Magister, state examination" = "master",
                                     "no completed degree" = "no", "doctorate/habilitation" = "phd"),
    educ_uni_quali = case_when(
      grepl("Bachelor", educ_uni_quali) ~ "bachelor",
      grepl("Master", educ_uni_quali) ~ "master",
      grepl("Magister", educ_uni_quali) ~ "master",
      grepl("Diploma", educ_uni_quali) ~ "diploma",
      grepl("state examination", educ_uni_quali) ~ "state_examination",
      grepl("doctorate", educ_uni_quali) ~ "phd",
      is.na(educ_uni_quali) ~ as.character(NA), TRUE ~ "other"
    ), 
    educ_highest_degree = case_when(
      grepl("higher education entrance qualification", educ_highest_degree_casmin) ~ "high_degree",
      grepl("3a", educ_highest_degree_casmin) ~ "uni_applied",
      grepl("3b", educ_highest_degree_casmin) ~ "uni_general",
    ),
    educ_uni_type = case_when(
      educ_uni_type == "University" ~ "uni_general",
      grepl("University of applied sciences", educ_uni_type) ~ "uni_applied",
      TRUE ~ as.character(NA)
    ),
    uni_learn_group_partic = recode(uni_learn_group_partic, "yes, namely:" = 1, "no" = 0),
    uni_institution_choice = recode(uni_institution_choice, 
      "I didn't really have a preferred higher education institution" = "no_choice"),
    prof_expected = case_when(
      grepl("\\[AGR\\]", educ_profession_expected) ~ "agriculture", 
      grepl("\\[EVB\\]", educ_profession_expected) ~ "commercial", 
      grepl("Simple", educ_profession_expected) ~ "simple", 
      grepl("Qualified", educ_profession_expected) ~ "qualified", 
      grepl("\\[ING\\]", educ_profession_expected) ~ "it",
      grepl("\\[TEC\\]", educ_profession_expected) ~ "it",
      grepl("\\[PROF\\]", educ_profession_expected) ~ "prof",
      grepl("\\[SEMI\\]", educ_profession_expected) ~ "prof",
      grepl("\\[MAN\\]", educ_profession_expected) ~ "manager",
      TRUE ~ as.character(NA)
    ),
    friends_study_share = case_when(
      grepl("all", friends_study_share) ~ "(almost)all",
      friends_study_share == "nobody" ~ "almost no one",
      grepl("half", friends_study_share) ~ "(almost)half",
      TRUE ~ as.character(NA)
    )
  ) %>%
  select(-c(educ_highest_degree_casmin, educ_highest_degree_isced, educ_profession_expected))

table(data_prep_1$educ_uni_major, useNA = "always")
table(data_prep_1$educ_uni_degree_aspire, useNA = "always")
table(data_prep_1$educ_uni_degree_achieve, useNA = "always")
table(data_prep_1$educ_highest_degree, useNA = "always")
table(data_prep_1$educ_uni_type, useNA = "always")
table(data_prep_1$uni_learn_group_partic, useNA = "always")
table(data_prep_1$uni_institution_choice, useNA = "always")



## Parents ##
#+++++++++++#

data_prep_1 <- data_prep_1 %>%
  mutate(
    # Importance profession: set NA everything not indicating importance
    status_prof_mother = recode(status_prof_mother, "has never been employed" = as.character(NA)),
    status_prof_father = recode(status_prof_father, "Father deceased/no contact" = as.character(NA)),
    # parents degree wish
    parents_degree_wish = recode(parents_degree_wish, 
                                 "Master, Diploma, Magister, state examination" = "master",
                                 "no completed degree" = "no_degree", "doctorate/habilitation" = "phd",
                                 "My parents have no opinion on that." = "no_opinion"),
    # mother highest degree
    mother_degree_highest = case_when(
      grepl("Hauptschule", mother_degree_highest) ~ "low", 
      grepl("Mittlere Reife", mother_degree_highest) ~ "middle",
      grepl("education entrance qualification", mother_degree_highest) ~ "high", 
      grepl("university", mother_degree_highest) ~ "uni",
      is.na(mother_degree_highest) ~ as.character(NA),
      TRUE ~ "no"),
    # father highest degree
    father_degree_highest = case_when(
      grepl("Hauptschule", father_degree_highest) ~ "low", 
      grepl("Mittlere Reife", father_degree_highest) ~ "middle",
      grepl("education entrance qualification", father_degree_highest) ~ "high", 
      grepl("university", father_degree_highest) ~ "uni",
      is.na(father_degree_highest) ~ as.character(NA),
      TRUE ~ "no"),
    # mother profession
    mother_emp_prof = case_when(
      grepl("\\[AGR\\]", mother_emp_prof_blk) ~ "agriculture", 
      grepl("\\[EVB\\]", mother_emp_prof_blk) ~ "commercial", 
      grepl("Simple", mother_emp_prof_blk) ~ "simple", 
      grepl("Qualified", mother_emp_prof_blk) ~ "qualified", 
      grepl("\\[ING\\]", mother_emp_prof_blk) ~ "it",
      grepl("\\[TEC\\]", mother_emp_prof_blk) ~ "it",
      grepl("\\[PROF\\]", mother_emp_prof_blk) ~ "prof",
      grepl("\\[SEMI\\]", mother_emp_prof_blk) ~ "prof",
      grepl("\\[MAN\\]", mother_emp_prof_blk) ~ "manager",
      TRUE ~ as.character(NA)
    ),
    # father profession
    father_emp_prof = case_when(
      grepl("\\[AGR\\]", father_emp_prof_blk) ~ "agriculture", 
      grepl("\\[EVB\\]", father_emp_prof_blk) ~ "commercial", 
      grepl("Simple", father_emp_prof_blk) ~ "simple", 
      grepl("Qualified", father_emp_prof_blk) ~ "qualified", 
      grepl("\\[ING\\]", father_emp_prof_blk) ~ "it",
      grepl("\\[TEC\\]", father_emp_prof_blk) ~ "it",
      grepl("\\[PROF\\]", father_emp_prof_blk) ~ "prof",
      grepl("\\[SEMI\\]", father_emp_prof_blk) ~ "prof",
      grepl("\\[MAN\\]", father_emp_prof_blk) ~ "manager",
      TRUE ~ as.character(NA)
    )
  ) %>% select(-c(father_emp_prof_blk, father_emp_prof_egp, father_emp_prof_isei, father_emp_prof_pos,
                  mother_emp_prof_blk, mother_emp_prof_egp, mother_emp_prof_isei, mother_emp_prof_pos))


table(data_prep_1$mother_emp_prof, useNA = "always")
table(data_prep_1$father_emp_prof, useNA = "always")


## interests ##
#+++++++++++++#

table(data_prep_1$interest_art_musuem, useNA = "always")

data_prep_1 <- data_prep_1 %>%
  mutate(interest_art_musuem = case_when(
    interest_art_musuem %in% c("2 to 3 times", "4 to 5 times") ~ "2_to_5_times",
    TRUE ~ interest_art_musuem
  ))


## other ##
#+++++++++#

# living type
data_prep_1 <- data_prep_1 %>%
  mutate(
    living_type = recode(living_type, "in a dormitory?" = "dormitory", "in an apartment/house that you own?" = "own",
                         "in some other rental accommodation?" = "rent", "with parents or relatives?" = "parents",
                         "with private individuals for subtenancy?" = "subtenancy")
  )
table(data_prep_1$living_type)

# birth in germany (east-west) and current place of residence
table(data_prep_1$birth_ger_eastwest)
data_prep_1 <- data_prep_1 %>% mutate(
  birth_ger_eastwest = recode(birth_ger_eastwest, "East Germany incl. Berlin" = "East",
                             "West Germany" = "West"),
  place_residence = recode(current_residence_eastwest, "East Germany incl. Berlin" = "East",
                           "West Germany" = "West", "location is abroad" = "abroad")
  ) %>% select(-current_residence_eastwest)


# religion
table(data_prep_1$religion_degree)
data_prep_1 <- data_prep_1 %>% mutate(
  religion_degree = recode(religion_degree, "rather religious" = "religious",
                          "very religious" = "religious", "not religious at all" = "non-religious",
                          "rather non-religious" = "non-religious"))


# gender
table(data_prep_1$gender)
data_prep_1 <- data_prep_1 %>%
  mutate(gender = recode(gender, "[m] male" = "male", "[w] female" = "female"))



# general health
table(data_prep_1$health_general, useNA = "always")

data_prep_1 <- data_prep_1 %>%
  mutate(
    health_general = case_when(
      health_general %in% c("good", "very good") ~ "good",
      health_general %in% c("poor", "very poor") ~ "poor",
      health_general %in% c("moderate") ~ "moderate",
      TRUE ~ as.character(NA)
    )
  ) %>% select(-c(health_allergic, health_neuro)) 

table(data_prep_1$health_general, useNA = "always")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Combine CATI & CAWI Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Variables that measure the same thing but are in both, CATI & CAWI, are combined in one variable.
# Mean is taken across CATI and CAWI
data_prep_1 <- data_prep_1 %>% ungroup()

# define variables which are in CATI and CAWI
vars_both <- c("personality_assertiveness", "personality_conflicts", "satisfaction_life")

# perform operation:
for (vars_both_sel in vars_both) {
  
  ## identify all variables starting with string
  vars_both_sel_all <- data_prep_1 %>%
    select(starts_with(vars_both_sel)) %>% colnames() %>% 
    str_remove_all("_CATI") %>% str_remove_all("_CAWI") %>% unique()
  
  ## for all of those take the mean (and round)
  for (vars_both_sel_all_rep  in vars_both_sel_all) {
    data_prep_1 <- data_prep_1 %>%
      mutate(
        !!vars_both_sel_all_rep := round(rowMeans(
          select(data_prep_1, !!!rlang::syms(paste0(vars_both_sel_all_rep, "_CAWI")), 
                 !!!rlang::syms(paste0(vars_both_sel_all_rep, "_CATI"))),
          na.rm = TRUE))
      )
  }
  
  ## delete all variables ending with CATI and CAWI
  data_prep_1 <- data_prep_1 %>% select(-matches(paste0(vars_both_sel, ".*[_CAWI|_CATI]$")))
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#

data_prep_2 <- data_prep_1 %>% ungroup()

## Statistics ##
#++++++++++++++#

# percent of columns containing missing values
n_col <- ncol(data_prep_2)
n_col_miss <- length(colnames(data_prep_2)[colSums(is.na(data_prep_2)) > 0])
n_col_miss / n_col # percent containing missing values

# percentage of missing values
col_miss <- colnames(data_prep_2)[colSums(is.na(data_prep_2)) > 0]
summary(unname((colSums(is.na(data_prep_2 %>% select(all_of(col_miss)))) / nrow(data_prep_2))*100))

# number of complete observations
data_prep_2 %>% na.omit() %>% nrow()


## Replace Missing Values in "Non-Existence" Variables ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Students who do not work, for instance, have an income of 0, do not obtain
# a student job etc.
sum(is.na(data_prep_2 %>% select(starts_with("emp_current_"))))
data_prep_2 <- data_prep_2 %>%
  mutate(across(starts_with("emp_current_"), ~ ifelse(emp_current == 0, 0, .)))
sum(is.na(data_prep_2 %>% select(starts_with("emp_current_"))))
sum(is.na(data_prep_2 %>% filter(emp_current == 0) %>% select(starts_with("emp_current_"))))

# Partner (already done in 04_c)
sum(is.na(data_prep_2 %>% select(starts_with("partner_"))))
sum(is.na(data_prep_2 %>% filter(partner_current == 0) %>% select(starts_with("partner_"))))

# Child (already done in 04_c)
sum(is.na(data_prep_2 %>% select(starts_with("child_"))))
sum(is.na(data_prep_2 %>% filter(child == 0) %>% select(starts_with("child_"))))

# Sibling (already done in 04_c)
sum(is.na(data_prep_2 %>% select(starts_with("sibling_"))))
sum(is.na(data_prep_2 %>% filter(sibling == 0) %>% select(starts_with("sibling_"))))


## Drop Variables with too many Missing Values ##
#+++++++++++++++++++++++++++++++++++++++++++++++#

# variables with more than 40% of missing values are dropped
perc_drop_na <- round(nrow(data_prep_2) * 0.4)
## identify those variables
col_names_na <- colSums(is.na(data_prep_2))
col_names_na_drop <- col_names_na[col_names_na > perc_drop_na]
col_names_na_drop <- sort(names(col_names_na_drop))

# generate vector containing columns which I keep anyway
col_keep <- c("comp_", "educ_uni_degree_achieve", "educ_uni_degree_aspire", 
              "prof_expected", "stress","motivation", "sibling", "partner", "child")
col_keep_all <- data_prep_2 %>% select(matches(paste0(col_keep, collapse = "|"))) %>% colnames()

# adjust vector with colnames to drop
col_names_na_drop <- col_names_na_drop[!col_names_na_drop %in% col_keep_all]

# drop those column names
data_prep_2 <- data_prep_2 %>%
  ungroup() %>% 
  select(-all_of(col_names_na_drop))


## Drop variables not needed anymore ##
#+++++++++++++++++++++++++++++++++++++#

# there are some variables which are just not useful anymore
vars_drop <- c(
  "educ_uni_start", "birth_date", "day", "educ_study",
  "current_family_status", "current_residence_country", 
  "educ_uni_break_deregist_nform", "educ_uni_break_deregist_temp", "educ_uni_break_term_off",
  "educ_uni_degree_1", "educ_uni_degree_2", "educ_uni_start", "educ_uni_start_WT10",
  "end_date_adj", "end_date_orig", "educ_uni_type_inst", "wave", "wave_2", 
  "uni_time_employment", "birth_nationality_ger", "educ_degree_uentrance_ger", "educ_profession_aspired",
  "father_emp_manager", "mother_emp_manager", "living_rent",
  "helpless_grades_improve", "helpless_grades_revision", 
  "intern_study_rel", "intern_type", "educ_voc_prep", 
  "uni_admission_restr_other", "educ_uni_degree_achieve", "educ_uni_degree_aspire",
  "personality_sociable", "personality_thorough", "personality_imaginative"
)
# ensure that those variables are really in data frame
vars_drop <- vars_drop[vars_drop  %in% colnames(data_prep_2)]
# drop variables
data_prep_2 <- data_prep_2 %>%
  select(-all_of(vars_drop))


## Create Percentage of Missings ##
#+++++++++++++++++++++++++++++++++#

data_prep_2$NA_COUNT <- apply(data_prep_2, 1, function(x) sum(is.na(x)))
data_prep_2$NA_COUNT_perc <- (data_prep_2$NA_COUNT / ncol(data_prep_2))*100
summary(data_prep_2$NA_COUNT)
summary(data_prep_2$NA_COUNT_perc)

data_prep_2 <- data_prep_2 %>% mutate(
  NA_low = ifelse(NA_COUNT <= unname(quantile(NA_COUNT, probs = 0.25)), 1, 0),
  NA_high = ifelse(NA_COUNT >= unname(quantile(NA_COUNT, probs = 0.75)), 1, 0)
)
table(data_prep_2$NA_low)
table(data_prep_2$NA_high)



## Create NA Dummies ##
#+++++++++++++++++++++#

# ATTENTION: IF YOU CREATE NA DUMMIES THEY MUST BE EXCLUDED WHEN
# AGGREGATING THE VARIABLES

data_prep_3 <- data_prep_2

# NA dummies equal 1 if variable is actually missing and 0 otherwise
# As there are a few hundred NA dummies, I probably drop them later

# # extract all columns containing any missing values
# colnames_any_missing <- colSums(is.na(data_prep_3))
# colnames_any_missing <- names(colnames_any_missing[colnames_any_missing > 0])
# 
# # for every variable containing at least one missing value, a dummy variable
# # is generated determining that the value was initially missing (-> replaced
# # in next step)
# source("Functions/func_generate_NA_dummies.R")
# 
# i <- 0
# for (col_sel in colnames_any_missing) {
#   i <- i + 1
#   data_prep_3 <- func_generate_NA_dummies(data_prep_3, col_sel)
# }

# drop NA dummies
# data_prep_3 <- data_prep_3 %>% select(-ends_with("_NA"))



## Replace NAs ##
#+++++++++++++++#

# https://stefvanbuuren.name/fimd/ch-longitudinal.html
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

data_prep_4 <- data_prep_3

# number of missing values before replacement
  ## total
sum(is.na(data_prep_4))
  ## percentage of missing values per column
unique(sort(unname(colSums(is.na(data_prep_4))))) / nrow(data_prep_4)*100
length(colSums(is.na(data_prep_4))[colSums(is.na(data_prep_4)) > 0])

# to make replacement for categorical variables they need to be converted 
# as factors
data_prep_4 <- data_prep_4 %>%
  mutate_if(is.character, as.factor)

# all continuous variables as numeric
# data_prep_4 <- data_prep_4 %>%
#   mutate_if(is.integer, as.numeric)

# transform data set in wide format
data_prep_4_wide <- data_prep_4
data_prep_4_wide <- data_prep_4_wide %>% 
  complete(ID_t, nesting(treatment_period)) %>% 
  group_by(ID_t) %>% 
  fill(all_of(colnames(data_prep_4_wide)), .direction = "down")
data_prep_4_wide <- dcast(
  melt(data_prep_4_wide, id.vars = c("ID_t", "treatment_period")), 
  ID_t ~ variable + treatment_period)
  ## melt() converts all variables as numeric; hence re-convert them
data_prep_4_wide <- data_prep_4_wide %>%
  type.convert(as.is = TRUE) %>% # R finds correct type
  mutate_if(is.character, as.factor) # character as factors (needed for mice)

# set up mice
init <- mice(data_prep_4_wide, maxit = 0) 
pred_matrix <- init$predictorMatrix

# adjust predictor matrix to reduce the number of variables
  ## ID_t and interview dates are not used as a predictor
pred_matrix[, c("ID_t")] <- 0
pred_matrix[, c(data_prep_4_wide %>% select(starts_with("interview_date")) %>% colnames())] <- 0
  ## for all waves only predictor variables of current wave are used (except for variable to be predicted)
pred_matrix_vars <- rownames(pred_matrix)
for (pred_matrix_vars_sel in pred_matrix_vars) {
  pred_matrix_vars_num <- str_sub(pred_matrix_vars_sel, -1, -1)
  pred_matrix_vars_set_0 <- pred_matrix_vars[
    str_ends(pred_matrix_vars, paste0("_", pred_matrix_vars_num), negate = TRUE)]
  pred_matrix[pred_matrix_vars_sel, c(pred_matrix_vars_set_0)] <- 0
}
  ## only closely related variables are used, e.g. for personality variables only personality variables.
pred_matrix_vars <- c("educ", "interest", "uni", "comp", "child", "sibling", "partner")
for (pred_matrix_vars_sel in pred_matrix_vars) {
  pred_matrix_vars_num <- pred_matrix %>% as.data.frame() %>% 
    select(starts_with(pred_matrix_vars_sel)) %>% colnames()
  pred_matrix_vars_set_0 <- pred_matrix %>% as.data.frame() %>% 
    select(!starts_with(pred_matrix_vars_sel)) %>% colnames()
  pred_matrix[pred_matrix_vars_num, c(pred_matrix_vars_set_0)] <- 0
}
pred_matrix_vars_num <- pred_matrix %>% as.data.frame() %>% 
  select(starts_with("personality"), starts_with("bigfive")) %>% 
  colnames()
pred_matrix_vars_set_0 <- pred_matrix %>% as.data.frame() %>% 
  select(!c(starts_with("personality"), starts_with("bigfive"))) %>% 
  colnames()
pred_matrix[pred_matrix_vars_num, c(pred_matrix_vars_set_0)] <- 0

# apply mice (using defaults: 5 data sets and 5 iterations)
gc()
mice_num_data_sets <- 5
mice_result <- mice(data_prep_4_wide, method = "cart", predictorMatrix = pred_matrix, 
                    seed = 1234, m = mice_num_data_sets, maxit = 1)

# extract data frames in list
data_result_mice <- complete(mice_result, "all")
gc()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### ITERATE OVER MICE DATA FRAMES ####

# NEXT STEPS ARE PERFORMED FOR EACH DATA SET RESULTING FROM MICE 

for (mice_result_sel in 1:mice_num_data_sets) {
  
  # extract data set
  data_mice_sub <- data_result_mice[[mice_result_sel]]
  # saveRDS(data_mice_sub, "data_mice_result_wide_1.rds")
  # sum(is.na(data_mice_sub))
  
  # convert back to long format and drop waves in which individual did not participated
  ## create data frame with ID_t and respective treatment periods
  data_prep_4_reshape <- data_prep_3 %>% select(ID_t, treatment_period)
  ## identify maximum of treatment periods
  max_treatment_periods <- max(data_prep_4_reshape$treatment_period)
  ## extract column names without suffixes _1, _2, _3, _4, _5
  col_reshape <- sort(unique(str_sub(colnames(data_mice_sub), 1, str_length(colnames(data_mice_sub)) - 2)))
  col_reshape <- col_reshape[!col_reshape %in% "ID"] # drop ID
  ## iterate over columns to convert data frame from wide to long
  for (col_reshape_sel in col_reshape) {
    ## generate columns: colname_1 to colname_* where * is maximum of treatment periods
    col_reshape_sel_all <- paste0(col_reshape_sel, "_", 1:max_treatment_periods)
    ## select columns
    data_result_mice_sub <- data_mice_sub %>% select(ID_t, all_of(col_reshape_sel_all))
    ## reshape in long format
    data_result_mice_sub <- 
      reshape(data_result_mice_sub, idvar = "ID_t", varying = list(2:(max_treatment_periods + 1)), 
              v.names = col_reshape_sel, direction = "long") %>% arrange(ID_t)
    ## enumerate rows
    rownames(data_result_mice_sub) <- 1:nrow(data_result_mice_sub)
    ## join to treatment periods
    data_prep_4_reshape <- left_join(
      data_prep_4_reshape, data_result_mice_sub, by = c("ID_t", "treatment_period" = "time")
    )
  }
  
  data_prep_4 <- data_prep_4_reshape
  
  
  # check plausibility of missing value replacement
  ## BMI
  summary(data_prep_3$health_bmi)
  summary(data_prep_4$health_bmi)
  ## motivation degree
  summary(data_prep_3$motivation_degree_1)
  unique(data_prep_3$motivation_degree_1)
  summary(data_prep_4$motivation_degree_1)
  unique(data_prep_4$motivation_degree_1)
  ## personality nervous
  table(data_prep_3$personality_nervous, useNA = "always")
  table(data_prep_4$personality_nervous, useNA = "always")
  ## big five personality conscientiousness
  table(data_prep_3$bigfive_conscientiousness, useNA = "always")
  table(data_prep_4$bigfive_conscientiousness, useNA = "always")
  ## competence domains
  summary(data_prep_3$comp_ict_wle)
  summary(data_prep_4$comp_ict_wle)
  summary(data_prep_3$comp_math_wle)
  summary(data_prep_4$comp_math_wle)
  # reconvert factor variables as character
  data_prep_4 <- data_prep_4 %>%
    mutate_if(is.factor, as.character)
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Aggregate Variables ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  data_prep_5 <- data_prep_4
  
  
  ## REVERSE SCORE ##
  #+++++++++++++++++#
  
  
  # recode positive /negative values: some variables measure the same thing but
  # in a different direction. For instance "stress" is measured negatively 
  # "I often feel lonely" and positively "I do meaningful work". Those variables
  # need to be changed manually in one direction.
  
  # load function
  # source("Functions/func_reverse_score.R")
  
  
  # create data frame with variable name and highest variable value number
  # for those variables for which the order of the scale needs to be reversed.
  df_reverse_vars <- data.frame(
    "vars_reverse" = c("uni_termination_4", paste0("stress_", c(1, 3, 5:7, 11)),
                       "uni_commitment_1", "uni_commitment_4",
                       paste0("personality_selfesteem_", c(2, 5, 6, 8, 9)),
                       paste0("opinion_educ_", c(2,4,8,9,14,15)),
                       paste0("satisfaction_study_", c(2,3,5,6,8,9)),
                       "uni_prep_4"#, 
                       #"bigfive_conscientiousness", "bigfive_openness",
                       #"bigfive_extraversion", "bigfive_neuroticism"
                       ),
    "num_scores" = c(4, rep(5, 6), 5, 5, rep(5, 5), rep(5,6), rep(10, 6), 4#,
                    # rep(5, 4)
                     )
  )
  
  
  # apply function and check
  data_prep_5 %>% select(uni_termination_4, stress_3, satisfaction_study_2) %>% head(5)
  data_prep_5 <- func_reverse_score(data_prep_5, df_reverse_vars)
  data_prep_5 %>% select(uni_termination_4, stress_3, satisfaction_study_2) %>% head(5)
  
  # recode manually (because number of categories do not match)
  data_prep_5 <- data_prep_5 %>%
    mutate(uni_achievement_comp_2 = recode(
      uni_achievement_comp_2, "1" = "5", "2" = "4", "3" = "2", "4" = "1")
    )
  
  
  
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
    "uni_achievement_comp", "uni_perf_satisfied", "uni_offers",  
    "personality_assertiveness", "personality_conflicts", 
    "personality_selfesteem", "parents_opinion_degree", "opinion_educ",
    "motivation_degree", "satisfaction_study", "satisfaction_life",
    "interest_math", "interest_german", "risk", "uni_offers_.*_helpful",
    "friends_opinion_degree"
  )
  # ensure that variables exist (are not dropped above in missing value selection)
  colnames_exist <- unique(sub("_[^_]+$", "", 
                               data_prep_5 %>% 
                                 select(matches(paste0(vars_aggregated_mean, collapse = "|"))) %>% 
                                 colnames()))
  vars_aggregated_mean <- vars_aggregated_mean[vars_aggregated_mean %in% colnames_exist]
  
  ## vector with variables aggregated as sum
  vars_aggregated_sum <- c(
    "uni_counsel_.*_offer", "uni_counsel_.*_use", "uni_offers_.*_partic"
  )
  
  
  
  # load function 
  # source("Functions/func_aggregate_vars.R")
  
  # ungroup data frame
  data_prep_5 <- data_prep_5 %>% ungroup()
  
  # show example
  data_prep_5 %>% select(ID_t, matches("uni_counsel_.*_offer")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("personality_goal_pers")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("friends_opinion_degree")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("satisfaction_study")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("uni_achievement_comp")) %>% head(5)
  
  # apply aggregation
  ## pca / mean
  vars_not_aggr_all <- c()
  for (vars_aggr in vars_aggregated_mean) {
    # ensure that all variables are numeric
    data_prep_5 <- data_prep_5 %>% mutate(across(starts_with(vars_aggr), as.numeric))
    # then apply aggregation
    result_agrregate_vars_ls <- 
      func_aggregate_vars(data_prep_5, vars_aggr, cronbach_a, aggr_vars)
    ## extract data set with aggregated variables
    data_prep_5 <- result_agrregate_vars_ls[[1]]
    ## extract vector with variable prefix that could not be aggregated and append it
    vars_not_aggr <- result_agrregate_vars_ls[[2]]
    vars_not_aggr_all <- c(vars_not_aggr_all, vars_not_aggr)
  }
  ## sum
  for (vars_aggr in vars_aggregated_sum) {
    # ensure that all variables are numeric
    data_prep_5 <- data_prep_5 %>% mutate(across(starts_with(vars_aggr), as.numeric))
    # then apply aggregation
    result_agrregate_vars_ls <- func_aggregate_vars(data_prep_5, vars_aggr, "no", "sum")
    data_prep_5 <- result_agrregate_vars_ls[[1]]
  }
  
  
  data_prep_5 %>% select(ID_t, matches("uni_counsel_offer")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("personality_goal_pers"))  %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("friends_opinion_degree"))  %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("satisfaction_study")) %>% head(5)
  data_prep_5 %>% select(ID_t, starts_with("uni_achievement_comp")) %>% head(5)
  
  
  # aggregate uni_offers as sum (not possible above because of uni_offers_*_helpful)
  data_prep_5 <- data_prep_5 %>% select(-uni_offers_no)
  cols_offers <- data_prep_5 %>% select(starts_with("uni_offers")) %>% colnames()
  cols_offers_help <- data_prep_5 %>% select(starts_with("uni_offers") & ends_with("helpful")) %>% colnames()
  cols_offers <- cols_offers[!cols_offers %in% cols_offers_help]
  
  data_prep_5 <- data_prep_5 %>% 
    mutate("uni_offers" = round(rowSums(select(
      data_prep_5, all_of(cols_offers)), na.rm = TRUE))) %>%
    select(-(all_of(cols_offers)))
  
  
  
  ## Re-label Variables ##
  #++++++++++++++++++++++#
  
  # only if as aggregation method mean is chosen, the labels are re-labeled,
  # e.g. to 1 = "does not apply at all"
  
  if (aggr_vars == "mean") {
    table(data_prep_5$personality_goal_pers)
    table(data_prep_5$academic)
    
    # load lists with labels
    list_cawi_labels <- readRDS("Data/Prep_1/prep_1_target_cawi_list.rds")
    list_cati_labels <- readRDS("Data/Prep_1/prep_1_target_cati_list.rds")
    list_labels <- append(list_cawi_labels, list_cati_labels)
    
    for (i in 1:length(list_labels)) {
      
      # extract column name
      col_name_cawi_sel <- names(list_labels)[i]
      
      # extract values according to column name
      label_cawi_sel <- list_labels[[col_name_cawi_sel]]
      
      # swap names and values
      label_cawi_sel <- setNames(names(label_cawi_sel), label_cawi_sel)
      
      # label variable
      data_prep_5 <- data_prep_5 %>%
        mutate(
          {{col_name_cawi_sel}} := recode(!!!rlang::syms(col_name_cawi_sel), !!!label_cawi_sel)
        )
      
      
    }
    table(data_prep_5$personality_goal_pers)
    table(data_prep_5$academic)
    
    
  } else {
    list_cawi_labels <- readRDS("Data/Prep_1/prep_1_target_cawi_list.rds")
    list_cati_labels <- readRDS("Data/Prep_1/prep_1_target_cati_list.rds")
    list_labels <- append(list_cawi_labels, list_cati_labels)
    
    for (vars_relabel in vars_not_aggr_all) {
      
      # column names
      colnames_relabel <- data_prep_5 %>% select(starts_with(vars_relabel)) %>% colnames()
      
      # extract values according to column name
      label_cawi_sel <- list_labels[[vars_relabel]]
      label_cawi_sel <- setNames(names(label_cawi_sel), label_cawi_sel)
      
      if (length(label_cawi_sel) == 0) {
        data_prep_5 <- data_prep_5
      } else {
        for (colnames_relabel_sel in colnames_relabel) {
          # label variable
          data_prep_5 <- data_prep_5 %>%
            mutate(
              {{colnames_relabel_sel}} := recode(!!!rlang::syms(colnames_relabel_sel), !!!label_cawi_sel)
            )
        }
      }
      
    }
  }
  
  # relabel big five personality trait variables
  colnames_bigfive <- data_prep_5 %>% select(starts_with("bigfive")) %>% colnames()
  
  for (cols_bigfive_sel in colnames_bigfive) {
    # extract label
    label_bigfive_sel <- list_labels[[cols_bigfive_sel]]
    label_bigfive_sel <- setNames(names(label_bigfive_sel), label_bigfive_sel)
    
    # rename
    data_prep_5 <- data_prep_5 %>%
      mutate(
        {{cols_bigfive_sel}} := recode(!!!rlang::syms(cols_bigfive_sel), !!!label_bigfive_sel)
      )
  }

  
  # relabel uni_offers_.*_helpful variables
  label_cawi_sel <- list_labels[["uni_offers_helpful"]]
  label_cawi_sel <- setNames(names(label_cawi_sel), label_cawi_sel)
  
  for (cols_offers_help_sel in cols_offers_help) {
    data_prep_5 <- data_prep_5 %>%
      mutate(
        {{cols_offers_help_sel}} := recode(!!!rlang::syms(cols_offers_help_sel), !!!label_cawi_sel)
      )
  }
  
  # relabel helpless
  label_cawi_sel <- list_labels[["helpless"]]
  label_cawi_sel <- setNames(names(label_cawi_sel), label_cawi_sel)
  
  cols_helpless <- data_prep_5 %>% select(starts_with("helpless")) %>% colnames()
  
  for (cols_helpless_sel in cols_helpless) {
    data_prep_5 <- data_prep_5 %>%
      mutate(
        {{cols_helpless_sel}} := recode(!!!rlang::syms(cols_helpless_sel), !!!label_cawi_sel)
      )
  }
  
  
  
  # "apply" variables
  ## extract variables
  vars_recode_apply <- data_prep_5 %>%
    select_if(~ is.character(.)) %>%
    select_if(~ any(. == "does completely apply")) %>%
    colnames()
  ## recode variables
  data_prep_5 <- data_prep_5 %>% 
    mutate_at(
      all_of(vars_recode_apply), 
      list(
        ~recode(., `does completely apply` = "completely", `does rather apply` = "rather",
                `does partly apply` = "partly", `does rather not apply` = "rather_not",
                `does not apply at all`= "not", .default = as.character(NA)) 
      ) 
    ) 
  
  
  # variables not occuring often
  ## extract variables
  vars_recode_good <- data_prep_5 %>%
    select_if(~ is.character(.)) %>%
    select_if(~ any(. == "very bad") | any(. == "very high") | any(. == "very important") | any(. == "slightly less")) %>%
    colnames()
  ## only put "_" in between
  data_prep_5 <- data_prep_5 %>% 
    mutate_at(all_of(vars_recode_good), list(~ str_replace(., " ", "_")))
  
  
  data_prep_5 %>% select(starts_with("uni_quality")) %>% head(5)
  data_prep_5 %>% select(starts_with("friends_opinion_degree")) %>% head(5)
  data_prep_5 %>% select(starts_with("uni_offers")) %>% head(5)
  data_prep_5 %>% select(starts_with("uni_achievement_comp")) %>% head(5)
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Numeric in Categorical Variables ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  data_prep_6 <- data_prep_5
  
  
  ## BMI ##
  #+++++++#
  
  # generate categories
  # https://www.cdc.gov/obesity/basics/adult-defining.html and Flegal et al.
  # normal weight: BMI of 18.5-25
  # underweight: BMI < 18.5
  # overweight: 25-30
  # obesity: > 30
  summary(data_prep_6$health_bmi)
  data_prep_6 <- data_prep_6 %>%
    mutate(
      health_bmi_cat_over_under = case_when(health_bmi > 25 ~ 1, health_bmi < 18.5 ~ 1, TRUE ~ 0),
    )
  table(data_prep_6$health_bmi_cat_over_under, useNA = "always")
  
  
  
  ## OTHER VARIABLES ##
  #+++++++++++++++++++#
  
  ## ADJUSTMENTS ##
  
  # educ_years_total
  summary(data_prep_6$educ_years_total)
  table(round(data_prep_6$age), round(data_prep_6$educ_years_total))
  
  data_prep_6 <- data_prep_6 %>%
    # unreaöistic high values are adjusted
    mutate(educ_years_total = case_when(age - educ_years_total < 5 ~ age - 5, TRUE ~ educ_years_total)) %>%
    # unrealistic low values are adjusted
    mutate(educ_years_total = case_when(educ_years_total < 10 ~ age - 7, TRUE ~ educ_years_total))
  
  summary(data_prep_6$educ_years_total)
  table(round(data_prep_6$age), round(data_prep_6$educ_years_total))
  
  
  # working hours
  data_prep_6 <- data_prep_6 %>% mutate(
    emp_current_act_work_hours = case_when(
      emp_current == 1 & emp_current_act_work_hours == 0 ~ 1, TRUE ~ emp_current_act_work_hours 
    )
  )
  
  
  ## CATEGORIZE USING QUANTILES ##
  
  for (vars_categorized in c("age", "educ_years_total", "extracurricular_num",
                             "uni_offers", "uni_counsel_offer", "uni_counsel_use",
                             "emp_current_act_work_hours", "uni_time_courses",
                             "uni_time_studyact", "uni_time_household", "uni_time_childcare",
                             "uni_time_study")) {
    
    # generate splitting values
    split_one <- unname(quantile(data_prep_6 %>% pull(vars_categorized), 0.25))
    split_two <- unname(quantile(data_prep_6 %>% pull(vars_categorized), 0.75))
    split_max <- max(data_prep_6 %>% pull(vars_categorized))
    
    # new variable name
    vars_categorized_name <- paste0(vars_categorized, "_cat")
    
    if (split_one != split_two) {
      # generate new variable
      data_prep_6 <- data_prep_6 %>%
        mutate({{vars_categorized_name}} := as.character(
          cut(!!!syms(vars_categorized), breaks = c(-Inf, split_one, split_two, Inf),
              labels = c(as.character(round(split_one)), as.character(round(split_two)), 
                         as.character(ceiling(split_max))))
        ))
    } else {
      data_prep_6 <- data_prep_6 %>%
        mutate({{vars_categorized_name}} := as.character(
          cut(!!!syms(vars_categorized), breaks = c(-Inf, split_one, Inf),
              labels = c(as.character(round(split_one)),  as.character(ceiling(split_max))))
        ))
    }
    
    
  }
  
  table(data_prep_6$age_cat, useNA = "always")
  table(data_prep_6$educ_years_total_cat, useNA = "always")
  table(data_prep_6$extracurricular_num_cat, useNA = "always")
  table(data_prep_6$uni_counsel_offer_cat, useNA = "always")
  table(data_prep_6$uni_counsel_use_cat, useNA = "always")
  table(data_prep_6$emp_current_act_work_hours_cat, useNA = "always")
  table(data_prep_6$uni_time_courses_cat, useNA = "always")
  table(data_prep_6$uni_time_studyact_cat, useNA = "always")
  table(data_prep_6$uni_time_household_cat, useNA = "always")
  table(data_prep_6$uni_time_childcare_cat, useNA = "always")
  table(data_prep_6$uni_time_study_cat, useNA = "always")
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%#
  #### Dummy Variables ####
  #%%%%%%%%%%%%%%%%%%%%%%%#
  
  data_prep_7 <- data_prep_6
  
  
  ## Recategorize to avoid too many categories ##
  #+++++++++++++++++++++++++++++++++++++++++++++#
  
  # opinion_educ_* 
  table(data_prep_7$opinion_educ_1, useNA = "always")
  if (data_prep_7 %>% select(starts_with("opinion_educ")) %>% colnames() %>% length() > 1) {
    data_prep_7 <- data_prep_7 %>%
      mutate(across(starts_with("opinion_educ"), 
                    ~ case_when(. %in% c("completely disagree", "rather agree") ~ "agree",
                                TRUE ~ "not_agree")))
  }
  table(data_prep_7$opinion_educ_1, useNA = "always")
  
  # parents_importance_*
  table(data_prep_7$parents_importance_success_1, useNA = "always")
  if (data_prep_7 %>% select(starts_with("parents_importance")) %>% colnames() %>% length() > 1) {
    data_prep_7 <- data_prep_7 %>%
      mutate(across(starts_with("parents_importance"), 
                    ~ case_when(. %in% c("very_important", "rather_important") ~ "important",
                                TRUE ~ "not_important")))
  }
  table(data_prep_7$parents_importance_success_1, useNA = "always")
  
  # parents_opinion_degree_*
  table(data_prep_7$parents_opinion_degree_1, useNA = "always")
  if (data_prep_7 %>% select(starts_with("parents_opinion_degree")) %>% colnames() %>% length() > 1) {
    data_prep_7 <- data_prep_7 %>%
      mutate(across(starts_with("parents_opinion_degree"), 
                    ~ case_when(. %in% c("completely", "rather") ~ "agree",
                                TRUE ~ "not_agree")))
  }
  table(data_prep_7$parents_opinion_degree_1, useNA = "always")
  
  # stress
  table(data_prep_7$stress_1, useNA = "always")
  if (data_prep_7 %>% select(starts_with("stress")) %>% colnames() %>% length() > 1) {
    data_prep_7 <- data_prep_7 %>%
      mutate(across(starts_with("stress"), 
                    ~ case_when(. %in% c("completely", "rather") ~ "agree",
                                TRUE ~ "not_agree")))
  }
  table(data_prep_7$stress_1, useNA = "always")
  
  # uni_offers_*_heloful
  table(data_prep_7$uni_offers_central_facilities_helpful, useNA = "always")
  if (data_prep_7 %>% select(starts_with("uni_offers") & ends_with("helpful")) %>% colnames() %>% length() > 1) {
    data_prep_7 <- data_prep_7 %>%
      mutate(across(starts_with("uni_offers") & ends_with("helpful"), 
                    ~ case_when(. %in% c("very helpful", "rather helpful") ~ "helpful",
                                TRUE ~ "not_helpful")))
  }
  table(data_prep_7$uni_offers_central_facilities_helpful, useNA = "always")
  
  
  
  ## Generate dummies ##
  #++++++++++++++++++++#
  
  # automatically generate dummy variables for categorical variables
  # LASSO will select which are important
  ## identify all categorical columns
  vars_categoric <- data_prep_7 %>% ungroup() %>% select_if(~ is.character(.)) %>% colnames()
  vars_categoric <- vars_categoric[!str_detect(vars_categoric, "date")]
  data_prep_7 <- fastDummies::dummy_cols(
    # selected column cannot be removed due to descriptives (hence saved and removed later)
    # base category is omitted 
    data_prep_7, remove_selected_columns = FALSE, remove_first_dummy = TRUE, 
    select_columns = vars_categoric
  )
  
  # saveRDS(vars_categoric, "Data/Prep_7/prep_7_variables_drop_cat.rds")
  
  
  # birth month and birth year as additional dummys
  data_prep_7$birth_month_name <- as.character(month(data_prep_7$birth_month, label = TRUE, abbr = TRUE))
  data_prep_7 <- dummy_cols(
    # selected column is removed, and base category is omitted 
    data_prep_7, remove_selected_columns = FALSE, remove_first_dummy = TRUE, 
    select_columns = c("birth_month_name", "birth_year", "interview_start_year", "interview_end_year")
  ) %>% select(-c("birth_month_name", "birth_year", "interview_start_year", "interview_end_year"))
  
  # drop "name" in "birth_month_name"
  data_prep_7 <- data_prep_7 %>%
    rename_all(funs(stringr::str_replace_all(., 'birth_month_name_', 'birth_month_')))
  
  # change educ_uni to uni
  data_prep_7 <- data_prep_7 %>%
    rename_all(list(~ stringr::str_replace_all(., 'educ_uni_', 'uni_'))) %>%
    rename(uni_study_num = educ_study_num)
  
  
  #%%%%%%%%%%%%%%%%%%%#
  #### Final Steps ####
  #%%%%%%%%%%%%%%%%%%%#
  
  data_final <- data_prep_7
  
  # all variable names as lower case; whitespace are replace with "_"
  data_final <- data_final %>%
    rename_all(list(~ stringr::str_to_lower(.))) %>%
    rename_all(list(~ stringr::str_replace_all(., ' ', '_')))
  
  # ungroup
  data_final <- data_final %>% ungroup()
  
  # check for missing values
  sum(is.na(data_final))
  
  # check for duplicates
  sum(duplicated(data_final))
  
  # number of respondents, rows, and columns
  print(paste("Number of respondents:", length(unique(data_final$id_t))))
  print(paste("Number of rows", nrow(data_final)))
  print(paste("Number of columns", ncol(data_final)))
  
  # save data frame
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  
  if (cohort_prep == "controls_same_outcome") {
    data_save <- paste0("Data/Prep_7/prep_7_control_vars_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_mice", mice_result_sel, ".rds")
  } else {
    data_save <- paste0("Data/Prep_7/prep_7_control_vars_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_robustcheck", "_mice", 
                        mice_result_sel, ".rds")
  }
  
  saveRDS(data_final, data_save)
  
  # save number of rows, columns, and respondents in excel file
  df_excel_save <- data.frame(
    "data_prep_step" = "create_controls",
    "data_prep_choice_cohort" = cohort_prep,
    "data_prep_treatment_repl" = treatment_repl,
    "data_prep_treatment_def" = treatment_def,
    "data_prep_extraact" = extra_act, 
    "num_id" = length(unique(data_final$id_t)), 
    "num_rows" = nrow(data_final),
    "num_cols" = ncol(data_final),
    "time_stamp" = Sys.time()
  )
  ## load function
  source("Functions/func_save_sample_reduction.R")
  func_save_sample_reduction(df_excel_save)
  
} # close iteration over mice result data frames

