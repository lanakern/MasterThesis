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
rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", 
                          "df_inputs", "prep_sel_num", ls()[str_starts(ls(), "func_")])))

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
if (cohort_prep == "controls_bef_outcome" ) {
  load_data <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
} else {
  load_data <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, ".rds")
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
  mutate(educ_years = rowSums(
    across(c("spell_length_cum_School", "spell_length_cum_Uni", "spell_length_cum_VocTrain", "spell_length_cum_VocPrep")),
    na.rm = TRUE
    )) 
summary(data_prep_1$educ_years)

# summary across age and education years
table(round(data_prep_1$age), round(data_prep_1$educ_years))


## BMI ##
#+++++++#

# check weight and height for unrealistic values
summary(data_prep_1$health_weight)
summary(data_prep_1$health_height)
sum(rowSums(is.na(data_prep_1[, c("health_weight", "health_height")])) > 0) # number of NA in both

# BMI = weight / height^2
data_prep_1 <- data_prep_1 %>%
  mutate(health_bmi = health_weight / ((health_height/100)^2))

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
# for all variables already done except spell_length_current_Emp
colSums(is.na(data_prep_1 %>% select(starts_with("spell_length"))))

data_prep_1 <- data_prep_1 %>%
  mutate(spell_length_current_Emp = case_when(spell_length_current_Emp > 0 ~ 1, TRUE ~ 0))

sum(is.na(data_prep_1 %>% select(starts_with("spell_length"))))


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

table(data_prep_1$current_emp, useNA = "always")

# 1.) If any employment variable has a value, current_emp must equal 1
data_prep_1 <- data_prep_1 %>%
  ungroup() %>%
  mutate(current_emp = ifelse(
    rowSums(!is.na(data_prep_1 %>% select(starts_with("current_emp_")))) > 0, 1, current_emp)
    )

table(data_prep_1$current_emp, useNA = "always")


# 2.) Dummies
data_prep_1 <- data_prep_1 %>%
  mutate(
    current_emp_self = case_when(
      grepl("freelancer", current_emp_prof_pos) | grepl("self-employed", current_emp_prof_pos) | 
        grepl("freelance", current_emp_student_job_type) | grepl("self-employment", current_emp_student_job_type) ~ 1,
      is.na(current_emp_student_job_type) & is.na(current_emp_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    ),
    current_emp_student = case_when(
      grepl("student", current_emp_student_job_type) | grepl("tutoring", current_emp_student_job_type) |
        grepl("student", current_emp_prof_pos) ~ 1,
      current_emp_student_job == 1 ~ 1,
      is.na(current_emp_student_job_type) & is.na(current_emp_prof_pos) ~ as.double(NA),
      TRUE ~ 0
    )
  ) %>%
  select(-c(current_emp_student_job, current_emp_student_job_type, current_emp_prof_pos, current_emp_student_job_rel))



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
                                    "Master, Diploma, Magister, state examination" = "Master",
                                    "no completed degree" = "No", "doctorate/habilitation" = "PHD"),
    educ_uni_degree_achieve = recode(educ_uni_degree_achieve, 
                                     "Master, Diploma, Magister, state examination" = "Master",
                                     "no completed degree" = "No", "doctorate/habilitation" = "PHD"),
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
      "I didn't really have a preferred higher education institution" = "no_choice")
  ) %>%
  select(-c(educ_highest_degree_casmin, educ_highest_degree_isced))

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
    ),
  )


table(data_prep_1$mother_emp_prof, useNA = "always")
table(data_prep_1$mother_emp_prof_blk, useNA = "always")
table(data_prep_1$father_emp_prof, useNA = "always")
table(data_prep_1$father_emp_prof_blk, useNA = "always")


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

# birth in germany (east-west)
table(data_prep_1$birth_ger_eastwest)
data_prep_1 <- data_prep_1 %>% mutate(
  birth_ger_eastwest = recode(birth_ger_eastwest, "East Germany incl. Berlin" = "East",
                             "West Germany" = "West"))


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
  ) 

table(data_prep_1$health_general, useNA = "always")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#

data_prep_2 <- data_prep_1


## Replace Missing Values in Non-Existence Variables ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Students who do not work, for instance, have an income of 0, do not obtain
# a student job etc.
data_prep_2 <- data_prep_2 %>%
  mutate(across(starts_with("current_emp_"), ~ ifelse(current_emp == 0, 0, .)))

# Partner

# Child

# Sibling


## Drop Variables ##
#++++++++++++++++++#


## Create NA Dummies ##
#+++++++++++++++++++++#

# (probably dropped later)


## Create Percentage of Missings ##
#+++++++++++++++++++++++++++++++++#


## Replace NAs ##
#+++++++++++++++#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Aggregate Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_prep_2 <- data_prep_1


## VARIABLES IN CATI & CAWI ##
#++++++++++++++++++++++++++++#

# mean is taken across CATI and CAWI (below they are aggregated again. Hence, no rounding here)
  ## ungroup()
data_prep_2 <- data_prep_2 %>% ungroup()
  ## define variables which are in CATI and CAWI
vars_both <- c("personality_assertiveness", "personality_conflicts", "satisfaction_life")
  ## perform operation:
for (vars_both_sel in vars_both) {
  
  ## identify all variables starting with string
  vars_both_sel_all <- data_prep_2 %>%
    select(starts_with(vars_both_sel)) %>% colnames() %>% 
    str_remove_all("_CATI") %>% str_remove_all("_CAWI") %>% unique()
  
  ## for all of those take the mean
  for (vars_both_sel_all_rep  in vars_both_sel_all) {
    data_prep_2 <- data_prep_2 %>%
      mutate(
        !!vars_both_sel_all_rep := rowMeans(
          select(data_prep_2, !!!rlang::syms(paste0(vars_both_sel_all_rep, "_CAWI")), 
                 !!!rlang::syms(paste0(vars_both_sel_all_rep, "_CATI"))),
          na.rm = TRUE)
      )
  }
  
  ## delete all variables ending with CATI and CAWI
  data_prep_2 <- data_prep_2 %>% select(-matches(paste0(vars_both_sel, ".*[_CAWI|_CATI]$")))
  
}



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
data_prep_2 %>% select(uni_termination_4, stress_3, satisfaction_study_2)
data_prep_2 <- func_reverse_score(data_prep_2, df_reverse_vars)
data_prep_2 %>% select(uni_termination_4, stress_3, satisfaction_study_2)

# recode manually (because number of categories do not match)
data_prep_2 <- data_prep_2 %>%
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
  "uni_achievement_comp", "uni_perf_satisfied", 
  ## CATI
  "personality_assertiveness", "personality_conflicts", 
  "personality_selfesteem", "parents_opinion_degree", "opinion_educ",
  "motivation_degree", "satisfaction_study", "satisfaction_life",
  "interest_math", "interest_german", "risk", "uni_offers_.*_helpful",
  ## CATI & CAWI
  "friends_opinion_degree"
)
  ## vector with variables aggregated as sum
vars_aggregated_sum <- c(
  "uni_counsel_.*_offer", "uni_counsel_.*_use", "uni_offers_.*_partic"
)



# load function 
source("Functions/func_aggregate_vars.R")

# ungroup data frame
data_prep_2 <- data_prep_2 %>% ungroup()

# show example
data_prep_2 %>% select(ID_t, matches("uni_counsel_.*_offer"))
data_prep_2 %>% select(ID_t, starts_with("risk_"))

# apply aggregation
  ## mean
for (vars_aggr in vars_aggregated_mean) {
  # ensure that all variables are numeric
  data_prep_2 <- data_prep_2 %>% mutate(across(starts_with(vars_aggr), as.numeric))
  # then apply aggregation
  data_prep_2 <- func_aggregate_vars(data_prep_2, vars_aggr, "no", "mean")
  
}
  ## sum
for (vars_aggr in vars_aggregated_sum) {
  # ensure that all variables are numeric
  data_prep_2 <- data_prep_2 %>% mutate(across(starts_with(vars_aggr), as.numeric))
  # then apply aggregation
  data_prep_2 <- func_aggregate_vars(data_prep_2, vars_aggr, "no", "sum")
}



data_prep_2 %>% select(ID_t, matches("uni_counsel_offer"))
data_prep_2 %>% select(ID_t, starts_with("risk"))



## UNI OFFERS ##
#++++++++++++++#

# aggregated here because not possible to match
vars_offer <- c("uni_offers_people", "uni_offers_orga", "uni_offers_central_facilities",
                "uni_offers_course", "uni_offers_skills", "uni_offers_no")
data_prep_2 <- 
  data_prep_2 %>% 
  ungroup() %>%
  mutate(uni_offers = round(rowSums(select(data_prep_2, all_of(vars_offer)), na.rm = TRUE))) %>%
  select(-all_of(vars_offer))





#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Re-label Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_prep_3 <- data_prep_2

table(data_prep_3$personality_goal_pers)
table(data_prep_3$academic)
table(data_prep_3$risk)
table(data_prep_3$uni_offers_helpful)

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
  data_prep_3 <- data_prep_3 %>%
    mutate(
      {{col_name_cawi_sel}} := recode(!!!rlang::syms(col_name_cawi_sel), !!!label_cawi_sel)
    )
}


table(data_prep_3$personality_goal_pers)
table(data_prep_3$academic)
table(data_prep_3$risk)
table(data_prep_3$uni_offers_helpful)



# "apply" variables
## extract variables
vars_recode_apply <- data_prep_3 %>%
  select_if(~ is.character(.)) %>%
  select_if(~ any(. == "does completely apply")) %>%
  colnames()
## recode variables
data_prep_3 <- data_prep_3 %>% 
  mutate_at(
    all_of(vars_recode_apply), 
    list(
      ~recode(., `does completely apply` = "completely", `does rather apply` = "rather",
              `does partly apply` = "partly", `does rather not apply` = "rather_not",
              `does not apply at all`= "not", .default = as.character(NA)) 
    ) 
  ) 

table(data_prep_3$bigfive_extraversion, useNA = "always")


# variables not occuring often
## extract variables
vars_recode_good <- data_prep_3 %>%
  select_if(~ is.character(.)) %>%
  select_if(~ any(. == "very bad") | any(. == "very high") | any(. == "very important") | any(. == "slightly less")) %>%
  colnames()
## only put "_" in between
data_prep_3 <- data_prep_3 %>% 
  mutate_at(all_of(vars_recode_good), list(~ str_replace(., " ", "_")))



#%%%%%%%%%%%%%%%%%%%%%%#
#### Drop Variables ####
#%%%%%%%%%%%%%%%%%%%%%%#

# create new data frame for dropping
data_sub_1 <- data


## Drop Variables with too many Missing Values ##
#+++++++++++++++++++++++++++++++++++++++++++++++#

# variables with too many missing values are dropped
# that is variables with more than 40% of missing values
perc_drop_na <- nrow(data_sub_1) * 0.4
## identify those variables
col_names_na <- colSums(is.na(data_sub_1))
col_names_na_drop <- col_names_na[col_names_na > perc_drop_na]
col_names_na_drop <- names(col_names_na_drop)

# generate vector containing columns which I keep anyway
# KEEP mother_ and father_v variables, parents_
col_keep_emp <- data_sub_1 %>% select(starts_with("current_emp")) %>% select(-current_emp_2) %>% colnames()
col_keep_parents <- data_sub_1 %>% select(starts_with("mother") | starts_with("father_") | starts_with("parents")) %>% colnames()
col_keep <- c("educ_uni_master_current", "current_emp", "satisfaction_life", "motivation_degree", 
              "risk", "child", "extracurricular_freq", "health_physical_good", "health_mental_good",
              "health_smoking_current", "friends_study_share", "stress")
col_keep_all <- c(col_keep_emp, col_keep_parents, col_keep, 
                  col_names_na_drop[str_starts(col_names_na_drop, "sport")], 
                  col_names_na_drop[str_starts(col_names_na_drop, "parents")])

# adjust vector with colnames to drop
col_names_na_drop <- col_names_na_drop[!col_names_na_drop %in% col_keep_all]

# drop those column names
data_sub_1 <- data_sub_1 %>%
  select(-all_of(col_names_na_drop))


## Drop variables not needed anymore ##
#+++++++++++++++++++++++++++++++++++++#

# there are some variables which are just not useful anymore
vars_drop <- c(
  "educ_uni_start", "uni_first_eps", "birth_date", "interview_date_spell", 
  "spell_length_cum_Data edition gap", "spell_length_cum_Unemp", "spell_length_cum_ParLeave", 
  "spell_length_cum_Gap", "educ_profession_aspired", "current_family_status", 
  "mother_language_target", "father_language_target", "degree_uentrance_ger"
  )
data_sub_1 <- data_sub_1 %>%
  select(-all_of(vars_drop))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Handling Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++++++++++++#
## NA DUMMIES ##
#++++++++++++++#

data_sub_2 <- data_sub_1

# COMPETENCIES: two strategies (own idea and regression tree)
# # replace missing values
# ## WLE replaced by 0 (average)
# ## Share replaced by 0.5 (as it ranges from 0 to 1)
# ## Sum is replaced by mean
# 
# ## extract columsn with NAs
# cols_NA <- names(colSums(is.na(data_competencies_final))[colSums(is.na(data_competencies_final)) > 0])
# ## create NA dummies
# for (cols_NA_sel in cols_NA) {
#   cols_NA_mut <- paste0(cols_NA_sel, "_NA")
#   data_competencies_final <- data_competencies_final %>%
#     mutate(
#       {{cols_NA_mut}} := ifelse(is.na(!!!syms(cols_NA_sel)), 1, 0),
#       {{cols_NA_sel}} := ifelse(is.na(!!!syms(cols_NA_sel)), 0, !!!syms(cols_NA_sel))
#     )
# }
# ## replace missing values
# data_competencies_final <- data_competencies_final %>%
#   mutate(
#     across(matches("wle"), ~ replace_na(., 0)),
#     across(matches("share"), ~ replace_na(., 0.5)),
#     across(matches("sum"), ~ replace_na(., mean(., na.rm = TRUE)))
#   )

# THINK ABOUT NO NA DUMMY FOR VARIABLES WITH E.G. ONLY 100 MISSING VALUES OR LESS
# OR DROP OBSERVATIONS WITH LESS THAN 100 MISSING VALUES

# extract all columns containing any missing values
colnames_any_missing <- colSums(is.na(data_sub_2))
colnames_any_missing <- names(colnames_any_missing[colnames_any_missing > 0])

# for every variable containing at least one missing value, a dummy variable
# is generated determining that the value was initially missing (-> replaced
# in next step)
source("Functions/func_generate_NA_dummies.R")

i <- 0
for (col_sel in colnames_any_missing) {
  i <- i + 1
  data_sub_2 <- func_generate_NA_dummies(data_sub_2, col_sel)
}




#+++++++++++++++#
## REPLACEMENT ##
#+++++++++++++++#

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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Numeric in Categorical Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


if (convert_num_char == "yes") {
  data_sub_4 <- data_sub_3
  
  ## AGE ##
  
  # create age categories
  summary(data_sub_4$age)
  data_sub_4 <- data_sub_4 %>%
    mutate(
      age_cat = as.character(cut(age, breaks = c(-Inf, 21, 23, Inf), 
                                 labels = c("21", "23", "25")))
    )
  table(data_sub_4$age_cat, useNA = "always")
  
  
  ## BMI ##
  
  # generate categories
  # https://www.cdc.gov/obesity/basics/adult-defining.html and Flegal et al.
  # normal weight: BMI of 18.5-25
  # underweight: BMI < 18.5
  # overweight: 25-30
  # obesity: > 30
  summary(data_sub_4$BMI)
  data_sub_4 <- data_sub_4 %>%
    mutate(
      BMI_normal = case_when(BMI >= 18.5 & BMI <= 25 ~ 1, TRUE ~ 0),
      BMI_over = case_when(BMI > 25 ~ 1, TRUE ~ 0)
    )
  table(data_sub_4$BMI_normal, useNA = "always")
  table(data_sub_4$BMI_over, useNA = "always")
  
  
  ## YEARS OF EDUCATION ##
  
  summary(data_sub_4$educ_years)
  data_sub_4 <- data_sub_4 %>%
    mutate(educ_years_cat = as.character(
      cut(educ_years, breaks = c(-Inf,  14, 17, Inf), labels = c("14", "17", "20")
      )))
  table(data_sub_4$educ_years_cat, useNA = "always")
  
  # ## EXTRACURRICULAR ##
  # extracurricular_num 
  # extracurricular_freq
  # 
  # 
  # ## HOUSEHOLD SIZE ##
  # 
  # summary(data_sub_4$living_hh_size)
  # 
  # data_sub_4 <- data_sub_4 %>%
  #   mutate(
  #     living_hh_size_2 = ifelse(living_hh_size == 2, 1, 0),
  #     living_hh_size_3_4 = ifelse(living_hh_size %in% c(3, 4), 1, 0),
  #     living_hh_size_5plus = ifelse(living_hh_size >= 5, 1, 0)
  #   )
  # 
  # table(data_sub_4$living_hh_size_2)
  # table(data_sub_4$living_hh_size_3_4)
  # table(data_sub_4$living_hh_size_5plus)
  # 
  # 
  # ## RENT ##
  # 
  # summary(data_sub_4$living_rent)
  # 
  # data_sub_4$living_rent_cat <- cut(
  #   data_sub_4$living_rent, breaks = c(-Inf, 250, 500, 1000, Inf), 
  #   labels = paste0("living_rent_", c("250", "500", "1000", "1000plus"))
  #   )
  #   
  # 
  # ## UNI ##
  # 
  # uni_counsel_offer uni_counsel_use  uni_offers_partic   uni_offers
  # 
  # 
  # ## SPENDING TIME ##
  # 
  # uni_time_courses uni_time_studyact uni_time_employment
  # uni_time_household uni_time_childcare uni_time_study
  # 
  # # courses
  # 
  # # study
  # 
  # # employment
  # 
  # # household 
  # 
  # # child care
  # 
  # 
  # ## INCOME ##
  # 
  # current_emp_net_income
  # 
  # ## CHILD ##
  # 
  # child_total_num    
  # child_age_youngest 
  # child_age_oldest
  # child_school_num    
  # child_male_num   
  # 
  # 
  # ## PARTNER ##
  # 
  # partner_length_previous partner_length_current  partner_age
  # partner_num_total
  # 
  # ## LENGTH TREATMENT PERIOD
  # treatment_period_length
} else {
  data_sub_4 <- data_sub_3
}


#%%%%%%%%%%%%%%%%%%%%%%%#
#### Dummy Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%#

data_sub_5 <- data_sub_4

# automatically generate dummy variables for categorical variables
# LASSO will select which are important
## identify all categorical columns
vars_categoric <- data_sub_5 %>% ungroup() %>% select_if(~ is.character(.)) %>% colnames()
data_sub_5 <- dummy_cols(
  # selected column cannot be removed due to descriptives, 
  # base category is omitted 
  data_sub_5, remove_selected_columns = FALSE, remove_first_dummy = TRUE, 
  select_columns = vars_categoric
)

saveRDS(vars_categoric, "Data/prep_6_variables_drop_cat.rds")


# birth month and birth year as additional dummys
data_sub_5$birth_month_name <- as.character(month(data_sub_5$birth_month, label = TRUE, abbr = TRUE))
data_sub_5 <- dummy_cols(
  # selected column is removed, and base category is omitted 
  data_sub_5, remove_selected_columns = FALSE, remove_first_dummy = TRUE, 
  select_columns = c("birth_month_name", "birth_year", "interview_start_year", "interview_end_year")
) %>% select(-c("birth_month_name", "birth_year", "interview_start_year", "interview_end_year"))


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# check for missing values
sum(is.na(data_sub_5))

# check for duplicates
sum(duplicated(data_sub_5))
  ## remove duplicates
data_sub_5 <- data_sub_5 %>% distinct()

# number of respondents, rows, and columns
print(paste("Number of respondents:", length(unique(data_sub_5$ID_t))))
print(paste("Number of rows", nrow(data_sub_5)))
print(paste("Number of columns", ncol(data_sub_5)))

# save data frame
saveRDS(data_sub_5, "Data/prep_6_variables.rds")
