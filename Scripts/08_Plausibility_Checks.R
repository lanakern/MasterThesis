#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Plausibility Checks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, plausibility checks are made, e.g. if respondent
# made realistic answers. For instance, if student reports of 
# living alonge he/she cannot live with a roommate. If this
# appears adjustments are made.
#++++


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# extract path for loading data based on user selection
if (cohort_prep == "controls_same_outcome") {
  data_load <- paste0("Data/Prep_7/prep_7_control_vars_", treatment_def, 
                      "_", treatment_repl, ".rds")
} else {
  data_load <- paste0("Data/Prep_7/prep_7_control_vars_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
}

# load data
df_plausi <- readRDS(data_load)


# number of respondents, rows, and columns
num_id <- length(unique(df_plausi$id_t))
num_row <- nrow(df_plausi)
num_col <- ncol(df_plausi)

print(paste("Number of respondents:", num_id))
print(paste("Number of rows:", num_row))
print(paste("Number of columns:", num_col))


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### NUMERIC VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# find all numeric columns (except dummy variables)
vars_dummy <- 
  df_plausi %>%  ungroup() %>%
  select_if(~ all(. %in% (0:1) | is.na(.))) %>%
  colnames()

vars_numeric <- 
  df_plausi %>% ungroup() %>% select_if(is.numeric) %>% colnames()

vars_numeric <- vars_numeric[!vars_numeric %in% vars_dummy]

# summary statistics
df_plausi %>% select(all_of(vars_numeric)) %>% select(-id_t) %>% summary()


## YEARS OF EDUCATION ##
#++++++++++++++++++++++#

summary(df_plausi$educ_years_total)
table(round(df_plausi$age), round(df_plausi$educ_years_total))


## ADJUST INTEREST IN READING VARIABLES ##
#++++++++++++++++++++++++++++++++++++++++#

# unrealistic high values: 1200 minutes are 20 hours
# hence, observations with unrealistic high values are dropped
# realistic values are determined to be until 12 hours / 720 minutes
summary(df_plausi$interest_reading_study)
summary(df_plausi$interest_reading_leisure)

sum(df_plausi$interest_reading_study > 720, na.rm = TRUE)
sum(df_plausi$interest_reading_leisure > 720, na.rm = TRUE)

df_plausi <- df_plausi %>%
  mutate_at(
    vars(interest_reading_study, interest_reading_leisure),
    ~ replace(., . > 720, 720)
  )

summary(df_plausi$interest_reading_study)
summary(df_plausi$interest_reading_leisure)


## UNI TIME SPENDING ##
#+++++++++++++++++++++#

# weekly: let's say 24hours - 6 hours sleep = 18 hours * 7
max(df_plausi$uni_time_childcare) < 18*7
max(df_plausi$uni_time_courses) < 18*7
max(df_plausi$uni_time_household) < 18*7
max(df_plausi$uni_time_study) < 18*7
max(df_plausi$uni_time_studyact) < 18*7


# a little bit unrealistic high; but are the true answers of people 
# (no missing value imputation)
test <- df_plausi %>%
  mutate(uni_time_all = uni_time_childcare + uni_time_courses + 
           uni_time_household + uni_time_study + uni_time_studyact) %>%
  select(id_t, starts_with("uni_time"))

summary(test$uni_time_all)


## SPELL LENGTH ##
#++++++++++++++++#

# employment: usually earliest start at 14
summary(df_plausi$educ_years_emp)
table(round(df_plausi$age), round(df_plausi$educ_years_emp))

df_plausi <- df_plausi %>%
  mutate(educ_years_emp = case_when(
    age - educ_years_emp < 14 ~ age - 14, TRUE ~ educ_years_emp
    ))

summary(df_plausi$educ_years_emp)
table(round(df_plausi$age), round(df_plausi$educ_years_emp))


# internship
summary(df_plausi$educ_years_internship)
table(round(df_plausi$age), round(df_plausi$educ_years_internship))


## PARTNER LENGTH ##
#++++++++++++++++++#

# check for negative differences between length and age
summary(df_plausi$partner_current_length)
table(round(df_plausi$age), round(df_plausi$partner_current_length))
sum(df_plausi$age - df_plausi$partner_current_length < 0)

summary(df_plausi$partner_previous_length_total)
table(round(df_plausi$age), round(df_plausi$partner_previous_length_total))
sum(df_plausi$age - df_plausi$partner_previous_length_total < 0)


## AGE OF SIBLING ##
#++++++++++++++++++#

summary(df_plausi$sibling_age_1)
summary(df_plausi$sibling_age_2)

# maximum time difference 40 years
df_plausi <- df_plausi %>% mutate(
  sibling_age_1 = case_when(abs(sibling_age_1 - age) > 40 ~ age + 40, TRUE ~ sibling_age_1),
  sibling_age_2 = case_when(abs(sibling_age_2 - age) > 40 ~ age + 40, TRUE ~ sibling_age_2)
)

summary(df_plausi$sibling_age_1)
summary(df_plausi$sibling_age_2)


#%%%%%%%%%%%%%%%%%%%%%%%#
#### SPECIFIC CHECKS ####
#%%%%%%%%%%%%%%%%%%%%%%%#


## CHILD ##
#+++++++++#

# age of child cannot be larger than respondent's age
sum(df_plausi$age <= df_plausi$child_age_oldest)
sum(df_plausi$age <= df_plausi$child_age_youngest)

# 12 + age of child must be smaller or equal respondent's age
# I deem getting a child earlier at the age of 12 unrealistic
summary(df_plausi$age - df_plausi$child_age_oldest)
summary(df_plausi$age - df_plausi$child_age_youngest)

df_plausi <- df_plausi %>% mutate(
  child_age_oldest = case_when(age - child_age_oldest < 12 ~ age - 12, TRUE ~ child_age_oldest),
  child_age_youngest = case_when(age - child_age_youngest < 12 ~ age - 12, TRUE ~ child_age_youngest)
)


summary(df_plausi$age - df_plausi$child_age_oldest)
summary(df_plausi$age - df_plausi$child_age_youngest)


## PARTNER ##
#+++++++++++#

# having a partner and living with a partner
table(df_plausi$partner_current, df_plausi$partner_living_apart)


## EXTRACURRICULAR ##
#+++++++++++++++++++#

# extracurricular_num should also be zero (no extracurricular activity but sport)
summary(df_plausi$extracurricular_num)


## EMPLOYMENT ##
#++++++++++++++#

# current working hours are zero for individuals who do not work
table(round(df_plausi$emp_current_act_work_hours), df_plausi$emp_current) # due to rounding


# all other variables are zero for individuals who do not work
df_plausi %>% select(starts_with("emp")) %>% filter(emp_current == 0) %>% distinct() %>% pull()



## TREATMENT PERIODS ##
#+++++++++++++++++++++#

# check if treatment periods are enumerated correctly
df_plausi <- df_plausi %>% group_by(id_t) %>% mutate(treatment_period_check = row_number())
sum(df_plausi$treatment_period != df_plausi$treatment_period_check)
df_plausi <- df_plausi %>% select(-treatment_period_check)


## BIRTH COUNTRY ##
#+++++++++++++++++#

# if student is not born in Germany, both birth_country_ger and birth_ger_eastwest_West are zero
table(df_plausi$birth_country_ger, df_plausi$birth_ger_eastwest_west)

df_plausi <- df_plausi %>%
  mutate(birth_ger_eastwest_west = case_when(birth_country_ger == 0 ~ as.double(0), 
                                             TRUE ~ as.double(birth_ger_eastwest_west))) %>%
  mutate(birth_ger_eastwest_west = as.integer(birth_ger_eastwest_west))

table(df_plausi$birth_country_ger, df_plausi$birth_ger_eastwest_west)


## EDUC HIGHEST DEGREE ##
#+++++++++++++++++++++++#

# do students already obtain a degree
table(df_plausi$educ_highest_degree)


## LIVING CONDITIONS ##
#+++++++++++++++++++++#


# if one lives alone, person cannot live with partner
table(df_plausi$living_alone, df_plausi$living_partner)

# if one lives alone one cannot live with a roommate
table(df_plausi$living_alone, df_plausi$living_roommate)


df_plausi <- df_plausi %>% mutate(
  living_partner = as.integer(
    case_when(living_alone == 1 & living_partner == 1 ~ 0, 
              TRUE ~ as.double(living_partner))),
  living_roommate = as.integer(
    case_when(living_alone == 1 & living_roommate == 1 ~ 0, 
              TRUE ~ as.double(living_roommate)))
)


table(df_plausi$living_alone, df_plausi$living_partner)
table(df_plausi$living_alone, df_plausi$living_roommate)


# living alone and household size
table(df_plausi$living_alone, df_plausi$living_hh_size)

df_plausi <- df_plausi %>%
  mutate(
    # if living_alone == 1, then living_hh_size equals 1
    living_hh_size = case_when(
      living_alone == 1 & living_hh_size > 1 ~ as.double(1), TRUE ~ as.double(living_hh_size)
    )
  ) %>% mutate(living_hh_size = as.integer(living_hh_size))

table(df_plausi$living_alone, df_plausi$living_hh_size)


## UNI ECTS ##
#++++++++++++#

# if no ECTS points are awared in degree programm, uni_ects_total
# and uni_ects_current should be zero
df_plausi %>% filter(uni_ects_degree == 0) %>% select(starts_with("uni_ects")) %>% distinct()

# hence, change dummy
df_plausi <- df_plausi %>%
  mutate(uni_ects_degree = case_when(uni_ects_degree == 0 & (uni_ects_current > 0 | uni_ects_total > 0) ~ 1,
                                     TRUE ~ uni_ects_degree))
df_plausi %>% filter(uni_ects_degree == 0) %>% select(starts_with("uni_ects")) %>% distinct()
table(df_plausi$uni_ects_degree)

# hence variable is dropped
df_plausi <- df_plausi %>% select(-uni_ects_degree)

# uni_ects_current is allowed to be larger than uni_ects_total
# because one can obtain more credits than needed
sum(df_plausi$uni_ects_current > df_plausi$uni_ects_total)
df_plausi %>% filter(uni_ects_current > uni_ects_total) %>% select(starts_with("uni_ects"))

summary(df_plausi$uni_ects_total)
summary(df_plausi$uni_ects_current)




#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# check if no respondents, rows, and columns got dropped
num_id == length(unique(df_plausi$id_t))
num_row ==  nrow(df_plausi)
num_col == ncol(df_plausi)

# no missing values
print(paste("Number of missing values:", sum(is.na(df_plausi))))

# no duplicates
print(paste("Number of duplicates:", sum(duplicated(df_plausi))))

# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_save <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, 
                      "_", treatment_repl, ".rds")
} else {
  data_save <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
}

saveRDS(df_plausi, data_save)
