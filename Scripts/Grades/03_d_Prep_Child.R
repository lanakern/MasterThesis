#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE CHILD DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the child information of each respondent is prepared.
#++++
# 1.) Merge with cohort profile: Only respondents who are included in
# cohort profile are kept.
# -> Information from previous children is copied
# downwards so that at each interview date, the full information about 
# children is provided. For instance, respondent reports in wave 1 that he/she
# has 1 child. In wave 3, respondents reports to have another child. The date
# would be like:
# wave 1 - info on child 1
# wave 3 - info on child 2
# --> Restructure so that:
# wave 1 - info on child 1
# wave 3 - info on child 1
# wave 3 - info on child 2
# -> This simplifies further computations.
#++++
# 2,) Drop rows:
# -> Missing value in gender, child type or birth year.
# -> Interview date < birth date or for non-biological children wave between
# cohort profile and child data does not coincide.
#++++
# 3.) Generate variables: total number of children, age of children, and dummy
# variables indicating the gender, if the children is the respondents
# biological child, living always in the same household and going already
# to school. All those variables are created with respect to the interview
# date.
# -> Missing values in school variable are replaced: if child between 6 and 20 
# it is assumed that child goes to school
# -> Missing values in household variable are replaced: if child_type == "other child in household", 
# I assume that child lives in household.
# -> Remaining missing values are replaced in file 07
#++++
# --> FINAL DATA FRAME IS A PANEL DATA SET (one row for each respondent-interview_date combination).
#++++


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# cohort data based on selection
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile.rds") %>%
    filter(wave_2 == "CATI") %>%
    dplyr::select(ID_t, wave, interview_date)
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck.rds") %>%
    filter(wave_2 == "CATI") %>%
    dplyr::select(ID_t, wave, interview_date)
}

# child data
data_child <- readRDS("Data/Grades/Prep_1/prep_1_child.rds") 

# extract number of respondents having children
num_child <- length(unique(data_child$ID_t)) # 2,575

# keep only respondents who are in both data sets
id_child <- unique(data_child$ID_t)
id_cohort <- unique(data_cohort_profile$ID_t)
id_child_cohort <- intersect(id_child, id_cohort)

data_child <- data_child %>% subset(ID_t %in% id_child_cohort)
data_cohort_profile <- data_cohort_profile %>% subset(ID_t %in% id_child_cohort)

num_child_adj_1 <- length(unique(data_child$ID_t))
num_child - num_child_adj_1


# check for missing values
colSums(is.na(data_child))


# drop respondents with missing values in child birth year, child type, and gender
data_child <- data_child %>% filter(!is.na(child_gender))
num_child_adj_2 <- length(unique(data_child$ID_t))
num_child_adj_1 - num_child_adj_2

data_child <- data_child %>% filter(!is.na(child_birth_y))
num_child_adj_3 <- length(unique(data_child$ID_t))
num_child_adj_2 - num_child_adj_3


data_child <- data_child %>% filter(!is.na(child_type))
num_child_adj_4 <- length(unique(data_child$ID_t))
num_child_adj_3 - num_child_adj_4


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# cohort profile is merged to data child to get the interview date
# (needed for instance to calculate the age of the child)
# inner_join: keep only respondents who are also in cohort profile
data_child_2 <- inner_join(
  data_child %>% rename(wave_child = wave), data_cohort_profile, 
  by = c("ID_t")
) %>% distinct() %>% arrange(ID_t, interview_date)
length(unique(data_child_2$ID_t))

# child information is duplicated for each interview
data_child %>% subset(ID_t == 7002059)
data_cohort_profile %>% subset(ID_t == 7002059)
data_child_2 %>% subset(ID_t == 7002059)

data_child %>% subset(ID_t == 7002406)
data_cohort_profile %>% subset(ID_t == 7002406)
data_child_2 %>% subset(ID_t == 7002406)

data_child %>% subset(ID_t == 7001994)
data_cohort_profile %>% subset(ID_t == 7001994)
data_child_2 %>% subset(ID_t == 7001994)

# keep only observations for which the child's birth date is larger than the
# interview date (otherwise child is not born)
  ## generate birth date
source("Functions/func_generate_date.R")
sum(is.na(data_child_2$child_birth_m)) # missing months are replaced by June in function
sum(is.na(data_child_2$child_birth_y))
  # apply function to generate the birth date of the childen
data_child_2 <- func_generate_date(
  data = data_child_2,
  month = "child_birth_m", year = "child_birth_y",
  varname = "child_birth_date"
) %>%
  # drop month and year variables (-> not needed anymore)
  dplyr::select(-c(child_birth_m, child_birth_y)) 
  # check for missing values in birth date variable
sum(is.na(data_child_2$child_birth_date)) # 103 missings (because year is missing)
  ## subset
data_child_2 <- data_child_2 %>% filter(interview_date >= child_birth_date)
num_child_adj_5 <- length(unique(data_child_2$ID_t))
num_child_adj_4 - num_child_adj_5

# for every child type except "Biological child", keep only rows for which wave
# matches (biological child exists from birth date onwards, but other children
# may just be moved in from partner)
data_child_2 <- data_child_2 %>%
  mutate(drop = case_when(
    child_type != "Biological child" & wave_child != wave ~ 1,
    TRUE ~ 0)) %>%
  filter(drop == 0) %>% dplyr::select(-drop)
num_child_adj_6 <- length(unique(data_child_2$ID_t))
num_child_adj_5 - num_child_adj_6


# adjust child_num indicator
data_child_2 %>% subset(ID_t == 7011560) %>% dplyr::select(ID_t, interview_date, child_birth_date, child_num)

data_child_2 <- data_child_2 %>%
  group_by(ID_t, interview_date) %>%
  mutate(child_num = row_number()) %>% 
  distinct()

data_child_2 %>% subset(ID_t == 7011560) %>% dplyr::select(ID_t, interview_date, child_birth_date, child_num)


#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


## AGE ##
#+++++++#

# create current age of child in years: age = interview_date - birth_date
data_child_2 <- data_child_2 %>%
  mutate(
    child_age = as.numeric(difftime(interview_date, child_birth_date, units = "weeks")) / 52.5
  )
summary(data_child_2$child_age) # respondents with old children are also old at the start of studying

# in the analysis, the age of the youngest and oldest child is used
data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    group_by(ID_t, wave) %>%
    summarise(
      child_age_youngest = min(child_age),
      child_age_oldest = max(child_age)
    ),
  by = c("ID_t", "wave")
) %>% distinct() 


# create age categories and dummy for missing age
## https://www.cdc.gov/ncbddd/childdevelopment/positiveparenting/middle.html (Centers for Disease Control and Prevention)
## https://amchp.org/adolescent-health/
data_child_2 <- data_child_2 %>%
  mutate(
    child_age_toddler = ifelse(child_age <= 3, 1, 0),
    child_age_preschool = ifelse(child_age > 3 & child_age <= 5, 1, 0),
    child_age_school = ifelse(child_age > 5 & child_age <= 11, 1, 0),
    child_age_teen = ifelse(child_age > 11 & child_age <= 17, 1, 0),
  ) 
  ## numeric
data_child_2 <- data_child_2 %>%
  left_join(
    data_child_2 %>% 
      group_by(ID_t, interview_date) %>% 
      summarise(
        child_age_toddler_num = sum(child_age_toddler), child_age_preschool_num = sum(child_age_preschool),
        child_age_school_num = sum(child_age_school), child_age_teen_num = sum(child_age_teen)
      ) %>% ungroup(),
    by = c("ID_t", "interview_date")
  ) %>%
  dplyr::select(-c(child_age_toddler, child_age_preschool, child_age_school, child_age_teen)) %>%
  distinct()



## TOTAL NUMBER OF CHILDREN ##
#++++++++++++++++++++++++++++#

# generate the total number of children for each respondent at each interview date
data_child_2 <- data_child_2 %>%
  group_by(ID_t, interview_date) %>%
  mutate(child_total_num = max(child_num))

summary(data_child_2$child_total_num)
sum(is.na(data_child_2$child_total_num))



## SCHOOL VARIABLE ##
#+++++++++++++++++++#

# child_school variable has many missing values
  ## adjusted according to age of the child
  ## school if child is between 6 and 20 years old
table(data_child_2$child_school, useNA = "always")
data_child_2$child_school <- as.character(data_child_2$child_school) # needed for case_when
data_child_2 <- data_child_2 %>%
  mutate(child_school = case_when(child_school == 1 ~ "yes", child_school == 0 ~ "no", TRUE ~ as.character(NA))) %>%
  mutate(child_school = case_when(is.na(child_school) & child_age >= 6 & child_age <= 20 ~ "yes",
                                  !is.na(child_school) ~ child_school,
                                  TRUE ~ "no"))
table(data_child_2$child_school, useNA = "always")


# NUMBER CHILDREN SCHOOL 
data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_school == "yes") %>%
    group_by(ID_t, interview_date) %>%
    count() %>%
    rename(child_school_num = n),
  by = c("ID_t", "interview_date")
) %>%
  # missing values are replace with zero
  # no dummy for NA needed as child_school has no missing values
  mutate(child_school_num = replace_na(child_school_num, 0)) %>%
  distinct()

sum(is.na(data_child_2$child_school_num))


## NUMBER BIOLOGICAL CHILDREN ##
#++++++++++++++++++++++++++++++#

# number of biological children is counted for each ID_t and wave group
sum(is.na(data_child$child_type))

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_type == "Biological child") %>%
    group_by(ID_t, interview_date) %>%
    count() %>%
    rename(child_biological_num = n),
  by = c("ID_t", "interview_date")
) %>%
  # missing values are replaced with zero
  mutate(child_biological_num = replace_na(child_biological_num, 0)) %>%
  distinct()


sum(is.na(data_child$child_biological_num))



## NUMBER CHILDREN ALWAYS LIVING IN HOUSEHOLD ##
#++++++++++++++++++++++++++++++++++++++++++++++#

sum(is.na(data_child_2$child_hh)) 

# replace child_hh with "yes, always" if child_hh is NA and child_type == "other child in household"
data_child_2 <- data_child_2 %>%
  mutate(child_hh = case_when(
    is.na(child_hh) & child_type == "other child in household" ~ "yes, always",
    TRUE ~ child_hh
  ))


sum(is.na(data_child_2$child_hh)) 

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_hh == "yes, always") %>%
    group_by(ID_t, interview_date) %>%
    count() %>%
    rename(child_living_hh_num = n),
  by = c("ID_t", "interview_date")
) %>%
  # missing values are replace with zero
  mutate(child_living_hh_num = replace_na(child_living_hh_num, 0)) %>%
  distinct()

sum(is.na(data_child_2$child_living_hh_num))

# add dummy indicating if at least for one child in the current wave
# the respondent did not answer
# data_child_2 <- left_join(
#   data_child_2, 
#   data_child_2 %>%
#     select(ID_t, interview_date, child_hh) %>%
#     group_by(ID_t, interview_date) %>%
#     mutate(child_living_hh_num_NA = if_else(is.na(child_hh), 1, 0)) %>%
#     group_by(ID_t, interview_date) %>%
#     mutate(child_living_hh_num_NA = sum(child_living_hh_num_NA)) %>%
#     distinct() %>%
#     mutate(child_living_hh_num_NA = if_else(child_living_hh_num_NA > 0, 1, 0)) %>%
#     select(-child_hh) %>%
#     ungroup(),
#   by = c("ID_t", "interview_date")
# ) %>% distinct()
# 
# table(data_child_2$child_living_hh_num_NA, useNA = "always")



## NUMBER MALE CHILDREN ##
#++++++++++++++++++++++++#

sum(is.na(data_child_2$child_gender))

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    dplyr::select(ID_t, interview_date, child_gender) %>%
    filter(child_gender == "[m] male") %>%
    group_by(ID_t, interview_date) %>%
    count() %>%
    rename(child_male_num = n),
  by = c("ID_t", "interview_date")
) %>%
  # missing values are replace with zero
  mutate(child_male_num = replace_na(child_male_num, 0)) 


sum(is.na(data_child_2$child_male_num))



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

data_child_final <- data_child_2 

# generate sibling dummy
data_child_final <- data_child_final %>% mutate(child = 1)

# keep only variables of interest
data_child_final <- data_child_final %>%
  dplyr::select(ID_t, interview_date, child, child_total_num, 
                child_age_youngest, child_age_oldest, child_age_toddler_num, 
                child_age_preschool_num, child_age_school_num, child_age_teen_num, 
                child_school_num, child_biological_num,
                child_living_hh_num, child_male_num) %>%
  ungroup() %>%
  distinct() %>%
  arrange(ID_t, interview_date)

# check for remaining missing values (should not be any left)
colSums(is.na(data_child_final))
sum(is.na(data_child_final))

# check for duplicates in ID_t and wave (there should not be any)
sum(duplicated(data_child_final))
data_child_final %>% dplyr::select(ID_t, interview_date) %>% duplicated() %>% sum() == 0

# check variables
summary(data_child_final$child_age_youngest)
summary(data_child_final$child_age_oldest)
summary(data_child_final$child_total_num)
for (col_table in 
     data_child_final %>% 
     dplyr::select(-c(ID_t, interview_date, starts_with("child_age"), child_total_num)) %>% 
     colnames()
) {
  print(col_table)
  print(table(data_child_final[, col_table], useNA = "always"))
}


# number of respondents having children; ensure no observation got lost
print(paste("Number of respondents having children before data preparation:", num_child))
print(paste("Number of respondents having children and are in cohort profile:", num_child_adj_1))
print(paste("Number of respondents after dropping rows with missings in gender, child type and birth year:", num_child_adj_4))
print(paste("Number of respondents after dropping rows for which interview date is smaller than birth date:", num_child_adj_5))
print(paste("Number of respondents after dropping rows with not matching wave for non-biological children:", num_child_adj_6))
    

print(paste("Number of respondents having children:",length(unique(data_child_final$ID_t))))
print(paste("Number of rows", nrow(data_child_final)))
print(paste("Number of columns", ncol(data_child_final)))


# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_child_save <- "Data/Grades/Prep_3/prep_3_child.rds"
} else if (cohort_prep == "controls_bef_outcome") {
  data_child_save <- "Data/Grades/Prep_3/prep_3_child_robustcheck.rds"
}

saveRDS(data_child_final, data_child_save)

