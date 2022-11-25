#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE CHILD DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the child information of each respondent is prepared.
#++++
# 1.) Merge with cohort profile: Only respondents who are included in
# cohort profile are kept. This reduces the sample size from 2,575 respondents
# having children to 2,115 respondents.
#++++
# 2.) Restructure data set: Information from previous children is copied
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
# 3.) Generate variables: total number of children, age of children, and dummy
# variables indicating the gender, if the children is the respondents
# biological child, living always in the same household and going already
# to school. All those variables are created with respect to the interview
# date.
# -> extra dummies for NA categories.
#++++
# --> FINAL DATA FRAME IS A PANEL DATA SET (one row for each respondent-wave combination).


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages; if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("stringr")) install.packages("stringr")
library(stringr)  # for string manipulations

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to transform time data and work with dates

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to work with missing values

if (!require("purrr")) install.packages("purrr")
library(purrr) # for map_dfc() function


# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")




#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#


# load data
data_child <- readRDS("Data/Prep_1/prep_1_child.rds")
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")

# extract number of respondents having children
num_child <- length(unique(data_child$ID_t))




#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#

# DOES NOT MAKE SENSE HERE: One row per respondent and child -> info on this
# child cannot be transferred to other children of respondents.

# # number of missing values
# sum(is.na(data_child))
# colSums(is.na(data_child))
# 
# # fill missing values downwards for ID_t and child
# data_child <- data_child %>% 
#   arrange(ID_t, wave) %>%
#   group_by(ID_t, child_num) %>%
#   fill(names(data_child), .direction = "down")
# 
# colSums(is.na(data_child)) 




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# cohort profile is merged to data child to get the interview date
# (needed for instance to calculate the age of the child)
# inner_join: keep only respondents who are also in cohort profile
data_child <- inner_join(
  data_child, data_cohort_profile %>% select("ID_t", "wave", "interview_date"), 
  by = c("ID_t", "wave")
)


# some respondents are removed because they are not in prepared cohort date
num_child_adj <- length(unique(data_child$ID_t))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RESTRUCTURE DATE SET ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# the respondents report in subsequent interviews not all children again,
# but add "new" children.
# Hence the data set needs to be restructured so that in each wave, information
# on all children is given.
# SEE THIS EXAMPLE:

test <- data_child %>% subset(ID_t %in% c(7001994)) %>% arrange(ID_t, wave)
test
test_merge <- test %>% select(-wave)

test_2 <- 
  # group by ID_t and wave in order to complete missing child information
  # for those waves (in every wave full sequence of children mus be reported)
  # interview date is copied to duplications
  test %>%
  group_by(ID_t, wave) %>%
  complete(child_num = seq(1, max(child_num), by = 1), interview_date) %>%
  # left_join merge data in order to fill missing values
  left_join(
    test_merge, by = c("ID_t", "child_num")
  ) %>%
  # combine .x and .y variables
  split.default(str_remove(names(.), "\\.[x|y]$")) %>%
  map_dfc(~ exec(coalesce, !!!.x)) %>%
  # order columns and rows appropriately
  select(ID_t, wave, interview_date, child_num, everything()) %>%
  arrange(ID_t, wave, child_num)
test_2


# apply idea to full data frame
data_child_merge <- data_child %>% select(-wave)
data_child_2 <- 
  # group by ID_t and wave in order to complete missing child information
  # for those waves (in every wave full sequence of children mus be reported)
  # interview date is copied to duplications
  data_child %>%
  group_by(ID_t, wave) %>%
  complete(child_num = seq(1, max(child_num), by = 1), interview_date) %>%
  # left_join merge data in order to fill missing values
  left_join(
    data_child_merge, by = c("ID_t", "child_num")
  ) %>%
  # combine .x and .y variables
  split.default(str_remove(names(.), "\\.[x|y]$")) %>%
  map_dfc(~ exec(coalesce, !!!.x)) %>%
  # order columns and rows appropriately
  select(ID_t, wave, interview_date, child_num, everything()) %>%
  arrange(ID_t, wave, child_num)



#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


## AGE ##
#+++++++#

# load function to convert month and year variable to date
source("Functions/func_generate_date.R")
# check for missing values in year and month variable
sum(is.na(data_child_2$child_birth_m)) # missing months are replaced by June in function
sum(is.na(data_child_2$child_birth_y))
# apply function to generate the birth date of the children
data_child_2 <- func_generate_date(
  data = data_child_2,
  month = "child_birth_m", year = "child_birth_y",
  varname = "child_birth_date"
) %>%
  # drop month and year variables (-> not needed anymore)
  select(-c(child_birth_m, child_birth_y)) 
# check for missing values in birth date variable
sum(is.na(data_child_2$child_birth_date)) # 223 missings (because year is missing)

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
) %>%
  # replace variables where dummy is NA with 0
  mutate(
    child_age_youngest = replace_na(child_age_youngest, 0),
    child_age_oldest = replace_na(child_age_oldest, 0)
  )

# generate dummy for missing NA (equals 1 if at least for one child respondent has not reported a birth date)
data_child_2 <- left_join(
  data_child_2, 
  data_child_2 %>%
    select(ID_t, wave, child_age) %>%
    group_by(ID_t, wave) %>%
    mutate(child_age_NA = if_else(is.na(child_age), 1, 0)) %>%
    group_by(ID_t, wave) %>%
    mutate(child_age_NA = sum(child_age_NA)) %>%
    distinct() %>%
    mutate(child_age_NA = if_else(child_age_NA > 0, 1, 0)) %>%
    select(-child_age),
  by = c("ID_t", "wave")
) 


# create age categories and dummy for missing age
## https://www.cdc.gov/ncbddd/childdevelopment/positiveparenting/middle.html (Centers for Disease Control and Prevention)
## https://amchp.org/adolescent-health/
# data_child_2 <- data_child_2 %>%
#   mutate(
#     # infants (0-1) & toddlers (1-3)
#     child_age_3 = if_else(child_age <= 3, 1, 0),
#     # preschoolers (3-5)
#     child_age_5 = if_else(child_age <= 5 & child_age > 3, 1, 0),
#     # middle childhood (6-11)
#     child_age_11 = if_else(child_age <= 11 & child_age > 5, 1, 0),
#     # young teens (12-14) & teenagers (15-17)
#     child_age_17 = if_else(child_age <= 17 & child_age > 11, 1, 0),
#     # Young adulthood (18-25)
#     child_age_25 = if_else(child_age <= 25 & child_age > 17, 1, 0),
#     # Adults (above 25)
#     child_age_25plus = if_else(child_age > 25, 1, 0),
#     # Missing age
#     child_age_na = if_else(is.na(child_age), 1, 0)
#   )



## TOTAL NUMBER OF CHILDREN ##
#++++++++++++++++++++++++++++#

# generate the total number of children for each respondent at each interview date
data_child_2 <- data_child_2 %>%
  group_by(ID_t, wave) %>%
  mutate(child_total_num = max(child_num))

summary(data_child_2$child_total_num)
sum(is.na(data_child_2$child_total_num))



## SCHOOL VARIABLE ##
#+++++++++++++++++++#

# child_school variable has many missing values
  ## adjusted according to age of the child
  ## school if child is between 6 and 20 years old
table(data_child_2$child_school)
data_child_2$child_school <- as.character(data_child_2$child_school) # needed for case_when
data_child_2 <- data_child_2 %>%
  mutate(child_school = case_when(is.na(child_school) & child_age >= 6 & child_age <= 20 ~ "yes",
                                  TRUE ~ "no"))
sum(is.na(data_child_2$child_school))


# NUMBER CHILDREN SCHOOL 

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_school == "yes") %>%
    group_by(ID_t, wave) %>%
    count() %>%
    rename(child_school_num = n),
  by = c("ID_t", "wave")
) %>%
  # missing values are replace with zero
  # no dummy for NA needed as child_school has no missing values
  mutate(child_school_num = replace_na(child_school_num, 0)) 



## NUMBER BIOLOGICAL CHILDREN ##
#++++++++++++++++++++++++++++++#

# number of biological children is counted for each ID_t and wave group
sum(is.na(data_child$child_type))

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_type == "Biological child") %>%
    group_by(ID_t, wave) %>%
    count() %>%
    rename(child_biological_num = n),
  by = c("ID_t", "wave")
) %>%
  # missing values are replaced with zero
  mutate(child_biological_num = replace_na(child_biological_num, 0)) 



## NUMBER CHILDREN ALWAYS LIVING IN HOUSEHOLD ##
#++++++++++++++++++++++++++++++++++++++++++++++#

sum(is.na(data_child_2$child_hh)) 

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(child_hh == "yes, always") %>%
    group_by(ID_t, wave) %>%
    count() %>%
    rename(child_living_hh_num = n),
  by = c("ID_t", "wave")
) %>%
  # missing values are replace with zero
  mutate(child_living_hh_num = replace_na(child_living_hh_num, 0)) 

# add dummy indicating if at least for one child in the current wave
# the respondent did not answer
data_child_2 <- left_join(
  data_child_2, 
  data_child_2 %>%
    select(ID_t, wave, child_hh) %>%
    group_by(ID_t, wave) %>%
    mutate(child_living_hh_num_NA = if_else(is.na(child_hh), 1, 0)) %>%
    group_by(ID_t, wave) %>%
    mutate(child_living_hh_num_NA = sum(child_living_hh_num_NA)) %>%
    distinct() %>%
    mutate(child_living_hh_num_NA = if_else(child_living_hh_num_NA > 0, 1, 0)) %>%
    select(-child_hh),
  by = c("ID_t", "wave")
)



## NUMBER MALE CHILDREN ##
#++++++++++++++++++++++++#

sum(is.na(data_child_2$child_gender))

data_child_2 <- left_join(
  data_child_2,
  data_child_2 %>%
    filter(!grepl("female", child_gender)) %>%
    group_by(ID_t, wave) %>%
    count() %>%
    rename(child_male_num = n),
  by = c("ID_t", "wave")
) %>%
  # missing values are replace with zero
  # no dummy for missing (is okay because only 5 missings (not worth an extra variaböl))
  mutate(child_male_num = replace_na(child_male_num, 0)) 



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# keep only variables of interest
data_child_final <- data_child_2 %>%
  select(ID_t, wave, interview_date, child_total_num, 
         child_age_youngest, child_age_oldest, child_age_NA, 
         child_school_num, child_biological_num,
         child_living_hh_num, child_living_hh_num_NA, child_male_num) %>%
  ungroup() %>%
  distinct() %>%
  arrange(ID_t, wave)

# check for remaining missing values (should not be any left)
sum(is.na(data_child_final))

# check for duplicates in ID_t and wave (there should not be any)
data_child_final %>% select(ID_t, wave) %>% duplicated() %>% sum() == 0

# check variables
summary(data_child_final$child_age_youngest)
summary(data_child_final$child_age_oldest)
summary(data_child_final$child_total_num)
for (col_table in 
     data_child_final %>% 
     select(-c(ID_t, wave, interview_date, starts_with("child_age"), child_total_num)) %>% 
     colnames()
) {
  print(col_table)
  print(table(data_child_final[, col_table], useNA = "always"))
}


# number of respondents having children; ensure no observation got lost
length(unique(data_child_final$ID_t)) == num_child_adj

print(paste("Number of respondents having children:",length(unique(data_child_final$ID_t))))
print(paste("Number of rows", nrow(data_child_final)))
print(paste("Number of columns", ncol(data_child_final)))

# save data frame
saveRDS(data_child_final, "Data/Prep_3/prep_3_child.rds")

