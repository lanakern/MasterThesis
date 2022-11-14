#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE PARTNER DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the information about current and previous partners of each
# respondent is prepared. Only respondents who had at least once partner
# in their life are included in this data set.
#++++
# 1.) Partner and Cohort Profile data are merged via an inner join; this
# reduces the sample size from 13,411 respondents to 10,870 respondents.
# This is because in cohort data are not all respondents anymore (see
# file 02_b for further details why respondents are dropped)
#++++
# 2.) Variables are generated. In particular, the start and end date (if
# it exists), number of previous partners, length of previous and current
# partnership
#++++
# 3.) Detail variables such as education background are only kept for the 
# current partnership. 
#++++
# 4.) Handling missing values: If a respondent does not have a current partnership,
# those variables are set to 0; to re-identify individuals without a current partner
# a dummy is generated which equals 1 if a respondent does not have a partner
# at the interview day. For age and education year there are also missing values
# for the current partnership; those are set to zero and a dummy variable
# indicating that those were NA is generated. 
#++++
# -> note: "current" and "previous" refers to the respective interview date
# --> FINAL DATA FRAME IS A PANEL DATA SET (one row for each respondent-wave combination).


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages; if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to transform time data and work with dates

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to work with missing values

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")




#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#


# load data
data_partner <- readRDS("Data/Prep_1/prep_1_partner.rds") 
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")

# extract number of respondents having a partner
id_num_partner <- length(unique(data_partner$ID_t)) # 13,411

# number of respondents in cohort profile
length(unique(data_cohort_profile$ID_t)) # 12,670

# merge interview date from cohort profile
  ## inner_join to only keep respondents who are also in cohort profile and
  ## only have rows where wave information is in both data frames
data_partner <- data_partner %>% inner_join(
  data_cohort_profile %>% select(ID_t, wave, interview_date),
  by = c("ID_t", "wave")
)

# adjust number of partners
id_num_partner_adj <- length(unique(data_partner$ID_t)) # 10,870
print(paste("After merging partner with cohort profile, the sample size includes",
            id_num_partner_adj, "respondents, i.e.,", id_num_partner - id_num_partner_adj , 
            "respondents are dropped"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


# start and end date of partnership
  ## load function
source("Functions/func_generate_date.R")
  ## calculate start date
data_partner <- func_generate_date(
  data = data_partner,
  month = "partner_start_m", year = "partner_start_y",
  varname = "start_date"
)
  ## calculate end date
data_partner <- func_generate_date(
  data = data_partner,
  month = "partner_end_m", year = "partner_end_y",
  varname = "end_date"
)
  ## drop other date variables
data_partner <- data_partner %>%
  select(-starts_with("partner_start"), -starts_with("partner_end"))


# number of partners until each interview date
  ## create indicator for having a partner (always one)
data_partner$num <- 1
  ## create cumulated sum of partners
  ## do this grouped by ID and keep wave
data_partner_num <- data_partner %>% 
  arrange(ID_t, wave) %>% 
  group_by(ID_t) %>% 
  summarise(wave = wave, partner_num_total = cumsum(num))
  ## in case of duplicate, keep highest number
  ## this is for example for the first wave where respondent is asked about partners
  ## for the first time -> he/she can report multiple partners from the past
data_partner_num <-  data_partner_num %>% 
  group_by(ID_t, wave) %>% 
  slice(which.max(partner_num_total)) # keep row with highest number

data_partner <- left_join(
  data_partner, data_partner_num,
  by = c("ID_t", "wave")
)


# calculate length of current and previous partnership
  ## identify current and finished partnership: finished partnership if
  ## interview date is larger than end of partnership; current partnership if
  ## start date of partnership is smaller than interview date
data_partner <- data_partner %>%
  mutate(
    partner_finished = if_else(interview_date > end_date, 1, 0),
    partner_current = if_else(start_date < interview_date & (interview_date < end_date | is.na(end_date)), 1, 0)
    )
  ## total length:
  ## create new variable determining the end date of partnership: for current this is the interview
data_partner <- data_partner %>%
  mutate(
    end_date_adj = if_else(partner_current == 1, interview_date, end_date)
  )
  ## calculate length in months
data_partner <- data_partner %>%
  mutate(
    partner_length = as.numeric(difftime(end_date_adj, start_date, units = c("days"))) / 30
  )
  ## there are negative values which result due to  data pre-processing mistakes
  ## start_date is before end date
data_partner %>% 
  filter(partner_length < 0) %>% 
  select(ID_t, wave, interview_date, start_date, end_date, end_date_adj, partner_length, partner_finished, partner_current)
  ## in this case, dates are switched:
data_partner <- rbind(
  data_partner %>% filter(partner_length >= 0 | is.na(partner_length)),
  data_partner %>%
    filter(partner_length < 0) %>%
    mutate(
      partner_length = as.numeric(difftime(start_date, end_date_adj, units = c("days"))) / 30
    )
) %>% arrange(ID_t, wave)
  ## check if now all values are positive
summary(data_partner$partner_length)
  ## length of previous partnerships: cumulative sum of previous length
data_partner <- left_join(
  data_partner, 
  data_partner %>%
    filter(partner_finished == 1) %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    summarise(wave = wave, partner_length_previous = cumsum(partner_length)),
  by = c("ID_t", "wave")
) %>%
  # replace missings by 0
  replace_na(list(partner_length_previous = 0))
  ## length of current partnerships: just calculated length
data_partner <- data_partner %>% 
  mutate(partner_length_current = if_else(partner_current == 1, partner_length, 0))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Current Partner Information ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# only keep observations for current partner
data_partner_current <- data_partner %>%
  filter(partner_current == 1)

data_partner_current <- data_partner_current %>%
  mutate(
    # gender (NA are assumed as male)
    partner_male = if_else(!grepl("female", partner_gender), 1, 0),
    # indicator if partner's highest degree is university entrance qualification
    partner_school_degree_highest = if_else(grepl("Abitur|Fachhochschulreife", partner_school_degree), 1, 0),
    # indicator if partner has an university degree
    partner_uni_degree_highest = if_else(
      grepl("Diplom|M.A.|Magister|state examination|Bachelor|university|college|higher education institution|doctorate|university of applied sciences", 
            partner_uni_degree), 1, 0),
    # indicator if partner is currently also studying
    partner_study_current = if_else(!is.na(partner_uni), 1, 0), 
    # indicator if partner is employed
    partner_emp_current = if_else(!is.na(partner_emp), 1, 0), 
    # age of partner (can only approximately calculated using year of birth)
    # is missing for partners without birth date
    partner_age = as.numeric(difftime(interview_date, mdy(paste(1, 1, ",", partner_birth_year)), units = "weeks")) / 52.5,
    # indicator for seeing partner (almost) every day: täglich + fast täglich
    partner_daily = if_else(grepl("daily", partner_freq), 1, 0),
    # indicator for seeing partner once a month or less
    partner_monthly = if_else(partner_freq %in% c("monthly", "less frequently"), 1, 0)
  )
length(unique(data_partner_current$ID_t))

# only keep observations for individuals who do not have a current partner
# but had previous ones
id_no_current <- setdiff(unique(data_partner$ID_t), unique(data_partner_current$ID_t))
  
data_partner_no_current <- data_partner %>% 
  subset(ID_t %in% id_no_current) %>%
  select(ID_t, wave, interview_date, partner_num_total, partner_length_previous) %>%
  # create dummy for not having a partner
  mutate(partner_current_no = 1)

length(unique(data_partner_no_current$ID_t))


# add information together
data_partner_final <- full_join(
  data_partner_current, data_partner_no_current
)
length(unique(data_partner_final$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#

# number of missing values per column
colSums(is.na(data_partner_final))

# partner_current_nois 0 if NA, i.e., respondent has a partner
data_partner_final <- data_partner_final %>%
  mutate(partner_current_no = replace_na(partner_current_no, 0))

# all current partner variables are 0 if respondent does not have a partner
var_replace_vector <- 
  c("partner_length_current", "partner_age", "partner_male", "partner_educ_years",
    "partner_school_degree_highest", "partner_uni_degree_highest", "partner_study_current",
    "partner_emp_current", "partner_daily", "partner_monthly")

for (var_replace in var_replace_vector) {
  data_partner_final <- data_partner_final %>%
    # only replace missing value with 0 if respondent does not have a 
    # partner at the interview date
    mutate(
      {{var_replace}} := ifelse(
        partner_current_no == 1, 0, !!! rlang::syms(var_replace)
        )
      )
}

# number of missing values per column
colSums(is.na(data_partner_final))

# remaining missing values are due to missing responses
# for those variables NA dummies are generated
# the NAs in original variable are set to zero
names(colSums(is.na(data_partner_final))[colSums(is.na(data_partner_final)) != 0])

data_partner_final <- data_partner_final %>%
  mutate(
    partner_age_na = ifelse(is.na(partner_age), 1, 0),
    partner_educ_years_na = ifelse(is.na(partner_educ_years), 1, 0)
    ) %>%
  mutate(
    partner_age = replace_na(partner_age, 0), 
    partner_educ_years = replace_na(partner_educ_years, 0)
  ) 



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# keep only variables needed
data_partner_final <- data_partner_final %>%
  select(ID_t, wave, interview_date,
         partner_num_total, starts_with("partner_length_"), partner_age, partner_age_na, 
         partner_male, partner_educ_years, partner_educ_years_na, partner_school_degree_highest,
         partner_uni_degree_highest, partner_study_current, partner_emp_current,
         partner_daily, partner_monthly, partner_current_no)

# now no missing values should be left
sum(is.na(data_partner_final))

# check
summary(data_partner_final$partner_num_total)
summary(data_partner_final$partner_length_previous)
summary(data_partner_final$partner_length_current)
summary(data_partner_final$partner_age)
summary(data_partner_final$partner_educ_years)

table(data_partner_final$partner_male, useNA = "always")
table(data_partner_final$partner_school_degree_highest, useNA = "always")
table(data_partner_final$partner_uni_degree_highest, useNA = "always")
table(data_partner_final$partner_study_current, useNA = "always")
table(data_partner_final$partner_emp_current, useNA = "always")
table(data_partner_final$partner_daily, useNA = "always")
table(data_partner_final$partner_monthly, useNA = "always")


# number of respondents
print(paste("The final data frame includes", length(unique(data_partner_final$ID_t)),
            "respondents."))
# number of rows and columns
print(paste("Number of rows", nrow(data_partner_final)))
print(paste("Number of columns", ncol(data_partner_final)))

# save data frame
length(unique(data_partner_final$ID_t))
saveRDS(data_partner_final, "Data/Prep_3/prep_3_partner.rds")





