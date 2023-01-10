#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE PARTNER DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the information about current and previous partners of each
# respondent is prepared. Only respondents who had at least one partner
# in their life are included in this data set.
#++++
# 1.) Partner and Cohort Profile data are merged via an inner join: only 
# respondents who are in both data sets are kept.
#++++
# 2.) Variables are generated. In particular, the start and end date (if
# it exists), number of previous partners, length of previous and current
# partnership
#++++
# 3.) Detailed variables such as education background are only kept for the 
# current partnership. For individuals with no current partner those are 
# set to 0. 
#++++
# 4.) Handling missing values: All missing values are set to zero and a dummy variable
# indicating that those were NA is generated. 
#++++
# --> FINAL DATA FRAME IS A PANEL DATA SET (one row for each respondent-wave combination).


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # install packages; if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate)  # to transform time data and work with dates
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to work with missing values
# 
# # set language for dates and times to German, since the NEPS month names
# # are written in German; otherwise date/time functions are not working
# # for German language
# Sys.setlocale("LC_TIME", "German")
# 
# # define inputs: selection on cohort preparation
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"


#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#


# load data
data_partner_raw <- readRDS("Data/Prep_1/prep_1_partner.rds") 

# cohort data based on selection
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds") %>%
    filter(wave_2 == "CATI") %>%
    select(ID_t, wave, interview_date)
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile_robustcheck.rds") %>%
    filter(wave_2 == "CATI") %>%
    select(ID_t, wave, interview_date)
}


# extract number of respondents having a partner
id_num_partner <- length(unique(data_partner_raw$ID_t)) # 13,411

# number of respondents in cohort profile
id_num_cohort <- length(unique(data_cohort_profile$ID_t)) 

# keep only respondents who are in both data sets
id_partner <- unique(data_partner_raw$ID_t)
id_cohort <- unique(data_cohort_profile$ID_t)
id_partner_cohort <- intersect(id_partner, id_cohort)
id_num_partner_adj_1 <- length(id_partner_cohort)

data_partner <- data_partner_raw
data_partner <- data_partner %>% subset(ID_t %in% id_partner_cohort)

# merge interview date from cohort profile
  ## inner_join to only keep respondents who are also in cohort profile and
  ## only have rows where wave information is in both data frames
data_partner <- data_partner %>% rename(wave_partner = wave) %>% 
  inner_join(data_cohort_profile %>% select(ID_t, wave, interview_date),
             by = c("ID_t"))
length(unique(data_partner$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


# start and end date of partnership
  ## missing values
colSums(is.na(data_partner %>% select(starts_with("partner_start"), starts_with("partner_end"))))
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
sum(is.na(data_partner$start_date))
sum(is.na(data_partner$end_date))

# correct: end_date is smaller than start_date
sum(data_partner$end_date < data_partner$start_date, na.rm = TRUE)
data_partner %>% filter(end_date < start_date)
  ## set end_date NA
data_partner <- data_partner %>%
  mutate(end_date = case_when(end_date < start_date ~ as.Date(NA), TRUE ~ end_date))

# after merge I have duplicates because I only merged by the ID
# hence, I only keep rows with a start_date (of partnership) smaller
# than interview_date
data_partner %>% 
  select(ID_t, start_date, end_date, interview_date, wave, wave_partner) %>% 
  arrange(ID_t, interview_date) %>%
  subset(ID_t == 7001969)
data_partner <- data_partner %>% filter(start_date <= interview_date)
id_num_partner_adj_2 <- length(unique(data_partner$ID_t))


# create indicator for current partnership
data_partner %>% 
  select(ID_t, start_date, end_date, interview_date, wave, wave_partner) %>% 
  arrange(ID_t, interview_date) %>%
  subset(ID_t == 7002012) # 7002018

data_partner <- data_partner %>%
  arrange(ID_t, interview_date) %>%
  group_by(ID_t, interview_date) %>%
  mutate(current_date = max(start_date)) %>%
  mutate(partner_current = ifelse(
    current_date == start_date & (end_date > interview_date | is.na(end_date)), 1, 0)
    ) 

# number of partners until each interview date (including current partnership)
df_partner_num <- data_partner %>% 
  arrange(ID_t, interview_date) %>% 
  group_by(ID_t, interview_date) %>% 
  count() %>% rename(partner_num_total = n)
  ## join result
data_partner <- left_join(data_partner, df_partner_num, by = c("ID_t", "interview_date"))


# enumerate number of partners
data_partner <- 
  data_partner %>% 
  arrange(ID_t, interview_date, start_date) %>% 
  group_by(ID_t, interview_date) %>% 
  mutate(partner_num = row_number())

# calculate length of current and previous partnership (in years)
  ## to do so, end_date needs to be adjusted: if partner_current = 0 and end_date is missing
  ## start_date of subsequent partnership is used as end_date
data_partner <- data_partner %>%
  arrange(ID_t, interview_date, start_date) %>%
  mutate(start_date_lead = lead(start_date)) %>%
  mutate(end_date_adj = case_when(partner_current == 0 & is.na(end_date) ~ start_date_lead, 
                                  TRUE ~ end_date)) %>%
  select(-start_date_lead)
  ## if partner_current = 1 end_date is interview_date (only assumed for the calculation)
data_partner <- data_partner %>%
  arrange(ID_t, interview_date, start_date) %>%
  mutate(end_date_adj = case_when(partner_current == 1 & is.na(end_date) ~ interview_date, 
                                  TRUE ~ end_date_adj))
sum(data_partner$end_date_adj < data_partner$start_date, na.rm = TRUE)
sum(is.na(data_partner$end_date_adj))
  ## length of current partnership
data_partner <- data_partner %>%
  mutate(partner_current_length = case_when(
    partner_current == 1 ~ as.numeric(difftime(end_date_adj, start_date, units = "weeks")) / 52.5,
    TRUE ~ as.numeric(NA)
    ))
summary(data_partner$partner_current_length) # no negative values

  ## length of previous partnership: 1.) calculate length 2.) cumsum
data_partner <- data_partner %>%
  mutate(partner_previous_length = case_when(
    partner_current == 0 ~ as.numeric(difftime(end_date_adj, start_date, units = "weeks")) / 52.5,
    TRUE ~ as.numeric(NA)
  )) %>%
  group_by(ID_t, interview_date) %>%
  mutate(partner_previous_length_total = cumsum(replace_na(partner_previous_length, 0))) 

summary(data_partner$partner_previous_length) # no negative values

# ungroup
data_partner <- data_partner %>% ungroup()

# EXAMPLE
data_partner %>% 
  select(ID_t, start_date, end_date, interview_date, end_date_adj,  partner_num, partner_num_total, 
         partner_current, partner_current_length, partner_previous_length, partner_previous_length_total) %>%
  subset(ID_t %in% c(7002147, 7002012, 7001970)) %>% 
  arrange(ID_t, interview_date)

# checks: 
  ## no missings in length
sum(is.na(data_partner %>% filter(partner_current == 1) %>% select(partner_current_length)))
sum(is.na(data_partner$partner_previous_length_total))
  ## partner_num equals partner_num_total for partner_current == 1
check <- data_partner %>% filter(partner_current == 1) %>% mutate(
  wrong_1 = ifelse(partner_num != partner_num_total, 1, 0)
) %>% filter(wrong_1 == 1)
sum(check$partner_num != check$partner_num_total)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Current Partner Information ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# only keep observations for current partner
data_partner_current <- data_partner %>%
  filter(partner_current == 1) %>%
  select(-c(partner_num_total, partner_number, partner_previous_length)) %>%
  group_by(ID_t, interview_date) %>%
  filter(partner_num == max(partner_num)) %>%
  ungroup()

id_num_current_partner <- length(unique(data_partner_current$ID_t))

# check missing values of relevant variables
colSums(is.na(
  data_partner_current %>% select(partner_gender, partner_school_degree, partner_uni_degree, 
                                  partner_uni, partner_emp, partner_birth_year, partner_freq)
  ))

data_partner_current <- data_partner_current %>%
  mutate(
    # gender (NA are assumed as male as there are not a lot)
    partner_male = if_else(!grepl("female", partner_gender), 1, 0),
    # indicator if partner's highest degree is university entrance qualification
    partner_school_degree_highest = if_else(grepl("Abitur|Fachhochschulreife", partner_school_degree), 1, 0),
    # indicator if partner has an university degree
    partner_uni_degree = case_when(
      grepl("Diplom|M.A.|Magister|state examination|Bachelor|university|college|higher education institution|doctorate|university of applied sciences", 
            partner_uni_degree) ~ 1, is.na(partner_uni_degree) ~ as.numeric(NA), TRUE ~ 0),
    # indicator if partner is currently also studying
    partner_study_current = case_when(
      !is.na(partner_uni) ~ 1, is.na(partner_uni) & !is.na(partner_emp) ~ 0,
      TRUE ~ as.numeric(NA)), 
    # indicator if partner is employed
    partner_emp_current = case_when(
      partner_emp %in% c("part-time employed", "primarily working") ~ 1, 
      partner_emp == "unemployed" ~ 0, TRUE ~ as.numeric(NA)), 
    # age of partner (can only approximately calculated using year of birth)
    # is missing for partners without birth date
    partner_age = as.numeric(difftime(interview_date, mdy(paste(1, 1, ",", partner_birth_year)), units = "weeks")) / 52.5,
    # indicator for partner frequency seeing each other
    partner_freq_daily = if_else(grepl("daily", partner_freq), 1, 0), # seeing partner (almost) every day: daily + almost daily
    partner_freq_monthly = if_else(partner_freq %in% c("monthly", "less frequently"), 1, 0), # seeing partner once a month or less
    partner_freq_NA = if_else(is.na(partner_freq), 1, 0), # NA
    # partner_living_apart
    partner_living_apart = if_else(partner_living_apart == 1, 1, 0),
    # partner_living_ger
    partner_living_ger = case_when(partner_living_ger == "yes, partner was/is living in Germany" ~ 1,
                                   is.na(partner_living_ger) ~ as.numeric(NA), TRUE ~ 0),
    # partner_educ_years
    partner_educ_years_12 = if_else(partner_educ_years %in% c(9:12), 1, 0),
    partner_educ_years_15 = if_else(partner_educ_years %in% c(13:15), 1, 0),
    partner_educ_years_18 = if_else(partner_educ_years %in% c(16:18), 1, 0),
    partner_educ_years_NA = if_else(is.na(partner_educ_years), 1, 0),
    # partner: migration
    partner_migration = case_when(
      partner_birth_country != "in Germany/within the current borders of Germany" | partner_ger == 0 ~ 1,
      is.na(partner_birth_country) & is.na(partner_ger) ~ as.numeric(NA), TRUE ~ 0
    )
  )

# keep only variables of interest
data_partner_current <- data_partner_current %>%
  select(ID_t, interview_date, partner_current, partner_num, partner_current_length, partner_previous_length_total, 
         partner_male, partner_age, partner_migration, partner_living_ger, 
         partner_school_degree_highest, partner_uni_degree, starts_with("partner_educ"), 
         partner_study_current, partner_emp_current,
         partner_living_apart, partner_freq_daily, partner_freq_monthly, partner_freq_NA)


# NO CURRENT PARTNER #
#++++++++++++++++++++#

# Add observations for individuals who do not have a current partner
# but had previous ones

# Subset
id_no_current <- setdiff(unique(data_partner$ID_t), unique(data_partner_current$ID_t))

data_partner_no_current <- data_partner %>% 
  subset(ID_t %in% id_no_current) %>%
  select(ID_t, interview_date, partner_current, partner_num, partner_previous_length_total) %>%
  distinct()

# In case of duplicates keep observation with higher partner_num
data_partner_no_current <- data_partner_no_current %>%
  group_by(ID_t, interview_date) %>%
  filter(partner_num == max(partner_num)) %>%
  ungroup()

unique(data_partner_no_current$partner_current)

length(unique(data_partner_no_current$ID_t))

# add missing columns from data_partner_current
add_cols <- colnames(data_partner_current)[!colnames(data_partner_current) %in% colnames(data_partner_no_current)]
data_partner_no_current <- 
  cbind(data_partner_no_current, setNames(lapply(add_cols, function(x) x = 0), add_cols)) %>%
  # create one dummy for partner
  mutate(partner = 1) %>% 
  select(ID_t, interview_date, partner_current, partner_num, 
         partner_current_length, partner_previous_length_total, 
         partner_male, partner_age, partner_migration, partner_living_ger, 
         partner_school_degree_highest, partner_uni_degree, starts_with("partner_educ"), 
         partner_study_current, partner_emp_current,
         partner_living_apart, partner_freq_daily, partner_freq_monthly, partner_freq_NA)

# add information together
data_partner_final <- rbind(data_partner_current, data_partner_no_current)
length(unique(data_partner_final$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#

# number of missing values per column
colSums(is.na(data_partner_final))

# remaining missing values are due to missing responses
# for those variables NA dummies are generated
# the NAs in original variable are set to zero
colnames_any_missing <- colSums(is.na(data_partner_final))
colnames_any_missing <- names(colnames_any_missing[colnames_any_missing > 0])
source("Functions/func_generate_NA_dummies.R")
for (col_sel in colnames_any_missing) {
  data_partner_final <- func_generate_NA_dummies(data_partner_final, col_sel)
}
data_partner_final <- data_partner_final %>% replace(is.na(.), 0)

sum(is.na(data_partner_final))


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# ungroup 
data_partner_final <- data_partner_final %>% ungroup()

# no duplicates
sum(duplicated(data_partner_final))
sum(duplicated(data_partner_final %>% select(ID_t, interview_date)))

# now no missing values should be left
sum(is.na(data_partner_final))

# check
summary(data_partner_final$partner_num)
summary(data_partner_final$partner_previous_length_total)
summary(data_partner_final$partner_current_length)
summary(data_partner_final$partner_age)
summary(data_partner_final$partner_educ_years)

table(data_partner_final$partner_male, useNA = "always")
table(data_partner_final$partner_school_degree_highest, useNA = "always")
table(data_partner_final$partner_uni_degree, useNA = "always")
table(data_partner_final$partner_study_current, useNA = "always")
table(data_partner_final$partner_emp_current, useNA = "always")
table(data_partner_final$partner_freq_daily, useNA = "always")
table(data_partner_final$partner_freq_monthly, useNA = "always")


# number of respondents
print(paste("Number of respondents before data preparation:", id_num_partner))
print(paste("Number of respondents after subsetting on cohort profile:", id_num_partner_adj_1))
print(paste("Number of respondents after keeping only respondents where partner information matches interview dates:", id_num_partner_adj_2))
print(paste("The final data frame includes", length(unique(data_partner_final$ID_t)), "respondents."))
print(paste("Number of rows", nrow(data_partner_final)))
print(paste("Number of columns", ncol(data_partner_final)))

# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_partner_save <- "Data/Prep_3/prep_3_partner.rds"
} else if (cohort_prep == "controls_bef_outcome") {
  data_partner_save <- "Data/Prep_3/prep_3_partner_robustcheck.rds"
}

saveRDS(data_partner_final, data_partner_save)

# CHECKS
# 7001968, 7002012, 7002018, 7001970, 7002521 7005568
# data_partner %>% subset(ID_t == 7005568) %>% select(ID_t, start_date, end_date, end_date_adj, interview_date, partner_num, partner_current,
#                                                     partner_gender, partner_uni_degree, partner_school_degree, partner_birth_year)
# data_partner_final %>% subset(ID_t == 7005568) %>% select(ID_t, interview_date, partner_num, partner_current, partner_current_length,
#                                                           partner_previous_length_total, partner_male, partner_uni_degree, partner_uni_degree_NA,
#                                                           partner_school_degree_highest, partner_age, partner_age_NA)
# data_partner_final %>% filter(partner_age > 70) %>% select(ID_t, interview_date, partner_current, partner_num, partner_age)
# data_partner %>% subset(ID_t == 7018170) %>% select(ID_t, interview_date, partner_birth_year)

