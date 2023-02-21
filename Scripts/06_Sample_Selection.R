#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample and Variable Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Extracurricular Activity
# Only respondents are kept who take part in at least one extracurricular activity
# (definition see in respective section below).
# -> ONLY done if selected!
#++++
# 2.) Subset on age: 18-29 (emerging adulthood) at start of study
#++++
# -> Panel data set includes the final sample size.
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # install packages if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("stringr")) install.packages("stringr")
# library(stringr)  # for string manipulations
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # for fill() function
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # for excel file
# 
# # define inputs
#   ## selection on cohort preparation
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"
#   ## only for saving
# treatment_repl <- "downup" 
#   ## treatment definition: all frequency levels or only weekly
# #treatment_def <- "all"
# treatment_def <- "weekly"

# load data
if (cohort_prep == "controls_same_outcome") {
  data_load <- paste0("Data/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, ".rds")  
} else {
  data_load <- paste0("Data/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")  
}

data_raw <- readRDS(data_load)

# number of respondents
id_num_start <- length(unique(data_raw$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_sub_1 <- data_raw

#++
# I consider three types of extracurricular activities:
#++
# 1.) Extracurricular activities within the university (extracurricular_type) -> current semester
# For instance, being part of a (political) student association.
#++
# 2.) Extracurricular activities outside the university (extracurricular_leisure_num) -> last twelve month
# For example, participating in a music group or neighborhood help.
#++
# 3.) Other activities: playing musing 
#++
# -> all those activities are voluntary during university studies and lead to
# less time for studying
#++


# Number of extracurricular activities (excluding sport) a respondent takes part #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# select column names containing info of extracurricular activity
colnames_extra <- data_sub_1 %>%
  select(matches("extracurricular_") & !ends_with("freq"), "interest_music_play") %>%
  colnames()

# generate new data frame
data_count <- data.frame()

# sum extracurricular activities
data_count <- data_sub_1 %>%
  ungroup() %>%
  select(ID_t, treatment_period, all_of(colnames_extra)) %>%
  # recode involved and not involved to number to sum up
  #mutate_at(vars(colnames_count_1), ~ recode(., "involved" = 1, "not involved" = 0)) %>%
  # ensure that values are numeric
  mutate_at(vars(colnames_extra), ~ as.integer(.)) %>%
  # sum up how often individual has "involved" (or rather 1)
  # ignore NAs (handled in next step)
  mutate(extracurricular_num = rowSums(select(., all_of(colnames_extra)), na.rm = TRUE)) %>%
  # sum up NA in extracurricular activity
  mutate(sum_na = rowSums(is.na(select(., all_of(colnames_extra))))) %>%
  # only if all are NA, then "extracurricular_num" variable is set to NA instead of 0
  mutate(extracurricular_num = case_when(sum_na == length(colnames_extra) ~ as.double(NA), 
                                         TRUE ~ extracurricular_num)) %>%
  # keep only variables of interest
  select(ID_t, treatment_period, extracurricular_num) %>%
  # fill missing values up and down
  arrange(ID_t, treatment_period) %>%
  group_by(ID_t) %>% fill(extracurricular_num, .direction = "down") %>% ungroup() %>%
  # generate NA dummy
  mutate(extracurricular_NA = ifelse(is.na(extracurricular_num), 1, 0)) %>% 
  arrange(ID_t, treatment_period) %>%
  distinct() %>% ungroup()

summary(data_count$extracurricular_num)


# Subset #
#++++++++#

# add information about number of extracurricular activities and subset data frame
# individuals either take part in extracurricular activity or do sports
if (extra_act == "yes") {
  data_sub_2 <- left_join(
    data_sub_1, data_count, by = c("ID_t", "treatment_period")
  ) %>%
    filter(extracurricular_num > 0 | treatment_sport == 1)
} else {
  data_sub_2 <- left_join(
    data_sub_1, data_count, by = c("ID_t", "treatment_period")
  )
}


id_num_adj_1 <- length(unique(data_sub_2$ID_t)) 
id_treatment_periods_1 <- nrow(data_sub_2)
id_num_drop <- id_num_start - id_num_adj_1

# set NA in extracurricular_num to zero
table(data_sub_2$extracurricular_num, useNA = "always")
data_sub_2 <- data_sub_2 %>%
  mutate(extracurricular_num = case_when(
    is.na(extracurricular_num) ~ 0, TRUE ~ extracurricular_num
  ))
table(data_sub_2$extracurricular_num, useNA = "always")

# sample size if NA would not be dropped
left_join(
  data_sub_1, data_count, by = c("ID_t", "treatment_period")
) %>%
  filter(extracurricular_num > 0  | is.na(extracurricular_num)) %>%
  pull(ID_t) %>% unique() %>% length()



# Aggregated Frequency Variable #
#+++++++++++++++++++++++++++++++#

# extract frequency columns
colnames_extra_freq <- data_sub_2 %>%
  select(matches("extracurricular_") & ends_with("freq"), "interest_music_play_freq") %>%
  colnames()

# recode interest_music_play_freq variable (asked per month)
##++ 1 = daily, 
##++ 2 = several times a week, 
##++ 3 = once a week, 
##++ 4 = several times a month, 
##++ 5 = once a month, 
##++ 6 = less frequently

table(data_sub_2$interest_music_play_freq)
data_sub_2 <- data_sub_2 %>%
  mutate(interest_music_play_freq_orig = interest_music_play_freq) %>%
  mutate(interest_music_play_freq = case_when(
    interest_music_play_freq >= 28 ~ 1, 
    interest_music_play_freq < 28 & interest_music_play_freq > 5 ~ 2,
    interest_music_play_freq %in% c(4,5) ~ 3, 
    interest_music_play_freq < 4 & interest_music_play_freq > 1 ~ 4, 
    interest_music_play_freq == 1 ~ 5, 
    interest_music_play_freq == 0 ~ 6,
    is.na(interest_music_play_freq) ~ as.double(NA), TRUE ~ 7
  ))
table(data_sub_2$interest_music_play_freq)


# aggregated frequency variable: across all frequency measures take the
# highest
data_freq <- data_sub_2 %>%
  ungroup() %>%
  select(ID_t, treatment_period, all_of(colnames_extra_freq)) %>%
  mutate(extracurricular_freq = pmax(!!!rlang::syms(colnames_extra_freq), na.rm = TRUE)) %>%
  select(ID_t, treatment_period, extracurricular_freq) %>%
  mutate(extracurricular_freq = case_when(
    extracurricular_freq == 1 ~ "daily",
    extracurricular_freq == 2 ~ "several times a week",
    extracurricular_freq == 3 ~ "once a week",
    extracurricular_freq == 4 ~ "several times a month",
    extracurricular_freq == 5 ~ "once a month",
    extracurricular_freq == 6 ~ "less frequently",
    is.na(extracurricular_freq) ~ as.character(NA), TRUE ~ "never")) %>%
  group_by(ID_t) %>% fill(extracurricular_freq, .direction = "down") %>% ungroup()
    
table(data_freq$extracurricular_freq, useNA = "always")
sum(duplicated(data_freq %>% select(ID_t, treatment_period)))


# merge to other data frame
data_sub_2 <- left_join(
  data_sub_2, data_freq, by = c("ID_t", "treatment_period")
)



## Type of Extracurricular Activity ##
#++++++++++++++++++++++++++++++++++++#

# university
cols_extra_uni <- c("extracurricular_association", "extracurricular_committee", 
                    "extracurricular_action", "extracurricular_fraternal", "extracurricular_culture", 
                    "extracurricular_political")#, "extracurricular_other")

df_extracurricular_uni <- data_sub_2 %>% 
  select(ID_t, treatment_period, all_of(cols_extra_uni)) %>%
  mutate(check_uni = rowSums(select(., all_of(cols_extra_uni)), na.rm = TRUE)) %>%
  mutate(check_uni_sum_na = rowSums(is.na(select(., all_of(cols_extra_uni))))) %>%
  mutate(extracurricular_uni = case_when(
    check_uni_sum_na == length(cols_extra_uni) ~ as.double(NA), 
    check_uni == 0 & check_uni_sum_na != length(cols_extra_uni) ~ 0, 
    TRUE ~ 1)) %>%
  select(-c(check_uni, check_uni_sum_na)) %>%
  select(ID_t, treatment_period, extracurricular_uni) %>%
  group_by(ID_t) %>% fill(extracurricular_uni, .direction = "down") %>% ungroup() 

data_sub_2 <- left_join(data_sub_2, df_extracurricular_uni, 
                        by = c("ID_t", "treatment_period"))


# outside
cols_extra_uni_no <- data_sub_2 %>% select(matches("extracurricular_leisure") & !ends_with("freq")) %>% colnames()

df_extracurricular_uni_no <- data_sub_2 %>% 
  select(ID_t, treatment_period, all_of(cols_extra_uni_no)) %>%
  mutate(check_uni = rowSums(select(., all_of(cols_extra_uni_no)), na.rm = TRUE)) %>%
  mutate(check_uni_sum_na = rowSums(is.na(select(., all_of(cols_extra_uni_no))))) %>%
  mutate(extracurricular_uni_no = case_when(
    check_uni_sum_na == length(cols_extra_uni_no) ~ as.double(NA), 
    check_uni == 0 & check_uni_sum_na != length(cols_extra_uni) ~ 0, TRUE ~ 1)) %>%
  select(-c(check_uni, check_uni_sum_na)) %>%
  select(ID_t, treatment_period, extracurricular_uni_no) %>%
  group_by(ID_t) %>% fill(extracurricular_uni_no, .direction = "down") %>% ungroup() 

data_sub_2 <- left_join(data_sub_2, df_extracurricular_uni_no, 
                        by = c("ID_t", "treatment_period"))


# music
data_sub_2 <- data_sub_2 %>%
  mutate(extracurricular_music = case_when(
    interest_music_play == 1 ~ 1, is.na(interest_music_play) ~ as.double(NA),
    TRUE ~ 0
  ))


table(data_sub_2$extracurricular_uni, useNA = "always")
table(data_sub_2$extracurricular_uni_no, useNA = "always")
table(data_sub_2$extracurricular_music, useNA = "always")

# checks
check_uni <- data_sub_2 %>%
  select(ID_t, treatment_period, all_of(cols_extra_uni), extracurricular_uni)

check_uni_no <- data_sub_2 %>%
  select(ID_t, treatment_period, all_of(cols_extra_uni_no), extracurricular_uni_no) 

check_music <- data_sub_2 %>%
  select(ID_t, treatment_period, interest_music_play, extracurricular_music) 

check_freq <- data_sub_2 %>%
  select(ID_t, treatment_period, matches("extracurricular_.*_freq"), interest_music_play_freq, extracurricular_freq)

check_num <- data_sub_2 %>%
  select(ID_t, treatment_period, starts_with("extracurricular") & !ends_with("_freq"), interest_music_play, extracurricular_num)


# drop all extracurricular variables not needed anymore
colnames_extra <- data_sub_2 %>% select(starts_with("extracurricular")) %>% colnames()
colnames_extra_keep <- c("extracurricular_freq", "extracurricular_num", 
                         "extracurricular_uni", "extracurricular_uni_no", "extracurricular_music")
colnames_extra_drop <- colnames_extra[!colnames_extra %in% colnames_extra_keep]


data_sub_2 <- data_sub_2 %>%
  ungroup() %>%
  select(-(all_of(colnames_extra_drop)))




#%%%%%%%%%%%#
#### AGE ####
#%%%%%%%%%%%#

data_sub_3 <- data_sub_2

# # Create birth date from month and year with day = 1
source("Functions/func_generate_date.R")
data_sub_3 <- func_generate_date(data_sub_3, month = "birth_month",
                                 year = "birth_year", varname = "birth_date")

# Age = interview date used for spell - birth date
data_sub_3 <- data_sub_3 %>%
  mutate(age = as.numeric(difftime(interview_date_spell, birth_date, units = "weeks") / 52.5))
summary(data_sub_3$age)

# Subset on age between 18 and 29 at start of study
id_keep <-
  data_sub_3 %>% 
  group_by(ID_t) %>% 
  filter(interview_date_spell == min(interview_date_spell) & between(age, 18, 29)) %>% 
  pull(ID_t) %>% 
  unique()

# Subset
data_sub_3 <- data_sub_3 %>%
  subset(ID_t %in% id_keep)
summary(data_sub_3$age)

id_num_adj_2 <- length(unique(data_sub_3$ID_t))
id_treatment_periods_2 <- nrow(data_sub_3)


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# adjust treatment period
data_sub_3 <- data_sub_3 %>% 
  arrange(ID_t, interview_date_spell) %>%
  group_by(ID_t) %>% 
  mutate(treatment_period = row_number())

# ungroup and sort 
data_sub_3 <- data_sub_3 %>% ungroup() %>% arrange(ID_t, treatment_period)

# check for duplicates
sum(duplicated(data_sub_3))
sum(duplicated(data_sub_3 %>% select(ID_t, treatment_period)))

# number of respondents, number of rows, and number of columns before sample selection
print(paste("Number of respondents before sample selection:", id_num_start))
print(paste("Number of respondents after dropping students who do not participate in any extracurricular activity:", 
            id_num_adj_1))
print(paste("Number of treatment periods:", id_treatment_periods_1))
print(paste("Number of respondents after dropping students who are not in age range 17 to 30:", id_num_adj_2))
print(paste("Number of treatment periods:", id_treatment_periods_2))

# number of respondents, number of rows, and number of columns after sample selection
print("AFTER SAMPLE SELECTION:")
print(paste("Number of respondents:", length(unique(data_sub_3$ID_t))))
print(paste("Number of rows:", nrow(data_sub_3)))
print(paste("Number of columns:", ncol(data_sub_3)))


# save data frame
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}

if (cohort_prep == "controls_same_outcome") {
  data_save <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, extra_act_save, ".rds")
} else {
  data_save <- paste0("Data/Prep_6/prep_6_sample_selection_", treatment_def, 
                      "_", treatment_repl, extra_act_save, "_robustcheck.rds")
}

saveRDS(data_sub_3, data_save)


# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "sample_selection",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl,
  "data_prep_treatment_def" = treatment_def,
  "data_prep_extraact" = extra_act, 
  "num_id" = length(unique(data_sub_3$ID_t)), 
  "num_rows" = nrow(data_sub_3),
  "num_cols" = ncol(data_sub_3),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "grade")
