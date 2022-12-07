#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample and Variable Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Outcome & Treatment
# -> Treatment: User can decide on the definition on participating in sport based on its
# frequency. If frequency is considered, all students who participate in sports only
# once a month or less are considered as students who do not do sports at all. Otherwise,
# they also belong to the treatment group.
# -> Outcome: Only respondents with plausible values in grades are kept (that is
# grades between 1 and 5).
#++++
# 2.) Extracurricular Activity
# Only respondents are kept who take part in at least one extracurricular activity
# The code is generated flexible: 1.) Definition of extracurricular activity can
# be selected; 2.) If frequency in participating is relevant can be selected (-> see ROBUSTNESS CHECKS)
# Moreover, frequency variable is aggregated (not included for each extracurricular
# activity because this would be too many correlated controls)
#++++
# 3.) Age
# Only respondents in the age range 18 to 25 (young adulthood) are kept.
#++++
# 4.) School
# -> Drop respondents who report that there highest degree is an intermediate degree (Mittlere Reife)
# or even no qualification -> For studying high degree is necessary
# -> Drop respondents with unrealistic short and long school spells
#++++
# 5.) Length treatment period
# -> Drop observations with treatment periods above two years. In this case, the
# distance between control variables and outcome/treatment are too long.
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("stringr")) install.packages("stringr")
library(stringr)  # for string manipulations

# load data
data <- readRDS("Data/prep_4_merge.rds")

# number of respondents
id_num_start <- length(unique(data$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ROBUSTNESS CHECKS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# in the following parameters are defined which allow to adjust the sample
# size easily


## TREATMENT ##
#+++++++++++++#

# main result: considers all frequencies as participating in sports
# robustness check: only considers at least "several times a month" as
# valid treatment
sport_freq <- "no"


## EXTRACURRICULAR ACTIVITY ##
#++++++++++++++++++++++++++++#

# main result: considers all extracurricular activities and every frequency
# robustness checks consideres different extracurricular activities and different
# frequencies
  ## extracurricular activity type -> definition:
  ## 1: university; 2: outside university; 3: playing music, reading, political engagement
extra_incl <- c(1, 2, 3) # change number to include less and run code -> see how sample size changes
  ## frequency of participating: at least several times per month; otherwise individual
  ## is considered as not participating
extra_freq <- "no"



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### TREATMENT AND OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# ensure that there are no missing values in treatment and outcome variable
data %>%
  summarise(check_na = any(is.na(treatment_sport), 
                           is.na(treatment_sport_freq), 
                           is.na(outcome_grade)))

# check plausibility of treatment and outcome observations
table(data$treatment_sport, useNA = "always")
table(data$treatment_sport_freq, useNA = "always")
summary(data$outcome_grade)


# decide if you exclude treatment observation due to low frequency: in this case
# the binary treatment indicator "treatment_sport" is adjusted so that students
# who participate in physical activity only once a month or less are considered
# as students who do not do sports.
if (sport_freq == "yes") {
  data_sub_1 <- data %>%
    mutate(
      treatment_sport = case_when(
        treatment_sport_freq == 1 ~ 0,
        TRUE ~ treatment_sport
      )
    )
} else {
  data_sub_1 <- data
}

table(data_sub_1$treatment_sport, useNA = "always")


# outcome includes some unplausible values
# I define as plausible values, every value between 1.0 and 5.0
# implausible values are set to NA; then missing values are downward replaced
# observations for remaining missing values are dropped
length(unique(data$ID_t)) # 9,062
data_sub_1 <- data_sub_1 %>%
  mutate(outcome_grade = replace(outcome_grade, outcome_grade > 5 | outcome_grade < 1, NA)) %>%
  group_by(ID_t) %>%
  fill(outcome_grade, .direction = "down") %>%
  filter(!is.na(outcome_grade)) # 53 observations are dropped

summary(data_sub_1$outcome_grade)

id_num_outcome <- length(unique(data_sub_1$ID_t)) # 9,045


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# I consider three types of extracurricular activities:
#++
# 1.) Extracurricular activities within the university (extracurricular_type) -> current semester
# For instance, being part of a student association.
#++
# 2.) Extracurricular activities outside the university (extracurricular_leisure_num) -> last twelve month
# For example, participating in a music group or neighborhood help.
#++
# 3.) Other activities: playing musing, reading in leisure time, interest in politics, 
# or working during university studies
#++
# -> all those activities are voluntary during university studies and lead to
# less time for studying
#++
# -> Possible selection for robustness check: a respondent is only considered to 
# take part in an extracurricular activity if he or she participates at least
# several times per month
#++


# data %>% subset(extracurricular_leisure_3_freq == "once a month" | extracurricular_action_freq == "daily") %>%
#   select(ID_t, treatment_period, extracurricular_leisure_3, extracurricular_leisure_3_freq, extracurricular_action, extracurricular_action_freq) 
# data[c(1:5, 80:83), ]


#### Frequency ####
#+++++++++++++++++#

if (extra_freq == "yes") {
  # extract all extracurricular frequency variables based on the extracurricular definition
  if (sum(extra_incl %in% c(1, 2)) == 2) {
    extra_cols_freq <- data_sub_1 %>% ungroup() %>% select(matches("extracurricular_.*_freq$")) %>% colnames()
  } else {
    if (1 %in% extra_incl) {
      extra_cols_freq <- data_sub_1 %>% ungroup() %>% select(matches("extracurricular_[^leisure].*_freq$")) %>% colnames()
    }
    if (2 %in% extra_incl) {
      extra_cols_freq <- data_sub_1 %>% ungroup() %>% select(matches("extracurricular_leisure_.*_freq$")) %>% colnames()
    }
  }
  
  # to not destroy original data
  data_sub_2 <- data_sub_1
  
  # iterate over the column to make replacements
  # NOTE: for NAs no replacements are made
  for (cols_sel in extra_cols_freq) {
    new_column_name <- str_split(cols_sel, "_freq", simplify = TRUE)[, 1]
    data_sub_2 <- data_sub_2 %>%
      mutate(
        {{new_column_name}} := case_when(
          !!sym(cols_sel) %in% c(5, 6) ~ 0,
          TRUE ~ !!sym(new_column_name)
        )
      )
  }
  
} else {
  data_sub_2 <- data_sub_1
}


length(unique(data_sub_2$ID_t)) 


#### Definition ####
#++++++++++++++++++#


# # generate sub data frame to not destroy original data 
# #data_sub_2 <- data
# data_keep <- data.frame()
# 
# # subset based on selection
#   ## extracurricular activity at university: at least in one involved
# if (1 %in% extra_incl) {
#   data_keep_1 <- data_sub_2 %>%
#     filter_at(
#       # no frequency and no leisure 
#       vars(matches("extracurricular_") & matches("extracurricular_[^_leisure]") & !ends_with("freq")), 
#       any_vars(. == "involved")
#     ) %>%
#     # ID_t and treatment_period is sufficient for selection later via merge
#     select(ID_t, treatment_period)
#   
#   data_keep <- rbind(data_keep, data_keep_1)
# } 
#   ## extracurricular activity outside university: at least in one involved
# if (2 %in% extra_incl) {
#   data_keep_2 <- data_sub_2 %>%
#     filter_at(
#       # no frequency and no leisure 
#       vars(matches("extracurricular_leisure_[0-9]$") & !ends_with("freq")), 
#       any_vars(. == "involved")
#     ) %>%
#     select(ID_t, treatment_period)
#   
#   data_keep <- rbind(data_keep, data_keep_2)
# }
#   ## extracurricular activity such as reading: at least in one involved
# if (3 %in% extra_incl) {
#   data_keep_3 <- data_sub_2 %>%
#     mutate(
#       extracurricular_leisure_gen = case_when(
#         interest_music_play == "yes" ~ "involved",
#         #interest_music_classic == "yes" ~ "involved",
#         interest_reading_leisure > 0 ~ "involved",
#         interest_politics_signatures == "yes" ~ "involved",
#         interest_politics_demo == "yes" ~ "involved",
#         interest_politics_discussion != "rarely or never" & !is.na(interest_politics_discussion) ~ "involved",
#         !is.na(spell_length_current_Emp) ~ "involved",
#         TRUE ~ "not involved"
#       )
#     ) %>%
#     filter_at(
#       vars("extracurricular_leisure_gen"), 
#       any_vars(. == "involved")
#     ) %>%
#     select(ID_t, treatment_period)
#   
#   data_keep <- rbind(data_keep, data_keep_3)
# }
# 
# data_keep <- data_keep %>% distinct()
# 
# data_sub_2 <- inner_join(
#   data_sub_2, data_keep, by = c("ID_t", "treatment_period")
# )

data_count <- data.frame()

# Number of extracurricular activities (excluding sport) a respondent takes part
if (1 %in% extra_incl) {
  
  # select columns to simplify code for upcoming calculation
  colnames_count_1 <- 
    data_sub_2 %>%
    ungroup() %>%
    select(matches("extracurricular_") & matches("extracurricular_[^_leisure]") & !ends_with("freq")) %>% 
    colnames()
  
  # sum extracurricular activities
  data_count_1 <- data_sub_2 %>%
    ungroup() %>%
    select(ID_t, treatment_period, all_of(colnames_count_1)) %>%
    # recode involved and not involved to number to sum up
    #mutate_at(vars(colnames_count_1), ~ recode(., "involved" = 1, "not involved" = 0)) %>%
    # ensure that values are numeric
    mutate_at(vars(colnames_count_1), ~ as.integer(.)) %>%
    # sum up how often individual has "involved" (or rather 1)
    # ignore NAs (handled in next step)
    mutate(extracurricular_num = rowSums(select(., colnames_count_1), na.rm = TRUE)) %>%
    # sum up NA in extracurricular activity
    mutate(sum_na = rowSums(is.na(select(., colnames_count_1)))) %>%
    # only if all are NA, then "extracurricular_num" variable is set to NA instead of 0
    mutate(extracurricular_num = case_when(sum_na == length(colnames_count_1) ~ as.double(NA), 
                                           TRUE ~ extracurricular_num)) %>%
    select(ID_t, treatment_period, extracurricular_num)
  
  
  data_count <- rbind(data_count, data_count_1)
} 

if (2 %in% extra_incl) {
  
  # select columns to simplify code for upcoming calculation
  colnames_count_2 <- 
    data_sub_2 %>%
    ungroup() %>%
    select(matches("extracurricular_leisure_[0-9]$") & !ends_with("freq")) %>% 
    colnames()
  
  # sum extracurricular activities
  data_count_2 <- data_sub_2 %>%
    ungroup() %>%
    select(ID_t, treatment_period, all_of(colnames_count_2)) %>%
    # recode involved and not involved to number to sum up
    #mutate_at(vars(colnames_count_2), ~ recode(., "involved" = 1, "not involved" = 0)) %>%
    # ensure that values are numeric
    mutate_at(vars(colnames_count_2), ~ as.integer(.)) %>%
    # sum up how often individual has "involved" (or rather 1)
    # ignore NAs (handled in next step)
    mutate(extracurricular_num = rowSums(select(., colnames_count_2), na.rm = TRUE)) %>%
    # sum up NA in extracurricular activity
    mutate(sum_na = rowSums(is.na(select(., colnames_count_2)))) %>%
    # only if all are NA, then "extracurricular_num" variable is set to NA instead of 0
    mutate(extracurricular_num = case_when(sum_na == length(colnames_count_2) ~ as.double(NA), 
                                           TRUE ~ extracurricular_num)) %>%
    select(ID_t, treatment_period, extracurricular_num)
  
  
  data_count <- rbind(data_count, data_count_2)
} 

if (3 %in% extra_incl) {
  
  # select columns to simplify code for upcoming calculation
  colnames_count_3 <- c(
    "interest_music_play", "interest_reading_leisure", "interest_politics_signatures", 
    "interest_politics_demo", "interest_politics_discussion", "spell_length_current_Emp"
  )
  
  # sum extracurricular activities
  data_count_3 <- data_sub_2 %>%
    ungroup() %>%
    # change some variables
    mutate(
      interest_reading_leisure = case_when(
        interest_reading_leisure > 0 ~ 1, is.na(interest_reading_leisure) ~ as.double(NA), 
        TRUE ~ 0),
      interest_politics_discussion = case_when(
        interest_politics_discussion != 5 & !is.na(interest_politics_discussion) ~ 1,
        is.na(interest_politics_discussion) ~ as.double(NA),
        TRUE ~ 0),
      spell_length_current_Emp = case_when(
        spell_length_current_Emp > 0 ~ 1,
        is.na(spell_length_current_Emp) ~ as.double(NA),
        TRUE ~ 0)
    ) %>%
    mutate_at(vars(colnames_count_3), ~ recode(., "2" = 0, "1" = 1)) %>% 
    mutate(extracurricular_num = rowSums(select(., all_of(colnames_count_3)), na.rm = TRUE)) %>%
    mutate(sum_na = rowSums(is.na(select(., all_of(colnames_count_3))))) %>%
    mutate(extracurricular_num = case_when(sum_na == length(colnames_count_3) ~ as.double(NA), 
                                           TRUE ~ extracurricular_num)) %>%
    select(ID_t, treatment_period, extracurricular_num)
  
  data_count <- rbind(data_count, data_count_3)
} 

data_count <- data_count %>%
  arrange(ID_t, treatment_period) %>%
  distinct() %>%
  group_by(ID_t, treatment_period) %>%
  summarize(extracurricular_num = sum(extracurricular_num, na.rm = TRUE)) %>%
  distinct()

summary(data_count$extracurricular_num)
sum(data_count$extracurricular_num == 0)

data_sub_3 <- left_join(
  data_sub_2, data_count, by = c("ID_t", "treatment_period")
) %>%
  filter(extracurricular_num != 0)

id_num_extra <- length(unique(data_sub_3$ID_t)) # 8874



#### Aggregated Frequency Variable ####
#+++++++++++++++++++++++++++++++++++++#

data_freq <- data.frame()

# aggregated frequency variable: across all frequency measures take the
# highest
if (1 %in% extra_incl) {
  
  # select columns to simplify code for upcoming calculation
  colnames_freq_1 <- 
    data_sub_3 %>%
    ungroup() %>%
    select(matches("extracurricular_") & matches("extracurricular_[^_leisure]") & ends_with("freq")) %>% 
    colnames()
  
  # extract maximum
  data_freq_1 <- 
    data_sub_3 %>%
    ungroup() %>%
    select(ID_t, treatment_period, all_of(colnames_freq_1)) %>%
    mutate(extracurricular_max = pmax(!!!rlang::syms(colnames_freq_1), na.rm = TRUE)) %>%
    select(ID_t, treatment_period, extracurricular_max)
  
  data_freq <- rbind(data_freq, data_freq_1)
}


if (2 %in% extra_incl) {
  
  # select columns to simplify code for upcoming calculation
  colnames_freq_2 <- 
    data_sub_3 %>%
    ungroup() %>%
    select(matches("extracurricular_leisure_[0-9]") & ends_with("freq")) %>% 
    colnames()
  
  # extract maximum
  data_freq_2 <- 
    data_sub_3 %>%
    ungroup() %>%
    select(ID_t, treatment_period, all_of(colnames_freq_2)) %>%
    mutate(extracurricular_max = pmax(!!!rlang::syms(colnames_freq_2), na.rm = TRUE)) %>%
    select(ID_t, treatment_period, extracurricular_max)
  
  data_freq <- rbind(data_freq, data_freq_2)
}


# prepare final freq
data_freq <-
  data_freq %>%
  select(ID_t, treatment_period, extracurricular_max) %>%
  distinct() %>%
  group_by(ID_t, treatment_period) %>%
  summarize(extracurricular_freq = max(extracurricular_max)) %>%
  distinct()


# merge to other data frame
data_sub_3 <- left_join(
  data_sub_3, data_freq, by = c("ID_t", "treatment_period")
)

# drop all frequency variables
data_sub_3 <- data_sub_3 %>%
  ungroup() %>%
  select(-(matches("extracurricular_.*_freq$")))



#%%%%%%%%%%%#
#### AGE ####
#%%%%%%%%%%%#

data_sub_4 <- data_sub_3

# Create birth date from month and year with day = 1
source("Functions/func_generate_date.R")
data_sub_4 <- func_generate_date(data_sub_4, month = "birth_month", 
                                 year = "birth_year", varname = "birth_date")

# Age = interview date used for spell - birth date 
data_sub_4 <- data_sub_4 %>%
  mutate(age = as.numeric(difftime(interview_date_spell, birth_date, units = "weeks") / 52.5))
summary(data_sub_4$age)

# Subset
data_sub_4 <- data_sub_4 %>%
  filter(age >= 18 & age <= 25)
summary(data_sub_4$age)

id_num_age <- length(unique(data_sub_4$ID_t)) # 7,466


#%%%%%%%%%%%%%%#
#### SCHOOL ####
#%%%%%%%%%%%%%%#

data_sub_5 <- data_sub_4

## DEGREE ##
#++++++++++#

# there are individuals who have as highest degree an intermediat degree (Mittlere Reife)
# In Germany, one needs a high degree to study. Hence, I drop those individuals
# even they have a university spell (something is wrong with them; interviewer or
# respondent made a mistake)
data_sub_5 <- data_sub_5 %>%
  filter(!grepl("Mittlere Reife", educ_highest_degree_casmin)) %>%
  filter(!grepl("no qualification", educ_highest_degree_casmin))

id_num_degree <- length(unique(data_sub_5$ID_t))


## SPELL LENGTH ##
#++++++++++++++++#

# check spell length
summary(data_sub_5$spell_length_cum_School)
summary(data_sub_5$spell_length_cum_Uni)
summary(data_sub_5$spell_length_cum_Emp)
summary(data_sub_5$spell_length_cum_Internship)
summary(data_sub_5$spell_length_cum_Military)
summary(data_sub_5$spell_length_cum_VocTrain)
summary(data_sub_5$spell_length_cum_VocPrep)

# drop observations with unrealistic spell length
data_sub_5 <- data_sub_5 %>%
  filter(spell_length_cum_School >= 10 & spell_length_cum_School <= 16)

data_sub_5 <- data_sub_5 %>%
  filter(spell_length_cum_Emp <= 15) # because earliest start date for work is 12 (25-12 = 13 ~ 15)

# summary(data_merge_1$spell_length_cum_School)
# summary(data_merge_1$spell_length_cum_Uni)
# summary(data_merge_1$spell_length_current_Uni)

id_num_spell <- length(unique(data_sub_5$ID_t)) # 7,466



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### LENGTH TREATMENT PERIOD ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_sub_6 <- data_sub_5


# calculate length of treatment period in years
data_sub_6 <- data_sub_6 %>%
  mutate(
    treatment_period_length = abs(as.numeric(
      (interview_date_start - interview_date_end) / 365
      ))
  )

summary(data_sub_6$treatment_period_length)

# I only keep observations with a treatment period of maximum 2 years
# otherwise control variables are two old
data_sub_6 <- data_sub_6 %>% filter(treatment_period_length <= 2)
  

id_num_length <- length(unique(data_sub_6$ID_t)) 


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#


## COMPARE SAMPLE SIZES ##
#++++++++++++++++++++++++#

# selection
cat("You made the following selections:")
if (sport_freq == "yes") {
  cat("Sport frequency at least several times a month.")
}
if (extra_freq == "yes") {
  cat("Extracurricular activity frequency at least several times a month.")
}
if (1 %in% extra_incl) {
  cat("Extracurricular activities within university.")
}
if (2 %in% extra_incl) {
  cat("Extracurricular activities outside university.")
}
if (3 %in% extra_incl) {
  cat("Other extracurricular activities.")
}

# number of respondents, number of rows, and number of columns before sample selection
print("BEFORE SAMPLE SELECTION:")
print(paste("Number of respondents:", id_num_start))
print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))

# step 1: drop individuals with unrealistic values in grade (below 1.0 and above 5.0)
print("DROP RESPONDENTS WITH UNREALISTIC VALUES IN GRADE:")
print(paste("Number of respondents:", id_num_outcome))

# step 2: drop individuals not taking part in any extracurricular activity
# (according to the chosen definition)
print("DROP RESPONDENTS NOT TAKING PART IN ANY EXTRACURRICULAR ACTIVITY:")
print(paste("Number of respondents:", id_num_extra))

# step 3: drop individuals not in the rage of 18-25
print("DROP RESPONDENTS NOT BEING IN THE DATE RANGE OF YOUNG ADULTS (18-25):")
print(paste("Number of respondents:", id_num_age))

# step 4: drop observations with unrealistic spell lengths
print("DROP RESPONDENTS WITH NO HIGHER DEGREE:")
print(paste("Number of respondents:", id_num_degree))

# step 5: drop observations with unrealistic spell lengths
print("DROP RESPONDENTS WITH UNREALISTIC SPELL LENGTHS:")
print(paste("Number of respondents:", id_num_spell))

# step 6: drop observations with treatment periods above 2 years
print("DROP RESPONDENTS WITH TREATMENT PERIODS ABOVE 2 YEARS:")
print(paste("Number of respondents:", id_num_length))

# number of respondents, number of rows, and number of columns after sample selection
print("AFTER SAMPLE SELECTION:")
print(paste("Number of respondents:", length(unique(data_sub_6$ID_t))))
print(paste("Number of rows:", nrow(data_sub_6)))
print(paste("Number of columns:", ncol(data_sub_6)))



## Generate suffix for saving ##
#++++++++++++++++++++++++++++++#

suffix_save <- c()
if (sport_freq == "yes") {
  suffix_save <- c(suffix_save, "_sportfreq")
}
if (extra_freq == "yes") {
  suffix_save <- c(suffix_save, "_extrafreq")
}

if (all(c(1, 2, 3) %in% extra_incl)) {
  suffix_save <- suffix_save
} else {
  suffix_save <- paste0(c(suffix_save, paste0(extra_incl, collapse = "_")), collapse = "_")
}

suffix_save_final <- paste0("Data/prep_5_sample_selection", suffix_save, ".rds")

# save data frame
saveRDS(data_sub_6, suffix_save_final)

