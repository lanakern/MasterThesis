#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### GENERATE TREATMENT AND OUTCOME VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the treatment and outcome variables are generated.
# -> Outcome: current average grade.
# -> Treatment: sport participation as binary variable (0,1) and multiple
# treatment variable considering the frequency.
#++++
# 1.) Treatment
# -> Binary and multivalued treatment indicator: binary considers simply sport
# participation vs. non-participation while multivalued treatment indicator
# considers sport frequency.
# -> Leisure/general and university sport is aggregated: if only one information
# is provided, this info is used. If both are provided and do not coincide
# the higher frequency is considered. 
#++++
# 2.) Outcome
# -> Current grade average is replaced by final grade if current grade average
# is missing (however, never the case here).
#++++
# 3.) Sample Selection
# -> Drop students who have a missing value in binary treatment variable and/or
# outcome
# -> Drop students with grade outside interval [1,5]
#++++
# -> RESULT: PANEL DATA FRAME
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
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # for fill() function -> missing values
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
# treatment_def <- "all"
# #treatment_def <- "weekly"


# load data
if (cohort_prep == "controls_same_outcome") {
  data_raw <- readRDS(paste0("Data/Prep_4/prep_4_merge_all_treat", treatment_repl, ".rds"))
} else if (cohort_prep == "controls_bef_outcome") {
  data_raw <- readRDS(paste0("Data/Prep_4/prep_4_merge_all_treat", treatment_repl, "_robustcheck.rds") )
}
num_id <- length(unique(data_raw$ID_t))




#%%%%%%%%%%%%%%%%%#
#### Treatment ####
#%%%%%%%%%%%%%%%%%#

data_raw %>% dplyr::select(ID_t, starts_with("interview_date"), starts_with("sport"))

# count missing values in treatment variables
colSums(is.na(data_raw %>% dplyr::select(starts_with("sport"))))

# there are no non-missing values for sport_uni_freq if sport_uni is missing
data_raw %>% filter(is.na(sport_uni) & !is.na(sport_uni_freq))

# sport_uni_freq is always NA if sport_uni not involved
data_raw %>% filter(sport_uni == "not involved") %>% pull(sport_uni_freq) %>% unique() # should return NA



## CREATE TREATMENT VARIABLE FOR SPORT FREQUENCY ##
#+++++++++++++++++++++++++++++++++++++++++++++++++#

data_1 <- data_raw

# 1.) Recode sport_uni_freq as "never" for sport_uni == "not involved"
data_1 <- data_1 %>%
  mutate(sport_uni_freq = ifelse(
    sport_uni == "not involved" & is.na(sport_uni_freq), "never", sport_uni_freq
    ))

table(data_1$sport_uni, useNA = "always")
table(data_1$sport_uni_freq, useNA = "always")
data_1 %>% filter(sport_uni == "not involved") %>% pull(sport_uni_freq) %>% unique()


# 2.) Recode values: 
## General sport (g)
##++ 1 = never
##++ 2 = once a month or less
##++ 3 = several times a month or once a week
##++ 4 = several times a week
##++ 5 = almost daily or daily
## University sport (u)
##++ 1 = daily, 
##++ 2 = several times a week, 
##++ 3 = once a week, 
##++ 4 = several times a month, 
##++ 5 = once a month, 
##++ 6 = less frequently
##++ 7 = never (own generation; see above)
## AGGREGATED:
##++ 1 = never: g1 & u7
##++ 2 = less frequently: g2 & u5 & u6
##++ 3 = monthly: g3 & u4 & u3
##++ 4 = weekly: g4 & u2
##++ 5 = daily: g5 & u1

table(data_1$sport_uni_freq, useNA = "always")
table(data_1$sport_leisure_freq, useNA = "always")


data_1 <- data_1 %>%
  # labels
  mutate(
    sport_uni_freq = recode(
      sport_uni_freq,
      "never" = 1, "less frequently" = 2, "once a month" = 2, 
      "several times a month" = 3, "once a week" = 3, 
      "several times a week" = 4, "daily" = 5
    ),
    sport_leisure_freq = recode(
      sport_leisure_freq, 
      "never" = 1, "once a month or less" = 2, 
      "several times a month or once a week" = 3,
      "several times a week" = 4, "almost daily or daily" = 5
    )
  ) 


table(data_1$sport_uni_freq, useNA = "always")
table(data_1$sport_leisure_freq, useNA = "always")


# generate one treatment frequency variable (for both university and leisure sport)
data_1 <- data_1 %>%
  mutate(
    treatment_sport_freq = case_when(
      # if uni sport is missing use info on leisure sport
      is.na(sport_uni_freq) ~ sport_leisure_freq,
      # if leisure sport frequency is higher, however, use info on leisure sport
      sport_leisure_freq > sport_uni_freq & !is.na(sport_leisure_freq) & !is.na(sport_uni_freq) ~ sport_leisure_freq,
      # in all other cases use uni sport
      TRUE ~ sport_uni_freq
    )
  )

table(data_1$treatment_sport_freq, useNA = "always")


# create dummy to know if I used uni or leisure sport information
data_1 <- data_1 %>%
  mutate(sport_freq_source = 
           case_when(
             # if frequency of university sport equals frequency of sport in treatment variable
             # AND frequency of uni sport does not equal leisure sport or leisure sport is NA
             # then the source is uni
             treatment_sport_freq == sport_uni_freq & 
               (sport_uni_freq != sport_leisure_freq | is.na(sport_leisure_freq)) ~ "uni",
             # same idea for leisure than for uni
             treatment_sport_freq == sport_leisure_freq  & 
               (sport_uni_freq != sport_leisure_freq | is.na(sport_uni_freq)) ~ "leisure",
             # "both" is selected if frequency of leisure and university sport is the same
             sport_uni_freq == sport_leisure_freq & !is.na(sport_uni_freq) & !is.na(sport_leisure_freq) ~ "both",
             # otherwise NA because no info about sport participation is given
             TRUE ~ as.character(NA)
           )
  )

table(data_1$sport_freq_source, useNA = "always")


# recode frequency variable
data_1 <- data_1 %>% mutate(
  treatment_sport_freq = recode(treatment_sport_freq,
    "1" = "never", "2" = "less frequently", "3" = "monthly", "4" = "weekly", "5" = "daily"
  ))




## CREATE BINARY TREATMENT INDICATOR ##
#+++++++++++++++++++++++++++++++++++++#

data_2 <- data_1

# sport_uni = 1 if respondent participates in university sport
# sport_leisure = 1 if respondent participates in leisure sport
# -> also both variables can take on the value 1
table(data_2$sport_uni, useNA = "always")
table(data_2$sport_uni_freq, useNA = "always")
table(data_2$sport_leisure_freq, useNA = "always")


## CONSIDER ALL FREQUENCY LEVELS ##
if (treatment_def == "all") {
  data_2 <- data_2 %>%
    # rename original sport_uni variable
    rename(sport_uni_orig = sport_uni) %>%
    # create variables
    mutate(
      # respondent participates in university sport if sport_uni_orig equals
      # "involved".
      sport_uni = ifelse(sport_uni_orig == "not involved", 0, 
                         ifelse(is.na(sport_uni_orig), NA, 1)),
      # respondent participates in leisure sport if sport_leisure_freq is NOT NA and not 0
      sport_leisure = ifelse(sport_leisure_freq > 1, 1, 
                             ifelse(is.na(sport_leisure_freq), NA, 0))
    ) 
  
  table(data_2$sport_uni, useNA = "always")
  table(data_2$sport_leisure, useNA = "always")
  
  
  # create general dummy for sport-participation (=1) and non-participation (=0)
  # keep NA as later individuals and treatment periods without sport information are dropped
  data_2 <- data_2 %>%
    mutate(
      treatment_sport = case_when(sport_leisure == 1 | sport_uni == 1 ~ 1,
                                  is.na(sport_leisure) & is.na(sport_uni) ~ as.double(NA),
                                  TRUE ~ 0)
    ) %>%
    # create indicator for source
    mutate(
      sport_source = 
        case_when(
          is.na(treatment_sport) ~ as.character(NA),
          !is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "both",
          !is.na(sport_uni_orig) & is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "uni",
          is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "leisure",
          TRUE ~ as.character(NA)
        )
    )
## ONLY CONSIDER AS SPORT PARTICIPATION WEEKLY PARTICIPATION
} else {
  data_2 <- data_2 %>%
    rename(sport_uni_orig = sport_uni) %>%
    # create variables
    mutate(
      # respondent participates in university sport if sport_uni_freq variable
      # equals at least 4
      sport_uni = ifelse(sport_uni_freq > 3, 1, 
                         ifelse(is.na(sport_uni_freq), NA, 0)),
      # respondent participates in leisure sport if sport_leisure_freq is NOT NA and not 0
      sport_leisure = ifelse(sport_leisure_freq > 3, 1, 
                             ifelse(is.na(sport_leisure_freq), NA, 0))
    ) 
  
  table(data_2$sport_uni, useNA = "always")
  table(data_2$sport_leisure, useNA = "always")
  
  
  # create general dummy for sport-participation (=1) and non-participation (=0)
  # keep NA as later individuals and treatment periods without sport information are dropped
  data_2 <- data_2 %>%
    mutate(
      treatment_sport = case_when(sport_leisure == 1 | sport_uni == 1 ~ 1,
                                  is.na(sport_leisure) & is.na(sport_uni) ~ as.double(NA),
                                  TRUE ~ 0)
    ) %>%
    # create indicator for source
    mutate(
      sport_source = 
        case_when(
          is.na(treatment_sport) ~ as.character(NA),
          !is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "both",
          !is.na(sport_uni_orig) & is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "uni",
          is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "leisure",
          TRUE ~ as.character(NA)
        )
    )
}



table(data_2$sport_uni, useNA = "always")
table(data_2$sport_leisure, useNA = "always")
table(data_2$sport_uni_freq, useNA = "always")
table(data_2$sport_leisure_freq, useNA = "always")
table(data_2$treatment_sport, useNA = "always")
table(data_2$sport_source, useNA = "always")


# create NA variable
data_2 <- data_2 %>% 
  mutate(
    treatment_sport_NA = ifelse(sport_uni_NA == 1 & sport_leisure_freq_NA == 1, 1, 0),
    treatment_sport_freq_NA = ifelse(sport_uni_freq_NA == 1 & sport_leisure_freq_NA == 1, 1, 0)
    )


## CHECK ##
#+++++++++#

# check
data_2 %>% subset(ID_t == 7002010) %>% 
  dplyr::select(ID_t, starts_with("sport"), starts_with("treatment"))
data_2 %>% subset(ID_t == 7001969) %>% 
  dplyr::select(ID_t, starts_with("sport"), starts_with("treatment"))
data_2 %>% subset(ID_t == 7001977) %>% 
  dplyr::select(ID_t, starts_with("sport"), starts_with("treatment"))
data_2 %>% subset(ID_t == 7002007) %>% 
  dplyr::select(ID_t, starts_with("sport"), starts_with("treatment"))

# drop variables not needed anymore
data_2 <- data_2 %>% dplyr::select(-starts_with("sport"))

# number of respondents
length(unique(data_2$ID_t)) # should be unchanged



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%#
#### Outcome ####
#%%%%%%%%%%%%%%%#

data_3 <- data_2

# better outcome is "grade_current" because it determines the grade for the
# academic achievements so far
# however, if it is missing, the final grade is used (does not happen often)
summary(data_3$grade_current)

data_3 %>% filter(is.na(grade_current) & !is.na(grade_final)) %>%
  pull(ID_t) %>% unique() %>% length()

data_3 <- data_3 %>%
  mutate(outcome_grade = ifelse(is.na(grade_current), grade_final, grade_current))

summary(data_3$outcome_grade) 


# create NA variable
data_3 <- data_3 %>% 
  mutate(outcome_grade_NA = ifelse(grade_current_NA == 1, 1, 0))

# number of respondents
length(unique(data_3$ID_t)) # should be unchanged



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample Selection: Treatment and Outcome ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_4 <- data_3

# subset: keep only respondents who do not have any missing in treatment and
# grade 
data_4 <- data_4 %>% filter(!is.na(treatment_sport) & !is.na(outcome_grade))

num_id_adj_1 <- length(unique(data_4$ID_t)) 
drop_na <- num_id - num_id_adj_1

# plausible values: every value above 5.0 is implausible
# hence those are set NA; missing values are downward replaced; remaining
# missing values are deleted
summary(data_4$outcome_grade)
data_4 <- data_4 %>%
  mutate(outcome_grade = replace(outcome_grade, outcome_grade > 5, NA)) %>%
  group_by(ID_t) %>%
  fill(outcome_grade, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(outcome_grade))

summary(data_4$outcome_grade)
num_id_adj_2 <- length(unique(data_4$ID_t)) 
drop_grade <- num_id_adj_1 - num_id_adj_2

# drop grade variables
data_4 <- data_4 %>% dplyr::select(-starts_with("grade"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# ungroup data frame
data_4 <- data_4 %>% ungroup()

# ensure that they are no missing values for treatment and outcome (except freq)
colSums(is.na(data_4 %>% dplyr::select(starts_with("treatment_s"), starts_with("outcome"))))

# check for duplicates
sum(duplicated(data_4))

# print sample reduction
print(paste("Number of respondents before data preparation:", num_id))
print(paste("Number of respondents after dropping respondents with missing value in treatment:", num_id_adj_1))
print(paste("Number of respondents after dropping respondents with missing value in outcome:", num_id_adj_2))


# save
if (cohort_prep == "controls_same_outcome") {
  data_save <- paste0("Data/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, ".rds")  
} else {
  data_save <- paste0("Data/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")  
}



saveRDS(data_4, data_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "treatment_outcome",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl,
  "data_prep_treatment_def" = treatment_def,
  "num_id" = length(unique(data_4$ID_t)), 
  "num_rows" = nrow(data_4),
  "num_cols" = ncol(data_4),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "grade")
