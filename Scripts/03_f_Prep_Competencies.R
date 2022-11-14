#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE COMPETENCE DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the information about competence measures is prepared.
# Competence measure tests are only conducted in wave 1 (2010/2011 (CATI+competencies)), 
# wave 5 (2013 (CATI+competencies)), and wave 12 (2017 (CATI)).
# Moreover, not all competence measures are assessed in each wave.
# In total 11,810 individuals take part in competence measure tests.
#++++
# 1.) The competence measure data set is in wide-format, i.e., one row per
# respondent. I restructure the data set in long-format, i.e., one row per
# respondent + wave. This is necessary for the merge with cohort profile and
# for the further analysis.
#++++
# 2.) Merge with cohort profile. First a right join is conducted, to keep
# all respondents in cohort profile. This is necessary to downward replace
# the results of the competence measure tests to further survey in step 3.).
# Next, only respondents who are in both data sets, cohort profile and
# competence measures, are kept. This reduces the sample size to 10,226 respondents.
#++++
# 3.) Handle missing values: 
# 3.1) Downward replacement of missing values. For example, respondent has info
# on competence measure mathematics in wave 1. The next competence measure test
# on mathematics is conducted in wave 12. Hence, from wave 2 to wave 11, the
# values from wave 1 are used.
# 3.2) Domain-general competencies are paper-, computer-based or online. Missing
# values are repalced across those survey methods.
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

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to transform time data and work with dates

if (!require("stringr")) install.packages("stringr")
library(stringr)  # to work with strings

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # for fill() function

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")




#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#

# competencies
data_competencies <- readRDS("Data/Prep_1/prep_1_competencies.rds")
num_id_comp <- length(unique(data_competencies$ID_t))

# cohort profile
data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")
num_id_prof <- length(unique(data_cohort_profile$ID_t))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Restructure Competencies Data Set ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# competence data set is in wide-format; I want to have it in long format
  ## vector containing waves
wave_comp <- c("2010/2011 (CATI+competencies)", "2013 (CATI+competencies)", "2017 (CATI)")
  ## iterator to extract respective wave information from wave_comp
iter_num <- 0
  ## data set for merge (includes only ID and missing wave variable)
data_competencies_long <- 
  data_competencies %>% select(ID_t) %>% mutate(wave = NA)

# loop over wave 1, wave 5, and wave 12
for (wave_sel in c("w1", "w5", "w12")) {
  
  # adjust iterator
  iter_num <- iter_num + 1
  
  # create data set for wave selected by wave_sel
  data_competencies_sub_wave <- 
    data_competencies %>%
      # keep only individuals who participated in competence measures in wave 1
      filter((!!sym(paste0("wave_", wave_sel))) == "yes") %>%
      # keep only wave 1 variables
      select(ID_t, ends_with(wave_sel)) %>%
      # omit individuals with at least one missing value
      # na.omit() %>%
      # adjust wave variable
      rename(wave = paste0("wave_", wave_sel)) %>%
      mutate(wave = wave_comp[iter_num]) %>%
      # drop suffix of variables
      rename_with(~ str_remove(., paste0("_", wave_sel)))
  
  # define columns for merge
    ## columns which are in both data sets except wave
  col_merge <- 
    intersect(colnames(data_competencies_long), colnames(data_competencies_sub_wave)) 
  col_merge <- col_merge[col_merge != "wave"]
  
  # merge
  data_competencies_long <- 
    full_join(
      data_competencies_long, data_competencies_sub_wave, by = col_merge
    ) %>%
    # wave variables are duplicated -> create one
    mutate(wave.x = if_else(is.na(wave.x), wave.y, as.character(wave.x))) %>%
    select(-wave.y) %>%
    rename(wave = wave.x)
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# right join: keeps all information in cohort profile
# this ensures that I can downward replace competence measures across waves
length(unique(data_competencies_long$ID_t)) # 11,810
data_competencies_final <- right_join(
  data_competencies_long, data_cohort_profile %>% select(ID_t, wave, interview_date), 
  by = c("ID_t", "wave")
)
length(unique(data_competencies_final$ID_t)) # 12,670

# keep only individuals who have at least taken part once in the
# competence measures
data_competencies_final <- data_competencies_final %>%
  subset(ID_t %in% unique(data_competencies_long$ID_t))
length(unique(data_competencies_final$ID_t)) # 10226

# -> 11,810 individuals have taken part at least once in a competence measure
# survey. 
# -> 12,670 individuals are in cohort profile (have taken part in several CATI and CAWI surveys)
# -> 10,226 individuals are in both data sets: competence and cohort profile


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Handle Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# drop individuals with missing values in wave
# those individuals did not take part in the competence measures
data_competencies_final <- data_competencies_final %>%
  filter(!is.na(wave))
length(unique(data_competencies_final$ID_t))

# replace missing values downwards
data_competencies_final <- 
  data_competencies_final %>% 
  group_by(ID_t) %>%
  fill(data_competencies_final %>% select(-c(ID_t, wave)) %>% colnames(), 
       .direction = "down")

# domain-general competencies can be paper-based, computer-based or online.
# hence, missing values are replaced across those survey possibilities
colSums(is.na(data_competencies_final))
data_competencies_final <- data_competencies_final %>%
  mutate(
    # reasoning
    reasoning_sum = ifelse(is.na(reasoning_paper_sum), reasoning_comp_sum, reasoning_paper_sum),
    reasoning_sum = ifelse(is.na(reasoning_sum), reasoning_online_sum, reasoning_sum), 
    # perceptual speed
    percspeed_sum = ifelse(is.na(percspeed_paper_sum), percspeed_comp_sum, percspeed_paper_sum),
    percspeed_sum = ifelse(is.na(reasoning_sum), percspeed_online_sum, reasoning_sum)
  ) %>%
  select(-c(matches(".*_comp_.*"), matches(".*_paper_.*"), matches(".*_online_.*")))
colSums(is.na(data_competencies_final))



# Ideas: for missing values in WLE 0 is inserted (average)
# for missing values in share 0.5
# for missing values in sum average
# -> OR: use plausible values



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# sort data frame
data_competencies_final <- data_competencies_final %>%
  arrange(ID_t, wave)

# sample size: 10,266
print(paste("The final sample size includes", 
            length(unique(data_competencies_final$ID_t)),
            "respondents."))

# number of rows and columns
print(paste("Number of rows", nrow(data_competencies_final)))
print(paste("Number of columns", ncol(data_competencies_final)))

# save prepared competence data
saveRDS(data_competencies_final, "Data/Prep_3/prep_3_competencies.rds")


