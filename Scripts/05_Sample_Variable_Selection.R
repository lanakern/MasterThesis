#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample and Variable Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++
# by Lana Kern
#++++
# 1.) Only respondents are kept who take part in at least one extracurricular activity
#++++
# 2.) Only variables are kept which have less than 70% of missing values
#++++



#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

# load data
data <- readRDS("Data/prep_4_merge.rds")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# extracurricular activities outside the university (extracurricular_number). Those
# include for instance participating in a music group or neighborhood help.
# I also consider other extracurricular activities, in particular
# playing music (interest_music_play and/or interest_music_class equals "yes"), 
# reading books (interest_reading_leisure > 0), being politically engaged
# (interest_politics_signatures and/or interest_politics_demo equals "yes" and/or
# interest_politics_discussion != "rarely or never"), or working during study
# (spell_length_current_Emp != NA)
# -> all those activities are voluntary during university studies and lead to
# less time for studying
print(paste("Number of respondents:", length(unique(data$ID_t))))
print(paste("Number of rows:", nrow(data)))


# Filter based on extracurricular activity: if not at least one "involved" or
# only NA treatment period is dropped OR subset on the other activities (for those
# dummy variable is generated for easier computation)
data_sub <- data %>%
  mutate(
    extracurricular_leisure_gen = case_when(
      interest_music_play == "yes" ~ "involved",
      interest_music_classic == "yes" ~ "involved",
      interest_reading_leisure > 0 ~ "involved",
      interest_politics_signatures == "yes" ~ "involved",
      interest_politics_demo == "yes" ~ "involved",
      interest_politics_discussion != "rarely or never" & !is.na(interest_politics_discussion) ~ "involved",
      !is.na(spell_length_current_Emp) ~ "involved",
      TRUE ~ "not involved"
    )
  ) %>%
  filter_at(
    vars(matches("extracurricular_leisure_[0-9]$") | "extracurricular_leisure_gen"), 
     any_vars(. == "involved")
  )

print(paste("Number of respondents:", length(unique(data_sub$ID_t))))
print(paste("Number of rows:", nrow(data_sub)))





#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### VARIABLE SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#



