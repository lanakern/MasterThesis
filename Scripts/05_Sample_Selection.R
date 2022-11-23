#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample and Variable Selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# ATTENTION: FREQUENCY NOT WORKING SO FAR

#++++
# by Lana Kern
#++++
# Only respondents are kept who take part in at least one extracurricular activity
# The code is generated flexible: 1.) Definition of extracurricular activity can
# be selected; 2.) If frequency in participating is relevant can be selected (-> see SETUP)
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

# for flexibel code, define inputs
  ## extracurricular activity type -> definition:
  ## 1: university; 2: outside university; 3: playing music, reading, political engagement
extra_incl <- c(1, 2, 3) # change number to include less and run code -> see how sample size changes
  ## frequency of participating: at least several times per month; otherwise individual
  ## is considered as not participating
extra_freq <- "yes"



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


# data %>% subset(extracurricular_leisure_3_freq == "once a month" | extracurricular_action_freq == "daily") %>%
#   select(ID_t, treatment_starts, extracurricular_leisure_3, extracurricular_leisure_3_freq, extracurricular_action, extracurricular_action_freq) 
# data[c(1:5, 80:83), ]


#### Frequency ####
#+++++++++++++++++#

if (extra_freq == "yes") {
  # extract all extracurricular frequency variables based on the extracurricular definition
  if (sum(extra_incl %in% c(1, 2)) == 2) {
    extra_cols_freq <- data %>% ungroup() %>% select(matches("extracurricular_.*_freq$")) %>% colnames()
  } else {
    if (1 %in% extra_incl) {
      extra_cols_freq <- data %>% ungroup() %>% select(matches("extracurricular_[^leisure].*_freq$")) %>% colnames()
    }
    if (2 %in% extra_incl) {
      extra_cols_freq <- data %>% ungroup() %>% select(matches("extracurricular_leisure_.*_freq$")) %>% colnames()
    }
  }
  
  # to not destroy original data
  data_sub <- data
  
  # iterate over the column to make replacements
  # NOTE: for NAs no replacements are made
  for (cols_sel in extra_cols_freq) {
    new_column_name <- str_split(cols_sel, "_freq", simplify = TRUE)[, 1]
    data_sub <- data_sub %>%
      mutate(
        {{new_column_name}} := case_when(
          !!sym(cols_sel) %in% c("once a month", "less frequently") ~ "not involved",
          TRUE ~ !!sym(new_column_name)
        )
      )
  }
  
} else {
  data_sub <- data
}



#### Definition ####
#++++++++++++++++++#


# generate sub data frame to not destroy original data 
#data_sub <- data
data_keep <- data.frame()

# subset based on selection
  ## extracurricular activity at university: at least in one involved
if (1 %in% extra_incl) {
  data_keep_1 <- data_sub %>%
    filter_at(
      # no frequency and no leisure 
      vars(matches("extracurricular_") & matches("extracurricular_[^_leisure]") & !ends_with("freq")), 
      any_vars(. == "involved")
    ) %>%
    # ID_t and treatment_starts is sufficient for selection later via merge
    select(ID_t, treatment_starts)
  
  data_keep <- rbind(data_keep, data_keep_1)
} 
  ## extracurricular activity outside university: at least in one involved
if (2 %in% extra_incl) {
  data_keep_2 <- data_sub %>%
    filter_at(
      # no frequency and no leisure 
      vars(matches("extracurricular_leisure_[0-9]$") & !ends_with("freq")), 
      any_vars(. == "involved")
    ) %>%
    select(ID_t, treatment_starts)
  
  data_keep <- rbind(data_keep, data_keep_2)
}
  ## extracurricular activity such as reading: at least in one involved
if (3 %in% extra_incl) {
  data_keep_3 <- data_sub %>%
    mutate(
      extracurricular_leisure_gen = case_when(
        interest_music_play == "yes" ~ "involved",
        #interest_music_classic == "yes" ~ "involved",
        interest_reading_leisure > 0 ~ "involved",
        interest_politics_signatures == "yes" ~ "involved",
        interest_politics_demo == "yes" ~ "involved",
        interest_politics_discussion != "rarely or never" & !is.na(interest_politics_discussion) ~ "involved",
        !is.na(spell_length_current_Emp) ~ "involved",
        TRUE ~ "not involved"
      )
    ) %>%
    filter_at(
      vars("extracurricular_leisure_gen"), 
      any_vars(. == "involved")
    ) %>%
    select(ID_t, treatment_starts)
  
  data_keep <- rbind(data_keep, data_keep_3)
}

data_keep <- data_keep %>% distinct()

data_sub <- inner_join(
  data_sub, data_keep, by = c("ID_t", "treatment_starts")
)



## COMPARE SAMPLE SIZES ##
#++++++++++++++++++++++++#

# number of respondents, number of rows, and number of columns before sample selection
print("BEFORE SAMPLE SELECTION:")
print(paste("Number of respondents:", length(unique(data$ID_t))))
print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))


# number of respondents, number of rows, and number of columns after sample selection
print("AFTER SAMPLE SELECTION:")
print(paste("Number of respondents:", length(unique(data_sub$ID_t))))
print(paste("Number of rows:", nrow(data_sub)))
print(paste("Number of columns:", ncol(data_sub)))



#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#


# save data frame
saveRDS(data_sub, "Data/prep_5_sample_selection.rds")

