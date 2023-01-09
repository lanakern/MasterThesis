#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE pTargetCAWI DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#++++
# by Lana Kern
#++++
# 1.) Load data and join with cohort profile
# -> CAWI data set is loaded
# -> Cohort Profile data set is loaded based on selection 
# -> CAWI and Cohort Profile are merged so that only respondents who are in 
# both data sets are kept.
# -> Missing values are replaced downwards
#++++
# 2.) Data Preparation (also depends on selected cohort profile step)
# -> In both types, missing values for treatment variable are also replaced
# upwards, if user selects this option.
# -> "controls_same_outcome": only treatment_starts variable is dropped as it
# is always NA because treatment period starts with CATI and ends with CAWI
# -> "controls_bef_outcome": treatment-outcome and control variables are
# prepared separately as they are measured in different survey. Then, they
# are merged via the treatment period indicator.
#++++
# --> Resulting data frame is panel data frame



#%%%%%%%%%%%%%#
#### Setup ####
#%%%%%%%%%%%%%#

# clear workspace
rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # load and install (if necessary) packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to manipulate data
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # to load and write excel files
# 
# # selection on treatment replacement
# treatment_repl <- "downup" # with any other selection, only downward replacement is made
# 
# # selection on cohort prepration
# cohort_prep <- "controls_bef_outcome" 
# #cohort_prep <- "controls_same_outcome"



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Load Data & Join with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# load data
data_target_cawi <- readRDS("Data/Prep_1/prep_1_target_cawi.rds")
  ## cohort profile depends on data preparation
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Prep_2/prep_2_cohort_profile_robustcheck.rds")
} else {
  stop("specify which cohort data preparation should be used")
}


# number of respondents
id_num_cawi <- length(unique(data_target_cawi$ID_t)) # 15,239 respondents
id_num_cohort <- length(unique(data_cohort_profile$ID_t)) 


# check if number of respondents are the same in both data sets
id_cawi <- unique(data_target_cawi$ID_t)
id_cohort <- unique(data_cohort_profile$ID_t)
setdiff(id_cohort, id_cawi) # should be "integer(0)"
sum(id_cohort %in% id_cawi) == length(id_cohort) # should be TRUE


# CAWI: keep only respondents who are also in data cohort
data_target_cawi <- data_target_cawi %>% filter(ID_t %in% id_cohort)
id_num_cawi_adj_1 <- length(unique(data_target_cawi$ID_t))

# indicator if treatment and outcome are NA
data_target_cawi <- data_target_cawi %>%
  mutate(
    sport_uni_NA = ifelse(is.na(sport_uni), 1, 0),
    sport_uni_freq_NA = ifelse(is.na(sport_uni_freq), 1, 0),
    grade_current_NA = ifelse(is.na(grade_current), 1, 0) 
  )

# fill missing values of CAWI: down here because cohort profile does not
# contain all waves anymore
# depending on selection missing values in treatment variable may also be
# replaced upwards
if (treatment_repl == "downup") {
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cawi), .direction = "down")
  
  # also upward replacement of treatment
  data_target_cawi <- data_target_cawi %>%
    group_by(ID_t) %>%
    fill(c(sport_uni, sport_uni_freq), .direction = "downup") %>% ungroup()

} else if (treatment_repl == "down") {
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(names(data_target_cawi), .direction = "down")
} else {
  repl_controls <- names(data_target_cawi)[!names(data_target_cawi) %in% c("sport_uni", "sport_uni_freq", "grade_current")]
  data_target_cawi <- data_target_cawi %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    fill(all_of(repl_controls), .direction = "down")
}


# merge cohort data to cawi data via inner_join:
# only keep respondents who are in both data sets (also done above)
data_cawi <- inner_join(
  data_target_cawi, 
  data_cohort_profile %>% 
    select(-c(starts_with("competence"))), 
  by = c("ID_t", "wave")
)
  ## examples
data_target_cawi %>% subset(ID_t == 7003857) %>% select(ID_t, wave)
data_cohort_profile %>% subset(ID_t == 7003857) %>% select(ID_t, wave, treatment_starts, treatment_ends)
data_cawi %>% subset(ID_t == 7003857) %>% select(ID_t, wave)

data_target_cawi %>% subset(ID_t == 7036384) %>% select(ID_t, wave)
data_cohort_profile %>% subset(ID_t == 7036384) %>% select(ID_t, wave)
data_cawi %>% subset(ID_t == 7036384) %>% select(ID_t, wave)


# extract number of respondents
id <- unique(data_cawi$ID_t)
id_num <- length(unique(data_cawi$ID_t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Further data preparation depends on the selection of cohort profile preparation


if (cohort_prep == "controls_same_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 1; Outcome at same time than Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # drop unnecessary columns and order
    ## treatment_starts is missing for all rows because treatment always starts
    ## with CATI interview (hence variable not necessary)
  data_cawi <- data_cawi %>% select(-treatment_starts) %>%
    select(ID_t, wave, wave_2, interview_date, treatment_ends, everything())
  
  # generate treatment_period variable
  data_cawi <- data_cawi %>% rename(treatment_period = treatment_ends, 
                                    interview_date_end = interview_date)
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    select(ID_t, treatment_period, interview_date_end, 
           starts_with("sport_"), grade_current, everything())
  
  
} else if (cohort_prep == "controls_bef_outcome") {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Preparation 2; Outcome after Controls ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # because control variables are taken from an earlier interview than the
  # outcome and treatment variable, they need to be prepared separately
  
  ## Outcome and Treatment ##
  
  # select treatment and outcome variables
  data_treatment_outcome <- data_cawi %>%
    select(ID_t, interview_date, treatment_ends,
           sport_uni, sport_uni_freq, grade_current) %>%
    subset(!is.na(treatment_ends)) %>% 
    rename(treatment_period = treatment_ends, interview_date_end = interview_date)

  
  length(unique(data_treatment_outcome$ID_t))
  
  
  
  ## Control Variables ##
  
  # keep only variables which are used for start of treatment period
  # drop outcome and treatment variables
  data_controls <- data_cawi %>%
    select(-c(sport_uni, sport_uni_freq, grade_current, treatment_ends, wave)) %>%
    subset(!is.na(treatment_starts)) %>%
    rename(treatment_period = treatment_starts, interview_date_start = interview_date)
  
  
  length(unique(data_controls$ID_t))
  
  ## Merge ##
  
  # treatment-outcome and control variable data frames are merged via treatment_period
  # data_treatment_outcome has less observations because data_controls contains
  # treatment starts for which no treatment ends exists
  data_cawi %>% subset(ID_t == 7036384) %>% select(ID_t, wave, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% select(ID_t, wave, starts_with("treatment"))
  
  
  data_cawi <- inner_join(
    data_treatment_outcome, data_controls, by = c("ID_t", "treatment_period")
  )
  
  # examples
  data_treatment_outcome %>% subset(ID_t == 7036384) %>% select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7036384) %>% select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7036384) %>% select(ID_t, starts_with("treatment"))
  
  data_treatment_outcome %>% subset(ID_t == 7002171) %>% select(ID_t, starts_with("treatment"))
  data_controls %>% subset(ID_t == 7002171) %>% select(ID_t, starts_with("treatment"))
  data_cawi %>% subset(ID_t == 7002171) %>% select(ID_t, starts_with("treatment"))
  
  
  # order columns
  data_cawi <- data_cawi %>% 
    select(ID_t, treatment_period, interview_date_start, interview_date_end, 
           starts_with("sport_"), grade_current, everything())
  
  length(unique(data_controls$ID_t))
  
  
} else {
  stop("specify which cohort data preparation should be used")
}



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# check for duplicates
sum(duplicated(data_cawi))

# check for missing values
colSums(is.na(data_cawi))

# number of rows, columns and respondents
print(paste("Number of respondents before data preparation:", id_num_cawi))
print(paste("Number of respondents after merge with cohort profile:", id_num_cawi_adj_1))
print(paste("Number of rows:", nrow(data_cawi)))
print(paste("Number of columns:", ncol(data_cawi)))

# save
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile_save <- paste0("Data/Prep_3/prep_3_cawi_treat", treatment_repl, ".rds")
} else {
  data_cohort_profile_save <- paste0("Data/Prep_3/prep_3_cawi_treat", treatment_repl, "_robustcheck.rds") 
}

saveRDS(data_cawi, data_cohort_profile_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "cawi",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl, 
  "num_id" = length(unique(data_cawi$ID_t)), 
  "num_rows" = nrow(data_cawi),
  "num_cols" = ncol(data_cawi),
  "time_stamp" = Sys.time()
)
  ## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save)