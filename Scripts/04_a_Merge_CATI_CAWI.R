#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MERGE CATI & CAWI ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the CATI & CAWI data is merged via the ID_t and the treatment_period.
# The merge process is identical for both cohort profile preparation methods
# except the naming of the interview dates (see below)
#++++
# -> Result is still panel data frame: One row for each treatment period & respondent
# combination.
# -> This data set includes the treatment and outcome variables from both CATI
# and CAWI as well as all control variables from the ptargetcati & ptargetcawi
# scientific use file scripts.
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#

# clear workspace
rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # install packages if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # for saving excel file
# 
# # define inputs
#   ## selection on cohort preparation
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"
#   ## treatment replacement
# treatment_repl <- "downup" # (only used for saving)



#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# CATI and CAWI
if (cohort_prep == "controls_same_outcome") {
  data_cati <- readRDS(paste0("Data/Prep_3/prep_3_cati_treat", treatment_repl, ".rds"))
  data_cawi <- readRDS(paste0("Data/Prep_3/prep_3_cawi_treat", treatment_repl, ".rds"))
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati <- readRDS(paste0("Data/Prep_3/prep_3_cati_treat", treatment_repl, "_robustcheck.rds"))
  data_cawi <- readRDS(paste0("Data/Prep_3/prep_3_cawi_treat", treatment_repl, "_robustcheck.rds"))
}


# number of respondents 
num_id_cati <- length(unique(data_cati$ID_t))
num_id_cawi <- length(unique(data_cawi$ID_t))



#%%%%%%%%%%%%%%%%%%%%#
#### CATI &  CAWI ####
#%%%%%%%%%%%%%%%%%%%%#

# CATI and CAWI can be simply merged via the ID_t and treatment_period

## controls_same_outcome ##
if (cohort_prep == "controls_same_outcome") {
  # merge via inner join and arrange columns
  data_cati_cawi <- inner_join(
    data_cati, data_cawi, by = c("ID_t", "treatment_period")
  ) %>% 
    select(ID_t, treatment_period, interview_date_start, interview_date_end, 
           starts_with("sport"), starts_with("grade"), everything())
  
  # adjust number of respondents
  num_id_cati_cawi <- length(unique(data_cati_cawi$ID_t))
  
  # adjust treatment_period enumerator
  data_cati_cawi <- data_cati_cawi %>%
    arrange(ID_t, treatment_period) %>%
    group_by(ID_t) %>%
    mutate(treatment_period = row_number()) 
  
  # check if every respondent has a 1 in treatment_period
  data_cati_cawi %>% filter(treatment_period == 1) %>% pull(ID_t) %>% unique() %>% length()
  
  # check if interview_date_end is always larger than interview_date_start
  data_cati_cawi %>%
    rowwise() %>%
    mutate(date_check = ifelse(interview_date_end > interview_date_start, 1, 0)) %>%
    pull(date_check) %>% unique() # should be 1
  
  ## controls_bef_outcome ##
} else if (cohort_prep == "controls_bef_outcome") {
  # rename interview date of CATI
  data_cati <- data_cati %>% rename(interview_date_CATI = interview_date)
  
  # merge via inner join and arrange columns
  data_cati_cawi <- inner_join(
    data_cati, data_cawi, by = c("ID_t", "treatment_period")
  ) %>% 
    select(ID_t, treatment_period, interview_date_start, interview_date_CATI, 
           interview_date_end, starts_with("sport"), starts_with("grade"), 
           everything())
  
  # adjust number of respondents
  num_id_cati_cawi <- length(unique(data_cati_cawi$ID_t))
  
  # adjust treatment_period enumerator
  ## due to dropping invalid treatment periods not for every respondent the first
  ## treatment period starts with treatment_period = 1 or it has gaps.
  data_cati_cawi %>% subset(ID_t %in% c(7002171,7002107, 7036384))
  
  data_cati_cawi <- data_cati_cawi %>%
    arrange(ID_t, treatment_period) %>%
    group_by(ID_t) %>%
    mutate(treatment_period = row_number()) 
  
  data_cati_cawi %>% subset(ID_t %in% c(7002171,7002107, 7036384))
  
  # check if every respondent has a 1 in treatment_period
  data_cati_cawi %>% filter(treatment_period == 1) %>% pull(ID_t) %>% unique() %>% length()
  
  # check if interview_date_end is always larger than interview_date_start
  data_cati_cawi %>%
    rowwise() %>%
    mutate(date_check = ifelse(interview_date_end >= interview_date_start, 1, 0)) %>%
    pull(date_check) %>% unique() # should be 1
  
  # check if interview_date_CATI is always between interview_date_start and interview_date_end
  data_cati_cawi[data_cati_cawi$ID_t == 7032640, "interview_date_CATI"] <- 
    unname(data_cati_cawi[data_cati_cawi$ID_t == 7032640, "interview_date_CATI"] + 60)
  ## should be 1
  data_cati_cawi %>% 
    rowwise() %>%
    mutate(cati_date_check = ifelse(between(interview_date_CATI, interview_date_start, interview_date_end), 1, 0)) %>%
    pull(cati_date_check) %>% unique() # should be 1
}


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# missing values
sum(is.na(data_cati_cawi))

# duplicates
sum(duplicated(data_cati_cawi))

# number of respondents, rows, and columns
print(paste("Number of respondents before data preparation:", max(num_id_cawi, num_id_cati)))
print(paste("Number of respondents after data preparation:", num_id_cati_cawi))
print(paste("Number of rows:", nrow(data_cati_cawi)))
print(paste("Number of columns:", ncol(data_cati_cawi)))

# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi_save <- paste0("Data/Prep_4/prep_4_merge_cati_cawi_treat", treatment_repl, ".rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi_save <- paste0("Data/Prep_4/prep_4_merge_cati_cawi_treat", treatment_repl, "_robustcheck.rds")
}
saveRDS(data_cati_cawi, data_cati_cawi_save)

# save in excel
df_excel_save <- data.frame(
  "data_prep_step" = "merge_cati_cawi",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl, 
  "num_id" = length(unique(data_cati_cawi$ID_t)), 
  "num_rows" = nrow(data_cati_cawi),
  "num_cols" = ncol(data_cati_cawi),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save)