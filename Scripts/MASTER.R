#%%%%%%%%%%%%%%%%%%%#
#### MASTER FILE ####
#%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This master file is established to run all files in the respective order.
# Moreover, input parameters like the data preparation method can be 
# changed in this file once so that it is valid for all other files.
#+++

# clear workspace
rm(list = ls())


#### LOAD ALL PACKAGES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# install packages if needed, load packages
if (!require("readstata13")) install.packages("readstata13")
library(readstata13)  # to import stata (.dta) file into R (see data manual why this function is used)

if (!require("xlsx")) install.packages("xlsx")
library(xlsx)  # for saving and loading excel

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to work with missing values

if (!require("stringr")) install.packages("stringr")
library(stringr)  # to work with strings

if (!require("naniar")) install.packages("naniar")
library(naniar)  # to work with missing values

if (!require("purrr")) install.packages("purrr")
library(purrr) # for map_dfc() function

if (!require("zoo")) install.packages("zoo")
library(zoo)  # to transform time data

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to create a date variable

if (!require("sqldf")) install.packages("sqldf")
library(sqldf)  # for sql syntax

if (!require("fastDummies")) install.packages("fastDummies")
library(fastDummies)  # to generate dummy variables

if (!require("psych")) install.packages("psych")
library(psych)  # for Cronbach#s alpha

if (!require("mice")) install.packages("mice")
library(mice)  # for replacing missing values with mice()

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")

# keep_env <- rm(list = setdiff(
#   ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", 
#           "prep_sel_num", "aggr_vars", "cronbach_a")
#   ))

#### DEFINE INPUTS ####
#%%%%%%%%%%%%%%%%%%%%%#

# # cohort profile #
# cohort_prep <- "controls_bef_outcome" 
# #cohort_prep <- "controls_same_outcome"
# 
# # treatment: down and upward replacement #
# treatment_repl <- "no"
# #treatment_repl <- "downup" 
# #treatment_repl <- "down"
# 
# # treatment definition: all frequency levels or only weekly #
# #treatment_def <- "all"
# treatment_def <- "weekly"
# 
# # convert numeric variables into categorical
# convert_num_char <- "yes" 
# #convert_num_char <- "no"

aggr_vars <- "pca"
#aggr_vars <- "mean"
cronbach_a <- "yes" # "no"

# generate all possible combination of user inputs (to iterate over it below)
df_inputs <- data.frame(
  "cohort_prep" = c("controls_bef_outcome", "controls_same_outcome", NA),
  "treatment_repl" = c("down", "downup", "no"),
  "treatment_def" = c("weekly", "all", NA)
)

df_inputs <- df_inputs %>% expand(cohort_prep, treatment_repl, treatment_def) %>% na.omit()


#### LOAD ALL FUNCTIONS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

load_function <- paste0("Functions/", list.files(path = "Functions/"))
for (func_load in load_function) {
  source(func_load)
}


#### RUN DATA PREPARATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load all data files and make basic preparations like renaming variables and
# recoding missing values as NA
source("Scripts/01_Load_Data.R")

# Prepare episode / life course data, i.e., educational history of each respondents
source("Scripts/02_a_Prep_Data_Life_Course.R")

# Prepare treatment periods
for (cohort_prep_sel in unique(na.omit(df_inputs$cohort_prep))) {
  cohort_prep <- cohort_prep_sel
  print(cohort_prep)
  source("Scripts/02_b_Prep_Data_Interview_Participation.R")
}

# Prepare everything else
for (prep_sel_num in 1:nrow(df_inputs)) {
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  
  # Prepare individual data sets
  source("Scripts/03_a_Prep_Cati.R")
  source("Scripts/03_b_Prep_Cawi.R")
  source("Scripts/03_c_Prep_Sibling.R")
  source("Scripts/03_d_Prep_Child.R")
  source("Scripts/03_e_Prep_Partner.R")
  source("Scripts/03_f_Prep_Competencies.R")
  
  # Merge 
  source("Scripts/04_a_Merge_CATI_CAWI.R") # merge CATI & CAWI
  source("Scripts/04_b_Merge_Prepare_Episode.R") # add episode data
  source("Scripts/04_c_Merge_All.R") # add all other data sets
  
  # Prepare treatment and outcome
  source("Scripts/05_Create_Treatment_Outcome.R") 
  
  # Sample selection
  source("Scripts/06_Sample_Selection.R") 
  
  print(paste0("FINISHED COMBINATION", prep_sel_num))
  gc()
}



  # load file showing sample reduction
df_excel_save_hist <- read.xlsx("Data/SAMPLE_REDUCTION_STEPS.xlsx", sheetName = "Sheet1")
df_excel_save_hist


#
# Prepare control variables
# now set language for dates and times to English
Sys.setlocale("LC_TIME", "English")

# Plausibility analysis


# FINAL ESTIMATION SAMPLES (depends on model and treatment setting)



#### RUN DATA ANALYSIS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#


# Descriptive statistics


# Main drivers for selection



#### RUN ESTIMATION ####
#%%%%%%%%%%%%%%%%%%%%%%#


