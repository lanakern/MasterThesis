#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER FILE FOR DATA PREPARATION: PERSONALITY AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This master file is established to run all files in the respective order.
# Moreover, input parameters like the data preparation method can be 
# changed in this file once so that it is valid for all other files.
#+++

# clear workspace
rm(list = ls())


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### LOAD ALL PACKAGES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# install packages if needed, load packages
if (!require("readstata13")) install.packages("readstata13")
library(readstata13)  # to import stata (.dta) file into R (see data manual why this function is used)

if (!require("xlsx")) install.packages("xlsx")
library(xlsx)  # for saving and loading excel

if (!require("data.table")) install.packages("data.table")
library(data.table)  # for data.table

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

if (!require("reshape2")) install.packages("reshape2")
library(reshape2)  # for converting data frame from long to wide format

if (!require("tidymodels")) install.packages("tidymodels")
library(tidymodels)  # for machine learning (prediction of nuisance functions)

if (!require("rsample")) install.packages("rsample")
library(rsample)  # for k-fold cross-fitting (group_vfold_cv )

if (!require("glmnet")) install.packages("glmnet")
library(glmnet)  # for lasso

# if (!require("hdm")) install.packages("hdm")
# library(hdm)  # for rlasso() (-> post-lasso)

if (!require("xgboost")) install.packages("xgboost")
library(xgboost)  # for xgboost()

if (!require("randomForest")) install.packages("randomForest")
library(randomForest)  # for xgboost()

if (!require("DoubleML")) install.packages("DoubleML")
library(DoubleML)  # for applying DML with function

if (!require("mlr3")) install.packages("mlr3")
library(mlr3)  # for applying DML with function

# ONLY DONE IN FILE 12_d because it makes troubles!
# if (!require("devtools")) install.packages("devtools")
# library(devtools)  # for install_github()
# install_github(repo = "MCKnaus/dmlmt") # download package
# library(dmlmt) # for DML in multivalued treatment setting

if (!require("caret")) install.packages("caret")
library(caret)  # for confusingMatrix()

if (!require("janitor")) install.packages("janitor")
library(janitor)  # for remove_constant() (-> dropping constant variables)

if (!require("pROC")) install.packages("pROC")
library(pROC) # for multiclass AUC

if (!require("purrr")) install.packages("purrr")
library(purrr) # for reduce function (union of variables)

if (!require("MASS")) install.packages("MASS")
library(MASS) # for ginv() function (-> weights for covariate balancing)

# if (!require("cobalt")) install.packages("cobalt")
# library(cobalt) # for bal.tab() function (covariate balance assessment)

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")

# do not display warning messages
options(warn = -1)


#%%%%%%%%%%%%%%%%%%%%%#
#### DEFINE INPUTS ####
#%%%%%%%%%%%%%%%%%%%%%#


# define main model
main_cohort_prep <- "controls_same_outcome"
main_treatment_def <- "weekly"
main_treatment_repl <- "down"
main_extra_act <- "yes"
main_model_treatment <- "binary"
main_model_type <- "all"
main_model_k <- 4
main_model_k_tuning <- 2
main_model_s_rep <- 2
main_model_trimming <- 0.01
main_model_outcome <- "stand"
main_model_controls <- "no_lags"

# generate all possible combinations of user inputs (to iterate over it below)
df_inputs <- data.frame(
  # for interview data preparation
  "cohort_prep" = c("controls_bef_outcome", rep("controls_same_outcome", 5)), 
  # for treatment and outcome missing value replacement
  "treatment_repl" = c("down", "down", "down", "down", "onelag", "no"),
  # for treatment generation
  "treatment_def" = c("weekly", "all", "weekly", "weekly", "weekly", "weekly"),
  # for sample selection: only keeping respondents with extracurricular activity
  "extra_act" = c("yes", "yes", "no", "yes", "yes", "yes")
)

# aggregation of variables
aggr_vars <- "pca" # "mean" (not used anymore)
cronbach_a <- "yes" # "no" (not used anymore)


# define variables which may be endogenous
vars_endogenous <- c("educ_school_grade_math", "educ_school_grade_ger", "educ_school_grade_final")


# define variables to keep after deleting environment
keep_after_file_run <- 'rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", 
"treatment_def", "extra_act", "prep_sel_num", "aggr_vars", "cronbach_a",
"df_inputs", "df_inputs_indiv", "df_inputs_dml", "df_inputs_dml_lasso",
 "keep_after_file_run", "vars_baseline", "model_type", "model_algo", 
"model_k", "model_k_tuning", "model_s_rep", "model_trimming", "model_controls", 
"model_outcome", "dml_num",  ls()[str_starts(ls(), "func_")], ls()[str_starts(ls(), "main_")])))'




#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### LOAD ALL FUNCTIONS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

load_function <- paste0("Functions/", list.files(path = "Functions/"))
for (func_load in load_function) {
  source(func_load)
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### All Files  ####
#++++++++++++++++++#

# Load all data files and make basic preparations like renaming variables and
# recoding missing values as NA
source("Scripts/Personality/01_Load_Data_Personality.R")
eval(parse(text = keep_after_file_run))


#### Episode Data ####
#++++++++++++++++++++#

# Prepare episode / life course data, i.e., educational history of each respondents
source("Scripts/Personality/02_a_Prep_Data_Life_Course_Personality.R")
eval(parse(text = keep_after_file_run))


#### Treatment Periods ####
#+++++++++++++++++++++++++#

# Prepare treatment periods / cohort profile
for (cohort_prep_sel in unique(na.omit(df_inputs$cohort_prep))) {
  cohort_prep <- cohort_prep_sel
  print(cohort_prep)
  source("Scripts/Personality/02_b_Prep_Data_Interview_Participation_Personality.R")
  eval(parse(text = keep_after_file_run))
}


#### Cati & Cawi ####
#+++++++++++++++++++#

# Prepare CATI and CAWI: iteration over cohort_prep and treatment_repl
# Note: generated data sets differ across treatment_repl but number of students,
# rows and columns only differ across cohort_prep

df_inputs_indiv <- df_inputs %>% dplyr::select(cohort_prep, treatment_repl) %>% distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {

  # select data preparation possibilities
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ] # subset data
  cohort_prep <- df_inputs_sel$cohort_prep # select cohort prep preparation
  treatment_repl <- df_inputs_sel$treatment_repl # select treatment/outcome replacement

  # Prepare individual data sets
  source("Scripts/Personality/03_a_Prep_Cati_Personality.R") # CATI
  eval(parse(text = keep_after_file_run))

  source("Scripts/Personality/03_b_Prep_Cawi_Personality.R") # CAWI
  eval(parse(text = keep_after_file_run))

  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}


#### Other individual data sets ####
#++++++++++++++++++++++++++++++++++#

# Prepare all other individual data sets: only iteration over cohort_prep 
# THE INDIVIDUAL DATA SETS ARE INDEPENDENTLY PREPARED FROM CATI & CAWI
# DURING THE MERGE PROCESS THE NUMBER OF OBS IS ADJUSTED

for (cohort_prep_sel in unique(na.omit(df_inputs$cohort_prep))) {
  
  # select cohort prep
  cohort_prep <- cohort_prep_sel # select cohort prep preparation
  
  print(cohort_prep)
  
  # Prepare individual data sets
  source("Scripts/Personality/03_c_Prep_Sibling_personality.R") # Sibling
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Personality/03_d_Prep_Child_personality.R") # Child
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Personality/03_e_Prep_Partner_personality.R") # Partner
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Personality/03_f_Prep_Competencies_personality.R") # Competencies
  eval(parse(text = keep_after_file_run))
  
  gc()
}


#### Merge ####
#+++++++++++++#

# Merge all individual data sets: iterate over cohort_prep and treatment_repl
# Note: generated data sets differ across treatment_repl but number of students,
# rows and columns only differ across cohort_prep

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  # select data preparation possibilities
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ] # subset data
  cohort_prep <- df_inputs_sel$cohort_prep # select cohort prep preparation
  treatment_repl <- df_inputs_sel$treatment_repl # select treatment/outcome replacement
  
  # Merge 
  source("Scripts/Personality/04_a_Merge_CATI_CAWI_Personality.R") # merge CATI & CAWI
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Personality/04_b_Merge_Prepare_Episode_Personality.R") # add episode data
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Personality/04_c_Merge_All_Personality.R") # add all other data sets
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}


#### Treatment and Outcome ####
#+++++++++++++++++++++++++++++#

# Prepare treatment and outcome 
# Here I iterate over all combinations except extracurricular activity
df_inputs_indiv <- df_inputs %>% 
  dplyr::select(cohort_prep, treatment_repl, treatment_def) %>% 
  distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  
  # Prepare treatment and outcome
  source("Scripts/Personality/05_Create_Treatment_Outcome_Personality.R") 
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}


#### Sample Selection ####
#++++++++++++++++++++++++#

# Conduct sample selection
# Here I iterate over all combinations
for (prep_sel_num in 1:nrow(df_inputs)) {
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Sample selection
  source("Scripts/Personality/06_Sample_Selection_Personality.R") 
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  gc()
}



#### Create Variables ####
#++++++++++++++++++++++++#

# now set language for dates and times to English
Sys.setlocale("LC_TIME", "English")


# perform further steps without sample selection reduction
for (prep_sel_num in 1:nrow(df_inputs)) {
  
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Prepare control variables
  eval(parse(text = keep_after_file_run))
  source("Scripts/Personality/07_Create_Control_Variables_Personality.R") 
  
  print(paste0("FINISHED COMBINATION", prep_sel_num, " FROM ", nrow(df_inputs)))
  gc()
}



#### Plausibility analysis ####
#+++++++++++++++++++++++++++++#

for (prep_sel_num in 1:nrow(df_inputs)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Prepare control variables
  source("Scripts/Personality/08_Plausibility_Checks_Personality.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  eval(parse(text = keep_after_file_run))
  gc()
}


#### Descriptive Statistics ####
#++++++++++++++++++++++++++++++#

gc()
eval(parse(text = keep_after_file_run))

# only for main model
cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
source("Scripts/Personality/09_Descriptive_Statistics_Personality.R") 



#### Final Estimation Samples ####
#++++++++++++++++++++++++++++++++#

for (prep_sel_num in 1:nrow(df_inputs)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Prepare control variables
  source("Scripts/Personality/10_Estimation_Sample_Personality.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  eval(parse(text = keep_after_file_run))
  gc()
}


#### Show Sample Reduction ####
#+++++++++++++++++++++++++++++#

read.xlsx("Output/SAMPLE_REDUCTION_STEPS_PERSONALITY.xlsx", sheetName = "Sheet1")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

