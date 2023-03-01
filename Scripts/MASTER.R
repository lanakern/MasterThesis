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

if (!require("matlib")) install.packages("matlib")
library(matlib) # for inv() function (-> weights for covariate balancing)


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
  "cohort_prep" = c("controls_bef_outcome", "controls_same_outcome", NA), 
  # for treatment and outcome missing value replacement
  "treatment_repl" = c("down", "onelag", "no"),
  # for treatment generation
  "treatment_def" = c("weekly", "all", NA),
  # for sample selection: only keeping respondents with extracurricular activity
  "extra_act" = c("yes", "no", NA)
)

df_inputs <- df_inputs %>% tidyr::expand(cohort_prep, treatment_repl, treatment_def, extra_act) %>% na.omit()

# only 5 combinations are considered; otherwise it is a too high computational burden
df_inputs <- df_inputs %>% filter(
  (cohort_prep == "controls_bef_outcome" & treatment_repl == main_treatment_repl & treatment_def == main_treatment_def & extra_act == main_extra_act) |
    (cohort_prep == "controls_same_outcome" & treatment_repl == main_treatment_repl & treatment_def == main_treatment_def & extra_act == main_extra_act) |
      (cohort_prep == "controls_same_outcome" & treatment_repl == main_treatment_repl & treatment_def == main_treatment_def & extra_act == "no") |
      (cohort_prep == "controls_same_outcome" & treatment_repl == "no" & treatment_def == main_treatment_def & extra_act == main_extra_act) |
      (cohort_prep == "controls_same_outcome" & treatment_repl == main_treatment_repl & treatment_def == "all" & extra_act == main_extra_act)
  )

# aggregation of variables
aggr_vars <- "pca" # "mean" (not used anymore)
cronbach_a <- "yes" # "no" (not used anymore)


# all possible DML combinations
# df_inputs_dml <- data.frame(
#   model_treatment = c("binary", NA, NA, NA),
#   model_type = c("base", NA, NA, NA),
#   model_algo = c("lasso", "postlasso", "randomforests", "xgboost"),
#   model_k = c(5, NA, NA, NA),
#   model_k_tuning = c(5, NA, NA, NA),
#   model_s_rep = c(2, NA, NA, NA),
#   model_trimming = c(0.01, 0.1, "min-max", NA),
#   model_outcome = c("level", "stand", NA, NA),
#   model_controls = c("all", "no_lags", NA, NA)
# )

df_inputs_dml <- data.frame(
  model_treatment = c("binary", NA, NA, NA),
  model_type = c("all", NA, NA, NA),
  model_algo = c("lasso", "postlasso", "randomforests", "xgboost"),
  model_k = c(4, NA, NA, NA),
  model_k_tuning = c(2, NA, NA, NA),
  model_s_rep = c(2, NA, NA, NA),
  model_trimming = c(0.01, NA, NA, NA),
  model_outcome = c("stand", NA, NA, NA),
  model_controls = c("no_lags", NA, NA, NA)
)

df_inputs_dml <- df_inputs_dml %>% 
  tidyr::expand(model_treatment, model_type, model_algo, model_k, model_k_tuning,
                model_s_rep, model_trimming, model_outcome, model_controls) %>% na.omit()




# define variables for baseline model
vars_baseline <- 'select(
group, starts_with("outcome"), starts_with("treatment"),

interview_start_year_num, interview_end_year_num,

gender_male, age, birth_year_num, birth_country_ger, birth_ger_eastwest_west,
migration, childhood_biological_parents, religion_christian,
bilingual, kindergarden,

starts_with("health"),

educ_years_total, educ_years_current_uni, starts_with("educ_school_degree"),
starts_with("educ_school_grade"), educ_school_rep_grade,

starts_with("motivation_degree"), starts_with("uni_degree_importance_well"), starts_with("uni_major"),
starts_with("uni_fear"), starts_with("uni_quali"), starts_with("uni_time"), uni_type_uni_general, 

starts_with("interest_art_muesum"), interest_math, interest_music_play, interest_politics, interest_reading_leisure, 

starts_with("comp_"), 
emp_current, emp_current_act_work_hours, 

starts_with("mother_degree_highest"), starts_with("father_degree_highest"), mother_emp_bef_15y, father_emp_bef_15y, 
starts_with("mother_emp_prof_"), starts_with("father_emp_prof_"), starts_with("parents"),
starts_with("friends_study_share"), 

starts_with("personality"), starts_with("bigfive"), 

starts_with("satisfaction_life"), starts_with("satisfaction_study"), starts_with("social_integr"), 

sibling_total, partner_current, child, living_alone, living_hh_size, starts_with("place_residence")

)'


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
source("Scripts/01_Load_Data.R")
eval(parse(text = keep_after_file_run))


#### Episode Data ####
#++++++++++++++++++++#

# Prepare episode / life course data, i.e., educational history of each respondents
source("Scripts/02_a_Prep_Data_Life_Course.R")
eval(parse(text = keep_after_file_run))


#### Treatment Periods ####
#+++++++++++++++++++++++++#

# Prepare treatment periods / cohort profile
for (cohort_prep_sel in unique(na.omit(df_inputs$cohort_prep))) {
  cohort_prep <- cohort_prep_sel
  print(cohort_prep)
  source("Scripts/02_b_Prep_Data_Interview_Participation.R")
  eval(parse(text = keep_after_file_run))
}


#### Cati & Cawi ####
#+++++++++++++++++++#

# Prepare CATI and CAWI: iteration over cohort_prep and treatment_repl
# Note: generated data sets differ across treatment_repl but number of students,
# rows and columns only differ across cohort_prep
df_inputs_indiv <- df_inputs %>% select(cohort_prep, treatment_repl) %>% distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  # select data preparation possibilities
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ] # subset data
  cohort_prep <- df_inputs_sel$cohort_prep # select cohort prep preparation
  treatment_repl <- df_inputs_sel$treatment_repl # select treatment/outcome replacement
  
  # Prepare individual data sets
  source("Scripts/03_a_Prep_Cati.R") # CATI
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/03_b_Prep_Cawi.R") # CAWI
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
  source("Scripts/03_c_Prep_Sibling.R") # Sibling
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/03_d_Prep_Child.R") # Child
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/03_e_Prep_Partner.R") # Partner
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/03_f_Prep_Competencies.R") # Competencies
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
  source("Scripts/04_a_Merge_CATI_CAWI.R") # merge CATI & CAWI
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/04_b_Merge_Prepare_Episode.R") # add episode data
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/04_c_Merge_All.R") # add all other data sets
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}



#### Treatment and Outcome ####
#+++++++++++++++++++++++++++++#

# Prepare treatment and outcome 
# Here I iterate over all combinations except extracurricular activity
df_inputs_indiv <- df_inputs %>% 
  select(cohort_prep, treatment_repl, treatment_def) %>% 
  distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  
  # Prepare treatment and outcome
  source("Scripts/05_Create_Treatment_Outcome.R") 
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
  source("Scripts/06_Sample_Selection.R") 
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION", prep_sel_num, " FROM ", nrow(df_inputs)))
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
  source("Scripts/07_Create_Control_Variables.R") 
  
  print(paste0("FINISHED COMBINATION", prep_sel_num, " FROM ", nrow(df_inputs)))
  gc()
}


# load file showing sample reduction
df_excel_save_hist <- read.xlsx("Output/SAMPLE_REDUCTION_STEPS.xlsx", sheetName = "Sheet1")
df_excel_save_hist


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
  source("Scripts/08_Plausibility_Checks.R") 
  
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
source("Scripts/09_Descriptive_Statistics.R") 



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
  source("Scripts/11_Estimation_Sample.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  eval(parse(text = keep_after_file_run))
  gc()
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Next, the treatment effect estimation using DML is performed. Note that I
# split this up (no loops as before) as this is VERY computationally
# expensive!


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "binary"

#%%%%%%%%%%%%%%%%%%%%%%%#
#### OUTCOME: GRADES ####
#%%%%%%%%%%%%%%%%%%%%%%%#

outcome_var <- "outcome_grade"


## MAIN MODEL ##
#++++++++++++++#

cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
model_treatment <- "binary"
model_type <- "all"
model_controls <- "no_lags"
model_trimming <- 0.01

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 # 4
model_k_tuning <- 2 # 4
model_s_rep <- 2 # 20


## LASSO ##
model_algo <- "lasso"
source("Scripts/12_a_Analysis_DML_Binary.R") 


## XGBoost ##
model_algo <- "xgboost"
source("Scripts/12_a_Analysis_DML_Binary.R") 


## RANDOM FORESTS ##

# for random forests smaller K and no parameter tuning as it is computationally expensive
model_k <- 2 # 2, evtl. 4
model_k_tuning <- 1 # 1
model_s_rep <- 2 # 2
model_algo <- "randomforests"
source("Scripts/12_a_Analysis_DML_Binary.R") 


## POST-LASSO ##
model_k_tuning <- 2 # parameter tuning
model_algo <- "postlasso"
source("Scripts/12_a_Analysis_DML_Binary.R") 



## ROBUSTNESS CHECKS ##
#+++++++++++++++++++++#




#%%%%%%%%%%%%%%%%%%%%%%#####%#
#### OUTCOME: PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%#####%#





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "multi"


#%%%%%%%%%%%%%%%%%%%%%%%#
#### OUTCOME: GRADES ####
#%%%%%%%%%%%%%%%%%%%%%%%#

outcome_var <- "outcome_grade"


#### MAIN MODEL ####
#++++++++++++++++++#

cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
model_treatment <- "multi"
model_type <- "all"
model_outcome <- "stand"
model_controls <- "no_lags"
model_trimming <- 0.01
probscore_separate <- TRUE

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 # 4
model_k_tuning <- 2 # 4
model_s_rep <- 2 # 20


## LASSO ##
multi_model_algo <- "lasso"
source("Scripts/12_c_Analysis_DML_Multi.R") 

## XGBoost ##
multi_model_algo <- "xgboost"
source("Scripts/12_c_Analysis_DML_Multi.R") 


## Random Forests ##
model_k <- 2 # 2, evtl. 4
model_k_tuning <- 1 # 1
model_s_rep <- 2 # 2
multi_model_algo <- "randomforests"
source("Scripts/12_c_Analysis_DML_Multi.R") 


## Post-Lasso ##
model_k_tuning <- 2 # 2
multi_model_algo <- "postlasso"
source("Scripts/12_c_Analysis_DML_Multi.R") 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

dml_num <- 1


#### LASSO ####

df_inputs_dml_lasso <- df_inputs_dml %>% filter(model_algo == "lasso")

for (dml_num in 1:nrow(df_inputs_dml_lasso)) {
  
  # delete unnecessary environment items
  eval(parse(text = keep_after_file_run))
  
  # print combination
  print(paste("KOMBINATION:", dml_num))
  
  # subset data
  df_inputs_dml_sub <- df_inputs_dml_lasso[dml_num, ]
  
  # extract inputs
  model_treatment <- df_inputs_dml_sub$model_treatment
  model_type <- df_inputs_dml_sub$model_type
  model_algo <-  df_inputs_dml_sub$model_algo
  model_k <- df_inputs_dml_sub$model_k
  model_k_tuning <- df_inputs_dml_sub$model_k_tuning
  model_s_rep <- df_inputs_dml_sub$model_s_rep
  model_trimming <- df_inputs_dml_sub$model_trimming
  model_outcome <- df_inputs_dml_sub$model_outcome
  model_controls <- df_inputs_dml_sub$model_controls
  
  # run file
  source("Scripts/12_a_Analysis_DML.R") 
  gc()
}


# show estimation results
read.xlsx("Output/ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")


#### POST-LASSO ####

gc()
df_inputs_dml_sub$model_algo <- "postlasso"

# extract inputs
model_treatment <- df_inputs_dml_sub$model_treatment
model_type <- df_inputs_dml_sub$model_type
model_algo <-  df_inputs_dml_sub$model_algo
model_k <- df_inputs_dml_sub$model_k
model_k_tuning <- df_inputs_dml_sub$model_k_tuning
model_s_rep <- df_inputs_dml_sub$model_s_rep
model_trimming <- df_inputs_dml_sub$model_trimming
model_outcome <- df_inputs_dml_sub$model_outcome
model_controls <- df_inputs_dml_sub$model_controls

# run file
source("Scripts/12_a_Analysis_DML.R") 


#### RANDOM FORESTS ####

gc()
df_inputs_dml_sub$model_algo <- "randomforests"

# extract inputs
model_treatment <- "binary"
model_type <- "all"
model_algo <-  "randomforests"
model_k <- 4
model_k_tuning <- 2
model_s_rep <- 5
model_trimming <- 0.01
model_outcome <- "level"
model_controls <- "no_lags"

# run file
start <- Sys.time()
source("Scripts/12_a_Analysis_DML.R") 
end <- Sys.time() - start
end


#### XGBOOST ####
gc()
#df_inputs_dml_sub$model_algo <- "xgboost"

# extract inputs
cohort_prep <- main_cohort_prep
treatment_def <- main_treatment_def
treatment_repl <- main_treatment_repl
extra_act <- main_extra_act

model_treatment <- "binary"
model_type <- "all"
model_algo <-  "xgboost"
model_k <- 4
model_k_tuning <- 2
model_s_rep <- 5
model_trimming <- 0.01
model_outcome <- "level"
model_controls <- "no_lags"

# run file
start <- Sys.time()
source("Scripts/12_a_Analysis_DML.R") 
end <- Sys.time() - start


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# DML in the multivalued treatment setting is only conducted for the 
# main model and xgboost.
# However, once the treatment classification models are trained separately
# for each treatment value (probscore_separate_sel = TRUE) and once using
# a multinominal logistic regression.

gc()
eval(parse(text = keep_after_file_run))

treatment_setting <- "multi"
multi_model_algo <- "xgboost"
probscore_separate_sel <- c(TRUE, FALSE)

for (i in 1:length(probscore_separate_sel)) {
  print(paste("Model Generation:", i))
  probscore_separate <- probscore_separate_sel[i]
  source("Scripts/12_c_Analysis_DML_Multi.R") 
}





