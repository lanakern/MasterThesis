#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER: DATA PREPARATION AND DML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the data preparation for both grades and the big five personality
# traits as outcome are conducted. Moreover, DML in both the binary and multivalued
# treatment setting is performed.
# This file ensures the above mentioned steps are conducted in the correct order.
# ATTENTION: THIS FILE HAS VERY LONG RUN TIMES OF SEVERAL WEEKS. Thus, it is
# recommended to perform the operations step by step.
#++++


#%%%%%%%%%%%%%#
#### SETUP ####
#%%%%%%%%%%%%%#

# clear workspace
rm(list = ls())


#### LOAD ALL PACKAGES ####
#+++++++++++++++++++++++++#

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

if (!require("vip")) install.packages("vip")
library(vip) # for feature importance

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")

# do not display warning messages
options(warn = -1)



#### DEFINE INPUTS ####
#+++++++++++++++++++++#

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
"model_outcome", "dml_num",  "treatment_setting", "outcome_var",
ls()[str_starts(ls(), "func_")], ls()[str_starts(ls(), "main_")])))'



#### LOAD ALL FUNCTIONS ####
#++++++++++++++++++++++++++#

load_function <- paste0("Functions/", list.files(path = "Functions/"))
for (func_load in load_function) {
  source(func_load)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION FOR GRADES AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

source("Personality/MASTER_PREP_PERSONALITY.R")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION FOR PERSONALITY AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

source("Grades/MASTER_PREP_GRADES.R")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML MAIN MODEL: BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "binary"

#%%%%%%%%%%%%%%%%%%%%%%#
#### Outome: Grades ####
#%%%%%%%%%%%%%%%%%%%%%%#

outcome_var <- "outcome_grade"


## MAIN MODEL ##
#++++++++++++++#

cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
model_type <- "all"
model_controls <- "no_lags"
model_trimming <- 0.01

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 # 4
model_k_tuning <- 2 # 4
model_s_rep <- 2 # 20


## LASSO ##
model_algo <- "lasso"
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))

## XGBoost ##
model_algo <- "xgboost"
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))

## RANDOM FORESTS ##

# for random forests smaller K and no parameter tuning as it is computationally expensive
model_k <- 2 # 2, evtl. 4
model_k_tuning <- 1 # 1
model_s_rep <- 2 # 2
model_algo <- "randomforests"
source("Scripts/11_a_DML_Binary.R") 


## POST-LASSO ##
model_k_tuning <- 2 # parameter tuning
model_algo <- "postlasso"
source("Scripts/11_a_DML_Binary.R") 



#%%%%%%%%%%%%%%%%%%%%%%#####%#
#### Outcome: Personality ####
#%%%%%%%%%%%%%%%%%%%%%%#####%#

# There are five personality variables

#### Agreeableness ####
#+++++++++++++++++++++#

outcome_var <- "bigfive_agreeableness"

## XGBoost ##
model_k <- 4 # 4
model_k_tuning <- 2 # 4
model_s_rep <- 2 # 20
model_algo <- "xgboost"
source("Scripts/11_a_Analysis_DML_Binary.R") 


outcome_var <- "bigfive_extraversion"
outcome_var <- "bigfive_openness"
outcome_var <- "bigfive_conscientiousness"
outcome_var <- "bigfive_neuroticism"




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

eval(parse(text = keep_after_file_run))
treatment_setting <- "multi"


#%%%%%%%%%%%%%%%%%%%%%%%#
#### OUTCOME: GRADES ####
#%%%%%%%%%%%%%%%%%%%%%%%#

outcome_var_multi <- "outcome_grade"


#### MAIN MODEL ####
#++++++++++++++++++#

cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
model_type <- "all"
model_controls <- "no_lags"
model_trimming <- 0.01
probscore_separate <- TRUE

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 # 4
model_k_tuning <- 2 # 4
model_s_rep <- 2 # 20


## LASSO ##
multi_model_algo <- "lasso"
source("Scripts/11_b_DML_Multi.R") 

## XGBoost ##
multi_model_algo <- "xgboost"
source("Scripts/11_b_DML_Multi.R") 


## Random Forests ##
model_k <- 2 # 2, evtl. 4
model_k_tuning <- 1 # 1
model_s_rep <- 2 # 2
multi_model_algo <- "randomforests"
source("Scripts/11_b_DML_Multi.R") 


## Post-Lasso ##
model_k_tuning <- 2 # 2
multi_model_algo <- "postlasso"
source("Scripts/11_b_DML_Multi.R") 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYSIS DML RESULTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Feature Importance ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

# only generated for main model
eval(parse(text = keep_after_file_run))
outcome_var <- "outcome_grade"
cohort_prep <- main_cohort_prep
treatment_def <- main_treatment_def
treatment_repl <- main_treatment_repl
extra_act <- main_extra_act
model_controls <- main_model_controls
n_features <- 20

source("Scripts/13_c_DML_FeatureImportance.R") 