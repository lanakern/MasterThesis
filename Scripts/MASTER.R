#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER: DATA PREPARATION AND DML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the data preparation for both grades and the big five personality
# traits as outcomes are conducted. Moreover, DML in both the binary and multivalued
# treatment setting is performed.
# This file ensures the above mentioned steps are conducted in the correct order.
#+++
# The user can specify the following parameters (not needed as done automatically):
# -> "cohort_prep" determines the treatment period generation method:
# "controls_same_outcome" (CAWI-CATI combinations for grades, CATI-CAWI combinations for personality) 
# or "controls_bef_outcome" (CAWI-CATI-CAWI combinations for grades whereby in the first CAWI
# interview the treatment and in the last the outcome is measured) or 
# "controls_bef_all" (CAWI-CATI-CAWI combinations for grades whereby in the last CAWI
# interview the treatment and outcome is measured)
# Note that for the personality sample only controls_same_outcome is applied.
# -> "treatment_repl" for missing value replacement of treatment and outcome variable.
# "down" means that the last value carried forward method is applied.
# "no" means that missing values are not replaced. This results in the smallest sample
# size; for the personality sample in even no observations.
# -> "treatment_def" defines the treatment group categorization. "weekly" means
# that only students who participate in sports at least weekly are considered as 
# sport participants. "all" means that students who exercise at any regularity
# are considered as sport participants. 
# -> "extra_act" determines if the sample only included students who participate
# in sport and/or any other extracurricular activity. If "yes" this is done;
# if "no" this sample reduction step is skipped. For "uni" only students who
# participate in an extra. act. within the university are considered.
# -> "treatment_setting" defines the "binary" and "multi"valued treatment setting.
# -> "outcome_var" defines the outcome variable in the binary treatment setting.
# -> "outcome_var_multi" defines the outcome variable in the multivalued treatment setting.
# -> "model_type" defines tne control variables: "all", "allpoly" (all + polynominals) 
# or "allintpoly" (all + polynominals + interactions). The control variables can then
# further be selected by "model_controls_lag" and "model_controls_endog".
# -> "model_controls_lag" indicates if lagged variables are included and if yes which.
# "no_lags" means that no lagged variables are included at all, "no_treatment_outcome_lags"
# means that only lags for outcome and treatment are dropped, "all" means that all
# lagged variables are included in the estimation. 
# -> "model_controls_endog" indicates if also possibly endogeneous variables are
# included ("yes") or ("no"); "no" should only be used for model_type == "all".
# -> "model_trimming" determines the trimming threshold for enforcing common
# support. It can take on numeric values (lowest range) or "min-max" or "no".
# -> "model_k" determines the number of partitions in K-fold cross-fitting.
# -> "model_k_tuning" determines the number of folds in K-fold CV for parameter tuning.
# -> "model_s_rep" determines the number of repetitions of the DML procedure.
# -> "model_algo" denotes the ML algorithm used to make the nuisance parameter
# predictions: "lasso", "postlasso", "randomforests" or "xgboost".
#+++
# ATTENTION: THIS FILE HAS VERY LONG RUN TIMES OF SEVERAL WEEKS. Thus, it is
# recommended to perform the operations step by step.
#+++
# The code is developed using "R version 4.1.2 (2021-11-01) -- "Bird Hippie"".
# Note that with the latest R version some functions are not working.
#+++


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

if (!require("xgboost")) install.packages("xgboost")
library(xgboost)  # for xgboost()

if (!require("randomForest")) install.packages("randomForest")
library(randomForest)  # for randomForest()

if (!require("DoubleML")) install.packages("DoubleML")
library(DoubleML)  # for applying DML with function

if (!require("mlr3")) install.packages("mlr3")
library(mlr3)  # for applying DML with function

if (!require("caret")) install.packages("caret")
library(caret)  # for confusionMatrix()

if (!require("janitor")) install.packages("janitor")
library(janitor)  # for remove_constant() (-> dropping constant variables)

if (!require("pROC")) install.packages("pROC")
library(pROC) # for multiclass AUC

if (!require("purrr")) install.packages("purrr")
library(purrr) # for reduce function (union of variables)

if (!require("MASS")) install.packages("MASS")
library(MASS) # for ginv() function (-> weights for covariate balancing)

if (!require("vip")) install.packages("vip")
library(vip) # for feature importance

if (!require("ggpubr")) install.packages("ggpubr") 
library(ggpubr) # for arranging multiple plots in one plot

if (!require("geomtextpath")) install.packages("geomtextpath") 
library(geomtextpath) # for geom_textvline()

if (!require("regclass")) install.packages("regclass") 
library(regclass) # for VIP()

if (!require("cobalt")) install.packages("cobalt") 
library(cobalt) # for checking own covariance balance calculation

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
main_treatment_def <- "all"
main_treatment_repl <- "down"
main_extra_act <- "yes"
main_model_treatment <- "binary"
main_model_type <- "all"
main_model_k <- 4
main_model_k_tuning <- 2
main_model_s_rep <- 5
main_model_trimming <- "min-max"
main_model_controls_lag <- "no_treatment_outcome_lags" # "no_lags", "all"
main_model_controls_endog  <- "yes"
main_cov_balance <- "yes"

# generate all possible combinations of user inputs (to iterate over it below)
df_inputs <- data.frame(
  # for interview data preparation
  "cohort_prep" = c("controls_bef_outcome", rep("controls_same_outcome", 4), "controls_bef_all", "controls_treatment_outcome",
                    "controls_bef_all", "controls_treatment_outcome", "controls_same_outcome"), 
  # for treatment and outcome missing value replacement
  "treatment_repl" = c("down", "down", "down", "down", "no", "down", "down", "down", "down", "down"),
  # for treatment generation
  "treatment_def" = c("weekly", "all", "weekly", "weekly", "weekly", "all", "all", "weekly", "weekly", "all"),
  # for sample selection: only keeping respondents with extracurricular activity
  "extra_act" = c("yes", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "uni")
)

# aggregation of variables
aggr_vars <- "pca" # "mean" (not used anymore)
cronbach_a <- "yes" # "no" (not used anymore)


# define variables which may be endogenous (only used in main model)
# also outcome and treatment lags but they are dropped within the code
vars_endogenous <- 'dplyr::select(starts_with("health"), starts_with("uni_time"),
  starts_with("extracurricular"), starts_with("social_integr"),
  starts_with("interest"), starts_with("comp"), starts_with("emp"), starts_with("personality"),
  starts_with("satisfaction_life"), starts_with("stress"), starts_with("uni_anxiety"),
  starts_with("bigfive"), starts_with("parents_degree_wish"), starts_with("parents_importance_success"), 
  starts_with("parents_opinion_degree"), starts_with("uni_achievement"),
  starts_with("outcome_grade_lag"), starts_with("treatment_sport_lag"),
  c("educ_school_grade_math", "educ_school_grade_ger", "educ_school_grade_final", 
    "educ_school_rep_grade", "educ_school_degree_general", "educ_school_degree_applied", 
    "uni_best_student", "uni_best_student_lag", "uni_ects_current"))'
    

# define variables to keep after deleting environment
keep_after_file_run <- 'rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", 
"treatment_def", "extra_act", "prep_sel_num", "aggr_vars", "cronbach_a",
"df_inputs", "df_inputs_indiv", "df_inputs_dml", "df_inputs_dml_lasso",
 "keep_after_file_run", "vars_baseline", "model_type", "model_algo", "model_post_sel",
"model_k", "model_k_tuning", "model_s_rep", "model_trimming", "model_controls_lag", 
"model_controls_endog", "model_outcome", "dml_num", "probscore_separate", "cov_balance", 
"treatment_setting", "outcome_var", "outcome_var_multi", "vars_endogenous",  
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
Sys.setlocale("LC_TIME", "German") # because this is changed within the file


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION FOR PERSONALITY AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

source("Grades/MASTER_PREP_GRADES.R")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "binary"

#%%%%%%%%%%%%%%%%%%%%%%#
#### Outome: Grades ####
#%%%%%%%%%%%%%%%%%%%%%%#

outcome_var <- "outcome_grade"

#### MAIN MODEL ####
#++++++++++++++++++#

cohort_prep <- main_cohort_prep 
treatment_repl <- main_treatment_repl 
treatment_def <- main_treatment_def 
extra_act <- main_extra_act 
model_type <- main_model_type 
model_controls_lag <- main_model_controls_lag 
model_controls_endog <- main_model_controls_endog 
model_trimming <- main_model_trimming 
model_hyperparam_sel <- "best"
model_post_sel <- FALSE
cov_balance <- main_cov_balance

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 
model_k_tuning <- 2 
model_s_rep <- 5 

## LASSO ##
model_algo <- "lasso"
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()

## POST-LASSO ##
model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R")
eval(parse(text = keep_after_file_run))
gc()
model_post_sel <- FALSE

## XGBoost ##
model_algo <- "xgboost"
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()

## RANDOM FORESTS ##
# for random forests smaller K and no parameter tuning as it is computationally expensive
model_k_tuning <- 1 # 1
model_algo <- "randomforests"
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()
model_k_tuning <- 2 # parameter tuning


#%%%%%%%%%%%%%%%%%%%%%%#####%#
#### Outcome: Personality ####
#%%%%%%%%%%%%%%%%%%%%%%#####%#

# There are five personality variables. Due to the high computational burden,
# only the main model is applied to the personality sample.

cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
model_type <- main_model_type
model_controls_lag <- main_model_controls_lag
model_controls_endog <- main_model_controls_endog
model_trimming <- main_model_trimming
model_hyperparam_sel <- "best"
model_post_sel <- FALSE
cov_balance <- main_cov_balance

model_k <- 4 
model_k_tuning <- 2 
model_s_rep <- 5 


#### Agreeableness ####
#+++++++++++++++++++++#

outcome_var <- "outcome_bigfive_agreeableness"

model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()
model_post_sel <- FALSE

####  Extroversion ####
#+++++++++++++++++++++#

outcome_var <- "outcome_bigfive_extraversion"

model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()
model_post_sel <- FALSE

model_algo <- "xgboost" # run only for MICE_sel = 1 in "11_a_DML_Binary.R"

####  Openness ####
#+++++++++++++++++#

outcome_var <- "outcome_bigfive_openness"

model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R") 
gc()
eval(parse(text = keep_after_file_run))


####  Conscientiousness ####
#++++++++++++++++++++++++++#

outcome_var <- "outcome_bigfive_conscientiousness"

model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Neuroticism ####
#+++++++++++++++++++#

outcome_var <- "outcome_bigfive_neuroticism"

model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_a_DML_Binary.R") 
gc()
eval(parse(text = keep_after_file_run))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "multi"


#%%%%%%%%%%%%%%%%%%%%%%#
#### Outome: Grades ####
#%%%%%%%%%%%%%%%%%%%%%%#

outcome_var_multi <- "outcome_grade"


## MAIN MODEL ##
#++++++++++++++#

cohort_prep <- main_cohort_prep 
treatment_repl <- main_treatment_repl 
treatment_def <- main_treatment_def 
extra_act <- main_extra_act 
model_type <- main_model_type 
model_controls_lag <- main_model_controls_lag 
model_controls_endog <- main_model_controls_endog 
model_trimming <- main_model_trimming 
model_post_sel <- FALSE
probscore_separate <- TRUE
hyperparam_sel <- "best"
model_hyperparam_sel <- "best"
cov_balance  <- main_cov_balance
prob_norm <- "yes"

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 
model_k_tuning <- 2 
model_s_rep <- 5 

## LASSO ##
multi_model_algo <- "lasso"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

## Post-Lasso ##
model_post_sel <- TRUE 
multi_model_algo <- "postlasso"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
model_post_sel <- FALSE

## XGBoost ##
multi_model_algo <- "xgboost"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

## Random Forests ##
model_k <- 4 
model_k_tuning <- 1 
model_s_rep <- 5 
multi_model_algo <- "randomforests"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
model_k_tuning <- 2 


#### ROBUSTNESS CHECKS ####
#+++++++++++++++++++++++++#

multi_model_algo <- "postlasso"
model_post_sel <- TRUE

## No LVCF ##
treatment_repl <- "no"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
treatment_repl <- main_treatment_repl

## No extracurricular activity ##
extra_act <- "no"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
extra_act <- main_extra_act

## Extracurricular activity within uni ##
extra_act <- "uni"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
extra_act <- main_extra_act

## controls_bef_outcome ##
cohort_prep <- "controls_bef_outcome"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
cohort_prep <- main_cohort_prep 

## controls_bef_all ##
cohort_prep <- "controls_bef_all"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
cohort_prep <- main_cohort_prep 

## No potentially endogeneous variables ##
model_controls_endog <- "no"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
model_controls_endog <- main_model_controls_endog

## lags ##
model_controls_lag <- "no_lags"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_controls_lag <- "only_lags"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_controls_lag <- "no_treatment_lag"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_controls_lag <- ain_model_controls_lag

## Polys ##
model_type <- "allpoly" 
source("Scripts/11_b_DML_Multi.R")
eval(parse(text = keep_after_file_run))
gc()
model_type <- main_model_type 

## Include treatment and outcome lags ##
model_controls_lag <- "all"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
model_controls_lag <- main_model_controls_lag

## Leisure Sport ##
cov_balance <- "no"
source("Scripts/11_b_DML_Multi.R")
eval(parse(text = keep_after_file_run))
gc()
cov_balance <- main_cov_balance

## Change trimming thresholds ##
model_trimming <- 0.01
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_trimming <- 0.1
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_trimming <- "no"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()
model_trimming <- main_model_trimming

model_trimming <- "min-max_001" 
source("Scripts/11_b_DML_Multi.R") 

## Change K and S ##
# combis: (4, 10, 5), (4,2,10), (5, 2, 5). (5, 10, 20)
model_k <- 4 
model_k_tuning <- 10
model_s_rep <- 5
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_k <- 4 
model_k_tuning <- 2
model_s_rep <- 10
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

model_k <- 5 
model_k_tuning <- 2
model_s_rep <- 5
source("Scripts/11_b_DML_Multi.R")
eval(parse(text = keep_after_file_run))
gc()

model_k <- 5 
model_k_tuning <- 10
model_s_rep <- 20
source("Scripts/11_b_DML_Multi.R")
eval(parse(text = keep_after_file_run))
gc()

# reset
model_k <- 4 
model_k_tuning <- 2 
model_s_rep <- 5 

## HYPERPARAMETERS ##
hyperparam_sel <- "1SE"
model_hyperparam_sel <- "1SE"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

hyperparam_sel <- "1SE_plus"
model_hyperparam_sel <- "1SE_plus"
source("Scripts/11_b_DML_Multi.R") 
eval(parse(text = keep_after_file_run))
gc()

# reset
model_hyperparam_sel <- model_hyperparam_sel
hyperparam_sel <- model_hyperparam_sel

## Not normalizing treatment probabilities ##
prob_norm <- "no"
model_trimming <- "min-max_001" 
source("Scripts/11_b_DML_Multi.R") 
prob_norm <- "yes"

## Multiclass classification ##
probscore_separate <- FALSE
source("Scripts/11_b_DML_Multi.R") 
probscore_separate <- TRUE


#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Outome: Personality ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

cohort_prep <- main_cohort_prep 
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def 
extra_act <- main_extra_act
model_type <- main_model_type 
model_controls_lag <- main_model_controls_lag 
model_controls_endog <- main_model_controls_endog 
model_trimming <- main_model_trimming 
model_post_sel <- FALSE
probscore_separate <- TRUE
hyperparam_sel <- "best"
model_hyperparam_sel <- "best"
cov_balance  <- main_cov_balance

# for lasso and xgboost higher K as they are computationally faster
model_k <- 4 
model_k_tuning <- 2 
model_s_rep <- 5 


#### Agreeableness ####
outcome_var_multi <- "outcome_bigfive_agreeableness"

multi_model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))

multi_model_algo <- "lasso"
model_post_sel <- FALSE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))


#### Conscientiousness ####
outcome_var_multi <- "outcome_bigfive_conscientiousness"
multi_model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))

multi_model_algo <- "lasso"
model_post_sel <- FALSE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))


#### Extroversion ####
outcome_var_multi <- "outcome_bigfive_extraversion"
multi_model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))

multi_model_algo <- "lasso"
model_post_sel <- FALSE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))


#### Openness ####
outcome_var_multi <- "outcome_bigfive_openness"
multi_model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))

multi_model_algo <- "lasso"
model_post_sel <- FALSE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))


#### Neuroticism ####
outcome_var_multi <- "outcome_bigfive_neuroticism"
multi_model_algo <- "postlasso"
model_post_sel <- TRUE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))

multi_model_algo <- "lasso"
model_post_sel <- FALSE
source("Scripts/11_b_DML_Multi.R") 
gc()
eval(parse(text = keep_after_file_run))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DML: Chernozhukov and Knaus Function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "binary"
cohort_prep <- main_cohort_prep 
treatment_repl <- main_treatment_repl 
treatment_def <- main_treatment_def 
extra_act <- main_extra_act 
model_type <- main_model_type 
model_controls_lag <- main_model_controls_lag 
model_controls_endog <- main_model_controls_endog 
model_trimming <- main_model_trimming 
cov_balance <- main_cov_balance
model_post_sel <- FALSE
model_k <- 4
model_k_tuning <- 3
model_s_rep <- 5
outcome_var <- "outcome_grade"
outcome_var_multi <- "outcome_grade"

source("Scripts/11_c_DML_Chernozhukov_Function.R") 
source("Scripts/11_d_DML_Knaus_Function.R") 


#%%%%%%%%%%%%%%%%%%%%%%%#
#### ANALYZE RESULTS ####
#%%%%%%%%%%%%%%%%%%%%%%%#

treatment_setting <- "binary"
cohort_prep <- main_cohort_prep 
treatment_repl <- main_treatment_repl 
treatment_def <- main_treatment_def 
extra_act <- main_extra_act 
model_type <- main_model_type 
model_controls_lag <- main_model_controls_lag 
model_controls_endog <- main_model_controls_endog 
model_trimming <- main_model_trimming 
cov_balance <- main_cov_balance
model_post_sel <- FALSE
model_k <- 4
model_k_tuning <- 2
model_s_rep <- 5


#### Overall ####
#+++++++++++++++#

source("Scripts/12_a_DML_Results.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Covariate Balance ####
#+++++++++++++++++++++++++#

source("Scripts/12_b_Assessment_Covariate_Balance.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Feature Importance ####
#++++++++++++++++++++++++++#

n_features <- 20
n_features_pers <- 10
n_features_multi <- 10

source("Scripts/12_c_FeatureImportance.R") 

eval(parse(text = keep_after_file_run))
gc()


#### Mean Values ####
#+++++++++++++++++++#

source("Scripts/13_MeanValues.R")


