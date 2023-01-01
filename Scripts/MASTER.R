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

# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")



#### DEFINE INPUTS ####
#%%%%%%%%%%%%%%%%%%%%%#

# cohort profile #
#cohort_prep <- "controls_bef_outcome" 
cohort_prep <- "controls_same_outcome"

# treatment: down and upward replacement #
treatment_repl <- "downup" 
#treatment_repl <- "down"

# treatment definition: all frequency levels or only weekly #
treatment_def <- "all"
#treatment_def <- "weekly"



#### RUN DATA PREPARATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load all data files and make basic preparations like renaming variables and
# recoding missing values as NA
source("01_Load_Data.R")

# Prepare episode / life course data, i.e., educational history of each respondents
source("02_a_Prep_Data_Life_Course.R")

# Prepare treatment periods
source("02_b_Prep_Data_Interview_Participation.R")

# Prepare individual data sets
source("03_a_Prep_Cati.R")
source("03_b_Prep_Cawi.R")
source("03_c_Prep_Sibling.R")
source("03_d_Prep_Child.R")
source("03_e_Prep_Partner.R")
source("03_f_Prep_Competencies.R")

# Merge 
source("04_a_Merge_CATI_CAWI.R") # merge CATI & CAWI
source("04_b_Merge_Prepare_Episode.R") # add episode data
source("04_c_Merge_All.R") # add all other data sets

# Prepare treatment and outcome
source("05_Create_Treatment_Outcome.R") 

# Sample selection
source("06_Sample_Selection.R") 

# Prepare control variables


# Plausibility analysis


# FINAL ESTIMATION SAMPLES (depends on model and treatment setting)



#### RUN DATA ANALYSIS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#




#### RUN ESTIMATION ####
#%%%%%%%%%%%%%%%%%%%%%%#


