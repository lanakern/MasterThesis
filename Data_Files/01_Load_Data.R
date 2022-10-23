#%%%%%%%%%%%%%%%%%%%%%%%%#
#### LOAD DATA FILES ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# by Lana Kern
# In this file, all data files are loaded which are needed for the
# upcoming analysis. Moreover, the variable names and values are adjusted.


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("haven")) install.packages("haven")
library(haven)  # to import stata (.dta) file into R

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data



#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#


#### CohortProfile ####
#+++++++++++++++++++++#

# According to the data manual, CohortProfile, is the file which should be
# used as a starting point for the analysis
data_cohort_profile <- read_dta("Data/Raw/SC5_CohortProfile_D_16-0-0.dta")
data_cohort_profile <- data_cohort_profile %>%
  arrange(ID_t, wave)


#### Education ####
#+++++++++++++++++#

# -> "information on transitions in respondents’ educational careers"
data_education <- read_dta("Data/Raw/SC5_Education_D_16-0-0.dta")


#### Child ####
#+++++++++++++#

# -> contains information on all biological, foster, and adopted children of the respondent, 
# and any other child that currently lives or has ever lived together with the respondent 
# (e. g., children of former and current partners).
data_child <- read_dta("Data/Raw/SC5_spChild_D_16-0-0.dta")


#### Internship ####
#++++++++++++++++++#

# -> contains information on internships during studies, including duration, context etc.
data_internship <- read_dta("Data/Raw/SC5_spInternship_D_16-0-0.dta")


#### Military ####
#++++++++++++++++#

# -> includes episodes of military or civilian service as well as gap years taken 
# to do voluntary work in the social or environmental sector.
data_military <- read_dta("Data/Raw/SC5_spMilitary_D_16-0-0.dta")


#### Partner ####
#+++++++++++++++#

# -> partnership history of the respondent.
data_partner <- read_dta("Data/Raw/SC5_spPartner_D_16-0-0.dta")


#### School ####
#++++++++++++++#

# -> general education history from school entry until the date of completion
data_school <- read_dta("Data/Raw/SC5_spSchool_D_16-0-0.dta")


#### Sibling ####
#+++++++++++++++#

# -> includes all siblings of the respondent reported in wave 1.
data_sibling <- read_dta("Data/Raw/SC5_spSibling_D_16-0-0.dta")


#### Break ####
#+++++++++++++#

# -> covers all breaks of further trainings, vocational and/or academic, that a respondent ever attended
data_break <- read_dta("Data/Raw/SC5_spVocBreaks_D_16-0-0.dta")


#### Vocational Preparation ####
#++++++++++++++++++++++++++++++#

# -> comprises episodes of vocational preparation after general education, including
# pre‐training course and  basic vocational training years
data_voc_prep <- read_dta("Data/Raw/SC5_spVocPrep_D_16-0-0.dta")


#### Vocational Training ####
#+++++++++++++++++++++++++++#

# -> covers all further trainings, vocational and/or academic, that a respondent ever
# attended, for instance tertiary education at universities, doctoral studies
# subject and degree changes over the course of studies, change of higher education institution
data_voc_train <- read_dta("Data/Raw/SC5_spVocTrain_D_16-0-0.dta")


#### Competencies ####
#++++++++++++++++++++#

# -> contains data from competence assessments conducted.
data_competencies <- read_dta("Data/Raw/SC5_xTargetCompetencies_D_16-0-0.dta")

  



