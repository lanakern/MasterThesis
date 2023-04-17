#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER FILE FOR DATA PREPARATION: PERSONALITY AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This master file is established to run all data preparation files for the
# big five personality traits in the respective order.
# Note that the personality data is only prepared for the main model.
#+++


# define input parameters
cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
cov_balance <- "yes"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### All Files  ####
#++++++++++++++++++#

# Load all data files and make basic preparations like renaming variables and
# recoding missing values as NA
source("Scripts/Personality/01_Load_Data_Personality.R")
eval(parse(text = keep_after_file_run))
gc()


#### Episode Data ####
#++++++++++++++++++++#

# Prepare episode / life course data, i.e., educational history of each respondents
source("Scripts/Personality/02_a_Prep_Data_Life_Course_Personality.R")
eval(parse(text = keep_after_file_run))
gc()


#### Treatment Periods ####
#+++++++++++++++++++++++++#

# Prepare treatment periods / cohort profile
source("Scripts/Personality/02_b_Prep_Data_Interview_Participation_Personality.R")
eval(parse(text = keep_after_file_run))
gc()


#### Cati & Cawi ####
#+++++++++++++++++++#

# Prepare CATI
source("Scripts/Personality/03_a_Prep_Cati_Personality.R") 
eval(parse(text = keep_after_file_run))

# Prepare CAWI
source("Scripts/Personality/03_b_Prep_Cawi_Personality.R") 
eval(parse(text = keep_after_file_run))

gc()


#### Other individual data sets ####
#++++++++++++++++++++++++++++++++++#

# Prepare individual data sets
source("Scripts/Personality/03_c_Prep_Sibling_personality.R") # Sibling
eval(parse(text = keep_after_file_run))
gc()

source("Scripts/Personality/03_d_Prep_Child_personality.R") # Child
eval(parse(text = keep_after_file_run))
gc()

source("Scripts/Personality/03_e_Prep_Partner_personality.R") # Partner
eval(parse(text = keep_after_file_run))
gc()

source("Scripts/Personality/03_f_Prep_Competencies_personality.R") # Competencies
eval(parse(text = keep_after_file_run))
gc()

  

#### Merge ####
#+++++++++++++#

# Merge CATI & CAWI
source("Scripts/Personality/04_a_Merge_CATI_CAWI_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()

# Merge episode data 
source("Scripts/Personality/04_b_Merge_Prepare_Episode_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()

# Merge all other data sets
source("Scripts/Personality/04_c_Merge_All_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()  




#### Treatment and Outcome ####
#+++++++++++++++++++++++++++++#

source("Scripts/Personality/05_Create_Treatment_Outcome_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()



#### Sample Selection ####
#++++++++++++++++++++++++#

source("Scripts/Personality/06_Sample_Selection_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()



#### Create Variables ####
#++++++++++++++++++++++++#

# now set language for dates and times to English
Sys.setlocale("LC_TIME", "English")


# Prepare control variables
source("Scripts/Personality/07_Create_Control_Variables_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()



#### Plausibility analysis ####
#+++++++++++++++++++++++++++++#

source("Scripts/Personality/08_Plausibility_Checks_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Descriptive Statistics ####
#++++++++++++++++++++++++++++++#

source("Scripts/Personality/09_Descriptive_Statistics_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Final Estimation Samples ####
#++++++++++++++++++++++++++++++++#

# Decide if interactions should be created (takes a long time)
create_interactions <- "yes" 
  
# Prepare control variables
source("Scripts/Personality/10_Estimation_Sample_Personality.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Show Sample Reduction ####
#+++++++++++++++++++++++++++++#

read.xlsx("Output/SAMPLE_REDUCTION_STEPS_PERSONALITY.xlsx", sheetName = "Sheet1")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

