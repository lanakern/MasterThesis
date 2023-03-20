#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER FILE FOR DATA PREPARATION: PERSONALITY AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This master file is established to run all data preparation files for the
# big five personality traits in the respective order.
# Moreover, input parameters like the data preparation method can be 
# changed in this file once so that it is valid for all other files.
#+++


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
  
  # Decide if interactions should be created (takes a long time)
  create_interactions <- "no" # "yes"
  
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

