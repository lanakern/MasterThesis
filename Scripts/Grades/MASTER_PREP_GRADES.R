#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MASTER FILE FOR DATA PREPARATION: GRADES AS OUTCOME ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This master file is established to run all data preparation files for 
# grades as outcome in the respective order.
# Moreover, input parameters like the data preparation method can be 
# changed in this file once so that it is valid for all other files. By
# default all different data preparations are conducted.
#+++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RUN DATA PREPARATION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### All Files  ####
#++++++++++++++++++#

# Load all data files and make basic preparations like renaming variables and
# recoding missing values as NA
source("Scripts/Grades/01_Load_Data.R")
eval(parse(text = keep_after_file_run))


#### Episode Data ####
#++++++++++++++++++++#

# Prepare episode / life course data, i.e., educational history of each respondents
source("Scripts/Grades/02_a_Prep_Data_Life_Course.R")
eval(parse(text = keep_after_file_run))


#### Treatment Periods ####
#+++++++++++++++++++++++++#

# Prepare treatment periods / cohort profile
# 1.) CATI-CAWI combinations ("controls_same_outcome")
# 2.) CAWI-CATI-CAWI combinations ("controls_bef_outcome")
for (cohort_prep_sel in unique(na.omit(df_inputs$cohort_prep))) {
  cohort_prep <- cohort_prep_sel
  print(cohort_prep)
  source("Scripts/Grades/02_b_Prep_Data_Interview_Participation.R")
  eval(parse(text = keep_after_file_run))
}


#### Cati & Cawi ####
#+++++++++++++++++++#

# Prepare CATI and CAWI: iteration over cohort_prep and treatment_repl. However,
# treatment_repl is always considered as "down". 
df_inputs_indiv <- df_inputs %>% 
  dplyr::select(cohort_prep, treatment_repl) %>% 
  filter(treatment_repl == "down") %>%
  distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  # select data preparation possibilities
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ] # subset data
  cohort_prep <- df_inputs_sel$cohort_prep # select cohort prep preparation
  treatment_repl <- df_inputs_sel$treatment_repl # select treatment/outcome replacement
  
  # Prepare individual data sets
  print("CATI")
  source("Scripts/Grades/03_a_Prep_Cati.R") # CATI
  eval(parse(text = keep_after_file_run))
  
  print("CAWI")
  source("Scripts/Grades/03_b_Prep_Cawi.R") # CAWI
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
  source("Scripts/Grades/03_c_Prep_Sibling.R") # Sibling
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Grades/03_d_Prep_Child.R") # Child
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Grades/03_e_Prep_Partner.R") # Partner
  eval(parse(text = keep_after_file_run))
  
  source("Scripts/Grades/03_f_Prep_Competencies.R") # Competencies
  eval(parse(text = keep_after_file_run))
  
  gc()
}



#### Merge ####
#+++++++++++++#

# Merge all 02_* and 03_* data sets. This differs between cohort_preps. 

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  # select data preparation possibilities
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ] # subset data
  cohort_prep <- df_inputs_sel$cohort_prep # select cohort prep preparation
  treatment_repl <- df_inputs_sel$treatment_repl # select treatment/outcome replacement
  
  # Merge 
  source("Scripts/Grades/04_a_Merge_CATI_CAWI.R") # merge CATI & CAWI
  eval(parse(text = keep_after_file_run))
  gc()
  
  source("Scripts/Grades/04_b_Merge_Prepare_Episode.R") # add episode data
  eval(parse(text = keep_after_file_run))
  gc()
  
  source("Scripts/Grades/04_c_Merge_All.R") # add all other data sets
  eval(parse(text = keep_after_file_run))
  gc()
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
}



#### Treatment and Outcome ####
#+++++++++++++++++++++++++++++#

# Prepare treatment and outcome 
# Differs also only across cohort_prep. treatment_repl and treatment_def are
# defined because file considers also other parameters which are not considered
# anymore. Here, treatment variable is generated for weekly sport participation
# and is adjusted in file 11.

df_inputs_indiv <- df_inputs %>% 
  dplyr::select(cohort_prep, treatment_repl, treatment_def) %>% 
  filter(treatment_repl == "down", treatment_def == "weekly") %>%
  distinct()

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  
  # Prepare treatment and outcome
  source("Scripts/Grades/05_Create_Treatment_Outcome.R") 
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}


#### Sample Selection ####
#++++++++++++++++++++++++#

# Conduct sample selection.
# For "controls_bef_outcome" students who do not participate in sports nor
# any other extracurricular activity are dropped here. 
# For "controls_same_outcome" all are kept and dropped in file 11.
df_inputs_indiv <- rbind(
  df_inputs %>%
    filter(cohort_prep == "controls_same_outcome" & extra_act == "no"),
  df_inputs %>%
    filter(cohort_prep == "controls_bef_outcome")
)

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Sample selection
  source("Scripts/Grades/06_Sample_Selection.R") 
  eval(parse(text = keep_after_file_run))
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}



#### Create Variables ####
#++++++++++++++++++++++++#

# now set language for dates and times to English
Sys.setlocale("LC_TIME", "English")

# create variables (differs for cohort_preps). Moreover, 5 different data sets
# are created. 
for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  # Prepare control variables
  eval(parse(text = keep_after_file_run))
  source("Scripts/Grades/07_Create_Control_Variables.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  gc()
}


# load file showing sample reduction
df_excel_save_hist <- read.xlsx("Output/SAMPLE_REDUCTION_STEPS_GRADES.xlsx", sheetName = "Sheet1")
df_excel_save_hist


#### Plausibility analysis ####
#+++++++++++++++++++++++++++++#

# Plausibility analyses are conducted to ensure that valuesare plausible.
# Again this differs across cohort_preps but also MICE data frames.

for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  source("Scripts/Grades/08_Plausibility_Checks.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  eval(parse(text = keep_after_file_run))
  gc()
}


#### Descriptive Statistics ####
#++++++++++++++++++++++++++++++#


# descriptive statistics are only made for main model
cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act
source("Scripts/Grades/09_Descriptive_Statistics.R") 
eval(parse(text = keep_after_file_run))
gc()


#### Final Estimation Samples ####
#++++++++++++++++++++++++++++++++#

# First create interaction and polynominals
for (prep_sel_num in 1:nrow(df_inputs_indiv)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  
  df_inputs_sel <- df_inputs_indiv[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  
  source("Scripts/Grades/10_a_Create_Interactions_Polys.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs_indiv)))
  eval(parse(text = keep_after_file_run))
  gc()
}

# Second create the final estimation samples. Here subsetting takes place.
# For example, regarding extracuriccular activities or treatment replacement.
# Also treatment group definition of "all" is considered.
for (prep_sel_num in 1:nrow(df_inputs)) {
  
  print(paste0("START COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  
  df_inputs_sel <- df_inputs[prep_sel_num, ]
  cohort_prep <- df_inputs_sel$cohort_prep
  treatment_repl <- df_inputs_sel$treatment_repl
  treatment_def <- df_inputs_sel$treatment_def
  extra_act <- df_inputs_sel$extra_act
  

  source("Scripts/Grades/10_b_Estimation_Sample.R") 
  
  print(paste0("FINISHED COMBINATION ", prep_sel_num, " FROM ", nrow(df_inputs)))
  eval(parse(text = keep_after_file_run))
  gc()
}


#### Show Sample Reduction ####
#+++++++++++++++++++++++++++++#

read.xlsx("Output/SAMPLE_REDUCTION_STEPS_GRADES.xlsx", sheetName = "Sheet1")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#





