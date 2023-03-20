#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML IN THE MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file the ATE and ATTE is estimated using DML in the multivalued
# treatment setting. As outcome both grades and the big five personality
# traits can be specified.
# As in the binary treatment setting, the outcomes are predicted separately
# for each treatment level t. 
# For the treatment the user can specify if the classification problem is
# transferred in t binary classification (probscore_separate = TRUE) problems 
# or one multinominal (probscore_separate = FALSE)
#++++
# Categories in the multivalued treatment setting are:
# -> 1: at least weekly
# -> 2: monthly or less frequently
# -> 3: never
#++++
# The estimation results are stored in an Excel file.
#++++

# Start time tracking
start_time <- Sys.time()

# set seed for reproducible results
set.seed(1234)

# empty data frames and lists to store results
dml_result_all <- list()


# iterate over mice data sets
for (mice_data_sel in 1:5) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
  # load data
    ## extracurricular activity ending
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
    ## extract outcome
  if (str_detect(outcome_var_multi, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var_multi, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
  ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- paste0(
      load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", 
      treatment_def, "_", treatment_repl, extra_act_save, "_mice", 
      mice_data_sel, load_data_ending
      )
  } else {
    load_data <- paste0(
      load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", 
      treatment_def, "_", treatment_repl, extra_act_save,
      "_robustcheck_mice", mice_data_sel, load_data_ending
    )
  }

  load_data <- str_replace(load_data, "_level", "") # drop level
  
  data_dml <- readRDS(load_data)
  
  # drop lags if desired by user
  if (model_controls == "no_lags") {
    # drop all lags
    data_dml <- data_dml %>% dplyr::select(-c(ends_with("_lag"))) %>% as.data.frame()
  } else if (model_controls == "no_to_lags") {
    # drop only treatment and outcome lags
  } else {
    # keep all lags
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # if personality is outcome further preparations are necessary
  if (str_detect(outcome_var_multi, "grade")) {
    # for grades no further steps are necessary
    data_dml <- data_dml
  } else if (str_detect(outcome_var_multi, "bigfive")) {
    # for personality selected outcome variable needs to be declared
    outcome_var_old <- outcome_var_multi
    outcome_var_multi <- paste0("outcome_", outcome_var_multi)
    data_dml <- data_dml %>%
      rename_with(~ outcome_var_multi, all_of(outcome_var_old))
    # lags for all other personality variables are dropped
    colnames_bigfive_lag_drop <- data_dml %>% 
      dplyr::select(starts_with("bigfive") & ends_with("lag") & !matches(outcome_var_old)) %>% colnames()
    data_dml <- data_dml %>% 
      dplyr::select(-all_of(colnames_bigfive_lag_drop))
  }
  
  
  #%%%%%%%%%%%#
  #### APE ####
  #%%%%%%%%%%%#
  
  df_ape <- data.frame(
    "weekly_no" = data_dml %>% filter(treatment_sport_freq == 1) %>% pull(outcome_var_multi) %>% mean() -
      data_dml %>% filter(treatment_sport_freq == 3) %>% pull(outcome_var_multi) %>% mean(),
    "monthly_no" = data_dml %>% filter(treatment_sport_freq == 2) %>% pull(outcome_var_multi ) %>% mean() -
      data_dml %>% filter(treatment_sport_freq == 3) %>% pull(outcome_var_multi) %>% mean(),
    "monthly_weekly" = data_dml %>% filter(treatment_sport_freq == 2) %>% pull(outcome_var_multi ) %>% mean() -
      data_dml %>% filter(treatment_sport_freq == 1) %>% pull(outcome_var_multi) %>% mean()
  )
  

  #%%%%%%%%%%%%%%%%%%#
  #### ATE & ATET ####
  #%%%%%%%%%%%%%%%%%%#
  
  # only save common support plot for main model
  if (cohort_prep == main_cohort_prep & treatment_def == main_treatment_def  &
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_controls == main_model_controls) {
    save_trimming_sel <- TRUE
  } else {
    save_trimming_sel <- FALSE
  }

  # run DML
  dml_result <- func_dml(
    treatment_setting = "multi", data = data_dml, 
    outcome = outcome_var_multi, treatment = "treatment_sport_freq", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = multi_model_algo, trimming = model_trimming, 
    # save common support plot
    save_trimming = save_trimming_sel,
    # model generation: separate 
    probscore_separate = probscore_separate, mice_data_sel
  )
  
  # append APE
  dml_result$ape <- df_ape
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
  
}


# save results
if (probscore_separate == FALSE) {
  probscore_separate_save <- "_nonseparate"
} else {
  probscore_separate_save <- ""
}

if (str_detect(outcome_var_multi, "grade")) {
  save_dml <- 
    paste0("Output/DML/Estimation/Grades/multi_grades_", multi_model_algo, 
           "_", model_type, "_", str_replace_all(cohort_prep, "_", ""),
           "_", treatment_def, "_", treatment_repl, extra_act_save, 
           probscore_separate_save, ".rds")
} else if (str_detect(outcome_var, "bigfive")) {
  paste0("Output/DML/Estimation/Personality/multi_personality", multi_model_algo, 
         "_", model_type, "_", str_replace_all(cohort_prep, "_", ""),
         "_", treatment_def, "_", treatment_repl, extra_act_save, 
         probscore_separate_save, ".rds")
}

saveRDS(dml_result_all, save_dml)


# calculate pooled estimate over multiple mice data sets
dml_result_pooled_all <- func_dml_pool_mice(dml_result_all, nrow(data_dml), 5)
dml_result_pooled <- dml_result_pooled_all[[1]]
dml_result_error <- dml_result_pooled_all[[2]]

# append columns
dml_result_save <- dml_result_pooled %>%
  mutate(
    # append outcome
    outcome = str_remove(outcome_var_multi, "outcome_"),
    # append model selections
    cohort_prep = main_cohort_prep, treatment_repl = main_treatment_repl, 
    treatment_def = main_treatment_def, extra_act = main_extra_act, 
    # append user selections
    model_type = main_model_type, model_algo = multi_model_algo, model_k = main_model_k, 
    model_k_tuning = main_model_k_tuning, model_s_rep = main_model_s_rep, 
    model_trimming = main_model_trimming, model_controls = main_model_controls,
    # number of treatment periods before and after after trimming
    n_treats_before = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_before"))), 
    n_treats_after = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_after"))), 
    # type of model generation
    Treatment_model_separate = probscore_separate, 
    # add date
    time_stamp = as.character(Sys.time()),
    # add computation time
    time_elapsed = paste(as.character(difftime(Sys.time(), start_time, units = "hours")), "hours")) %>%
  cbind(dml_result_error) %>%
  # re-order columns
  dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
                n_treats_before, n_treats_after, Treatment_model_separate, starts_with("num_pred"), 
                Treatment, everything()) %>%
  relocate(time_elapsed, time_stamp, .after = last_col()) # time-stamp is ordered last


# save estimation results
dml_result_save <- as.data.frame(dml_result_save)
if (probscore_separate == TRUE) {
  if (file.exists("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx")) {
    ## replace same estimations
    dml_result_save_all <- 
      read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
    dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
    cols_aggr <- dml_result_save_all %>%
      dplyr::select(cohort_prep, treatment_repl, treatment_def, starts_with("model")) %>%
      colnames()
    dml_result_save_all <- dml_result_save_all %>%
      group_by(across(all_of(cols_aggr))) %>%
      filter(time_stamp == max(time_stamp)) %>%
      ungroup() %>% data.frame()
    ## save
    write.xlsx(dml_result_save_all, "Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", 
               sheetName = "Sheet1", row.names = FALSE, append = FALSE, showNA = FALSE)
  } else {
    write.xlsx(dml_result_save, "Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", 
               row.names = FALSE)
  }
} else {
  if (file.exists("Output/DML/Treatment_Effects/DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx")) {
    ## replace same estimations
    dml_result_save_all <- 
      read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
    dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
    cols_aggr <- dml_result_save_all %>%
      dplyr::select(cohort_prep, treatment_repl, treatment_def, starts_with("model")) %>%
      colnames()
    dml_result_save_all <- dml_result_save_all %>%
      group_by(across(all_of(cols_aggr))) %>%
      filter(time_stamp == max(time_stamp)) %>%
      ungroup() %>% data.frame()
    ## save
    write.xlsx(dml_result_save_all, "Output/DML/Treatment_Effects/DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx", 
               sheetName = "Sheet1", row.names = FALSE, append = FALSE, showNA = FALSE)
  } else {
    write.xlsx(dml_result_save, "Output/DML/Treatment_Effects/DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx", 
               row.names = FALSE)
  }
}











