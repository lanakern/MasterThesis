#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML IN THE BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file the ATE and ATTE is estimated using DML in the binary
# treatment setting, that is sport participation vs. non participation. 
# As outcome both grades and the big five personality traits can be specified.
#++++
# The estimation results are stored in an Excel file.
#++++


# track time
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
    ## extract extracurricular activity ending
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
    ## extract outcome
  if (str_detect(outcome_var, "grade")) {
    load_data_folder <- "Data/Grades/"
    load_data_ending <- ".rds"
  } else if (str_detect(outcome_var, "bigfive")) {
    load_data_folder <- "Data/Personality/"
    load_data_ending <- "_personality.rds"
  } else {
    stop("Please specify correct outcome variable")
  }
  
    ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
  }
  
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
  if (str_detect(outcome_var, "grade")) {
    # for grades no further steps are necessary
    data_dml <- data_dml
  } else if (str_detect(outcome_var, "bigfive")) {
    # for personality selected outcome variable needs to be declared
    outcome_var_old <- outcome_var
    outcome_var <- paste0("outcome_", outcome_var)
    data_dml <- data_dml %>%
      rename_with(~ outcome_var, all_of(outcome_var_old))
    # lags for all other personality variables are dropped
    colnames_bigfive_lag_drop <- data_dml %>% 
      dplyr::select(starts_with("bigfive") & ends_with("lag") & !matches(outcome_var_old)) %>% colnames()
    data_dml <- data_dml %>% 
      dplyr::select(-all_of(colnames_bigfive_lag_drop))
  }
  

  #%%%%%%%%%%%#
  #### APE ####
  #%%%%%%%%%%%#
  
  # https://docs.doubleml.org/stable/examples/R_double_ml_pension.html
  # (unconditional) Average Predictive Effect (APE) of doing sports on grades.
  # This effect corresponds to the ATE if sport participation would be assigned to 
  # individuals in an entirely randomized way.
  # APE is a naive estimate of the ATE and biased since it does not account for 
  # endogeneity of participation.
  ape <- data_dml %>% filter(treatment_sport == 1) %>% pull(outcome_var) %>% mean() -
      data_dml %>% filter(treatment_sport == 0) %>% pull(outcome_var) %>% mean()
  
  # same across mice data sets as no missing values are replaced
  # df_ape <- data.frame("MICE" = mice_data_sel, "APE" = ape)
  # df_ape_all <- rbind(df_ape_all, df_ape)


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
    treatment_setting, data = data_dml, 
    outcome = outcome_var, treatment = "treatment_sport", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = model_algo, trimming = model_trimming, save_trimming = save_trimming_sel,
    mice_sel = mice_data_sel
  )
  
  # append APE
  dml_result$ape <- ape
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
  
}


# save results
if (str_detect(outcome_var, "grade")) {
  save_dml <- 
    paste0("Output/DML/Estimation/Grades/binary_grades_", model_algo, "_", 
           model_type, "_", str_replace_all(cohort_prep, "_", ""),
           "_", treatment_def, "_", treatment_repl, extra_act_save, ".rds")
} else if (str_detect(outcome_var, "bigfive")) {
  paste0("Output/DML/Estimation/Personality/binary_personality_", model_algo, "_", 
         model_type, "_", str_replace_all(cohort_prep, "_", ""),
         "_", treatment_def, "_", treatment_repl, extra_act_save, ".rds")
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
    outcome = str_remove(outcome_var, "outcome_"),
    # append model selections
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, treatment_def = treatment_def, 
    extra_act = extra_act, 
    # append user selections
    model_type = model_type, model_algo = model_algo, model_k = model_k, 
    model_k_tuning = model_k_tuning, model_s_rep = model_s_rep, model_trimming = model_trimming, 
    model_controls = model_controls,
    # number of treatment periods before and after after trimming
    n_treats_before = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_before"))), 
    n_treats_after = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_after"))), 
    # add date
    time_stamp = as.character(Sys.time()),
    # addt time
    time_elapsed = paste(as.character(difftime(Sys.time(), start_time, units = "hours")), "hours")) %>%
  cbind(dml_result_error) %>%
  # re-order columns
  dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act,  
                starts_with("model"), n_treats_before, n_treats_after,
                starts_with("num_pred"), everything()) %>%
  relocate(time_elapsed, time_stamp, .after = last_col()) # time-stamp is ordered last


# save estimation results
dml_result_save <- as.data.frame(dml_result_save)
if (file.exists("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx")) {
  ## replace same estimations
  dml_result_save_all <- read.xlsx("Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
  dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
  cols_aggr <- dml_result_save_all %>%
    dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, starts_with("model")) %>%
    colnames()
  dml_result_save_all <- dml_result_save_all %>%
    group_by(across(all_of(cols_aggr))) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  ## save
  write.xlsx(dml_result_save_all, "Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
} else {
  write.xlsx(dml_result_save, "Output/DML/Treatment_Effects/DML_BINARY_ESTIMATION_RESULTS.xlsx", row.names = FALSE)
}
