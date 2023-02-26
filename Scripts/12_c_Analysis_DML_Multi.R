#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML IN THE MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file the ATE and ATTE is estimated using DML in the mulivalued
# treatment setting. For simplicity this is only done for the main model.
# Moreover, the nuisance parameters are only estimated via XGBoost.
#++++
# treatment_sport_freq:
# -> 1: at least weekly
# -> 2: monthly or less frequently
# -> 3: never
#++++
# Outcome and treatment regressions
#++++

# Start time
start_time <- Sys.time()

# set seed for reproducible results
set.seed(1234)

# empty data frames and lists to store results
dml_result_all <- list()
# multi_model_algo <- c("lasso", "postlasso", "rf", "xgboost")

# iterate over mice data sets
for (mice_data_sel in 1:5) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
  # load data
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }

  load_data <- paste0(
    "Data/Prep_11/prep_11_dml_multi_", model_type, "_", model_outcome,
    "_", treatment_def, "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"
    )

  load_data <- str_replace(load_data, "_level", "") # drop level
  
  data_dml <- readRDS(load_data)
  
  if (model_controls == "no_lags") {
    data_dml <- data_dml %>% select(-c(ends_with("_lag")))
  } else {
    data_dml <- data_dml
  }
  
  # outcome variable depends on selection
  if (model_outcome == "level") {
    outcome_var <- "outcome_grade"
  } else if (model_outcome == "stand") {
    outcome_var <- "outcome_grade_stand"
  }
  
  
  #%%%%%%%%%%%#
  #### APE ####
  #%%%%%%%%%%%#
  
  ape <- data_dml %>% filter(treatment_sport == 1) %>% pull(outcome_var) %>% mean() -
    data_dml %>% filter(treatment_sport == 0) %>% pull(outcome_var) %>% mean()
  

  #%%%%%%%%%%%%%%%%%%#
  #### ATE & ATET ####
  #%%%%%%%%%%%%%%%%%%#
  

  # run DML
  dml_result <- func_dml(
    treatment_setting = "multi", data = data_dml, 
    outcome = outcome_var, treatment = "treatment_sport_freq", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = multi_model_algo, trimming = model_trimming, 
    # save common support plot
    save_trimming = TRUE,
    # model generation
    probscore_separate
  )
  
  # append APE
  dml_result$ape <- ape
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
}


# save results
save_dml <- 
  paste0("Output/DML/", treatment_setting, "_", multi_model_algo, "_", main_model_type, "_", 
         main_model_outcome, "_", str_replace_all(main_cohort_prep, "_", ""),
         "_", main_treatment_def, "_", main_treatment_repl, extra_act_save, ".rds")

saveRDS(dml_result_all, save_dml)


# calculate pooled estimate over multiple mice data sets
dml_result_pooled_all <- func_dml_pool_mice(dml_result_all, nrow(data_dml), 5)
dml_result_pooled <- dml_result_pooled_all[[1]]
dml_result_error <- dml_result_pooled_all[[2]]

# append columns
dml_result_save <- dml_result_pooled %>%
  mutate(
    # append model selections
    cohort_prep = main_cohort_prep, treatment_repl = main_treatment_repl, 
    treatment_def = main_treatment_def, extra_act = main_extra_act, 
    # append user selections
    model_type = main_model_type, model_algo = multi_model_algo, model_k = main_model_k, 
    model_k_tuning = main_model_k_tuning, model_s_rep = main_model_s_rep, 
    model_trimming = main_model_trimming, model_outcome = main_model_outcome, 
    model_controls = main_model_controls,
    # number of treatment periods after trimming
    n_treats_min = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats"))), 
    # type of model generation
    Treatment_model_separate = probscore_separate, 
    # add date
    time_stamp = as.character(Sys.time()),
    # add computation time
    time_elapsed = as.character(Sys.time() - start_time)) %>%
  cbind(dml_result_error) %>%
  # re-order columns
  select(cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
         n_treats_min, starts_with("num_pred"), Treatment_model_separate, Treatment, everything()) %>%
  relocate(time_elapsed, time_stamp, .after = last_col()) # time-stamp is ordered last


# save estimation results
dml_result_save <- as.data.frame(dml_result_save)
if (file.exists("Output/DML/DML_ESTIMATION_RESULTS_MULTI.xlsx")) {
  ## replace same estimations
  dml_result_save_all <- read.xlsx("Output/DML/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
  dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
  cols_aggr <- dml_result_save_all %>%
    select(cohort_prep, treatment_repl, treatment_def, starts_with("model")) %>%
    colnames()
  dml_result_save_all <- dml_result_save_all %>%
    group_by(across(all_of(cols_aggr))) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  ## save
  write.xlsx(dml_result_save_all, "Output/DML/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
} else {
  write.xlsx(dml_result_save, "Output/DML/DML_MULTI_ESTIMATION_RESULTS.xlsx", row.names = FALSE)
}









