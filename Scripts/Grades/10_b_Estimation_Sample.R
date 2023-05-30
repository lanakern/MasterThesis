#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the estimation samples are established. 
# Again, the preparations are made for the different MICE data sets and differ
# across the sample specifications. 
#+++
# 1.) Remove unnecessary variables like the id, character and constant variables.
#+++
# 2.) Binary Treatment Setting
# -> Sample with all predictors (saving also differs between lags and no lags)
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# 3.) Multivalud Treatment Setting
# -> Sample with all predictors (saving also differs between lags and no lags)
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# -> The resulting data frames are final data sets used for DML. No further
# adjustments are carried out. 
#+++

# extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else if (extra_act == "uni") {
  extra_act_save <- "_extrauni"
} else {
  extra_act_save <- ""
}

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}

# number of columns may differ across MICE data sets
data_binary_num_cols <- data.frame()
data_binary_num_cols_nolags <- data.frame()
data_binary_num_cols_all <- data.frame()
data_multi_num_cols <- data.frame()
data_multi_num_cols_nolags <- data.frame()
data_multi_num_cols_all <- data.frame()

# ITERATE OVER MICE DATA SETS
for (mice_data_sel in 1:5) {
  
  print(paste("Data Set:", mice_data_sel))
  
  gc()
  
  #%%%%%%%%%%%%%#
  ## LOAD DATA ##
  #%%%%%%%%%%%%%#
  
  # load data
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Grades/Prep_9/prep_9_intpoly_weekly_down_", 
                        "mice", mice_data_sel,  ".rds")
  } else {
    data_load <- paste0("Data/Grades/Prep_9/prep_9_intpoly_weekly_down", 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel,  ".rds")
  }
  
  data_final_raw <- readRDS(data_load)
  
  if (interactions == "yes") {
    data_final_raw <- data_final_raw
  } else {
    data_final_raw <- data_final_raw %>% dplyr::select(-contains(":"), -matches("_order[0-9]$"))
  }
  
  # ungroup and correct data types
  data_final <- data_final_raw %>% ungroup() %>% type.convert(as.is = TRUE)
  
  # change treatment group to all sport participation levels
  if (treatment_def == "all" & cohort_prep == main_cohort_prep) {
    data_final <- data_final %>% 
      mutate(treatment_sport = ifelse(treatment_sport_freq != "never", 1, 0)) 
  } else {
    data_final <- data_final
  }
  
  # drop replaced treatment and outcome information
  if (treatment_repl == "no" & cohort_prep == main_cohort_prep) {
    data_final <- data_final %>% 
      filter(outcome_grade_na == 0 & treatment_sport_na == 0)
  } else {
    data_final <- data_final
  }
  
  # drop students who do not take part in any extracurricular activity
  if (extra_act == "yes" & cohort_prep == main_cohort_prep) {
    data_final <- data_final %>%
      filter(extracurricular_num > 0 | treatment_sport == 1)
  } else if (extra_act == "uni") {
    data_final <- data_final %>% 
      filter(extracurricular_uni == 1 | treatment_sport == 1) %>%
      filter(extracurricular_num > 0 | treatment_sport == 1)
  } else {
    data_final <- data_final
  }
  
  id_no_extra <- length(unique(data_final$id_t))
  obs_no_extra <- nrow(data_final)
  
  # drop interview_start_year_num %in% c(1,8) and respective dummies to enforce
  # common support
  if (cohort_prep == "controls_same_outcome" & cov_balance == "yes") {
    data_final <- data_final %>% filter(!interview_start_year_num %in% c(1,8)) %>%
      dplyr::select(-contains("interview_end_year_2018"), -contains("interview_start_year_2018"))
  } else {
    data_final <- data_final
  }
  id_common_support <- length(unique(data_final$id_t))
  obs_common_support <- nrow(data_final)
  
  # save now for comparison with personality
  if (mice_data_sel == 1 & cohort_prep == main_cohort_prep & treatment_def == main_treatment_def &
      treatment_repl == main_treatment_repl & extra_act == main_extra_act) {
    saveRDS(data_final, "Data/Grades/Prep_10/COMPARE_ID_GRADES.rds")
  }
  
  # drop ID_t, interview_date, etc. which is not used in the estimation
  data_final <- data_final %>% 
    # instead of id enumerator is established which corresponds to which
    # group the observation belongs; this info is only used for sample splitting
    # in the cross-fitting procedure
    mutate(group = as.integer(factor(id_t,levels = unique(id_t))))  %>%
    dplyr::select(-id_t)
  
  # ensure all character variables are dropped
  treatment_sport_freq <- data_final$treatment_sport_freq # keep
  treatment_sport_freq_lag <- data_final$treatment_sport_freq_lag
  data_final <- data_final[, !sapply(data_final, is.character)]
  data_final$treatment_sport_freq <- treatment_sport_freq
  data_final$treatment_sport_freq_lag <- treatment_sport_freq_lag
  
  # ensure all constant variables are dropped
  data_final <- remove_constant(data_final)
  
  # adjust treatment period (may change due to drop outs)
  if (treatment_repl != "no") {
    data_final <- data_final %>%
      group_by(group) %>%
      mutate(treatment_period = row_number()) %>%
      ungroup()
  } else {
    if ("treatment_period" %in% colnames(data_final)) {
      # no treatment_period required for treatment_repl = "no" 
      data_final <- data_final %>% dplyr::select(-treatment_period)
    } else {
      data_final <- data_final
    }
  }

  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # generate "all" treatment group and then drop sport participation frequency variables
  data_binary <- data_final %>% 
    dplyr::select(-starts_with("treatment_sport_freq"), -contains(":"), -matches("_order[0-9]$"))
  
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_binary, paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
                                treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_binary, paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
                                treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  
  # SAVE NUMBER OF VARIABLES ETC. ONLY AFTER 5th ITERATION 
  # Then mean is saved across the five mice data sets
  data_binary_num_cols <- rbind(
    data_binary_num_cols, 
    data.frame("num_cols" = ncol(data_binary), "num_rows" = nrow(data_binary), 
               "num_id" = length(unique(data_binary$group)))
    )
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "binary_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "data_prep_covbal" = cov_balance,
      "num_id" = mean(data_binary_num_cols$num_id), 
      "num_rows" = mean(data_binary_num_cols$num_rows),
      "num_cols" = mean(data_binary_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }
  
  
  # Only save for non-lag variables (but no extra data set)
  data_binary_lags <- data_binary %>% dplyr::select(-ends_with("_lag"))
  data_binary_num_cols_nolags <- rbind(
    data_binary_num_cols_nolags, 
    data.frame("num_cols" = ncol(data_binary_lags), "num_rows" = nrow(data_binary_lags),
               "num_id" = length(unique(data_binary_lags$group)))
  )
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "binary_nolags",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "data_prep_covbal" = cov_balance,
      "num_id" = mean(data_binary_num_cols_nolags$num_id), 
      "num_rows" = mean(data_binary_num_cols_nolags$num_rows),
      "num_cols" = mean(data_binary_num_cols_nolags$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }

  
  
  #### All + Interaction + Polynomials ####
  #+++++++++++++++++++++++++++++++++++++++#
  
  if (interactions == "yes") {
    data_all_plus <- data_final %>% dplyr::select(-starts_with("treatment_sport_freq"))
    
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds")
      )
    } else {
      saveRDS(data_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds")
      )
    }
    
    # again save mean after 5th iteration
    data_binary_num_cols_all <- rbind(
      data_binary_num_cols_all, 
      data.frame("num_cols" = ncol(data_all_plus), "num_rows" = nrow(data_all_plus),
                 "num_id" = length(unique(data_all_plus$group))))
    
    if (mice_data_sel == 5) {
      # SAVE NUMBER OF VARIABLES ETC.
      df_excel_save <- data.frame(
        "data_prep_step" = "estimation_sample",
        "data_prep_step_2" = "binary_all_int_poly",
        "data_prep_choice_cohort" = cohort_prep,
        "data_prep_treatment_repl" = treatment_repl,
        "data_prep_treatment_def" = treatment_def,
        "data_prep_extraact" = extra_act,
        "data_prep_covbal" = cov_balance,
        "num_id" = mean(data_binary_num_cols_all$num_id),
        "num_rows" = mean(data_binary_num_cols_all$num_rows),
        "num_cols" = mean(data_binary_num_cols_all$num_cols),
        "time_stamp" = Sys.time()
      )
      ## load function
      source("Functions/func_save_sample_reduction.R")
      func_save_sample_reduction(df_excel_save, "grade")
      gc()
    }
  }

  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### MULTIPLE TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  # drop treatment variables from binary treatment setting
  # for data preparation of no treatment replacement some variables does not exist
  drop_vars_multi <- c("treatment_sport", "treatment_sport_na",
                       "treatment_sport_lag", "treatment_sport_lag_na", 
                       "treatment_sport_source_uni", "treatment_sport_source_leisure",
                       "treatment_sport_freq_never", "treatment_sport_freq_weekly_atleast",
                       "treatment_sport_freq_never_lag", "treatment_sport_freq_weekly_atleast_lag")
  drop_vars_multi <- drop_vars_multi[drop_vars_multi %in% colnames(data_final)]
  data_multi_sub_all <- data_final %>% dplyr::select(-all_of(drop_vars_multi))

  
  # create treatment_sport_freq dummy
  data_multi_sub_all <- fastDummies::dummy_cols(
    data_multi_sub_all, remove_selected_columns = FALSE, remove_first_dummy = FALSE, 
    select_columns = c("treatment_sport_freq", "treatment_sport_freq_lag")
  ) %>% dplyr::select(-c(treatment_sport_freq_lag, treatment_sport_freq_lag_never))
  
  # treatment_sport_freq as number
  data_multi_sub_all <- data_multi_sub_all %>%
    mutate(treatment_sport_freq = case_when(
      treatment_sport_freq == "weekly_atleast" ~ 1,
      treatment_sport_freq == "monthly_less" ~ 2,
      treatment_sport_freq == "never" ~ 3,
      TRUE ~ 4
    ))
  
  data_multi_all <- data_multi_sub_all %>% 
    dplyr::select(-contains(":"), -matches("_order[0-9]$"))
  
  # save data frames
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_multi_all, 
            paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_multi_all, 
            paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  # SAVE NUMBER OF VARIABLES ETC. AFTER 5th ITERATION
  data_multi_num_cols <- rbind(
    data_multi_num_cols, 
    data.frame("num_cols" = ncol(data_multi_all), "num_rows" = nrow(data_multi_all),
               "num_id" = length(unique(data_multi_all$group))))
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "multi_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act,
      "data_prep_covbal" = cov_balance,
      "num_id" = mean(data_multi_num_cols$num_id),
      "num_rows" = mean(data_multi_num_cols$num_rows),
      "num_cols" = mean(data_multi_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }
  
  
  # Only save for non-lag variables (but no extra data set)
  data_multi_all_lags <- data_multi_all %>% dplyr::select(-ends_with("_lag"))
  data_multi_num_cols_nolags <- rbind(
    data_multi_num_cols_nolags, 
    data.frame("num_cols" = ncol(data_multi_all_lags), "num_rows" = nrow(data_multi_all_lags),
               "num_id" = length(unique(data_multi_all_lags$group))))
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "multi_nolags",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "data_prep_covbal" = cov_balance,
      "num_id" = mean(data_multi_num_cols_nolags$num_id), 
      "num_rows" = mean(data_multi_num_cols_nolags$num_rows),
      "num_cols" = mean(data_multi_num_cols_nolags$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }
  
  #### All + Interaction + Polynomials ####
  #+++++++++++++++++++++++++++++++++++++++#
  
  if (interactions == "yes") {
    data_multi_all_plus <- data_multi_sub_all
    
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds")
      )
    } else {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds")
      )
    }
    
    # again save mean after 5th iteration
    data_multi_num_cols_all <- rbind(
      data_multi_num_cols_all, 
      data.frame("num_cols" = ncol(data_multi_all_plus), 
                 "num_rows" = nrow(data_multi_all_plus),
                 "num_id" = length(unique(data_multi_all_plus$group))))
    
    if (mice_data_sel == 5) {
      # SAVE NUMBER OF VARIABLES ETC.
      df_excel_save <- data.frame(
        "data_prep_step" = "estimation_sample",
        "data_prep_step_2" = "multi_all_int_poly",
        "data_prep_choice_cohort" = cohort_prep,
        "data_prep_treatment_repl" = treatment_repl,
        "data_prep_treatment_def" = treatment_def,
        "data_prep_extraact" = extra_act,
        "data_prep_covbal" = cov_balance,
        "num_id" = mean(data_multi_num_cols_all$num_id),
        "num_rows" = mean(data_multi_num_cols_all$num_rows),
        "num_cols" = mean(data_multi_num_cols_all$num_cols),
        "time_stamp" = Sys.time()
      )
      ## load function
      source("Functions/func_save_sample_reduction.R")
      func_save_sample_reduction(df_excel_save, "grade")
      gc()
    } # close saving
  }
  
} # close iteration over mice data sets
