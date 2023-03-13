#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the estimation samples are established. 
# Again, the preparations are made for the different MICE data sets.
# The binary treatment setting contains one variable more than the multivalued
# treatment setting because the monthly frequency is included as additional
# control in the binary setting.
#+++
# 1.) Remove unnecessary variables like the id, character and constant variables.
#+++
# 2.) Binary Treatment Setting
# -> Sample with all predictors
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# 3.) Multivalud Treatment Setting
# -> Sample with all predictors
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# -> The resulting data frames are final data sets used for DML
#+++

# extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}

# number of columns may differ across MICE data sets
data_binary_num_cols <- data.frame()
data_binary_num_cols_all <- data.frame()
data_multi_num_cols <- data.frame()

# ITERATE OVER MICE DATA SETS
for (mice_data_sel in 1:5) {
  
  print(paste("Data Set:", mice_data_sel))
  
  gc()
  
  #%%%%%%%%%%%%%#
  ## LOAD DATA ##
  #%%%%%%%%%%%%%#
  
  # load data
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_mice", mice_data_sel,  ".rds")
  } else {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_robustcheck", "_mice", mice_data_sel,  ".rds")
  }
  
  data_final_raw <- readRDS(data_load)
  
  # ungroup and correct data types
  data_final <- data_final_raw %>% ungroup() %>% type.convert(as.is = TRUE)
  
  # drop ID_t, interview_date, etc. which is not used in the estimation
  data_final <- data_final %>% 
    # instead of id enumerator is established which corresponds to which
    # group the observation belongs; this info is only used for sample splitting
    # in the cross-fitting procedure
    mutate(group = as.integer(factor(id_t,levels = unique(id_t))))  %>%
    dplyr::select(-c(id_t, starts_with("interview_date"), treatment_period, 
              starts_with("na_count"), ends_with("_cat")))
  
  # ensure all character variables are dropped
  treatment_sport_freq <- data_final$treatment_sport_freq # keep
  treatment_sport_freq_lag <- data_final$treatment_sport_freq_lag
  data_final <- data_final[, !sapply(data_final, is.character)]
  data_final$treatment_sport_freq <- treatment_sport_freq
  data_final$treatment_sport_freq_lag <- treatment_sport_freq_lag
  
  # ensure all constant variables are dropped
  data_final <- remove_constant(data_final)
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # drop sport participation frequency variables
  data_binary <- data_final %>% dplyr::select(-starts_with("treatment_sport_freq"))
  
  # dummy for monthly sport participation
    # never if treatment_sport = 0 and treatment_sport_freq_monthly_less = 0
    # never if treatment_sport = 1 and treatment_sport_freq_monthly_less = 0
  data_binary <- data_binary %>% mutate(
    treatment_sport_freq_monthly_less = 
      case_when(treatment_sport_freq == "monthly_less" ~ 1, TRUE ~ 0)
  )
  
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_binary, paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
                                treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_binary, paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
                                treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  
  # SAVE NUMBER OF VARIABLES ETC. ONLY AFTER 5th ITERATION 
  # Then mean is saved across the five mice data sets
  data_binary_num_cols <- rbind(
    data_binary_num_cols, 
    data.frame("num_cols" = ncol(data_binary %>% dplyr::select(
      -c("outcome_grade", "treatment_sport"))), 
      "num_rows" = nrow(data_binary))
    )
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "binary_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "num_id" = length(unique(data_final_raw$id_t)), 
      "num_rows" = mean(data_binary_num_cols$num_rows),
      "num_cols" = mean(data_binary_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }
  

  
  
  #### All + Interaction + Polynomials ####
  #+++++++++++++++++++++++++++++++++++++++#

  if (create_interactions == "yes") { 
    # create new data frame
    data_all_plus <- data_binary
  
    #-- INTERACTION TERMS --#
  
    ## 1.) Drop treatment and outcome variables which are not used to generate interactions
    df_interaction <- data_all_plus %>% 
      dplyr::select(-c(starts_with("treatment"), starts_with("outcome"), group))
    ## 2.) Generate interactions
    # https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
    df_interaction <- do.call(cbind, combn(colnames(df_interaction), 2, FUN = function(x)
      list(setNames(data.frame(df_interaction[,x[1]]*df_interaction[,x[2]]),
                    paste(x, collapse = ":")))))
    ## 3.) Add interactions to full data frame
    data_all_plus <- cbind(data_all_plus, df_interaction)
    ## 4.) Drop interactions containing 80% or more empty cells
    ## only for those with at least 80% of zeros this is operation relevant
    colnames_interaction <- colnames(
      df_interaction[, which(as.numeric(colSums(df_interaction == 0) / nrow(df_interaction)) >= 0.80)]
      )
    #colnames_interaction_sub <- colnames_interaction[1:10000]
    gc()
    i <- 0
    for (colnames_interaction_sel in colnames_interaction) {
      i <- i + 1
      if (i %% 5000 == 0) {print(paste("Iteration", i))}
    
      colnames_interaction_sel_1 <- str_split(colnames_interaction_sel, ":")[[1]][1]
      colnames_interaction_sel_2 <- str_split(colnames_interaction_sel, ":")[[1]][2]
      num_rows_interaction <- 
        data_all_plus %>% 
        dplyr::select(all_of(colnames_interaction_sel), 
                      all_of(colnames_interaction_sel_1), 
                      all_of(colnames_interaction_sel_2)) %>%
        filter(!!rlang::sym(colnames_interaction_sel_1) == 0 & 
                 !!rlang::sym(colnames_interaction_sel_2) == 0) %>%
        nrow()
      num_rows_interaction <- num_rows_interaction / nrow(data_all_plus)
      if (num_rows_interaction >= 0.80) {
        data_all_plus <- data_all_plus %>% dplyr::select(-all_of(colnames_interaction_sel))
      } else {
        data_all_plus <- data_all_plus
      }
    }
    # OLD: drop zeros
    # df_interaction <- df_interaction[, which(as.numeric(colSums(df_interaction == 0) / nrow(df_interaction)) < 0.95)]
  
    #-- POLYNOMIALS --#
  
    ## Extract all numeric variables (from data frame without interactions)
    cols_numeric_all <- names(unlist(lapply(data_binary, class)[lapply(data_binary, class) == "numeric"]))
  
    ## Drop columns that should not contain polynomials
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "outcome")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "treatment")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "na_")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "group")]
  
    ## Generate polynomials of degree 2 to 4
    data_all_plus <- data_all_plus %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))
  
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
              )
    } else {
      saveRDS(data_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds")
              )
    }
  
    # again save mean after 5th iteration
    data_binary_num_cols_all <- rbind(
      data_binary_num_cols_all, 
      data.frame("num_cols" = ncol(data_all_plus %>% dplyr::select(
        -c("outcome_grade", "treatment_sport"))), 
        "num_rows" = nrow(data_all_plus))
    )
    
    if (mice_data_sel == 5) {
      # SAVE NUMBER OF VARIABLES ETC.
      df_excel_save <- data.frame(
        "data_prep_step" = "estimation_sample",
        "data_prep_step_2" = "binary_all_int_poly",
        "data_prep_choice_cohort" = cohort_prep,
        "data_prep_treatment_repl" = treatment_repl,
        "data_prep_treatment_def" = treatment_def,
        "data_prep_extraact" = extra_act,
        "num_id" = length(unique(data_final_raw$id_t)),
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
  
  # drop treatment variables for binary treatment setting
  data_multi_all <- data_final %>%
    dplyr::select(-c(treatment_sport, treatment_sport_lag, treatment_sport_na,
                     treatment_sport_source_uni, treatment_sport_source_leisure,
                     treatment_sport_freq_never, treatment_sport_freq_weekly_atleast,
                     treatment_sport_freq_lag_monthly_less, treatment_sport_freq_lag_never, 
                     treatment_sport_freq_lag_weekly_atleast))
  
  # create treatment_sport_freq dummy
  data_multi_all <- fastDummies::dummy_cols(
    data_multi_all, remove_selected_columns = FALSE, remove_first_dummy = FALSE, 
    select_columns = "treatment_sport_freq"
  )
  
  # treatment_sport_freq as number
  data_multi_all <- data_multi_all %>%
    mutate(treatment_sport_freq = case_when(
      treatment_sport_freq == "weekly_atleast" ~ 1,
      treatment_sport_freq == "monthly_less" ~ 2,
      TRUE ~ 3
    ))
  
  # lag also as number
  data_multi_all <- data_multi_all %>%
    mutate(treatment_sport_freq_lag = case_when(
      treatment_sport_freq_lag == "weekly_atleast" ~ 1,
      treatment_sport_freq_lag == "monthly_less" ~ 2,
      treatment_sport_freq_lag == "never" ~ 3,
      TRUE ~ 4
    ))
    ## drop lag dummies
  data_multi_all <- data_multi_all %>% dplyr::select(-starts_with("treatment_sport_freq_lag_"))
  
  # save data frames
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_multi_all, 
            paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_multi_all, 
            paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  # SAVE NUMBER OF VARIABLES ETC. AFTER 5th ITERATION
  data_multi_num_cols <- rbind(
    data_multi_num_cols, 
    data.frame("num_cols" = ncol(data_multi_all %>% dplyr::select(-c(
      "treatment_sport_freq_never", "treatment_sport_freq_weekly_atleast", 
      "treatment_sport_freq_source_leisure", "treatment_sport_freq", "outcome_grade"
    ))), 
    "num_rows" = nrow(data_multi_all))
  )
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "multi_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act,
      "num_id" = length(unique(data_final_raw$id_t)),
      "num_rows" = mean(data_multi_num_cols$num_rows),
      "num_cols" = mean(data_multi_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "grade")
    gc()
  }
  
  
  #### All + Interaction + Polynomials ####
  #+++++++++++++++++++++++++++++++++++++++#
  
  if (create_interactions == "yes") { 
    # create new data frame
    data_multi_all_plus <- data_multi_all
    
    #-- INTERACTION TERMS --#
    
    ## 1.) Drop treatment and outcome variables which are not used to generate interactions
    df_interaction_multi <- data_multi_all_plus %>% 
      dplyr::select(-c(starts_with("treatment"), starts_with("outcome"), group))
    ## 2.) Generate interactions
    # https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
    df_interaction_multi <- do.call(cbind, combn(colnames(df_interaction_multi), 2, FUN = function(x)
      list(setNames(data.frame(df_interaction_multi[,x[1]]*df_interaction_multi[,x[2]]),
                    paste(x, collapse = ":")))))
    ## 3.) Add interactions to full data frame
    data_multi_all_plus <- cbind(data_multi_all_plus, df_interaction_multi)
    ## 4.) Drop interactions containing 80% or more empty cells
    ## only for those with at least 80% of zeros this is operation relevant
    colnames_interaction <- colnames(
      df_interaction_multi[, which(as.numeric(colSums(df_interaction_multi == 0) / nrow(df_interaction_multi)) >= 0.80)]
    )
    #colnames_interaction_sub <- colnames_interaction[1:10000]
    gc()
    i <- 0
    for (colnames_interaction_sel in colnames_interaction) {
      i <- i + 1
      if (i %% 5000 == 0) {print(paste("Iteration", i))}
      
      colnames_interaction_sel_1 <- str_split(colnames_interaction_sel, ":")[[1]][1]
      colnames_interaction_sel_2 <- str_split(colnames_interaction_sel, ":")[[1]][2]
      num_rows_interaction <- 
        data_multi_all_plus %>% 
        dplyr::select(all_of(colnames_interaction_sel), 
                      all_of(colnames_interaction_sel_1), 
                      all_of(colnames_interaction_sel_2)) %>%
        filter(!!rlang::sym(colnames_interaction_sel_1) == 0 & 
                 !!rlang::sym(colnames_interaction_sel_2) == 0) %>%
        nrow()
      num_rows_interaction <- num_rows_interaction / nrow(data_multi_all_plus)
      if (num_rows_interaction >= 0.80) {
        data_multi_all_plus <- data_multi_all_plus %>% dplyr::select(-all_of(colnames_interaction_sel))
      } else {
        data_multi_all_plus <- data_multi_all_plus
      }
    }
    
    #-- POLYNOMIALS --#
    
    ## Extract all numeric variables (from data frame without interactions)
    cols_numeric_all <- names(unlist(
      lapply(data_multi_all, class)[lapply(data_multi_all, class) == "numeric"]
    ))
    
    ## Drop columns that should not contain polynomials
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "outcome")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "treatment")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "na_")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "group")]
    
    ## Generate polynomials of degree 2 to 4
    data_multi_all_plus <- data_multi_all_plus %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))
    
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
      )
    } else {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Grades/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds")
      )
    }
    
    # again save mean after 5th iteration
    data_multi_num_cols_all <- rbind(
      data_multi_num_cols_all, 
      data.frame("num_cols" = ncol(data_multi_all_plus %>% dplyr::select(-c(
        "treatment_sport_freq_never", "treatment_sport_freq_weekly_atleast", 
        "treatment_sport_freq_source_leisure", "treatment_sport_freq", "outcome_grade"
      ))), 
      "num_rows" = nrow(data_multi_all_plus))
    )
    
    if (mice_data_sel == 5) {
      # SAVE NUMBER OF VARIABLES ETC.
      df_excel_save <- data.frame(
        "data_prep_step" = "estimation_sample",
        "data_prep_step_2" = "multi_all_int_poly",
        "data_prep_choice_cohort" = cohort_prep,
        "data_prep_treatment_repl" = treatment_repl,
        "data_prep_treatment_def" = treatment_def,
        "data_prep_extraact" = extra_act,
        "num_id" = length(unique(data_final_raw$id_t)),
        "num_rows" = mean(data_multi_num_cols_all$num_rows),
        "num_cols" = mean(data_multi_num_cols_all$num_cols),
        "time_stamp" = Sys.time()
      )
      ## load function
      source("Functions/func_save_sample_reduction.R")
      func_save_sample_reduction(df_excel_save, "grade")
      gc()
    }
  }
}
