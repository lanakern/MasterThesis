#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PERSONALITY ESTIMATION SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the estimation samples are generated. 
# Again, the preparations are made for the different MICE data sets.
#+++
# 1.) Remove unnecessary variables like the id, character and constant variables.
#+++
# 2.) Binary Treatment Setting
# -> Sample with all predictors (saving also select on no lags)
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# 3.) Multivalud Treatment Setting
# -> Sample with all predictors (saving also select on no lags)
# -> Sample with all predictors + second order interactions + polynomial up
# to degree 4
#+++
# -> The resulting data frames are final data sets used for DML. No further
# adjustments are amde.
#+++


# extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
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
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_mice", mice_data_sel,  "_personality.rds")
  } else {
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_robustcheck", "_mice", mice_data_sel,  "_personality.rds")
  }
  
  data_final_raw <- readRDS(data_load)
  
  # ungroup and correct data types
  data_final <- data_final_raw %>% ungroup() %>% type.convert(as.is = TRUE)
  
  #+++ NEU 
  # drop interview_start_year_num %in% c(1,8) and respective dummies to enforce
  # common support
  if (cohort_prep == "controls_same_outcome" & cov_balance == "yes") {
    data_final <- data_final %>% filter(!interview_start_year_num %in% c(1,8)) %>%
      dplyr::select(-contains("interview_end_year_2019"), -contains("interview_start_year_2018"))
  } else {
    data_final <- data_final
  }
  #+++
  #+

  
  # drop ID_t, interview_date, etc. which is not used in the estimation
  data_final <- data_final %>% 
    # instead of id enumerator is established which corresponds to which
    # group the observation belongs; this info is only used for sample splitting
    # in the cross-fitting procedure
    mutate(group = as.integer(factor(id_t,levels = unique(id_t))))  %>%
    dplyr::select(-c(id_t, starts_with("interview_date"), 
                     starts_with("na_count"), ends_with("_cat"), ends_with("_cat_lag"),
                     starts_with("uni_time_employment"), starts_with("uni_entrance_quali_access_")))
  
  # also drop big five lags because they are identical to true value
  data_final <- data_final %>% dplyr::select(-c(starts_with("bigfive") & ends_with("lag")))
  
  # ensure all character variables are dropped
  treatment_sport_freq <- data_final$treatment_sport_freq # keep
  treatment_sport_freq_lag <- data_final$treatment_sport_freq_lag
  data_final <- data_final[, !sapply(data_final, is.character)]
  data_final$treatment_sport_freq <- treatment_sport_freq
  data_final$treatment_sport_freq_lag <- treatment_sport_freq_lag
  
  # ensure all constant variables are dropped
  data_final <- remove_constant(data_final)
  
  # create interactions
  if (create_interactions == "yes") {
    # create new data frame
    data_all_plus <- data_final
    
    #-- INTERACTION TERMS --#
    
    ## 1.) Drop treatment and outcome variables which are not used to generate interactions
    df_interaction <- data_all_plus %>% 
      dplyr::select(-c(starts_with("treatment"), starts_with("bigfive"), group))
    ## 2.) Generate interactions
    # https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
    df_interaction <- do.call(cbind, combn(colnames(df_interaction), 2, FUN = function(x)
      list(setNames(data.frame(df_interaction[,x[1]]*df_interaction[,x[2]]),
                    paste(x, collapse = ":")))))
    
    #+++
    seq_interaction <- seq(10000, ncol(df_interaction), 10000)
    seq_interaction[length(seq_interaction)] <- ncol(df_interaction)
    seq_interaction_start <- 1
    interaction_drop_all <- c()
    for (col_num_max in seq_interaction) {
      interaction_col_sums <- colSums(df_interaction[,seq_interaction_start:col_num_max] == 0) / nrow(df_interaction)
      interaction_drop <- names(interaction_col_sums[interaction_col_sums >= 0.80])
      interaction_drop_all <- c(interaction_drop_all, interaction_drop)
      
      seq_interaction_start <- col_num_max + 1
    }
    #+++
    
    df_interaction <- df_interaction %>% 
      dplyr::select(-all_of(interaction_drop_all)) 
    ## 3.) Add interactions to full data frame
    data_all_plus <- cbind(data_all_plus, df_interaction)
    ## 4.) Drop interactions containing 80% or more empty cells
    ## only for those with at least 80% of zeros this is operation relevant
    # colnames_interaction <- colnames(
    #   df_interaction[, which(as.numeric(colSums(df_interaction == 0) / nrow(df_interaction)) >= 0.80)]
    # )
    # #colnames_interaction_sub <- colnames_interaction[1:10000]
    # gc()
    # i <- 0
    # for (colnames_interaction_sel in colnames_interaction) {
    #   i <- i + 1
    #   if (i %% 5000 == 0) {print(paste("Iteration", i))}
    #   
    #   colnames_interaction_sel_1 <- str_split(colnames_interaction_sel, ":")[[1]][1]
    #   colnames_interaction_sel_2 <- str_split(colnames_interaction_sel, ":")[[1]][2]
    #   num_rows_interaction <- 
    #     data_all_plus %>% 
    #     dplyr::select(all_of(colnames_interaction_sel), 
    #                   all_of(colnames_interaction_sel_1), 
    #                   all_of(colnames_interaction_sel_2)) %>%
    #     filter(!!rlang::sym(colnames_interaction_sel_1) == 0 & 
    #              !!rlang::sym(colnames_interaction_sel_2) == 0) %>%
    #     nrow()
    #   num_rows_interaction <- num_rows_interaction / nrow(data_all_plus)
    #   if (num_rows_interaction >= 0.80) {
    #     data_all_plus <- data_all_plus %>% dplyr::select(-all_of(colnames_interaction_sel))
    #   } else {
    #     data_all_plus <- data_all_plus
    #   }
    #   
    # }

    # generate polynominals
    cols_numeric_all <- names(unlist(lapply(data_final, class)[lapply(data_final, class) == "numeric"]))
    
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "outcome")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "treatment")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "na_")]
    cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "group")]
    
    data_all_plus <- data_all_plus %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
      mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))
    
    data_all_plus <- remove_constant(data_all_plus)
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # drop sport participation frequency variables
  data_binary <- data_final %>% dplyr::select(-starts_with("treatment_sport_freq"))
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_binary, paste0(
      "Data/Personality/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
      treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, "_personality.rds"))
  } else {
    saveRDS(data_binary, paste0(
      "Data/Personality/Prep_10/prep_10_dml_binary_all_", treatment_def, "_", 
      treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, 
      "_personality.rds"))
  }
  
  
  
  # SAVE NUMBER OF VARIABLES ETC. ONLY AFTER 5th ITERATION 
  # Then mean is saved across the five mice data sets
  data_binary_num_cols <- rbind(
    data_binary_num_cols, 
    data.frame("num_cols" = ncol(data_binary), "num_rows" = nrow(data_binary)))
  
  if (mice_data_sel == 5) {
    # SAVE NUMBER OF VARIABLES ETC.
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "binary_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "data_prep_covbal" = cov_balance,
      "num_id" = length(unique(data_final_raw$id_t)), 
      "num_rows" = mean(data_binary_num_cols$num_rows),
      "num_cols" = mean(data_binary_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "personality")
    gc()
  }
  
  # Only save for non-lag variables (but no extra data set)
  data_binary_lags <- data_binary %>% dplyr::select(-ends_with("_lag"))
  data_binary_num_cols_nolags <- rbind(
    data_binary_num_cols_nolags, 
    data.frame("num_cols" = ncol(data_binary_lags), "num_rows" = nrow(data_binary_lags))
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
      "num_id" = length(unique(data_final_raw$id_t)), 
      "num_rows" = mean(data_binary_num_cols_nolags$num_rows),
      "num_cols" = mean(data_binary_num_cols_nolags$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "personality")
    gc()
  }
  
  
  #### All + Interactions + Polynomials ####
  #++++++++++++++++++++++++++++++++++++++++#

  if (create_interactions == "yes") {
    
    # create new data frame
    data_binary_all_plus <- data_all_plus %>% dplyr::select(-starts_with("treatment_sport_freq"))
    
    
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_binary_all_plus, 
              paste0("Data/Personality/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds")
      )
    } else {
      saveRDS(data_binary_all_plus, 
              paste0("Data/Personality/Prep_10/prep_10_dml_binary_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds")
      )
    }
    
    # again save mean after 5th iteration
    data_binary_num_cols_all <- rbind(
      data_binary_num_cols_all, 
      data.frame("num_cols" = ncol(data_binary_all_plus), 
                 "num_rows" = nrow(data_binary_all_plus))
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
        "data_prep_covbal" = cov_balance,
        "num_id" = length(unique(data_final_raw$id_t)),
        "num_rows" = round(mean(data_binary_num_cols_all$num_rows)),
        "num_cols" = round(mean(data_binary_num_cols_all$num_cols)),
        "time_stamp" = Sys.time()
      )
      ## load function
      source("Functions/func_save_sample_reduction.R")
      func_save_sample_reduction(df_excel_save, "personality")
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
  data_multi_all <- data_final %>% dplyr::select(-all_of(drop_vars_multi))
  
  # create treatment_sport_freq dummy
  data_multi_all <- fastDummies::dummy_cols(
    data_multi_all, remove_selected_columns = FALSE, remove_first_dummy = FALSE, 
    select_columns = c("treatment_sport_freq", "treatment_sport_freq_lag")
  ) %>% dplyr::select(-c(treatment_sport_freq_lag, treatment_sport_freq_lag_never))
  
  # treatment_sport_freq as number
  data_multi_all <- data_multi_all %>%
    mutate(treatment_sport_freq = case_when(
      treatment_sport_freq == "weekly_atleast" ~ 1,
      treatment_sport_freq == "monthly_less" ~ 2,
      treatment_sport_freq == "never" ~ 3,
      TRUE ~ 4
    ))

  # save data frames
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_multi_all, 
            paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, "_personality.rds"))
  } else {
    saveRDS(data_multi_all, 
            paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", 
                   mice_data_sel, "_personality.rds"))
  }
  
  
  # SAVE NUMBER OF VARIABLES ETC. AFTER 5th ITERATION
  data_multi_num_cols <- rbind(
    data_multi_num_cols, 
    data.frame("num_cols" = ncol(data_multi_all), "num_rows" = nrow(data_multi_all)))
  
  if (mice_data_sel == 5) {
    # SAVE NUMBER OF VARIABLES ETC.
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "multi_all",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act,
      "data_prep_covbal" = cov_balance,
      "num_id" = length(unique(data_final_raw$id_t)),
      "num_rows" = mean(data_multi_num_cols$num_rows),
      "num_cols" = mean(data_multi_num_cols$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "personality")
    gc()
  }  


  # Only save for non-lag variables (but no extra data set)
  data_multi_all_lags <- data_multi_all %>% dplyr::select(-ends_with("_lag"))
  data_multi_num_cols_nolags <- rbind(
    data_multi_num_cols_nolags, 
    data.frame("num_cols" = ncol(data_multi_all_lags), "num_rows" = nrow(data_multi_all_lags))
  )
  
  if (mice_data_sel == 5) {
    df_excel_save <- data.frame(
      "data_prep_step" = "estimation_sample",
      "data_prep_step_2" = "multi_nolags",
      "data_prep_choice_cohort" = cohort_prep,
      "data_prep_treatment_repl" = treatment_repl,
      "data_prep_treatment_def" = treatment_def,
      "data_prep_extraact" = extra_act, 
      "data_prep_covbal" = cov_balance,
      "num_id" = length(unique(data_final_raw$id_t)), 
      "num_rows" = mean(data_multi_num_cols_nolags$num_rows),
      "num_cols" = mean(data_multi_num_cols_nolags$num_cols),
      "time_stamp" = Sys.time()
    )
    ## load function
    source("Functions/func_save_sample_reduction.R")
    func_save_sample_reduction(df_excel_save, "personality")
    gc()
  }
  
  
  #### All + Interactions + Polynomials ####
  #++++++++++++++++++++++++++++++++++++++++#
  
  if (create_interactions == "yes") {
    
    # create new data frame
    drop_vars_multi <- c("treatment_sport", "treatment_sport_na",
                         "treatment_sport_lag", "treatment_sport_lag_na", 
                         "treatment_sport_source_uni", "treatment_sport_source_leisure",
                         "treatment_sport_freq_never", "treatment_sport_freq_weekly_atleast",
                         "treatment_sport_freq_never_lag", "treatment_sport_freq_weekly_atleast_lag")
    drop_vars_multi <- drop_vars_multi[drop_vars_multi %in% colnames(data_all_plus)]
    data_multi_all_plus <- data_all_plus %>% dplyr::select(-all_of(drop_vars_multi))
    
    
    # create treatment_sport_freq dummy
    data_multi_all_plus <- fastDummies::dummy_cols(
      data_multi_all_plus, remove_selected_columns = FALSE, remove_first_dummy = FALSE, 
      select_columns = c("treatment_sport_freq", "treatment_sport_freq_lag")
    ) %>% dplyr::select(-c(treatment_sport_freq_lag, treatment_sport_freq_lag_never))
    
    # treatment_sport_freq as number
    data_multi_all_plus <- data_multi_all_plus %>%
      mutate(treatment_sport_freq = case_when(
        treatment_sport_freq == "weekly_atleast" ~ 1,
        treatment_sport_freq == "monthly_less" ~ 2,
        treatment_sport_freq == "never" ~ 3,
        TRUE ~ 4
      ))
    
    # save data frames
    if (cohort_prep == "controls_same_outcome") {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Personality/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, ".rds")
      )
    } else {
      saveRDS(data_multi_all_plus, 
              paste0("Data/Personality/Prep_10/prep_10_dml_multi_allintpoly_", treatment_def, "_",
                     treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, ".rds")
      )
    }
    
    # again save mean after 5th iteration
    data_multi_num_cols_all <- rbind(
      data_multi_num_cols_all, 
      data.frame("num_cols" = ncol(data_multi_all_plus), 
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
        "data_prep_covbal" = cov_balance,
        "num_id" = length(unique(data_final_raw$id_t)),
        "num_rows" = round(mean(data_multi_num_cols_all$num_rows)),
        "num_cols" = round(mean(data_multi_num_cols_all$num_cols)),
        "time_stamp" = Sys.time()
      )
      ## load function
      source("Functions/func_save_sample_reduction.R")
      func_save_sample_reduction(df_excel_save, "personality")
      gc()
    }
  }
}
