#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# KNAUS: We drop those interactions that create empty cells or nearly empty cells containing less than 1%
# of the observations. We keep only one variable of variable groups that show absolute correlations
# above 0.99.

#+++
# by Lana Kern
#+++
# In this file, the estimation samples are established. For instance,
# for estimation the ID_t needs to be removed and different confounding
# factors are used for different models.
# Again, the preparations are made for the different MICE data sets.
#+++
# -> The resulting data frames are final data sets used for DML
#+++


# extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}


# ITERATE OVER MICE DATA SETS
for (mice_data_sel in 1:5) {
  
  print(paste("Data Set:", mice_data_sel))
  
  gc()
  
  #%%%%%%%%%%%%%#
  ## LOAD DATA ##
  #%%%%%%%%%%%%%#
  
  # load data
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_mice", mice_data_sel,  ".rds")
  } else {
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", 
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
    select(-c(id_t, starts_with("interview_date"), treatment_period, 
              starts_with("na_count"), ends_with("stand"), ends_with("stand_lag"),
              ends_with("_cat")))
  
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
  data_binary <- data_final %>% select(-starts_with("treatment_sport_freq"))
  
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(binary, paste0("Data/Prep_11/prep_11_dml_binary_all_", treatment_def, "_", 
                           treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(binary, paste0("Data/Prep_11/prep_11_dml_binary_all_", treatment_def, "_", 
                           treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  
  # SAVE NUMBER OF VARIABLES ETC.
  df_excel_save <- data.frame(
    "data_prep_step" = "estimation_sample",
    "data_prep_step_2" = "binary_all",
    "data_prep_choice_cohort" = cohort_prep,
    "data_prep_treatment_repl" = treatment_repl,
    "data_prep_treatment_def" = treatment_def,
    "data_prep_extraact" = extra_act, 
    "num_id" = length(unique(data_final_raw$id_t)), 
    "num_rows" = nrow(data_all),
    "num_cols" = ncol(data_all),
    "time_stamp" = Sys.time()
  )
  ## load function
  source("Functions/func_save_sample_reduction.R")
  func_save_sample_reduction(df_excel_save)
  gc()
  
  
  #### All + Interaction + Polynomials ####
  #+++++++++++++++++++++++++++++++++++++++#

  # create new data frame
  data_all_plus <- data_binary

  #-- INTERACTION TERMS --#

  ## 1.) Drop treatment and outcome variables which are not used to generate interactions
  df_interaction <- data_all_plus %>% select(-c(starts_with("treatment"), starts_with("outcome")))
  ## 2.) Generate interactions
  # https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
  df_interaction <- do.call(cbind, combn(colnames(df_interaction), 2, FUN = function(x)
    list(setNames(data.frame(df_interaction[,x[1]]*df_interaction[,x[2]]),
                  paste(x, collapse = "_")) )))
  ## 3.) Drop interactions containing 95% or more zero values
  df_interaction <- df_interaction[, which(as.numeric(colSums(df_interaction == 0) / nrow(df_interaction)) < 0.95)]
  ## 4.) Add interactions to full data frame
  data_all_plus <- cbind(data_all_plus, df_interaction)


  #-- POLYNOMIALS --#

  ## Extract all numeric variables (from data frame without interactions)
  cols_numeric_all <- names(unlist(lapply(data_binary, class)[lapply(data_binary, class) == "numeric"]))

  ## Ensure that numeric columns do not contain treatment and/or outcome
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "outcome")]
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "treatment")]
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "na_")]

  ## Generate polynomials of degree 2 to 4
  data_all_plus <- data_all_plus %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))

  # save data frames
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_all_plus, 
            paste0("Data/Prep_11/prep_11_dml_binary_allintpoly_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
            )
  } else {
    saveRDS(data_all_plus, 
            paste0("Data/Prep_11/prep_11_dml_binary_allintpoly_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds")
            )
  }


  # SAVE NUMBER OF VARIABLES ETC.
  df_excel_save <- data.frame(
    "data_prep_step" = "estimation_sample",
    "data_prep_step_2" = "binary_all_int_poly",
    "data_prep_choice_cohort" = cohort_prep,
    "data_prep_treatment_repl" = treatment_repl,
    "data_prep_treatment_def" = treatment_def,
    "data_prep_extraact" = extra_act,
    "num_id" = length(unique(data_final_raw$id_t)),
    "num_rows" = nrow(data_all_plus),
    "num_cols" = ncol(data_all_plus),
    "time_stamp" = Sys.time()
  )
  ## load function
  source("Functions/func_save_sample_reduction.R")
  func_save_sample_reduction(df_excel_save)
  gc()
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### MULTIPLE TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### All Predictors ####
  #++++++++++++++++++++++#
  
  # drop treatment variables for binary treatment setting
  data_multi_all <- data_final %>%
    select(-c(treatment_sport, treatment_sport_lag, treatment_sport_na))
  

  # DELETE
  # # drop variables not needed
  # if ("treatment_sport_na" %in% colnames(data_final)) {
  #   data_multi_all <- data_final %>% 
  #     select(-c(treatment_sport, treatment_sport_lag, treatment_sport_na, 
  #               all_of(data_final %>% select(starts_with("treatment_sport_freq_") & (!contains("na") | !ends_with("lag"))) %>% colnames())))
  # } else {
  #   data_multi_all <- data_final %>% 
  #     select(-c(treatment_sport, treatment_sport_lag, 
  #               all_of(data_final %>% select(starts_with("treatment_sport_freq_") & !contains("na")) %>% colnames())))
  # }
  # 
  # 
  # # small groups: aggregate
  # table(data_multi_all$treatment_sport_freq)
  # data_multi_all <- data_multi_all %>%
  #   mutate(treatment_sport_freq = case_when(
  #     treatment_sport_freq == "daily" | treatment_sport_freq == "weekly" ~ "weekly_atleast",
  #     treatment_sport_freq == "monthly" | treatment_sport_freq == "less frequently" ~ "monthly_less",
  #     TRUE ~ "never"
  #   ))
  # table(data_multi_all$treatment_sport_freq)
  # 
  # # new dummy variables
  # data_multi_all <- fastDummies::dummy_cols(
  #   # selected column cannot be removed due to descriptives (hence saved and removed later)
  #   # base category is omitted 
  #   data_multi_all, remove_selected_columns = FALSE, remove_first_dummy = FALSE, 
  #   select_columns = "treatment_sport_freq"
  # )
  
  # treatment_sport_freq as number
  data_multi_all <- data_multi_all %>%
    mutate(treatment_sport_freq = case_when(
      treatment_sport_freq == "weekly_atleast" ~ 1,
      treatment_sport_freq == "monthly_less" ~ 2,
      TRUE ~ 3
    ))
  
  # save data frames
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_multi_all, 
            paste0("Data/Prep_11/prep_11_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_multi_all, 
            paste0("Data/Prep_11/prep_11_dml_multi_all_", treatment_def, "_",
                   treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
  
  
  # SAVE NUMBER OF VARIABLES ETC.
  df_excel_save <- data.frame(
    "data_prep_step" = "estimation_sample",
    "data_prep_step_2" = "multi_all",
    "data_prep_choice_cohort" = cohort_prep,
    "data_prep_treatment_repl" = treatment_repl,
    "data_prep_treatment_def" = treatment_def,
    "data_prep_extraact" = extra_act,
    "num_id" = length(unique(data_final_raw$id_t)),
    "num_rows" = nrow(data_multi_all),
    "num_cols" = ncol(data_multi_all),
    "time_stamp" = Sys.time()
  )
  ## load function
  source("Functions/func_save_sample_reduction.R")
  func_save_sample_reduction(df_excel_save)
  gc()
  
}



# COMPARE NUMBER OF VARIABLES #

# data_main <- readRDS("Data/Prep_11/prep_11_dml_binary_all_weekly_down_extradrop_mice1.rds")
# colnames_main <- colnames(data_main)
# data_1 <- readRDS("Data/Prep_11/prep_11_dml_binary_all_weekly_down_extradrop_robustcheck_mice1.rds")
# colnames_1 <- colnames(data_1)
# data_2 <- readRDS("Data/Prep_11/prep_11_dml_binary_all_all_down_extradrop_mice1.rds")
# colnames_2 <- colnames(data_2)
# data_3 <- readRDS("Data/Prep_11/prep_11_dml_binary_all_weekly_no_extradrop_mice1.rds")
# colnames_3 <- colnames(data_3)
# data_4 <- readRDS("Data/Prep_11/prep_11_dml_binary_all_weekly_down_mice1.rds")
# colnames_4 <- colnames(data_4)
# 
# setdiff(colnames_main, colnames_1)
# data_main %>% select(starts_with("personality_goal_pers")) %>% colnames()
# data_1 %>% select(starts_with("personality_goal_pers")) %>% colnames()
# data_main %>% select(starts_with("age_cat")) %>% colnames()
# data_1 %>% select(starts_with("age_cat")) %>% colnames()
