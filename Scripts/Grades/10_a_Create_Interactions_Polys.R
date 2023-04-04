#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create Interactions and Polynominals ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#+++
# by Lana Kern
#+++
# In this file, interactions and polynominals are created. Note that this
# file has a long computation time as over 100,000 interaction terms are created
# which are finally subsetting based on empty cells.
# Polynominals are created for all numeric variables.
# NOTE: RESULTS ARE STORED IN PREP_9 TO NOT CONFUSE WITH FINAL PREP_10 DATA SETS.
#+++

# extract extracurricular activity ending
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}


# iterate over mice data sets
for (mice_data_sel in 1:5) {
  
  print(paste("Data Set:", mice_data_sel))
  
  gc()
  
  #%%%%%%%%%%%%%#
  ## LOAD DATA ##
  #%%%%%%%%%%%%%#
  
  # load data
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_weekly_down_", 
                        "mice", mice_data_sel,  ".rds")
  } else {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_weekly_down", 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel,  ".rds")
  }
  
  data_final_raw <- readRDS(data_load)
  
  # ungroup and correct data types
  data_final <- data_final_raw %>% ungroup() %>% type.convert(as.is = TRUE)
  
  # drop ID_t, interview_date, etc. which is not used in the estimation
  data_final <- data_final %>% 
    # instead of id enumerator is established which corresponds to which
    # group the observation belongs; this info is only used for sample splitting
    # in the cross-fitting procedure
    dplyr::select(-c(starts_with("interview_date"), #treatment_period, 
                     starts_with("na_count"), ends_with("_cat"), ends_with("_cat_lag"),
                     starts_with("uni_time_employment"), starts_with("uni_entrance_quali_access_"),
                     starts_with("motivation_degree_4")))
  
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
  
  #-- INTERACTION TERMS --#
  
  # create new data frame
  data_final_all <- data_final
  
  ## 1.) Drop treatment and outcome variables which are not used to generate interactions
  df_interaction <- data_final_all %>% 
    dplyr::select(-c(starts_with("treatment"), starts_with("outcome"), id_t))
  ## 2.) Generate interactions
  # https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables
  df_interaction <- do.call(cbind, combn(colnames(df_interaction), 2, FUN = function(x)
    list(setNames(data.frame(df_interaction[,x[1]]*df_interaction[,x[2]]),
                  paste(x, collapse = ":")))))
  
  #++ OLD
  # seq_interaction <- seq(10000, ncol(df_interaction), 10000)
  # seq_interaction[length(seq_interaction)] <- ncol(df_interaction)
  # seq_interaction_start <- 1
  # interaction_drop_all <- c()
  # for (col_num_max in seq_interaction) {
  #   interaction_col_sums <- colSums(df_interaction[,seq_interaction_start:col_num_max] == 0) / nrow(df_interaction)
  #   interaction_drop <- names(interaction_col_sums[interaction_col_sums >= 0.80])
  #   interaction_drop_all <- c(interaction_drop_all, interaction_drop)
  # 
  #   seq_interaction_start <- col_num_max + 1
  # }
  # 
  # df_interaction <- df_interaction %>% 
  #   dplyr::select(-all_of(interaction_drop_all)) 
  #++ OLD
  
  ## 3.) Add interactions to full data frame
  data_final_all <- cbind(data_final_all, df_interaction)
  ## 4.) Drop interactions containing 80% or more empty cells
  ## only for those with at least 80% of zeros this is operation relevant
  colnames_interaction <- colnames(
    df_interaction[, which(as.numeric(colSums(df_interaction == 0) / nrow(df_interaction)) >= 0.80)]
  )
  gc()
  i <- 0
  for (colnames_interaction_sel in colnames_interaction) {
    i <- i + 1
    if (i %% 5000 == 0) {print(paste("Iteration", i))}

    colnames_interaction_sel_1 <- str_split(colnames_interaction_sel, ":")[[1]][1]
    colnames_interaction_sel_2 <- str_split(colnames_interaction_sel, ":")[[1]][2]
    num_rows_interaction <-
      data_final_all %>%
      dplyr::select(all_of(colnames_interaction_sel),
                    all_of(colnames_interaction_sel_1),
                    all_of(colnames_interaction_sel_2)) %>%
      filter(!!rlang::sym(colnames_interaction_sel_1) == 0 &
               !!rlang::sym(colnames_interaction_sel_2) == 0) %>%
      nrow()
    num_rows_interaction <- num_rows_interaction / nrow(data_final_all)
    if (num_rows_interaction >= 0.80) {
      data_final_all <- data_final_all %>% dplyr::select(-all_of(colnames_interaction_sel))
    } else {
      data_final_all <- data_final_all
    }
  }

  #-- POLYNOMIALS --#
  
  ## Extract all numeric variables (from data frame without interactions)
  cols_numeric_all <- names(unlist(lapply(data_final, class)[lapply(data_final, class) == "numeric"]))
  
  ## Drop columns that should not contain polynomials
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "outcome")]
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "treatment")]
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "na_")]
  cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "group")]
  #cols_numeric_all <- cols_numeric_all[!str_detect(cols_numeric_all, "_lag")]
  
  ## Generate polynomials of degree 2 to 4
  data_final_all <- data_final_all %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
    mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))
  
  if (cohort_prep == "controls_same_outcome") {
    saveRDS(data_final_all, paste0("Data/Grades/Prep_9/prep_9_intpoly_", treatment_def, "_", 
                                   treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"))
  } else {
    saveRDS(data_final_all, paste0("Data/Grades/Prep_9/prep_9_intpoly_", treatment_def, "_", 
                                   treatment_repl, extra_act_save, "_robustcheck_mice", mice_data_sel, ".rds"))
  }
}