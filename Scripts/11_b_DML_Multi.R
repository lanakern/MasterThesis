#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML IN THE MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file the ATE and ATET is estimated using DML in the multivalued
# treatment setting. As outcomes the GPA and the big five personality
# traits can be specified.
# As in the binary treatment setting, the outcomes are predicted separately
# for each treatment level t. 
# For the treatment the user can specify if the classification problem is
# transferred in t binary classifications (probscore_separate = TRUE) problems 
# or one multinominal classifciation (probscore_separate = FALSE)
#++++
# Categories in the multivalued treatment setting are:
# -> 1: at least weekly
# -> 2: monthly or less frequently
# -> 3: never
#++++
# Output
# -> The aggregated estimation results are stored in Excel files.
# For probscore_separate = TRUE: "DML_MULTI_ESTIMATION_RESULTS.xlsx"
# For probscore_separate = FALSE: "DML_MULTI_SEPARATE_ESTIMATION_RESULTS.xlsx"
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
    ## covariate balance ending
  if (cov_balance == "yes") {
    cov_balance_save <- "_covbal"
  } else {
    cov_balance_save <- ""
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
    ## polynominals or not
  if (model_type == "allpoly") {
    model_type_2 <- model_type
    model_type <- "allintpoly"
  } else {
    model_type_2 <- model_type
  }
    ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- paste0(
      load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", 
      treatment_def, "_", treatment_repl, extra_act_save, cov_balance_save, "_mice", 
      mice_data_sel, load_data_ending
      )
  } else if (cohort_prep == "controls_bef_outcome") {
    load_data <- paste0(
      load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", 
      treatment_def, "_", treatment_repl, extra_act_save, cov_balance_save,
      "_robustcheck_mice", mice_data_sel, load_data_ending
    )
  } else {
    load_data <- paste0(
      load_data_folder, "Prep_10/prep_10_dml_multi_", model_type, "_", 
      treatment_def, "_", treatment_repl, extra_act_save, cov_balance_save,
      "_robustcheck_", cohort_prep, "_mice", mice_data_sel, load_data_ending
    )
  }

  load_data <- str_replace(load_data, "_level", "") # drop level
  data_dml <- readRDS(load_data) # load data
  
  # keep only polynominals but no interactions, if selected by user
  if (model_type == "allintpoly") {
    if (model_type_2 == "allpoly") {
      data_dml <- data_dml %>% dplyr::select(-contains(":")) # drop interactions
    } else {
      data_dml <- data_dml
    }
  } else {
    data_dml <- data_dml
  }
  
  print(paste("Number of predictors:", ncol(data_dml)))
  
  # drop lags if desired by user
  if (model_controls_lag == "no_lags") {
    # drop all lags
    data_dml <- data_dml %>% 
      dplyr::select(-c(contains("_lag"))) %>% 
      as.data.frame()
  } else if (model_controls_lag == "no_treatment_outcome_lags") {
    # drop only treatment and outcome lags
    # here differentiate between GPA and personality outcome
    if (str_detect(outcome_var_multi, "grade")) {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment_sport_freq") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with("outcome") & contains("_lag"))) %>%
        as.data.frame()
    } else {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment_sport_freq") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with(outcome_var_multi) & contains("_lag"))) %>%
        as.data.frame()
    }
  } else if (model_controls_lag == "only_lags") {
    data_dml <- data_dml %>% 
      dplyr::select(starts_with("treatment_sport_freq"), starts_with("outcome"), 
                    group, contains("_lag")) %>% 
      as.data.frame()
  } else if (model_controls_lag == "no_treatment_lag") {
    if (str_detect(outcome_var_multi, "grade")) {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment_sport_freq") & contains("_lag"))) %>% 
        as.data.frame()
    } else {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment_sport_freq") & contains("_lag"))) %>% 
        as.data.frame()
    }
  } else {
    # keep all lags
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # drop endogeneous variables if desired by user
  if (model_controls_endog == "no") {
    colnames_endog_drop <- 
      eval(parse(text = paste(paste("data_dml", "%>%"), vars_endogenous, "%>% colnames()")))
    colnames_endog_drop <- colnames_endog_drop[colnames_endog_drop %in% colnames(data_dml)]
    data_dml <- data_dml %>% dplyr::select(-all_of(colnames_endog_drop))
  } else {
    # keep endogeneous variables
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # if personality is outcome further preparations are necessary
  if (str_detect(outcome_var_multi, "grade")) {
    # for grades no further steps are necessary
    data_dml <- data_dml
  } else if (str_detect(outcome_var_multi, "bigfive")) {
    # for personality selected outcome variable needs to be declared
    data_dml <- data_dml %>%
      rename_with(~ outcome_var_multi, all_of(str_remove(outcome_var_multi, "outcome_")))
  }
  
  
  # remove linearly dependent terms
  data_train_test_m1 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
    mutate(treatment_sport_freq_weekly_atleast = data_dml$treatment_sport_freq_weekly_atleast)
  data_train_test_m2 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
    mutate(treatment_sport_freq_monthly_less = data_dml$treatment_sport_freq_monthly_less)
  data_train_test_m3 <- data_dml %>% dplyr::select(-all_of(outcome_var_multi), -starts_with("treatment")) %>% 
    mutate(treatment_sport_freq_never = data_dml$treatment_sport_freq_never)
  
  data_train_test_g1 <- data_dml %>% filter(treatment_sport_freq == 1) %>% dplyr::select(-treatment_sport_freq)
  data_train_test_g2 <- data_dml %>% filter(treatment_sport_freq == 2) %>% dplyr::select(-treatment_sport_freq)
  data_train_test_g3 <- data_dml %>% filter(treatment_sport_freq == 3) %>% dplyr::select(-treatment_sport_freq)
  
  # remove alias coefficients
  model_m1 <- glm(paste("treatment_sport_freq_weekly_atleast", "~ ."), family = binomial(link = "logit"), data = data_train_test_m1)
  model_m2 <- glm(paste("treatment_sport_freq_monthly_less", "~ ."), family = binomial(link = "logit"), data = data_train_test_m2)
  model_m3 <- glm(paste("treatment_sport_freq_never", "~ ."), family = binomial(link = "logit"), data = data_train_test_m3)
  
  model_lm_1 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g1)
  model_lm_2 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g2)
  model_lm_3 <- lm(paste(outcome_var_multi, "~ ."), data = data_train_test_g3)
  
  vars_multicoll_drop <- c(
    attributes(alias(model_m1)$Complete)$dimnames[[1]], attributes(alias(model_m2)$Complete)$dimnames[[1]],
    attributes(alias(model_m3)$Complete)$dimnames[[1]],
    attributes(alias(model_lm_1)$Complete)$dimnames[[1]], attributes(alias(model_lm_2)$Complete)$dimnames[[1]],
    attributes(alias(model_lm_3)$Complete)$dimnames[[1]]) %>%
    unique()
  
  vars_multicoll_drop <- vars_multicoll_drop[!str_detect(
    vars_multicoll_drop, "treatment_sport|outcome_grade|grade|agreeableness|extraversion|neuroticism|openness|conscientiousness"
  )]
  
  if (length(vars_multicoll_drop) < 30) {data_dml <- data_dml %>% dplyr::select(-all_of(vars_multicoll_drop))}
  
  print(paste("Number of predictors after dropping linearly dependent columns:", ncol(data_dml)))
  
  #%%%%%%%%%%%#
  #### APE ####
  #%%%%%%%%%%%#
  
  # NOT USED ANYMORE BECAUSE THIS CALCULATION IS BASED ON UNSTANDARIZED DATA!!
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
  

  # run DML
  dml_result <- func_dml(
    treatment_setting = "multi", data = data_dml, 
    outcome = outcome_var_multi, treatment = "treatment_sport_freq", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = multi_model_algo, trimming = model_trimming, 
    # save common support plot
    save_trimming = FALSE,
    # model generation: separate 
    probscore_separate = probscore_separate, 
    mice_sel = mice_data_sel, post = model_post_sel, 
    # how to determine hyperparameter (best, 1SE, 1SE+)
    hyperparam_sel = hyperparam_sel
  )
  
  # append APE
  dml_result$ape <- df_ape
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
  
}


# save detailed results
if (probscore_separate == FALSE) {
  probscore_separate_save <- "_nonseparate"
} else {
  probscore_separate_save <- ""
}

if (model_hyperparam_sel != "best") {
  model_hyperparam_sel_save <- paste0("_", model_hyperparam_sel)
} else {
  model_hyperparam_sel_save <- ""
}

if (prob_norm == "no") {
  prob_norm_save <- "_nonorm"
} else {
  prob_norm_save <- ""
}


if (str_detect(outcome_var_multi, "grade")) {
  save_dml <- 
    paste0("Output/DML/Estimation/Grades/multi_grades_", multi_model_algo, "_", 
           model_type, "_", str_replace_all(cohort_prep, "_", ""),
           "_", treatment_def, "_", treatment_repl, extra_act_save, 
           "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""), "_endog",
           model_controls_endog, "_trimming", model_trimming, "_K", model_k, 
           "-", model_k_tuning, "_Rep", model_s_rep, probscore_separate_save,
           model_hyperparam_sel_save, cov_balance_save, prob_norm_save, ".rds")
} else if (str_detect(outcome_var_multi, "bigfive")) {
  save_dml <- paste0("Output/DML/Estimation/Personality/multi_", 
                     str_remove(outcome_var_multi, "outcome_bigfive_"), "_", multi_model_algo, "_", 
                     model_type, "_", str_replace_all(cohort_prep, "_", ""),
                     "_", treatment_def, "_", treatment_repl, extra_act_save, 
                     "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""), "_endog",
                     model_controls_endog, "_trimming", model_trimming, "_K", model_k, 
                     "-", model_k_tuning, "_Rep", model_s_rep, probscore_separate_save,
                     model_hyperparam_sel_save, cov_balance_save, prob_norm_save, ".rds")
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
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, 
    treatment_def = treatment_def, extra_act = extra_act, 
    # append user selections
    model_type = model_type, model_algo = multi_model_algo, model_k = model_k, 
    model_k_tuning = model_k_tuning, model_s_rep = model_s_rep, 
    model_trimming = model_trimming, model_controls_lag = model_controls_lag,
    model_controls_endog = model_controls_endog, 
    model_hyperparam_sel = model_hyperparam_sel, model_covbal = cov_balance, 
    # number of treatment periods before and after after trimming
    n_treats_before = round(mean(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_before")))), 
    n_treats_after = round(mean(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_after")))), 
    # type of model generation
    Prob_norm = prob_norm, 
    # add date
    time_stamp = as.character(Sys.time()),
    # add computation time
    time_elapsed = paste(as.character(difftime(Sys.time(), start_time, units = "hours")), "hours")) %>%
  cbind(dml_result_error) %>%
  # re-order columns
  dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
                n_treats_before, n_treats_after, Prob_norm, starts_with("num_pred"), 
                Treatment, everything()) %>%
  relocate(time_elapsed, time_stamp, .after = last_col()) # time-stamp is ordered last


# save aggregated estimation results
dml_result_save <- as.data.frame(dml_result_save)

if (probscore_separate == TRUE) {
  if (file.exists("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx")) {
    ## replace same estimations
    dml_result_save_all <- 
      read.xlsx("Output/DML/Treatment_Effects/DML_MULTI_ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
    dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
    cols_aggr <- dml_result_save_all %>%
      dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), Prob_norm) %>%
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
      dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model")) %>%
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













