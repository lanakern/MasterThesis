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
  } else if (extra_act == "uni") {
    extra_act_save <- "_extrauni"
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
             "_", treatment_repl, extra_act_save, cov_balance_save, "_mice", mice_data_sel, load_data_ending)
  } else {
    load_data <- 
      paste0(load_data_folder, "Prep_10/prep_10_dml_binary_", model_type, "_", treatment_def, 
             "_", treatment_repl, extra_act_save, cov_balance_save, "_robustcheck_mice", mice_data_sel, load_data_ending)
  }
  
  data_dml_raw <- readRDS(load_data)
  data_dml <- data_dml_raw
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
    if (str_detect(outcome_var, "grade")) {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with("outcome") & contains("_lag"))) %>%
        as.data.frame()
    } else {
      data_dml <- data_dml %>% 
        dplyr::select(-c(starts_with("treatment") & contains("_lag"))) %>% 
        dplyr::select(-c(starts_with(str_remove(outcome_var, "outcome_")) & contains("_lag"))) %>%
        as.data.frame()
    }
  } else {
    # keep all lags
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # drop endogeneous variables if desired by user
  if (model_controls_endog == "no") {
    # drop variables defined as endogeneous
    colnames_endog_drop <- 
      eval(parse(text = paste(paste("data_dml_raw", "%>%"), vars_endogenous, "%>% colnames()")))
    colnames_endog_drop <- colnames_endog_drop[colnames_endog_drop %in% colnames(data_dml)]
    data_dml <- data_dml %>% dplyr::select(-all_of(colnames_endog_drop))
    # also all interactions and polynominals are dropped
    data_dml <- data_dml %>% dplyr::select(-contains(":")) %>% dplyr::select(-contains("_order"))
  } else {
    # keep endogeneous variables
    data_dml <- data_dml %>% as.data.frame()
  }
  
  # if personality is outcome further preparations are necessary
  if (str_detect(outcome_var, "grade")) {
    # for grades no further steps are necessary
    data_dml <- data_dml
  } else if (str_detect(outcome_var, "bigfive")) {
    # for personality selected outcome variable needs to be declared
    data_dml <- data_dml %>%
      rename_with(~ outcome_var, all_of(str_remove(outcome_var, "outcome_")))
    # # lags for all other personality variables are dropped
    # colnames_bigfive_lag_drop <- data_dml %>% 
    #   dplyr::select(starts_with("bigfive") & ends_with("lag") & !matches(outcome_var_old)) %>% colnames()
    # data_dml <- data_dml %>% 
    #   dplyr::select(-all_of(colnames_bigfive_lag_drop))
  }
  
  # remove linearly dependent terms
  data_train_test_m <- data_dml %>% dplyr::select(-all_of(outcome_var))
  data_train_test_g0 <- data_dml %>% filter(treatment_sport == 0) %>% dplyr::select(-treatment_sport)
  data_train_test_g1 <- data_dml %>% filter(treatment_sport == 1) %>% dplyr::select(-treatment_sport)
  
  # remove alias coefficients
  model_m <- glm(paste("treatment_sport", "~ ."), family = binomial(link = "logit"), data = data_train_test_m)
  model_lm_0 <- lm(paste(outcome_var, "~ ."), data = data_train_test_g0)
  model_lm_1 <- lm(paste(outcome_var, "~ ."), data = data_train_test_g1)
  
  vars_multicoll_drop <- c(
    attributes(alias(model_m)$Complete)$dimnames[[1]],
    attributes(alias(model_lm_0)$Complete)$dimnames[[1]], 
    attributes(alias(model_lm_1)$Complete)$dimnames[[1]]) %>%
    unique()
  
  vars_multicoll_drop <- vars_multicoll_drop[!str_detect(
    vars_multicoll_drop, "treatment_sport|outcome_grade|grade|agreeableness|extraversion|neuroticism|openness|conscientiousness"
    )]
  
  if (length(vars_multicoll_drop) < 30) {data_dml <- data_dml %>% dplyr::select(-all_of(vars_multicoll_drop))}
  
  print(paste("Number of predictors after dropping linearly dependent columns:", ncol(data_dml)))
  
  
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
  # if (cohort_prep == main_cohort_prep & treatment_def == main_treatment_def  &
  #     treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
  #     model_type == main_model_type & model_controls_lag == main_model_controls_lag &
  #     model_controls_endog == main_model_controls_endog) {
  #   save_trimming_sel <- TRUE
  # } else {
  #   save_trimming_sel <- FALSE
  # }
  
  # run DML
  dml_result <- func_dml(
    treatment_setting, data = data_dml, 
    outcome = outcome_var, treatment = "treatment_sport", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = model_algo, trimming = model_trimming, save_trimming = FALSE,
    mice_sel = mice_data_sel, hyperparam_sel = model_hyperparam_sel, post = model_post_sel
  )
  
  # append APE
  dml_result$ape <- ape
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
  
}

# sensitivity with respect to hyperparameter
if (model_hyperparam_sel != "best") {
  model_hyperparam_sel_save <- paste0("_", model_hyperparam_sel)
} else {
  model_hyperparam_sel_save <- ""
}

# save results
if (str_detect(outcome_var, "grade")) {
  save_dml <- 
    paste0("Output/DML/Estimation/Grades/binary_grades_", model_algo, "_", 
           model_type, "_", str_replace_all(cohort_prep, "_", ""),
           "_", treatment_def, "_", treatment_repl, extra_act_save, 
           "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""), "_endog",
           model_controls_endog, "_trimming", model_trimming, "_K", model_k, 
           "-", model_k_tuning, "_Rep", model_s_rep, model_hyperparam_sel_save,
           cov_balance_save,
           ".rds")
} else if (str_detect(outcome_var, "bigfive")) {
  save_dml <- paste0("Output/DML/Estimation/Personality/binary_", 
                     str_remove(outcome_var, "outcome_bigfive_"), "_", model_algo, "_", 
         model_type, "_", str_replace_all(cohort_prep, "_", ""),
         "_", treatment_def, "_", treatment_repl, extra_act_save, 
         "_", model_type, "_", str_replace_all(model_controls_lag, "_", ""), "_endog",
         model_controls_endog, "_trimming", model_trimming, "_K", model_k, 
         "-", model_k_tuning, "_Rep", model_s_rep, model_hyperparam_sel_save,
         cov_balance_save,
         ".rds")
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
    model_controls_lag = model_controls_lag, model_controls_endog = model_controls_endog,
    model_hyperparam_sel = model_hyperparam_sel, model_covbal = cov_balance, 
    # number of treatment periods before and after after trimming
    n_treats_before = round(mean(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_before")))), 
    n_treats_after = round(mean(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats_after")))), 
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
    dplyr::select(outcome, cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model")) %>%
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

