#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DML IN THE BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# LAGS müssen raus, da sonst kein common support

# set seed for reproducible results
set.seed(1234)

# inputs
# model_type <- c("base")
# model_algo <-  c("lasso")
# model_k <- 2
# model_k_tuning <- 2
# model_s_rep <- 2
# model_trimming <- 0.01
# model_outcome <- "level"
# model_controls <- "all"

# empty data frames and lists to store results
df_ape_all <- data.frame()
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
    ## cohort prep
  if (cohort_prep == "controls_same_outcome") {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds")
  } else {
    load_data <- 
      paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
             "_", treatment_def, "_", treatment_repl, extra_act_save, "robustcheck_mice", mice_data_sel, ".rds")
  }
  
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
  
  data_dml <- data_dml %>% as.data.frame()
  
  
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
  
  df_ape <- data.frame("MICE" = mice_data_sel, "APE" = ape)
  df_ape_all <- rbind(df_ape_all, df_ape)


  #%%%%%%%%%%%%%%%%%%#
  #### ATE & ATET ####
  #%%%%%%%%%%%%%%%%%%#
  
  # only save common support plot for main model
  if (cohort_prep == main_cohort_prep & treatment_def == main_treatment_def  &
      treatment_repl == main_treatment_repl & extra_act == main_extra_act & 
      model_type == main_model_type & model_outcome == main_model_outcome & 
      model_controls == main_model_controls & model_treatment == main_model_treatment) {
    save_trimming_sel <- TRUE
  } else {
    save_trimming_sel <- FALSE
  }
  
  # run DML
  dml_result <- func_dml(
    treatment_setting, data = data_dml, 
    outcome = outcome_var, treatment = "treatment_sport", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = model_algo, trimming = model_trimming, save_trimming = save_trimming_sel
  )
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
}

# save results
save_dml <- 
  paste0("Output/DML/", model_algo, "_", model_type, "_", model_outcome, "_", str_replace_all(cohort_prep, "_", ""),
         "_", treatment_def, "_", treatment_repl, extra_act_save, ".rds")

saveRDS(dml_result_all, save_dml)

# calculate pooled estimate over multiple mice data sets
dml_result_pooled_all <- func_dml_pool_mice(dml_result_all, nrow(data_dml), 5)
dml_result_pooled <- dml_result_pooled_all[[1]]
dml_result_error <- dml_result_pooled_all[[2]]

# append columns
dml_result_save <- dml_result_pooled %>%
  mutate(
    # append model selections
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, treatment_def = treatment_def, 
    extra_act = extra_act, 
    # append user selections
    model_type = model_type, model_algo = model_algo, model_k = model_k, 
    model_k_tuning = model_k_tuning, model_s_rep = model_s_rep, model_trimming = model_trimming, 
    model_outcome = model_outcome, model_controls = model_controls,
    # number of treatment periods after trimming
    n_treats_min = min(unlist(lapply(lapply(dml_result_all, "[[" , "trimming"), "[[", "n_treats"))), 
    # add date
    time_stamp = as.character(Sys.time())) %>%
  cbind(dml_result_error) %>%
  # re-order columns
  select(cohort_prep, treatment_repl, treatment_def, extra_act, starts_with("model"), 
         n_treats_min, starts_with("num_pred"), everything()) %>%
  relocate(time_stamp, .after = last_col()) # time-stamp is ordered last


# save estimation results
dml_result_save <- as.data.frame(dml_result_save)
if (file.exists("Output/ESTIMATION_RESULTS.xlsx")) {
  ## replace same estimations
  dml_result_save_all <- read.xlsx("Output/ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
  dml_result_save_all <- rbind(dml_result_save_all, dml_result_save)
  cols_aggr <- dml_result_save_all %>%
    select(cohort_prep, treatment_repl, treatment_def, starts_with("model")) %>%
    colnames()
  dml_result_save_all <- dml_result_save_all %>%
    group_by(across(all_of(cols_aggr))) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  ## save
  write.xlsx(dml_result_save_all, "Output/ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
} else {
  write.xlsx(dml_result_save, "Output/ESTIMATION_RESULTS.xlsx", row.names = FALSE)
}




#%%%%%%%%%%%%#
#### CATE ####
#%%%%%%%%%%%%#

# Conditional Average Treatment Effect (CATE)
# "What is the expected treatment effect for somebody with characteristics X = x?"
# Heterogeneous treatment effects?