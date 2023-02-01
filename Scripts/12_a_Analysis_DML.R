#%%%%%%%%%%%%%%%%#
#### DoubleML ####
#%%%%%%%%%%%%%%%%#

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

# list to store results
dml_result_all <- list()

# iterate over mice data sets
for (mice_data_sel in 1:2) {
  
  # data set number
  print(paste("Data Set", mice_data_sel))
  
  # load data
  load_data <- paste0("Data/Prep_11/prep_11_dml_binary_", model_type, "_", model_outcome,
                      "_", treatment_def, "_", treatment_repl, "_mice", mice_data_sel, ".rds")
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
  
  # https://docs.doubleml.org/stable/examples/R_double_ml_pension.html
  # (unconditional) Average Predictive Effect (APE) of doing sports on grades.
  # This effect corresponds to the ATE if sport participation would be assigned to 
  # individuals in an entirely randomized way.
  # APE is a naive estimate of the ATE and biased since it does not account for 
  # endogeneity of participation.
  ape <- data_dml %>% filter(treatment_sport == 1) %>% pull(outcome_var) %>% mean() -
      data_dml %>% filter(treatment_sport == 0) %>% pull(outcome_var) %>% mean()


  #%%%%%%%%%%%%%%%%%%#
  #### ATE & ATET ####
  #%%%%%%%%%%%%%%%%%%#
  
  # run DML
  dml_result <- func_dml(
    data = data_dml, 
    outcome = outcome_var, treatment = "treatment_sport", group = "group", 
    K = model_k, K_tuning = model_k_tuning, S = model_s_rep, 
    mlalgo = model_algo, trimming = model_trimming
  )
  
  # append to full data frame
  dml_result_all <- append(dml_result_all, list(dml_result))
}


# calculate pooled estimate over multiple mice data sets
dml_result_pooled_all <- func_dml_pool_mice(dml_result_all, nrow(data_dml), 2)
dml_result_pooled <- dml_result_pooled_all[[1]]
dml_result_error <- dml_result_pooled_all[[2]]

# append columns
dml_result_pooled <- dml_result_pooled %>%
  mutate(
    # append model selections
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, treatment_def = treatment_def, 
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
  select(cohort_prep, treatment_repl, treatment_def, starts_with("model"), n_treats_min, everything()) %>%
  relocate(time_stamp, .after = last_col()) # time-stamp is ordered last


# save estimation results
dml_result_save <- as.data.frame(dml_result_pooled)
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