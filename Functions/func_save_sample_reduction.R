#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: SAMPLE REDUCTION STEPS IN EXCEL ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# this function saves the sample reduction steps in the individual files
# in a excel file for a better overview-
# input: data frame which should be appended to excel file and outcome type (grades, personality)
# output: data frame with appended data (for user checking)

func_save_sample_reduction <- function(data, outcome) {
  
  # adjust data: ensure all columns exist and order them
  if (!"data_prep_treatment_repl" %in% colnames(data)) {
    data <- data %>% mutate(data_prep_treatment_repl = NA)
  }
  if (!"data_prep_treatment_def" %in% colnames(data)) {
    data <- data %>% mutate(data_prep_treatment_def = NA)
  }
  if (!"data_prep_extraact" %in% colnames(data)) {
    data <- data %>% mutate(data_prep_extraact = NA)
  }
  if (!"data_prep_step_2" %in% colnames(data)) {
    data <- data %>% mutate(data_prep_step_2 = NA)
  }
  
  data <- data %>% dplyr::select(
    data_prep_step, data_prep_step_2, data_prep_choice_cohort, data_prep_treatment_repl,
    data_prep_treatment_def, data_prep_extraact, num_id, num_rows, num_cols, time_stamp
    )
  
  
  # load history
  if (outcome == "grade") {
    df_excel_save_hist <- read.xlsx("Output/SAMPLE_REDUCTION_STEPS.xlsx", sheetName = "Sheet1")
  } else {
    df_excel_save_hist <- read.xlsx("Output/SAMPLE_REDUCTION_STEPS_PERSONALITY.xlsx", sheetName = "Sheet1")
  }
  
    ## add columns which may be missing
  if (!"data_prep_treatment_repl" %in% colnames(df_excel_save_hist)) {
    df_excel_save_hist <- df_excel_save_hist %>% mutate(data_prep_treatment_repl = NA)
  }
  if (!"data_prep_treatment_def" %in% colnames(df_excel_save_hist)) {
    df_excel_save_hist <- df_excel_save_hist %>% mutate(data_prep_treatment_def = NA)
  }
  
  df_excel_save_hist <- df_excel_save_hist %>% 
    dplyr::select(data_prep_step, data_prep_step_2, data_prep_choice_cohort, data_prep_treatment_repl,
                  data_prep_treatment_def, data_prep_extraact, num_id, num_rows, num_cols, time_stamp)
  
  
  # append current data frame
  df_excel_save <- rbind(df_excel_save_hist, data)
  # in case of duplicate, keep the most recent observation
  df_excel_save <- df_excel_save %>%
    group_by(data_prep_step, data_prep_step_2, data_prep_choice_cohort, data_prep_treatment_repl, 
             data_prep_treatment_def, data_prep_extraact) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  
  df_excel_save <- df_excel_save %>% distinct()

  # save
  if (outcome == "grade") {
    save_data <- "Output/SAMPLE_REDUCTION_STEPS.xlsx"
  } else {
    save_data <- "Output/SAMPLE_REDUCTION_STEPS_PERSONALITY.xlsx"
  }
  
  write.xlsx(df_excel_save, save_data, sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
  # output
  return(df_excel_save)
}