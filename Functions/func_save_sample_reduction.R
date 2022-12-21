#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: SAMPLE REDUCTION STEPS IN EXCEL ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# this function saves the sample reduction steps in the individual files
# in a excel file for a better overview-
# input: data frame which should be appended to excel file
# output: data frame with appended data (for user checking)

func_save_sample_reduction <- function(data) {
  # adjust data
  if (!"data_prep_treatment_repl" %in% colnames(data)) {
    data <- data %>% mutate(data_prep_treatment_repl = NA)
  }
  
  # load history
  df_excel_save_hist <- read.xlsx("Data/SAMPLE_REDUCTION_STEPS.xlsx", sheetName = "Sheet1")
    ## add columns which may be missing
  if (!"data_prep_treatment_repl" %in% colnames(df_excel_save_hist)) {
    df_excel_save_hist <- df_excel_save_hist %>% mutate(data_prep_treatment_repl = NA)
  }
  # append current data frame
  df_excel_save <- rbind(df_excel_save_hist, data)
  # in case of duplicate, keep the most recent observation
  df_excel_save <- df_excel_save %>%
    group_by(data_prep_step, data_prep_choice_cohort, data_prep_treatment_repl) %>%
    filter(time_stamp == max(time_stamp)) %>%
    ungroup() %>% data.frame()
  
  df_excel_save <- df_excel_save %>% distinct()

  # save
  write.xlsx(df_excel_save, "Data/SAMPLE_REDUCTION_STEPS.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
  # output
  return(df_excel_save)
}