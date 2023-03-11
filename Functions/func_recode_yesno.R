#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: RECODE YES-NO-VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# by Lana Kern

# In this file a function is generated that labels all yes-no variables
# correctly, that is yes = 1, and no = 0 (before no = 2).
# This function is applied to all individual data sets in file 01.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Create Function
  ## input: data frame
  ## output: data frame with correct yes-no labelling

func_recode_yesno <- function(data) {
  
  # identify variables with "yes" and "no" values
  vars_recode <- data %>%
    ungroup() %>%
    dplyr::select_if(~ all(. %in% c("yes", "no") | is.na(.))) %>%
    dplyr::select_if(~ any(. %in% c("yes", "no"))) %>%
    colnames()
  
  # for some data sets no variable includes "yes"-"no" In this case, skip
  # operation
  if (rlang::is_empty(vars_recode)) {
    data <- data
    # otherwise make replacement for those variables
  } else {
    # replace: yes = 1, no = 0, default NA
    data <- data %>% 
      mutate_at(
        all_of(vars_recode), 
        list(
          ~recode(., `yes` = 1, `no` = 0, .default = NaN) # NA does not work
        ) 
      ) %>%
      # replace NaN with NA
      mutate_at(all_of(vars_recode), list(~ifelse(is.nan(.), NA, .)))
  }
  
  
  # return data 
  return(data)
}