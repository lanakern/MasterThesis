#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: RECODE YES-NO-VARIABLES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# yes-no variables are labelled as follows: yes = 1, no = 2
# hence, they are not directly dummy variables

# generate function
  ## input is data frame

func_recode_yesno <- function(data) {
  
  # identify variables with specified
  vars_recode <- data %>%
    ungroup() %>%
    select_if(~ all(. %in% c("yes", "no") | is.na(.))) %>%
    select_if(~ any(. %in% c("yes", "no"))) %>%
    colnames()
  
  # for some data sets no variable includes specified. In this case, skip
  # operation
  if (rlang::is_empty(vars_recode)) {
    data <- data
    # otherwise make replacement for those variables
  } else {
    # replace: specified = 1, not specified = 0
    data <- data %>% 
      mutate_at(
        all_of(vars_recode), 
        list(
          ~recode(., `yes` = 1, `no` = 0, 
                  .default = NaN) # NA does not work
        ) 
      ) %>%
      # replace NaN with NA
      mutate_at(all_of(vars_recode), list(~ifelse(is.nan(.), NA, .)))
  }
  
  
  # return data 
  return(data)
}