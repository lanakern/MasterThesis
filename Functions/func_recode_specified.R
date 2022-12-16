#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: RECODE (NOT) SPECIFIED VALUES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# by Lana Kern

# This file includes a function to recode specified and not specified variables
# to specified = 1 and not specified = 0.
# This function is applied to all individual data sets in file 01.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Generate function
  ## input is data frame
  ## output is data frame with proper replacements

func_recode_specified <- function(data) {
  
  # identify variables with specified
  vars_recode <- data %>%
    select_if(~ any(. == "specified")) %>%
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
          ~recode(., `specified` = 1, `not specified` = 0, .default = NaN) # NA does not work
        ) 
      ) %>%
      # replace NaN with NA
      mutate_at(all_of(vars_recode), list(~ifelse(is.nan(.), NA, .)))
  }

  
  # return data 
  return(data)
}