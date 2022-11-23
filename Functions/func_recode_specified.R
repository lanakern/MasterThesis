#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: RECODE (NOT) SPECIFIED VALUES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# generate function
  ## input is data frame

func_recode_specified <- function(data) {
  
  # identify variables with specified
  vars_recode <- data %>%
    select_if(~ any(. == "specified")) %>%
    colnames()
  
  # replace: specified = 1, not specified = 0
  data <- data %>% 
    mutate_at(
      all_of(vars_recode), 
      list(
        ~recode(., `specified` = 1, `not specified` = 0, 
                .default = NaN) # NA does not work
           ) 
    ) %>%
    # replace NaN with NA
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  return(data)
}