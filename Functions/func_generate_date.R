#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: GENERATE DATE VARIABLES FROM MONTH AND YEAR ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file a function is generated which generates a date variable. This is
# done because this operation is carried out frequently across the scripts. 
#++++
# Inputs: 
# -> data: dataset containing month and year as numeric variables; day may be excluded,
# -> day: column name as character string of day variable in data (not mandatory; if NULL 1 is used)
# -> month: column name as character string of month variable in data (values must be numeric)
# -> year: column name as character string of year variable in date (values must be numeric)
# -> varname: variable name of generated variable as character string
#++++
# Output: The output is the dataset including the generated date variable
#++++


## LOAD PACKAGES ##
#+++++++++++++++++#

library(lubridate) # for mdy() function
library(dplyr) # for data manipulations


## WRITE FUNCTION ##
#++++++++++++++++++#

func_generate_date <- function(data, day = NULL, month, year, varname){
  
  # set day to 1 if it is not given
  if (is.null(day)) {
    data$day <- "1"
  } else {
    data$day <- data %>% select({{day}}) %>% pull()
  }
  
  # recode month variable
  data <- data %>%
    mutate(
      {{month}} := replace(!!! rlang::syms(month), !!sym(month) == 21, 1), # Beginning of the year is set to January
      {{month}} := replace(!!! rlang::syms(month), !!sym(month) == 24, 4), # Spring/Eastern -> April
      {{month}} := replace(!!! rlang::syms(month), !!sym(month) == 27, 7), # Midyear/Summer -> Juli
      {{month}} := replace(!!! rlang::syms(month), !!sym(month) == 30, 10), # Fall -> October
      {{month}} := replace(!!! rlang::syms(month), !!sym(month) == 32, 11) # End of the year -> December
    )

  # if month name is missing replace by June (6) (middle of the year)
  data <- data %>%
    mutate({{month}} := if_else(is.na(!!! rlang::syms(month)), 6, !!! rlang::syms(month)))
  
  # generate date
  data <- data %>%
    mutate(
      {{varname}} := mdy(paste(paste( !!! rlang::syms(month),  day),
                              !!! rlang::syms(year), sep = ","))
    )
  
  # return data set with new variable
  return(data)
  
}