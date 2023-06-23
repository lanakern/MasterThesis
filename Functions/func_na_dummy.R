#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION TO GENERATE NA DUMMY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function generates NA dummies, i.e., a variable that ends with _NA
# and takes on the value 1 if value of variable is NA and 0 otherwise.
#+++
# Input:
# -> "data": data frame containing variable for which NA dummy should be generated.
# -> "variable": variable name as string 
#+++
# Output: original "data" with NA dummy in addition. Name of NA dummy is: variable_NA
#+++

func_na_dummy <- function(data, variable) {
  
  # create name of new NA variable
  variable_NA <- paste0(variable, "_NA")
  
  # create NA dummy
  data %>%
    mutate(
      {{variable_NA}} := ifelse(is.na(!!sym(variable)), 1, 0)
    )
}