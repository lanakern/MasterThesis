#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: GENERATE NA DUMMIES ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# NOT USED ANYMORE

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# by Lana Kern
# this function generates NA dummies:
# input data: data frame ("data") containing variable ("variable_NA_dummy")
# which should be converted to a NA dummy
# output: data frame with NA dummy

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

func_generate_NA_dummies <- function(data, variable_NA_dummy) {
  varname_NA <- paste0(variable_NA_dummy, "_NA")
  data <- data %>%
    mutate(
      {{varname_NA}} := case_when(is.na(!!!rlang::syms(variable_NA_dummy)) ~ 1, TRUE ~ 0)
    )
  return(data)
}