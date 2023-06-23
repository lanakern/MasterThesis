#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: REPLACE MISSING VALUES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#+++
# by Lana Kern
#+++
# In this file a function is generated which replaces missing values.
# This function is applied to all individual data sets in file 01.
  ## input: d
  ## -> dataframe: data frame for which missing values should be replaced
  ## -> vec_missings_num: numeric vector containing all numeric values which
  ## should be replaced.
  ## -> vec_missings_char: character vector containing all character values which
  ## should be replaced.
  ## and numeric and character vector containing the values to be replaced
  ## (because of special characters it is less error prone to define them
  ## inside the script using instead within the function.
  ## output: data frame with correctly identified missing values
#+++

func_replace_missings <- function(dataframe, vec_missings_num, vec_missings_char){
  
  # extract numeric variable columns
  colnames_num <- colnames(dataframe)[unlist(lapply(dataframe, is.numeric))]
  
  # extract character and factor variable columns
  colnames_char <- c(
    colnames(dataframe)[unlist(lapply(dataframe, is.character))],
    colnames(dataframe)[unlist(lapply(dataframe, is.factor))]
  )
  
  
  # replace missings
    ## numeric 
  dataframe[colnames_num] <- lapply(
    dataframe[colnames_num], function(x) replace(x,x %in% vec_missings_num, NA) 
    )
    ## character
  for (vec_missings_char_sel in vec_missings_char) {
    dataframe[colnames_char] <- lapply(
      dataframe[colnames_char], function(x) replace(x,str_detect(x, vec_missings_char_sel), NA) 
    )
  }

  
  # return dataframe
  return(dataframe)
}