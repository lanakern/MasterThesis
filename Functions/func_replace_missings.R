#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: REPLACE MISSING VALUES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# by Lana Kern

# In this file a function is generated which replaces missing values



## WRITE FUNCTION ##
#++++++++++++++++++#

# create function
  ## input: data frame for which missing values should be replaced
  ## and numeric and character vector containing the values to be replaced
  ## (because of ß they need to be defined inside the script using
  ## the function; otherwise ß is wrongly imported)
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
  dataframe[colnames_char] <- lapply(
    dataframe[colnames_char], function(x) replace(x,x %in% vec_missings_char, NA) 
  )
  
  # special for "weiß nicht"
  # dataframe <- dataframe %>%
  #   mutate(across(colnames_char, na_if, "weiß nicht"))

  
  # return dataframe
  return(dataframe)
}



## TEST FUNCTION ##
#+++++++++++++++++#


# # create test data
# df_test_missing <- data.frame(
#   A = c(1, 2, 3, -98, -55),
#   B = c(1, 2, 3, -98, -55),
#   C =  c("weiß nicht", "A", "B", "weiß", "nicht ermittelbar"),
#   D = c("weiß nicht", "Angabe verweigert", "Angabe", "nicht", "ermittelbar"),
#   E = as.factor(c("weiß nicht", "nein", "weiß nicht", "ja", "nein"))
# )
# 
# # apply function
# df_test_missing
# func_replace_missings(df_test_missing)
