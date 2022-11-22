#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION FOR REVERSING VARIABLE VALUES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function reverses the score / variable values of a variable.
# This is necessary to aggregate the variables since for example some stress
# variables are positively and some others negatively scored.
#++++
# Inputs:
# -> data: data set which includes the variables which values should be reversed
# -> data_reverse: column "vars_reverse" with the variable names that should be
# reversed: column "num_scores" with highest value label, e.g. for 5-point likert
# scale this would be 5.
#++++
# Output: dataset including the variables with reversed order
#++++


func_reverse_score <- function(data, data_reverse) {
  
  # iterate over variables
  for (var_sel in data_reverse$vars_reverse) {
    
    # subset
    data_reverse_sub <- data_reverse %>% subset(vars_reverse == var_sel)
    
    # increase maximum value by one (needed for next step)
    num_scores_adj <- data_reverse_sub$num_scores + 1
    
    # to reverse the order subtract num_scores_adj by the current order
    # for instance, variable takes on values 1-4, then num_scores_adj = 5
    # 5-4 = 1, 5-3 = 2, 5-2 = 1, and 5-1 =4 --> REVERSED.
    data <- data %>%
      mutate({{var_sel}} := num_scores_adj - !!rlang::sym(var_sel))
  }
  
  # return data set
  return(data)
}


# test
# data_vars <- data %>% select(uni_termination_4, stress_3, satisfaction_study_2)
# data_reverse_vars <- df_reverse_vars %>% 
#   subset(vars_reverse %in% c("uni_termination_4", "stress_3", "satisfaction_study_2"))
# 
# func_reverse_score(data_vars, data_reverse_vars)