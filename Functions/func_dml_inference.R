#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inference after DML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function returns variance, asymptotic standard error, t-value, and p-value
# of the treatment effect estimates across the K-folds in the current repetition.
# This function can be applied to both the binary and multivalued treatment setting.
# The calculations are obtained from Chernozhukov et al. (2017, 2018).
#+++
# INPUT:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued 
# treatment setting ("multi")
# -> "effect": "ATE" or "ATTE" (only appended to resulting data frame)
# -> "theta": treatment effect estimates from K-folds
# -> "score": values of score function from K-folds 
# -> "N": number of observations in full data set, i.e., without train and test split
# -> "S_rep": current number of repetition (only appended to resulting data frame)
#+++
# OUTPUT:
# -> Data frame with variance, asymptotic standard error, t-value and
# p-value of estimated treatment effect across the K-folds in repetition S_rep.
#+++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# generate function 
func_dml_inference <- function(treatment_setting, effect, theta, score, N, S_rep) {
  
  
  ## BINARY TREATMENT SETTING ##
  #++++++++++++++++++++++++++++#
  
  if (treatment_setting == "binary") {
    
    # treatment effect is mean over the K estimators
    theta <- mean(theta) 
    # variance is the variance of the score function (same as sum(score^2) / N)
    # score_influence <- -score / mean(score_a_ATE) # same result with influence function
    variance <- stats::var(score) 
    # asymptotic standard error
    stderror <- sqrt(variance / N) 
    # t-value
    tvalue <- theta / stderror 
    # p-value
    pvalue <- 2 * pt(abs(tvalue), N, lower.tail = FALSE) 
    
    # data frame
    df_result <- data.frame(
      "Treatment" = "no_yes", "Type" = effect, "Rep" = S_rep, 
      "Treatment_Effect" = theta, "Variance" = variance, 
      "Standard_Error" = stderror, "T_Value" = tvalue, "P_Value" = pvalue
    )
    
    # return result
    return(df_result)
  
  
  ## MULTIVALUED TREATMENT SETTING ##
  #+++++++++++++++++++++++++++++++++#
  
  } else if (treatment_setting == "multi") {
    
    # treatment effect is mean over the K estimators for each treatment level comparison
    theta <- colMeans(theta)
    # variance is the variance of the score function for each treatment level comparison
    variance <- score %>% mutate(across(colnames(score), ~ stats::var(.))) %>% distinct()
    # asymptotic standard error
    stderror <- sqrt(variance / N) 
    # t-value
    tvalue <- theta / stderror
    # p-value
    pvalue <- tvalue %>% mutate(across(colnames(tvalue), ~ 2 * pt(abs(.), N, lower.tail = FALSE)))
    
    # data frame
    df_result <- data.frame(
      "Type" = effect, 
      "Rep" = S_rep, "Treatment_Effect" = t(t(theta)), 
      "Variance" = t(variance), "Standard_Error" = t(stderror),
      "T_Value" = t(tvalue), "P_Value" = t(pvalue)
    )
    
    # row names as column
    df_result <- df_result %>%
      mutate(Treatment = rownames(df_result)) %>%
      select(Treatment, everything())
    rownames(df_result) <- NULL
    
    # return result
    return(df_result)
    
  }
  
  
}