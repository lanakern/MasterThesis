#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Inference after DML ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function returns variance, asymptotic standard error, t-value, and p-value
# of the treatment effect estimate.
#+++
# INPUT:
# ->
# ->
#+++
# OUTPUT:
# -> Data frame with 
#+++


# generate function 
func_dml_inference <- function(effect, theta, score, N, S_rep) {
  # treatment effect is mean over the K estimators
  theta <- mean(theta) 
  # variance is the variance of the score function (same as sum(score^2) / N)
  # score_influence <- -score / mean(score_a_ATE) # same result with influence function
  variance <- var(score) 
  # asymptotic standard error
  stderror <- sqrt(variance / N) 
  # t-value
  tvalue <- theta / stderror 
  # p-value
  pvalue <- 2 * pt(abs(tvalue), N, lower.tail = FALSE) 
  
  df_result <- data.frame(
    "Type" = effect, 
    "Rep" = S_rep, "Treatment_Effect" = theta, 
    "Variance" = variance, "Standard_Error" = stderror,
    "T_Value" = tvalue, "P_Value" = pvalue
  )
  
  return(df_result)
  
}