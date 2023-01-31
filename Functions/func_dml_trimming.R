#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: TRIMMING PROPENSITY SCORE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This functions trims the nuisance parameter predictions of the propensity score
# according to the chosen trimming thresholds.
#+++
# INPUT:
# -> "data_pred": data frame containing all nuisance parameter predictions and 
# their true values. This data frame is the basis for conducting the trimming.
# -> "data_test": test data -> trimming is transferred to test data so that
# only observations with enough overlap are used to estimate the treatment effects.
# -> "trimming": trimming threshold
#+++
# OUTPUT:
# -> "data_pred": data frame containing only predictions for observations
# which are not dropped due to the trimming threshold.
# -> "data_test": data frame containing only observations which are not dropped
# due to the trimming threshold.
# -> "min_trimming": minimum trimming threshold (used for plot)
# -> "max_trimming": maximum trimming threshold (used for plot)
#+++

func_dml_trimming <- function(data_pred, data_test, trimming) {
  
  ## NO TRIMMING ##
  #+++++++++++++++#
  
  if (trimming == "no") {
    data_pred <- data_pred
    data_test <- data_test
    min_trimming <- 0
    max_trimming <- 1
  
  ## TRIMMING OF 0.01 and 0.99 ##
  #+++++++++++++++++++++++++++++#
  
  } else if (trimming == 0.01) {
    # trimming thresholds
    min_trimming <- 0.01
    max_trimming <- 1 - min_trimming
    # find indices 
    indices_keep <- which(between(data_pred$m, min_trimming, max_trimming))
    # subset
    data_pred <- data_pred[indices_keep, ]
    data_test <- data_test[indices_keep, ]
  
  ## TRIMMING OF 0.1 and 0.9 ##
  #+++++++++++++++++++++++++++#
  
  } else if (trimming == 0.1) {
    # trimming thresholds
    min_trimming <- 0.1
    max_trimming <- 1 - min_trimming
    # find indices 
    indices_keep <- which(between(data_pred$m, min_trimming, max_trimming))
    # subset
    data_pred <- data_pred[indices_keep, ]
    data_test <- data_test[indices_keep, ]
  
    
  ## MIN-MAX TRIMMING ##
  #++++++++++++++++++++#
  
  } else if (trimming == "min-max") {
    # identify smallest and largest propensity score per treatment status
    df_select_trimming <- data_pred %>% group_by(treatment) %>% 
      summarise(min_m = min(m), max_m = max(m))
    # trimming thresholds
    min_trimming <- max(df_select_trimming$min_m) # maximum of minimum
    max_trimming <- min(df_select_trimming$max_m) # minimum of maximum
    # find indices 
    indices_keep <- which(between(data_pred$m, min_trimming, max_trimming)) 
    # subset
    data_pred <- data_pred[indices_keep, ] 
    data_test <- data_test[indices_keep, ] 
  } else {
    stop("Please specify trimming threshold.")
  }
  
  
  # drop possibly missing values due to indices_keep
  data_pred <- na.omit(data_pred)
  data_test <- na.omit(data_test)
  
  
  # output
  return(list("data_pred" = data_pred, "data_test" = data_test, 
              "min_trimming" = min_trimming, "max_trimming" = max_trimming))
}