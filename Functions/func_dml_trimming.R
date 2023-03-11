#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: TRIMMING PROPENSITY SCORE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This functions trims the nuisance parameter predictions of the propensity score
# according to the chosen trimming thresholds.
# This function can be applied in the binary and multivalued treatment setting.
#+++
# INPUT:
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> "data_pred": data frame containing all nuisance parameter predictions and 
# their true values. This data frame is the basis for conducting the trimming.
# -> "data_test": test data -> trimming is transferred to test data so that
# only observations with enough overlap are used to estimate the treatment effects.
# -> "trimming": trimming threshold -> numeric or "min-max" or "no" (no trimming)
#+++
# OUTPUT:
# -> "data_pred": data frame containing only predictions for observations
# which are not dropped due to the trimming threshold.
# -> "data_test": data frame containing only observations which are not dropped
# due to the trimming threshold.
# -> "min_trimming": minimum trimming threshold (used for plot) -> especially relevant for "min-max" selection
# -> "max_trimming": maximum trimming threshold (used for plot) -> especially relevant for "min-max" selection
#+++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_dml_trimming <- function(treatment_setting, data_pred, data_test, trimming) {
  
  
  if (!treatment_setting %in% c("binary", "multi")) {
    stop("Treatment setting: binary or multi")
  }
  
  ## NO TRIMMING ##
  #+++++++++++++++#
  
  if (trimming == "no") {
    data_pred <- data_pred
    data_test <- data_test
    min_trimming <- 0
    max_trimming <- 1
  
  ## TRIMMING WITH NUMERIC NUMBER ##
  #++++++++++++++++++++++++++++++++#
  
  } else if (is.numeric(trimming)) {
    
    # trimming thresholds
    min_trimming <- trimming
    max_trimming <- 1 - min_trimming
    
    # find indices 
    if (treatment_setting == "binary") {
      indices_keep <- which(between(data_pred$m, min_trimming, max_trimming))
    } else if (treatment_setting == "multi") {
      indices_keep_1 <- which(between(data_pred$m1, min_trimming, max_trimming))
      indices_keep_2 <- which(between(data_pred$m2, min_trimming, max_trimming))
      indices_keep_3 <- which(between(data_pred$m3, min_trimming, max_trimming))
      indices_keep <- intersect(indices_keep_1, indices_keep_2)
      indices_keep <- intersect(indices_keep, indices_keep_3)
    }
    
    # subset
    data_pred <- data_pred[indices_keep, ]
    data_test <- data_test[indices_keep, ]
  
    
  ## MIN-MAX TRIMMING ##
  #++++++++++++++++++++#
  
  } else if (trimming == "min-max") {
    
    if (treatment_setting == "binary") {
      
      # identify smallest and largest propensity score per treatment status
      df_select_trimming <- data_pred %>% group_by(treatment) %>% 
        summarise(min_m = min(m), max_m = max(m))
      
      # trimming thresholds
      min_trimming <- max(df_select_trimming$min_m) # maximum of minimum
      max_trimming <- min(df_select_trimming$max_m) # minimum of maximum
      
      # find indices 
      indices_keep <- which(between(data_pred$m, min_trimming, max_trimming)) 
      
    } else if (treatment_setting == "multi") {
      # identify smallest and largest propensity score per treatment status
      df_select_trimming <- data_pred %>% dplyr::select(m1, m2, m3) %>%
        mutate(min_m = pmin(m1, m2, m3), max_m = pmax(m1, m2, m3)) %>%
        summarize(min_m = max(min_m), max_m = min(max_m))
      
      df_select_trimming <- rbind(
        data_pred %>% filter(treatment == 1) %>% dplyr::select(m1) %>% summarize(min_m = min(m1), max_m = max(m1)),
        data_pred %>% filter(treatment == 2) %>% dplyr::select(m2) %>% summarize(min_m = min(m2), max_m = max(m2)),
        data_pred %>% filter(treatment == 3) %>% dplyr::select(m3) %>% summarize(min_m = min(m3), max_m = max(m3))) 
      
      # trimming thresholds
      min_trimming <- max(df_select_trimming$min_m) # maximum of minimum
      max_trimming <- min(df_select_trimming$max_m) # minimum of maximum
      
      indices_keep_1 <- which(between(data_pred$m1, min_trimming, max_trimming))
      indices_keep_2 <- which(between(data_pred$m2, min_trimming, max_trimming))
      indices_keep_3 <- which(between(data_pred$m3, min_trimming, max_trimming))
      indices_keep <- intersect(indices_keep_1, indices_keep_2)
      indices_keep <- intersect(indices_keep, indices_keep_3)
    }
    
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