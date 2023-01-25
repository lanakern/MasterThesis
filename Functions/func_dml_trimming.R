func_dml_trimming <- function(data_pred, data_test, trimming) {
  if (trimming == "no") {
    data_pred <- data_pred
    data_test <- data_test
    min_trimming <- 0
    max_trimming <- 1
  } else if (trimming == 0.01) {
    # trimming thresholds
    min_trimming <- 0.01
    max_trimming <- 1 - min_trimming
    # find indices 
    indices_keep <- which(between(data_pred$m, min_trimming, max_trimming))
    # subset
    data_pred <- data_pred[indices_keep, ]
    data_test <- data_test[indices_keep, ]
  } else if (trimming == 0.1) {
    # trimming thresholds
    min_trimming <- 0.1
    max_trimming <- 1 - min_trimming
    # find indices 
    indices_keep <- which(between(data_pred$m, min_trimming, max_trimming))
    # subset
    data_pred <- data_pred[indices_keep, ]
    data_test <- data_test[indices_keep, ]
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
  
  data_pred <- na.omit(data_pred)
  data_test <- na.omit(data_test)
  
  return(list("data_pred" = data_pred, "data_test" = data_test, 
              "min_trimming" = min_trimming, "max_trimming" = max_trimming))
}