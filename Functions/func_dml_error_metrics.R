func_ml_error_metrics <- function(data_pred, S_rep, fold_sel) {

  ## create error metrics ##
  #++++++++++++++++++++++++#

  # Classification: treatment prediction
  data_pred <- data_pred %>%
    mutate(m_class = as.factor(round(m)), treatment = as.factor(treatment))
    ## accuracy
  m_acc <- accuracy(data_pred, truth = treatment, estimate = m_class) %>%
    select(.estimate) %>% pull()
    ## balanced accuracy
  m_bacc <- bal_accuracy(data_pred, truth = treatment, estimate = m_class) %>%
    select(.estimate) %>% pull()
  
  # Regression: outcome prediction
  g0_mse <- data_pred %>% filter(treatment == 0) %>% summarize(mean((outcome - g0)^2)) %>% pull()
  g0_rmse <- sqrt(g0_mse)
  g1_mse <- data_pred %>% filter(treatment == 1) %>% summarize(mean((outcome - g1)^2)) %>% pull()
  g1_rmse <- sqrt(g1_mse)
  
  # Report results in one data frame
  df_error <- data.frame("Repetition" = S_rep, "Fold" = fold_sel, 
                         "ACC" = m_acc, "BACC" = m_bacc, 
                         "RMSE_g0" = g0_rmse, "RMSE_g1" = g1_rmse)
  
  return(df_error)
}