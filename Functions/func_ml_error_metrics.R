#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: CALCULATE ERROR METRICS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function calculates the error metrics for the prediction results of
# the nuisance parameters. For the prediction of the treatment, the accuracy (ACC),
# balanced accuracy (BACC), and Area Under the Curve (AUC) is reported while for the 
# prediction of the outcome the Mean Squared Error (MSE), Root Mean Squared Error (RMSE), 
# and Mean Absolute Error (MAE) is reported.
#+++
# INPUT: 
# -> "data_pred": data frame containing the predictions of the treatment and outcome
# as well as the true values.
# -> "S_rep": number of repetition
# -> "fold_sel": number of fold
#+++
# OUTPUT:
# -> Data frame containing the error metrics across each fold and each
# repetition.
#+++

func_ml_error_metrics <- function(data_pred, S_rep, fold_sel) {

  # Classification: treatment prediction
  data_pred <- data_pred %>%
    mutate(m_class = as.factor(round(m)), treatment = as.factor(treatment))
    ## accuracy
  m_acc <- yardstick::accuracy(data_pred, truth = treatment, estimate = m_class) %>%
    select(.estimate) %>% pull()
    ## balanced accuracy
  m_bacc <- yardstick::bal_accuracy(data_pred, truth = treatment, estimate = m_class) %>%
    select(.estimate) %>% pull()
    ## area under the curve
  m_auc <- yardstick::roc_auc(data_pred, truth = treatment, 
                              # 1-m to get probability for class 0 (first class is taken as positive class in roc_auc)
                              estimate = 1 - m) %>% 
    select(.estimate) %>% pull()
  
  # Regression: outcome prediction
    ## MSE (not in yardstick only rmse; but would be the same for yardstick::rmse^1)
  g0_mse <- data_pred %>% filter(treatment == 0) %>% summarize(mean((outcome - g0)^2)) %>% pull()
  g1_mse <- data_pred %>% filter(treatment == 1) %>% summarize(mean((outcome - g1)^2)) %>% pull()
    ## RMSE
  g0_rmse <- sqrt(g0_mse)
  g1_rmse <- sqrt(g1_mse)
    ## MAE
  g0_mae <- yardstick::mae(data_pred, truth = data_pred %>% filter(treatment == 0) %>% pull(outcome), 
                           estimate = data_pred %>% filter(treatment == 0) %>% pull(g0)) %>%
    select(.estimate) %>% pull()
  g1_mae <- yardstick::mae(data_pred, truth = data_pred %>% filter(treatment == 1) %>% pull(outcome), 
                           estimate = data_pred %>% filter(treatment == 1) %>% pull(g1)) %>%
    select(.estimate) %>% pull()
    ## MAPE
  g0_mape <- yardstick::mape(data_pred, truth = data_pred %>% filter(treatment == 0) %>% pull(outcome), 
                           estimate = data_pred %>% filter(treatment == 0) %>% pull(g0)) %>%
    select(.estimate) %>% pull()
  g1_mape <- yardstick::mape(data_pred, truth = data_pred %>% filter(treatment == 1) %>% pull(outcome), 
                           estimate = data_pred %>% filter(treatment == 1) %>% pull(g1)) %>%
    select(.estimate) %>% pull()
  
  # Report results in one data frame
  df_error <- data.frame("Repetition" = S_rep, "Fold" = fold_sel, 
                         "ACC" = m_acc, "BACC" = m_bacc, "AUC" = m_auc,
                         "MAE_g0" = g0_mae, "MAE_g1" = g1_mae, 
                         "MAPE_g0" = g0_mape, "MAPE_g1" = g1_mape,
                         "MSE_g0" = g0_mse, "MSE_g1" = g1_mse, 
                         "RMSE_g0" = g0_rmse, "RMSE_g1" = g1_rmse)
  
  return(df_error)
}