#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: CALCULATE ERROR METRICS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function calculates the error metrics for the prediction results of
# the nuisance parameters. For the prediction of the treatment(s), the accuracy (ACC),
# balanced accuracy (BACC), cohen kappa score, and the Area Under the Curve (AUC)
# is reported. For the prediction of the outcome the Mean Squared Error (MSE), 
# Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), and Mean Absolute 
# Percentage Error (MAPE) is reported.
# This function can be applied in both the binary and multivalued treatment setting.
# For the multivalued treatment setting, the error metrics are calculated on a 
# "one-vs-rest" basis by converting the multiclass-classifiction in a binary one.
# In the multiclass-classification for the multinominal regression simply the mean of 
# the error metrics across classes is taken (macro approach).
#+++
# EXPLANATION OF ERROR METRICS
# -> ACC: Percent of correctly classified predictions.
# -> BACC: Similar to accuracy but taking class imbalance / occurence by chance into account.
# -> Cohen kappa score: Similar to accuracy but taking class imbalance / occurence by chance into account.
# -> AUC: Quantifies the models ability to distinguish between each class. To
# calculate the AUC class probabilities are required.
# -> MSE: expected value of the squared (quadratic) error to show large deviations due to outliers
# -> RMSE: square root of MSE for same units as the response variable 
# -> MAE: error between the prediction and actual value; measures the average magnitude of errors, 
# without considering the direction or sign of that error.
# -> MAPE: percentage equivaluen to the MAE
# https://scikit-learn.org/stable/modules/model_evaluation.html
# https://medium.com/analytics-vidhya/error-metrics-in-machine-learning-f9eed7b139f
# https://towardsdatascience.com/comprehensive-guide-on-multiclass-classification-metrics-af94cfb83fbd
# https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/#:~:text=The%20area%20under%20the%20ROC%20curve%20(AUC)%20is%20a%20useful,to%20the%20multi%2Dclass%20setting.
#+++
# INPUT: 
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> "data_pred": data frame containing the predictions of the treatment and outcome
# as well as the true values.
# -> "S_rep": number of repetition
# -> "fold_sel": number of fold
# -> "probscore_separate": only relevant for multivalued treatment setting ->
# if TRUE (default) multivalued treatment setting is split in binary treatment setting
# and separate binary logistic regressions are performed. 
#+++
# OUTPUT:
# -> Data frame containing the error metrics across each fold and each
# repetition.
#+++

func_ml_error_metrics <- function(treatment_setting, data_pred, S_rep, fold_sel, probscore_separate = TRUE) {
  
  if (!treatment_setting %in% c("binary", "multi")) {
    stop("Treatment setting: binary or multi")
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### CLASSIFICATION ERROR METRICS ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  
  #### BINARY ####
  #++++++++++++++#
  
  if (treatment_setting == "binary") {
    # for classification extract class; treatment as factor
    data_pred <- data_pred %>%
      mutate(m_class = ifelse(m >= 0.5, 1, 0), treatment = as.factor(treatment))
    
    # if all predictions are for one class, generate both factor levels
    if (length(unique(data_pred$m_class)) == 1) {
      data_pred$m_class <- factor(data_pred$m_class, levels = c(0, 1))
    } else {
      data_pred$m_class <- as.factor(data_pred$m_class)
    }
    
    # confusion matrix
    conf_matrix <- confusionMatrix(data_pred$m_class, data_pred$treatment)
    
    # accuracy
    m_acc <- unname(conf_matrix$overall["Accuracy"])
    
    # kappa
    m_kappa <- unname(conf_matrix$overall["Kappa"])
    
    # Balanced Accuracy
    m_bacc <- unname(conf_matrix$byClass["Balanced Accuracy"])
    
    # Area under the curve
    m_auc <- yardstick::roc_auc(data_pred %>% mutate(m_0 = 1 - m), truth = treatment, 
                                # 1-m to get probability for class 0 (first class is taken as positive class in roc_auc)
                                estimate = m_0) %>% select(.estimate) %>% pull()
    
    # combine data set
    df_error_class <- data.frame(
      "Repetition" = S_rep, "Fold" = fold_sel, 
      "ACC_m" = m_acc, "KAPPA_m" = m_kappa, "BACC_m" = m_bacc, "AUC_m" = m_auc
    )

    
    
    
  #### MULTI ####
  #+++++++++++++#
    
  } else if (treatment_setting == "multi") {
    # again differentiate between separate m(D) models and one multinominal m(D) model  
    
    ## SEPARTE ##
    
    if (probscore_separate == TRUE) {
      
      # predicted treatment and true treatmemt level
      data_pred <- data_pred %>%
        mutate(m1_class = as.factor(ifelse(m1 > 0.5, 1, 0)), m1_treatment = as.factor(ifelse(treatment == 1, 1, 0)),
               m2_class = as.factor(ifelse(m2 > 0.5, 1, 0)), m2_treatment = as.factor(ifelse(treatment == 2, 1, 0)),
               m3_class = as.factor(ifelse(m3 > 0.5, 1, 0)), m3_treatment = as.factor(ifelse(treatment == 3, 1, 0)))
      
      # confusion matrices
      m1_conf_matrix <- confusionMatrix(data_pred$m1_class, data_pred$m1_treatment)
      m2_conf_matrix <- confusionMatrix(data_pred$m2_class, data_pred$m2_treatment)
      m3_conf_matrix <- confusionMatrix(data_pred$m3_class, data_pred$m3_treatment)
      
      # accuracy
      ACC_m1 <- unname(m1_conf_matrix$overall["Accuracy"])
      ACC_m2 <- unname(m2_conf_matrix$overall["Accuracy"])
      ACC_m3 <- unname(m3_conf_matrix$overall["Accuracy"])
      
      # balanced accuracy
      BACC_m1 <- unname(m1_conf_matrix$byClass["Balanced Accuracy"])
      BACC_m2 <- unname(m2_conf_matrix$byClass["Balanced Accuracy"])
      BACC_m3 <- unname(m3_conf_matrix$byClass["Balanced Accuracy"])
      
      # kappa
      KAPPA_m1 <- unname(m1_conf_matrix$overall["Kappa"])
      KAPPA_m2 <- unname(m1_conf_matrix$overall["Kappa"])
      KAPPA_m3 <- unname(m1_conf_matrix$overall["Kappa"])
      
      # AUC
      AUC_m1 <- yardstick::roc_auc(data_pred %>% mutate(m_0 = 1 - m1), truth = m1_treatment, 
                                   estimate = m_0) %>% select(.estimate) %>% pull()
      
      AUC_m2 <- yardstick::roc_auc(data_pred %>% mutate(m_0 = 1 - m2), truth = m2_treatment, 
                                   estimate = m_0) %>% select(.estimate) %>% pull()
      
      AUC_m3 <- yardstick::roc_auc(data_pred %>% mutate(m_0 = 1 - m3), truth = m3_treatment, 
                                   estimate = m_0) %>% select(.estimate) %>% pull()
      
      # combine in one data frame
      df_error_class <- data.frame(
        "Repetition" = S_rep, "Fold" = fold_sel, 
        "ACC_m1" = ACC_m1, "ACC_m2" = ACC_m2, "ACC_m3" = ACC_m3, 
        "BACC_m1" = BACC_m1, "BACC_m2" = BACC_m2, "BACC_m3" = BACC_m3, 
        "KAPPA_m1" = KAPPA_m1, "KAPPA_m2" = KAPPA_m2, "KAPPA_m3" = KAPPA_m3, 
        "AUC_m1" = AUC_m1, "AUC_m2" = AUC_m2, "AUC_m3" = AUC_m3
      )
      
      
      
    ## MULTI ##
      
    } else {
      
      # for classification extract class; treatment as factor
      data_pred <- data_pred %>%
        mutate(m_max = pmax(m1, m2, m3), treatment = as.factor(treatment)) %>%
        mutate(m_class = case_when(m1 == m_max ~ 1, m2 == m_max ~ 2, TRUE ~ 3)) 
      
      # if all predictions are for one class, generate both factor levels
      if (length(unique(data_pred$m_class)) == 1) {
        data_pred$m_class <- factor(data_pred$m_class, levels = c(1, 2, 3))
      } else {
        data_pred$m_class <- as.factor(data_pred$m_class)
      }
      
      # confusion matrix
      conf_matrix <- confusionMatrix(data_pred$m_class, data_pred$treatment)
      
      # accuracy
      m_acc <- unname(conf_matrix$overall["Accuracy"])
      
      # kappa
      m_kappa <- unname(conf_matrix$overall["Kappa"])
      
      # Balanced Accuracy: mean over accuracies for each class
      m_bacc <- mean(unname(conf_matrix$byClass[, "Balanced Accuracy"])) 
      
      # AUC
      m_auc <- multiclass.roc(
        data_pred %>% mutate(treatment = case_when(treatment == 1 ~ "m1", treatment == 2 ~ "m2", TRUE ~ "m3")) %>% pull(treatment), 
        data_pred[, c("m1", "m2", "m3")])
      m_auc <- as.numeric(str_replace(m_auc$auc, "Multi-class area under the curve: ", ""))
      
      # combine in one data frame
      df_error_class <- data.frame(
        "Repetition" = S_rep, "Fold" = fold_sel, 
        "ACC_m" = m_acc, "KAPPA_m" = m_kappa, "BACC_m" = m_bacc, "AUC_m" = m_auc
      )
      
    } # close if-else over probscore_separate
    

  } # close if-else over treatment_setting
  

  

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### REGRESSION ERROR METRICS ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


  if (treatment_setting == "binary") {
    # Regression: outcome prediction
    ## MSE (not in yardstick only rmse; but would be the same for yardstick::rmse^1)
    g0_mse <- data_pred %>% filter(treatment == 0) %>% summarize(mean((outcome - g0)^2)) %>% pull()
    g1_mse <- data_pred %>% filter(treatment == 1) %>% summarize(mean((outcome - g1)^2)) %>% pull()
    ## RMSE
    g0_rmse <- sqrt(g0_mse)
    g1_rmse <- sqrt(g1_mse)
    ## MAE
    g0_mae <- yardstick::mae(data_pred %>% filter(treatment == 0), truth = outcome, estimate = g0) %>%
      select(.estimate) %>% pull()
    g1_mae <- yardstick::mae(data_pred  %>% filter(treatment == 1), truth = outcome, estimate = g1) %>%
      select(.estimate) %>% pull()
    ## MAPE
    g0_mape <- yardstick::mape(data_pred %>% filter(treatment == 0), truth = outcome, estimate = g0) %>%
      select(.estimate) %>% pull()
    g1_mape <- yardstick::mape(data_pred %>% filter(treatment == 1), truth = outcome, estimate = g1) %>%
      select(.estimate) %>% pull()
    
    # Report results in one data frame
    df_error_reg <- data.frame(
      "MAE_g0" = g0_mae, "MAE_g1" = g1_mae, "MAPE_g0" = g0_mape, "MAPE_g1" = g1_mape,
      "MSE_g0" = g0_mse, "MSE_g1" = g1_mse, "RMSE_g0" = g0_rmse, "RMSE_g1" = g1_rmse
      )
    
  } else if (treatment_setting == "multi") {
    # Regression: outcome prediction
    ## MSE (not in yardstick only rmse; but would be the same for yardstick::rmse^1)
    g1_mse <- data_pred %>% filter(treatment == 1) %>% summarize(mean((outcome - g1)^2)) %>% pull()
    g2_mse <- data_pred %>% filter(treatment == 2) %>% summarize(mean((outcome - g2)^2)) %>% pull()
    g3_mse <- data_pred %>% filter(treatment == 3) %>% summarize(mean((outcome - g3)^2)) %>% pull()
    ## RMSE
    g1_rmse <- sqrt(g1_mse)
    g2_rmse <- sqrt(g2_mse)
    g3_rmse <- sqrt(g3_mse)
    ## MAE
    g1_mae <- yardstick::mae(data_pred  %>% filter(treatment == 1), truth = outcome, estimate = g1) %>%
      select(.estimate) %>% pull()
    g2_mae <- yardstick::mae(data_pred %>% filter(treatment == 2), truth = outcome, estimate = g2) %>%
      select(.estimate) %>% pull()
    g3_mae <- yardstick::mae(data_pred %>% filter(treatment == 3), truth = outcome, estimate = g3) %>%
      select(.estimate) %>% pull()
    ## MAPE
    g1_mape <- yardstick::mape(data_pred %>% filter(treatment == 1), truth = outcome, estimate = g1) %>%
      select(.estimate) %>% pull()
    g2_mape <- yardstick::mape(data_pred %>% filter(treatment == 2), truth = outcome, estimate = g2) %>%
      select(.estimate) %>% pull()
    g3_mape <- yardstick::mape(data_pred %>% filter(treatment == 3), truth = outcome, estimate = g3) %>%
      select(.estimate) %>% pull()
    
    # Report results in one data frame
    df_error_reg <- data.frame(
      "MAE_g1" = g1_mae, "MAE_g2" = g2_mae, "MAE_g3" = g3_mae,
      "MAPE_g1" = g1_mape, "MAPE_g2" = g2_mape, "MAPE_g3" = g3_mape,
      "MSE_g1" = g1_mse, "MSE_g2" = g2_mse, "MSE_g3" = g3_mse,
      "RMSE_g1" = g1_rmse, "RMSE_g2" = g2_rmse, "RMSE_g3" = g3_rmse
      )
  }
  
  # combine both data sets
  df_error <- cbind(df_error_class, df_error_reg)

  return(df_error)
}