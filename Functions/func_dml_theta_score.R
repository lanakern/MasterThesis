#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATE THETA AND SCORE FUNCTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function estimates the neyman-orthogonal score function as well as
# the treatment parameter across the folds in both the binary and multivalued
# treatment setting.
#++++
# INPUT
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> "data_pred": Predictions for the nuisance functions
# -> "data_test": Test data set used to estimate the treatment effect and score
# -> "outcome": outcome variable included in data test
# -> "treatment": treatment variable included in data test
#++++
# OUTPUT
# -> theta_ATE_: vector containing the ATE estimate
# -> theta_ATTE: vector containing the ATTE estimate
# -> score_ATE: values for the neyman orthogonal score function of the ATE
# -> score_ATTE: values for the neyman orthogonal score function of the ATTE
# -> APO_*: APO for individuals with treatment status *
#++++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_dml_theta_score <- function(treatment_setting, data_pred, data_test, outcome, treatment) {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Binary Treatment Setting ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
    
    # extract outcome, treatment, etc.
    Y <- data_test[, outcome]  # outcome
    D <- data_test[, treatment] # treatment
    n <- nrow(data_test) # number of observations in test data
    m <- sum(data_test %>% pull(all_of(treatment))) / n # E(D)
    m_pred <- data_pred %>% pull(m)
    g0_pred <- data_pred %>% pull(g0)
    g1_pred <- data_pred %>% pull(g1)
    
    
    ## APO ##
    #-------#
    
    pseudo_0 <- g0_pred + (1 - D) * (Y - g0_pred) / (1 - m_pred)
    pseudo_1 <- g1_pred + D * (Y - g1_pred) / m_pred
    
    apo_0 <- mean(pseudo_0)
    apo_1 <- mean(pseudo_1)
    
    
    ## ATE ##
    #-------#
    
    # calculate theta
    score_a_ATE <- rep(-1, nrow(data_test))
    score_b_ATE <- pseudo_1 - pseudo_0
    theta_ATE <- -sum(score_b_ATE) / sum(score_a_ATE)
    
    
    # calculate score
    score_ATE <- theta_ATE * score_a_ATE + score_b_ATE
    
    
    
    ## ATTE ##
    #--------#
    
    # calculate theta
    score_a_ATTE <- -D / m 
    score_b_ATTE <- D * (Y - g0_pred) / m - 
      m_pred * (1 - D) * (Y - g0_pred) * (m*(1 - m_pred))
    theta_ATTE <- -sum(score_b_ATTE) / sum(score_a_ATTE)
    
    
    # calculate score
    score_ATTE <- theta_ATTE * score_a_ATTE + score_b_ATTE
    
    
    return(list("score_ATE" = score_ATE, "theta_ATE" = theta_ATE, 
                "score_ATTE" = score_ATTE, "theta_ATTE" = theta_ATTE,
                "APO_0" = apo_0, "APO_1" = apo_1))
  
    
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Multivalued Treatment Setting ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    
    # extract outcome
    Y <- data_test[, outcome]
    
    # extract treatments
    D_1 <- data_test$treatment_sport_freq_weekly_atleast
    D_2 <- data_test$treatment_sport_freq_monthly_less
    D_3 <- data_test$treatment_sport_freq_never
    
    # number of observations in test data
    n <- nrow(data_test) 
    
    # calculate Prob(D = t)
    m1 <- sum(data_test %>% pull(all_of(treatment_sport_freq_weekly_atleast))) / n 
    m2 <- sum(data_test %>% pull(all_of(treatment_sport_freq_monthly_less))) / n 
    m3 <- sum(data_test %>% pull(all_of(treatment_sport_freq_never))) / n 
    
    # treatment predictions
    m1_pred <- data_pred %>% pull(m1)
    m2_pred <- data_pred %>% pull(m2)
    m3_pred <- data_pred %>% pull(m3)
    
    # outcome predictions
    g1_pred <- data_pred %>% pull(g1)
    g2_pred <- data_pred %>% pull(g2)
    g3_pred <- data_pred %>% pull(g3)
    
    
    ## APO ##
    #-------#
    
    pseudo_1 <- g1_pred + D_1 * (Y - g1_pred) / m1_pred
    pseudo_2 <- g2_pred + D_2 * (Y - g2_pred) / m2_pred
    pseudo_3 <- g3_pred + D_3 * (Y - g3_pred) / m3_pred
    
    apo_1 <- mean(pseudo_1)
    apo_2 <- mean(pseudo_2)
    apo_3 <- mean(pseudo_3)
    
    
    ## ATE ##
    #-------#
    
    func_dml_ATE <- function(data, pseudo_0, pseudo_1) {
      
      # calculate theta
      score_a_ATE <- rep(-1, nrow(data))
      score_b_ATE <- pseudo_1 - pseudo_0
      theta_ATE <- -sum(score_b_ATE) / sum(score_a_ATE)
      
      # calculate score
      score_ATE <- theta_ATE * score_a_ATE + score_b_ATE
      
      # return
      return(list("theta" = theta_ATE, "score" = score_ATE))
    }
    

    result_ate_31 <- func_dml_ATE(data_test, pseudo_3, pseudo_1)
    result_ate_32 <- func_dml_ATE(data_test, pseudo_3, pseudo_2)
    result_ate_21 <- func_dml_ATE(data_test, pseudo_2, pseudo_1)
    
    df_ate <- data.frame("no_weekly" = result_ate_31$theta, 
                         "no_monthly" = result_ate_32$theta, 
                         "monthly_weekly" = result_ate_21$theta)
    
    df_ate_score <- data.frame("no_weekly" = result_ate_31$score, 
                               "no_monthly" = result_ate_32$score, 
                               "monthly_weekly" = result_ate_21$score)
    
    
    ## ATTE ##
    #--------#
    
    func_dml_ATTE <- function(D_0, D_1, m0_pred, m1_pred, g0_pred, m1) {
      # calculate theta
      score_a_ATTE <- -D_1 / m1
      score_b_ATTE <- D_1 * (Y - g0_pred) / m1 - 
        m1_pred * D_0 * (Y - g0_pred) / (m1 * m0_pred)
      
      theta_ATTE <- -sum(score_b_ATTE) / sum(score_a_ATTE)
      
      
      # calculate score
      score_ATTE <- theta_ATTE * score_a_ATTE + score_b_ATTE
      
      # return
      return(list("theta" = theta_ATTE, "score" = score_ATTE))
    }
    
    result_atte_31 <- func_dml_ATTE(D_3, D_1, m3_pred, m1_pred, g3_pred, m1)
    result_atte_32 <- func_dml_ATTE(D_3, D_2, m3_pred, m2_pred, g3_pred, m2)
    result_atte_21 <- func_dml_ATTE(D_2, D_1, m2_pred, m1_pred, g2_pred, m1)
    
    df_atte <- data.frame("no_weekly" = result_atte_31$theta, 
                          "no_monthly" = result_atte_32$theta, 
                          "monthly_weekly" = result_atte_21$theta)
    
    df_atte_score <- data.frame("no_weekly" = result_atte_31$score, 
                                "no_monthly" = result_atte_32$score, 
                                "monthly_weekly" = result_atte_21$score)
    
    
    return(list("score_ATE" = df_ate_score, "theta_ATE" = df_ate, 
                "score_ATTE" = df_atte_score, "theta_ATTE" = df_atte,
                "APO_1" = apo_1, "APO_2" = apo_2, "APO_3" = apo_3))
    
  }
  
  
  
}