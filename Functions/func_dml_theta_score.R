#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATE THETA AND SCORE FUNCTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function estimates the neyman-orthogonal score function as well as
# the treatment parameter across the folds.
#++++
# INPUT
# -> "data": Data set used for making the predictions
# -> "outcome": outcome variable included in data
# -> "treatment": treatment variable included in data
# -> "data_pred": Predictions for the nuisance functions
# -> "K-folds": data partition indices
#++++
# OUTPUT
# -> theta_ATE_all: vector containing K estimates for the ATE
# -> theta_ATTE_all: vector containing K estimates for the ATTE
# -> score_ATTE_all: values for the score function of the ATE
# -> score_ATTE_all: values for the score function of the ATTE
#++++

func_dml_theta_score <- function(data, outcome, treatment, data_pred, K_folds) {
  
  # create empty vectors to store results
    ## to store treatment effect estimates
  theta_ATE_all <- c()
  theta_ATTE_all <- c()
    ## to store values of the score function
  score_ATE_all <- c()
  score_ATTE_all <- c()
  
  # iterate over folds
  for (fold_sel in 1:nrow(K_folds)) {
    
    # extracting test data
    indices_fold_sel <- K_folds$splits[[fold_sel]]$in_id
    data_test <- data[-indices_fold_sel, ]
    
    # extract outcome, treatment, etc.
    Y <- data_test[, outcome] %>% pull() # outcome
    D <- data_test[, treatment] %>% pull() # treatment
    n <- nrow(data_test) # number of observations in test data
    m <- sum(data_test %>% dplyr::select(all_of(treatment)) %>% pull()) / n # E(D)
    m_pred <- data_pred %>% filter(fold == fold_sel) %>% pull(m)
    g0_pred <- data_pred %>% filter(fold == fold_sel) %>% pull(g0)
    g1_pred <- data_pred %>% filter(fold == fold_sel) %>% pull(g1)
    
    
    ## ATE ##
    #-------#
    
    # calculate theta
    score_a_ATE <- rep(-1, nrow(data_test))
    pseudo_0_ATE <- g0_pred + (1 - D) * (Y - g0_pred) / (1 - m_pred)
    pseudo_1_ATE <- g1_pred + D * (Y - g1_pred) / m_pred
    score_b_ATE <- pseudo_1_ATE - pseudo_0_ATE
    theta_ATE <- -sum(score_b_ATE) / sum(score_a_ATE)
    theta_ATE_all <- c(theta_ATE_all, theta_ATE)
    
    # calculate score
    score_ATE <- theta_ATE * score_a_ATE + score_b_ATE
    score_ATE_all <- c(score_ATE_all, score_ATE)
    
    
    ## ATTE ##
    #--------#
    
    # calculate theta
    score_a_ATTE <- -D / m 
    score_b_ATTE <- D * (Y - g0_pred) / m - 
      m_pred * (1 - D) * (Y - g0_pred) * (m*(1 - m_pred))
    theta_ATTE <- -sum(score_b_ATTE) / sum(score_a_ATTE)
    theta_ATTE_all <- c(theta_ATTE_all, theta_ATTE)
    
    # calculate score
    score_ATTE <- theta_ATTE * score_a_ATTE + score_b_ATTE
    score_ATTE_all <- c(score_ATTE_all, score_ATTE)
  }
  
  return(list("score_ATE_all" = score_ATE_all, "theta_ATE_all" = theta_ATE_all, 
              "score_ATTE_all" = score_ATTE_all, "theta_ATTE_all" = theta_ATTE_all))
}