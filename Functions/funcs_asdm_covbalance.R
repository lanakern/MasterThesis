#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: COVARIATE BALANCING AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##++
# by Lana Kern
#++
# This file includes all functions for covariate balancing assessment and main
# drivers of selection identification.
# -> func_diff_means: calculation of difference in means
# -> func_stand_factor: calculation of standardization factor
# -> func_asdm: calculation of absolute mean standardized differences (asdm)
# -> func_weights: calculate weights
# -> func_weights_normalize: normalized weights
# -> func_cov_balance: assessment of covariate balancing (runs all three
# previous function).
# Information about input and output is given in the respective section.
#++
# # Sources:
# -> https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# -> https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
# -> Thoemmes and Kim (2011)
# -> Knaus (2018)
#++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DIFFERENCE IN MEANS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Function for absolute difference in means
# Input:
# -> data: data set including all variables for which mean differences
# should be calculated.
# -> treatment_var: grouping variable included in data
# Output: Matrix containing mean differences

func_diff_means <- function(data, treatment_var) {
  
  # calculate absolute difference in means
  df_diff <- abs(diff(as.matrix(
    # mean across groups for all variables
    data %>% 
      group_by(!!sym(treatment_var)) %>%
      summarise(across(everything(), mean)) %>%
      dplyr::select(-all_of(treatment_var))
  ))) %>% 
    as.matrix()
  
  return(df_diff)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### STANDARDIZATION FACTOR ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Function for standardization factor: square root of the mean of the
# covariate variances across treatment and control group
# Input:
# -> data: data set including all variables for which mean differences
# should be calculated.
# -> treatment_var: grouping variable included in data
# Output: Matrix containing standardization factor for each covariate.

func_stand_factor <- function(data, treatment_var) {
  
  # calculate factor
  df_meanvar_binary_bef <- data %>% 
    group_by(!!sym(treatment_var)) %>%
    summarise(across(everything(), var)) %>%
    dplyr::select(-all_of(treatment_var)) %>%
    summarise(across(everything(), mean)) %>% 
    summarise(across(everything(), sqrt)) %>%
    as.matrix()
  
  return(df_meanvar_binary_bef)
  
}


#%%%%%%%%%%%%#
#### ASDM ####
#%%%%%%%%%%%%#

# Calculate Absolute Standardized Differences (Yang et al. 2006)
# that is mean divided by standardization factor multiplied by 100
# Input:
# -> treatment_setting: "binary" of "multi" (only needed for data frame column)
# -> num_controls: "all" or "selected" by post-lasso (only needed for data frame column)
# -> adjustment: "before" or "after" DML (only needed for data frame column)
# -> data: data set including all variables for which mean differences
# should be calculated.
# -> treatment_var: grouping variable included in data
# Output: Matrix containing mean absolute standardized differences

func_asdm <- function(treatment_setting, num_controls, adjustment, data, treatment_var) {
  
  ## Binary Treatment Setting ##
  #++++++++++++++++++++++++++++#
  
  if (treatment_setting == "binary") {
   
    # calculate absolute difference in means
    df_means <- func_diff_means(data, treatment_var)
    
    # calculate standardization factors
    df_factor <- func_stand_factor(data, treatment_var)
    
    # ASDM
    df_smd_all <- data.frame("SD" = t((df_means / df_factor) * 100)) %>% na.omit()
    
    # Columns
    num_cols <- ncol(data) - 4
    df_smd_summary <- data.frame(
      "treatment_setting" = treatment_setting, "adjustment" = adjustment, 
      "controls" = num_controls,
      "min" = min(df_smd_all$SD), "max" = max(df_smd_all$SD),
      "mean" = mean(df_smd_all$SD), "median" = median(df_smd_all$SD),
      "num_cov_smd_20" = sum(df_smd_all$SD > 20), 
      "num_cov_smd_10" = sum(df_smd_all$SD > 20), 
      "num_cov_smd_5" = sum(df_smd_all$SD > 20), 
      "perc_cov_smd_20" = sum(df_smd_all$SD > 20) / num_cols, 
      "perc_cov_smd_10" = sum(df_smd_all$SD > 10) / num_cols, 
      "perc_cov_smd_5" = sum(df_smd_all$SD > 5) / num_cols
    )
     
  ## Multiple Treatment Setting ##
  #++++++++++++++++++++++++++++++#
  
  } else {
    
    # split data sets to compare different groups
    data_split_1 <- data %>% filter(!!sym(treatment_var) %in% c(1, 2))
    data_split_2 <- data %>% filter(!!sym(treatment_var) %in% c(2, 3))
    data_split_3 <- data %>% filter(!!sym(treatment_var) %in% c(1, 3))
    
    # calculate absolute difference in means
    df_means_split_1 <- func_diff_means(data_split_1, treatment_var)
    df_means_split_2 <- func_diff_means(data_split_2, treatment_var)
    df_means_split_3 <- func_diff_means(data_split_3, treatment_var)
    
    # calculate standardization factors
    df_factor_split_1 <- func_stand_factor(data_split_1, treatment_var)
    df_factor_split_2 <- func_stand_factor(data_split_2, treatment_var)
    df_factor_split_3 <- func_stand_factor(data_split_3, treatment_var)
    
    # calculate ASDM
    df_smd_1 <- data.frame("SD_1" = t((df_means_split_1 / df_factor_split_1) * 100)) %>% na.omit()
    df_smd_2 <- data.frame("SD_2" = t((df_means_split_2 / df_factor_split_2) * 100)) %>% na.omit()
    df_smd_3 <- data.frame("SD_3" = t((df_means_split_3 / df_factor_split_3) * 100)) %>% na.omit()
    
    df_smd_all <- left_join(
      rownames_to_column(df_smd_1), rownames_to_column(df_smd_2), by = "rowname"
    )
    df_smd_all <- left_join(
      df_smd_all, rownames_to_column(df_smd_3), by = "rowname"
    ) %>% rename(control_var = rowname)
    
    # descriptives
    num_cols <- ncol(data) - 4
    df_smd_summary <- data.frame(
      "treatment_setting" = treatment_setting, "adjustment" = adjustment, 
      "controls" = num_controls,
      "min" = min(min(df_smd_1$SD), min(df_smd_2$SD), min(df_smd_3$SD)),
      "max" = max(max(df_smd_1$SD), max(df_smd_2$SD), max(df_smd_3$SD)),
      "mean" = mean(mean(df_smd_1$SD), mean(df_smd_2$SD), mean(df_smd_3$SD)), 
      "median" = mean(median(df_smd_1$SD), median(df_smd_2$SD), median(df_smd_3$SD)),
      "num_cov_smd_20" = mean(sum(df_smd_1 > 20), sum(df_smd_2 > 20), sum(df_smd_3 > 20)), 
      "num_cov_smd_10" = mean(sum(df_smd_1 > 10), sum(df_smd_2 > 10), sum(df_smd_3 > 10)), 
      "num_cov_smd_5" = mean(sum(df_smd_1 > 5), sum(df_smd_2 > 5), sum(df_smd_3 > 5)), 
      "perc_cov_smd_20" = mean(sum(df_smd_1 > 20) / num_cols, sum(df_smd_2 > 20) / num_cols, sum(df_smd_3 > 20) / num_cols), 
      "perc_cov_smd_10" = mean(sum(df_smd_1 > 10) / num_cols, sum(df_smd_2 > 10) / num_cols, sum(df_smd_3 > 10) / num_cols), 
      "perc_cov_smd_5" = mean(sum(df_smd_1 > 5) / num_cols, sum(df_smd_2 > 5) / num_cols, sum(df_smd_3 > 5) / num_cols)
    )
    
  }
  
  return(list("smd_summary" = df_smd_summary, "smd_all" = df_smd_all))
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CALCULATE WEIGHTS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# normalization: input is vector and output vector with normalized values
func_weights_normalize <- function(w) {
  w <- w / sum(w) * length(w)
  return(w)
}

# calculate weights
# INPUTS:
# -> "treatment_setting": binary or multi
# -> "data_pred": data frame containing propensity score predictions.
# -> "data_controls": data frame containing the control variables.

func_weights <- function(treatment_setting, data_pred, data_controls) {
  
  ## BINARY TREATMENT SETTING ##
  if (treatment_setting == "binary") {
    # extract and prepare information
    prob_score <- as.matrix(data_pred$m)
    prob_score <- cbind(1 - prob_score, prob_score)
    y <- as.matrix(data_pred$outcome, ncol = 1) 
    t <- data_pred$treatment %>% as.character() %>% as.numeric() 
    t <- cbind(1 - t, t)
    x <- data_controls %>% mutate(intercept = 1) %>% as.matrix()
    n <- nrow(t)
    num_t <- ncol(t)
    
    # Predict y's
    w_ipw <- matrix(0, n, num_t)
    w_ols <- matrix(0, n, num_t)
    w_adj <- matrix(0, n, num_t)
    
    for (i in 1:num_t) {
      
      # IPW weights (w_t^p)
      w_ipw[,i] <- as.matrix(t[,i] / prob_score[,i], ncol = 1)
      w_ipw[,i] <- func_weights_normalize(w_ipw[,i])
      
      #  X_t'X_t 
      XtX <- crossprod(x[t[,i] == 1,])
      
      # skip iteration if error occurs
      skip_to_next <- FALSE
      tryCatch(MASS::ginv(XtX), error = function(e) { skip_to_next <<- TRUE})
      if (skip_to_next) { next }
      
      # X_t(X_t'X_t)-1 for treated
      XXtX <- x[t[,i] == 1,] %*% MASS::ginv(XtX)
      
      for (r in 1:n) {
        w_ol <- matrix(0, n, 1)
        XXtXX <- XXtX %*% x[r,]
        w_ol[t[,i] == 1,] <- unname(XXtXX[, 1])
        w_ols[,i] <- w_ols[,i] + w_ol
        w_adj[,i] <- w_adj[,i] + w_ol * w_ipw[r,i]
      }
    } # End t
  
  ## MULTIVALUED TREATMENT SETTING ##
  } else if (treatment_setting == "multi") {
    # extract and prepare information
    prob_score <- as.matrix(data_pred %>% dplyr::select(m1, m2, m3))
    y <- as.matrix(data_pred$outcome, ncol = 1) 
    t <- data_pred$treatment %>% as.character() %>% as.numeric() 
    t <- cbind(ifelse(t == 1, 1, 0), ifelse(t == 2, 1, 0), ifelse(t == 3, 1, 0))
    x <- data_controls %>% mutate(intercept = 1) %>% as.matrix()
    n <- nrow(t)
    num_t <- ncol(t)
    
    # Predict y's
    w_ipw <- matrix(0, n, num_t)
    w_ols <- matrix(0, n, num_t)
    w_adj <- matrix(0, n, num_t)
    
    for (i in 1:num_t) {
      
      # IPW weights (w_t^p)
      w_ipw[,i] <- as.matrix(t[,i] / prob_score[,i], ncol = 1)
      w_ipw[,i] <- func_weights_normalize(w_ipw[,i])
      
      #  X_t'X_t 
      XtX <- crossprod(x[t[,i] == 1,])
      
      # X_t(X_t'X_t)-1 for treated
      XXtX <- x[t[,i] == 1,] %*% MASS::ginv(XtX)
      
      for (r in 1:n) {
        w_ol <- matrix(0, n, 1)
        XXtXX <- XXtX %*% x[r,]
        w_ol[t[,i] == 1,] <- unname(XXtXX[, 1])
        w_ols[,i] <- w_ols[,i] + w_ol
        w_adj[,i] <- w_adj[,i] + w_ol * w_ipw[r,i]
      }
    } # End t
  } else {
    stop("Treatment setting must be binary or multi.")
  }
  
  ## Calculate weight matrix##
  w_mat <- w_ipw + w_ols - w_adj
  weights <- rowSums(w_mat)
  
  return(weights)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### COVARIATE BALANCING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# In this function covariate balance assessment is conducted following Knaus, 2018.
# To do so, absolute mean standardized differences are calculated before and after
# DML in both the binary and multivalued treatment setting. To run this 
# function, all three previous functions are needed.
#++
# INPUT
# -> treatment_setting: "binary" or "multi"
# -> data: data set containing the covariates
# -> controls: name of control variables selected in post-lasso
# -> weights: calculated weights

func_cov_balance <- function(treatment_setting, data, controls, weights) {
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
    
    #### Before ####
    #++++++++++++++#
    
    # drop controls not needed
    controls_drop <- data %>% 
      dplyr::select(starts_with("treatment") & !ends_with("na"), 
             starts_with("outcome") & !ends_with("na"), group) %>% 
      colnames()
    controls_drop <- controls_drop[controls_drop != "treatment_sport"]
    data_binary_bef <- data %>% dplyr::select(-all_of(controls_drop))
    
    list_asdm_bef_binary <- 
      func_asdm(treatment_setting = "binary", num_controls = "all", adjustment = "before", 
                data = data_binary_bef, treatment_var = "treatment_sport")
    
    df_smd_summary_bef_binary <- list_asdm_bef_binary$smd_summary
    df_smd_all_bef_binary <- list_asdm_bef_binary$smd_all
    
    
    #### After ####
    #+++++++++++++#
    
    # for selected covariates #
    #-------------------------#
    
    controls <- controls[controls %in% colnames(data_binary_bef)]
    data_binary_after <- data_binary_bef %>% dplyr::select(all_of(controls))
    data_binary_after <- as.data.frame(as.matrix(data_binary_after) * weights)
    data_binary_after$treatment_sport <- data_binary_bef$treatment_sport
    
    list_asdm_after_binary <- 
      func_asdm(treatment_setting = "binary", num_controls = "selected", adjustment = "after", 
                data = data_binary_after, treatment_var = "treatment_sport")
    
    df_smd_summary_after_binary <- list_asdm_after_binary$smd_summary
    df_smd_all_after_binary <- list_asdm_after_binary$smd_all
    
   
    # for all covariates #
    #--------------------#
    
    data_binary_after <- as.data.frame(as.matrix(data_binary_bef) * weights)
    data_binary_after$treatment_sport <- data_binary_bef$treatment_sport
    
    list_asdm_after_all_binary <- 
      func_asdm(treatment_setting = "binary", num_controls = "all", adjustment = "after", 
                data = data_binary_after, treatment_var = "treatment_sport")
    
    df_smd_summary_after_all_binary <- list_asdm_after_all_binary$smd_summary
    df_smd_all_after_all_binary <- list_asdm_after_all_binary$smd_all
    
    
    df_smd <- rbind(df_smd_summary_bef_binary, df_smd_summary_after_binary)
    df_smd <- rbind(df_smd, df_smd_summary_after_all_binary)
    
    df_smd_all <- left_join(
      rownames_to_column(df_smd_all_bef_binary) %>% rename("SD_before" = SD),
      rownames_to_column(df_smd_all_after_binary) %>% rename("SD_after" = SD),
      by = "rowname"
    )
    
    df_smd_all <- left_join(
      df_smd_all,
      rownames_to_column(df_smd_all_after_all_binary) %>% rename("SD_after_all" = SD),
      by = "rowname"
    ) %>% rename("control_var" = "rowname")
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    #### MULTIVALUED TREATMENT SETTING ####
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%# 
    
  } else {
    
    #### Before ####
    #++++++++++++++#
    
    # drop controls not needed
    controls_drop <- data %>% 
      dplyr::select(starts_with("treatment") & !ends_with("na"), 
             starts_with("outcome") & !ends_with("na"), group) %>% 
      colnames()
    controls_drop <- controls_drop[controls_drop != "treatment_sport_freq"]
    data_multi_bef <- data %>% dplyr::select(-all_of(controls_drop))
    
    list_asdm_bef_multi <- 
      func_asdm(treatment_setting = "multi", num_controls = "all", adjustment = "before", 
                data = data_multi_bef, treatment_var = "treatment_sport_freq")
    
    df_smd_summary_bef_multi <- list_asdm_bef_multi$smd_summary
    df_smd_all_bef_multi <- list_asdm_bef_multi$smd_all
    
    
    
    #### After ####
    #+++++++++++++#
    
    # for selected covariates #
    #-------------------------#
    
    controls <- controls[controls %in% colnames(data_multi_bef)]
    data_multi_after <- data_multi_bef %>% dplyr::select(all_of(controls))
    data_multi_after <- as.data.frame(as.matrix(data_multi_after) * weights)
    data_multi_after$treatment_sport_freq <- data_multi_bef$treatment_sport_freq
    
    list_asdm_after_multi <- 
      func_asdm(treatment_setting = "multi", num_controls = "selected", adjustment = "after", 
                data = data_multi_after, treatment_var = "treatment_sport_freq")
    
    df_smd_summary_after_multi <- list_asdm_after_multi$smd_summary
    df_smd_all_after_multi <- list_asdm_after_multi$smd_all
    
    
    # for all covariates #
    #--------------------#
    
    data_multi_after <- as.data.frame(as.matrix(data_multi_bef) * weights)
    data_multi_after$treatment_sport_freq <- data_multi_bef$treatment_sport_freq
    
    list_asdm_all_after_multi <- 
      func_asdm(treatment_setting = "multi", num_controls = "all", adjustment = "after", 
                data = data_multi_after, treatment_var = "treatment_sport_freq")
    
    df_smd_summary_all_after_multi <- list_asdm_all_after_multi$smd_summary
    df_smd_all_all_after_multi <- list_asdm_all_after_multi$smd_all
    
    
    ## Combine ##
    #-----------#
    
    # summary
    df_smd <- rbind(df_smd_summary_bef_multi, df_smd_summary_after_multi)
    df_smd <- rbind(df_smd, df_smd_summary_all_after_multi)
    
  }
  
  if (treatment_setting == "binary") {
    # return data frame
    return(list("smd_summary" = df_smd, "smd_values" = df_smd_all))
  } else {
    # return data frame
    return(list("smd_summary" = df_smd, "smd_values" = list(
      "before" = df_smd_all_bef_multi, "after_sel" = df_smd_all_after_multi,
      "after_all" = df_smd_all_all_after_multi
    )))
  }

}