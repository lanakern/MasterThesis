#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: COVARIATE BALANCING AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_cov_balance <- function(treatment_setting, data, controls, weights) {
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### BINARY TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
   
    #### Before ####
    #++++++++++++++#
    
    # drop controls not needed
    controls_drop <- data %>% 
      select(starts_with("treatment"), starts_with("outcome"), group) %>% 
      colnames()
    controls_drop <- controls_drop[controls_drop != "treatment_sport"]
    data_binary_bef <- data %>% select(-all_of(controls_drop))

    # calculate absolute difference in means
    df_diff_binary_bef <- abs(diff(as.matrix(
      data_binary_bef %>% 
        group_by(treatment_sport) %>%
        summarise(across(everything(), mean)) %>%
        select(-treatment_sport)
    ))) %>% 
      as.matrix()
    
    
    # calculate factor
    df_meanvar_binary_bef <- data_binary_bef %>% 
      group_by(treatment_sport) %>%
      summarise(across(everything(), var)) %>%
      select(-treatment_sport) %>%
      summarise(across(everything(), mean)) %>% 
      summarise(across(everything(), sqrt)) %>%
      as.matrix()
    
    
    # fraction
    df_smd_bef_binary <- data.frame("SD" = t((df_diff_binary_bef / df_meanvar_binary_bef) * 100))
    
    # descriptives
    num_cols <- ncol(data_binary_bef) - 1
    df_smd_binary_bef <- data.frame(
      "treatment_setting" = "binary", "adjustment" = "before", 
      "controls" = "all",
      "min" = min(df_smd_bef_binary$SD), "max" = max(df_smd_bef_binary$SD), 
      "mean" = mean(df_smd_bef_binary$SD), "median" = median(df_smd_bef_binary$SD),
      "num_cov_smd_20" = sum(df_smd_bef_binary > 20), 
      "num_cov_smd_10" = sum(df_smd_bef_binary > 10), 
      "num_cov_smd_5" = sum(df_smd_bef_binary > 5),
      "perc_cov_smd_20" = sum(df_smd_bef_binary > 20) / num_cols, 
      "perc_cov_smd_10" = sum(df_smd_bef_binary > 10) / num_cols, 
      "perc_cov_smd_5" = sum(df_smd_bef_binary > 5) / num_cols
    )
    
    
    #### After ####
    #+++++++++++++#
    
    # for selected covariates #
    #-------------------------#
    
    controls <- controls[controls %in% colnames(data_binary_bef)]
    data_binary_after <- data_binary_bef %>% select(all_of(controls))
    data_binary_after <- as.data.frame(as.matrix(data_binary_after) * weights)
    data_binary_after$treatment_sport <- data_binary_bef$treatment_sport
    
    # calculate absolute difference in means
    df_diff_binary_after <- abs(diff(as.matrix(
      data_binary_after %>% 
        group_by(treatment_sport) %>%
        summarise(across(everything(), mean)) %>%
        select(-treatment_sport)
    ))) %>% 
      as.matrix()
    
    
    # calculate factor: same vector as for before
    df_meanvar_binary_after <- df_meanvar_binary_bef[, controls]
    # df_meanvar_binary_after <- data_binary_after %>%
    #   group_by(treatment_sport) %>%
    #   summarise(across(everything(), var)) %>%
    #   select(-treatment_sport) %>%
    #   summarise(across(everything(), mean)) %>%
    #   summarise(across(everything(), sqrt)) %>%
    #   as.matrix()
    
    
    # fraction
    df_smd_after_binary <- data.frame(
      "SD" = t((df_diff_binary_after / df_meanvar_binary_after) * 100)
      )
    
    # descriptives
    num_cols <- ncol(data_binary_after) - 1
    df_smd_binary_after <- data.frame(
      "treatment_setting" = "binary", "adjustment" = "after", 
      "controls" = "selected",
      "min" = min(df_smd_after_binary$SD), 
      "max" = max(df_smd_after_binary$SD), 
      "mean" = mean(df_smd_after_binary$SD), 
      "median" = median(df_smd_after_binary$SD),
      "num_cov_smd_20" = sum(df_smd_after_binary > 20), 
      "num_cov_smd_10" = sum(df_smd_after_binary > 10), 
      "num_cov_smd_5" = sum(df_smd_after_binary > 5),
      "perc_cov_smd_20" = sum(df_smd_after_binary > 20) / num_cols, 
      "perc_cov_smd_10" = sum(df_smd_after_binary > 10) / num_cols, 
      "perc_cov_smd_5" = sum(df_smd_after_binary > 5) / num_cols
    )
    
    df_smd <- rbind(df_smd_binary_bef, df_smd_binary_after)
    
    
    # for all covariates #
    #--------------------#
    
    data_binary_after <- as.data.frame(as.matrix(data_binary_bef) * weights)
    data_binary_after$treatment_sport <- data_binary_bef$treatment_sport
    
    # calculate absolute difference in means
    df_diff_binary_after <- abs(diff(as.matrix(
      data_binary_after %>% 
        group_by(treatment_sport) %>%
        summarise(across(everything(), mean)) %>%
        select(-treatment_sport)
    ))) %>% 
      as.matrix()
    
    
    # calculate factor: same vector as for before
    df_meanvar_binary_after <- df_meanvar_binary_bef[, controls]
    # df_meanvar_binary_after <- data_binary_after %>%
    #   group_by(treatment_sport) %>%
    #   summarise(across(everything(), var)) %>%
    #   select(-treatment_sport) %>%
    #   summarise(across(everything(), mean)) %>%
    #   summarise(across(everything(), sqrt)) %>%
    #   as.matrix()
    
    
    # fraction
    df_smd_after_binary <- data.frame(
      "SD" = t((df_diff_binary_after / df_meanvar_binary_after) * 100)
    )
    
    # descriptives
    num_cols <- ncol(data_binary_after) - 1
    df_smd_binary_after <- data.frame(
      "treatment_setting" = "binary", "adjustment" = "after", 
      "controls" = "all",
      "min" = min(df_smd_after_binary$SD), 
      "max" = max(df_smd_after_binary$SD), 
      "mean" = mean(df_smd_after_binary$SD), 
      "median" = median(df_smd_after_binary$SD),
      "num_cov_smd_20" = sum(df_smd_after_binary > 20), 
      "num_cov_smd_10" = sum(df_smd_after_binary > 10), 
      "num_cov_smd_5" = sum(df_smd_after_binary > 5),
      "perc_cov_smd_20" = sum(df_smd_after_binary > 20) / num_cols, 
      "perc_cov_smd_10" = sum(df_smd_after_binary > 10) / num_cols, 
      "perc_cov_smd_5" = sum(df_smd_after_binary > 5) / num_cols
    )
    
    
    df_smd <- rbind(df_smd, df_smd_binary_after)
    df_smd_all <- left_join(
      rownames_to_column(df_smd_bef_binary) %>% rename("SD_before" = SD),
      rownames_to_column(df_smd_after_binary) %>% rename("SD_after" = SD),
      by = "rowname"
    ) %>% rename("control_var" = "rowname")
    
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### MULTIVALUED TREATMENT SETTING ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%# 
    
  } else {
    
  }
  

  # return data frame
  return(list("smd_summary" = df_smd, "smd_values" = df_smd_all))

}