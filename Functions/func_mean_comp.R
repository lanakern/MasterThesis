#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Function: Mean Comparison ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function calculates mean comparisons across treatment groups.
# INPUTS:
# -> "df": data frame containing "treatment_sport" or "treatment_freq" and
# "y_variables".
# -> "y_variables": variables for which mean comparisons are conducted
# -> "treatment_setting": "binary" or "multi". For binary treatment setting,
# the x_variable is "treatment_sport", for multi "treatment_sport_freq"
# OUTPUT
# -> Data frame containing mean, se, p-value, t-value and number of observations.
#+++

func_mean_comp <- function(df, y_variables, treatment_setting){
  
  #### BINARY TREATMENT SETTING ####
  #++++++++++++++++++++++++++++++++#
  
  if (treatment_setting == "binary") {
    # function for standard error
    func_se <- function(var, num_obs) {
      sd(var) / sqrt(num_obs)
    }
    
    # calculate mean and standard error across treatment groups
    data_comp_mean_se <- left_join(
      df %>% dplyr::select(treatment_sport, all_of(y_variables)),
      df %>% group_by(treatment_sport)  %>% summarize(num_obs = n()),
      by = "treatment_sport"
    ) 
    
    data_comp_mean_se_all <- df %>% 
      dplyr::select(all_of(y_variables)) %>% 
      mutate(num_obs = df %>% summarize(num_obs = n()) %>% pull())

    # by group
    data_se <- left_join(
      # se
      data_comp_mean_se %>%
        group_by(treatment_sport) %>%
        mutate(across(y_variables, ~ func_se(., num_obs))) %>%
        distinct() %>%
        rename_with(~ str_c(., "_se"), everything()) %>%
        rename(treatment_sport = treatment_sport_se, num_obs = num_obs_se),
      # mean
      data_comp_mean_se %>%
        group_by(treatment_sport) %>%
        summarize_at(y_variables, list(mean = mean)),
      by = "treatment_sport"
    )
    
    if (length(y_variables) > 1) {
      data_se <- data_se %>% 
        gather(-c(treatment_sport, num_obs), key = "variable", value = "value") %>%
        mutate(variable_2 = sub(".*\\_", "", variable),
               variable = sub('_[^_]*$', '', variable)) %>%
        spread(variable_2, value) %>%
        mutate(treatment_sport = as.character(treatment_sport)) %>%
        dplyr::select(variable, treatment_sport, num_obs, mean, se)
    } else {
      data_se <- data_se %>%
        mutate(treatment_sport = as.character(treatment_sport),
               variable = y_variables)
    }
    
    
    # all
    data_se_all <- left_join(
      # se
      data_comp_mean_se_all %>%
        dplyr::select(y_variables, num_obs) %>%
        mutate(across(y_variables, ~ func_se(., num_obs))) %>%
        distinct() %>%
        rename_with(~ str_c(., "_se"), everything()) %>%
        rename(num_obs = num_obs_se) %>%
        mutate(treatment_sport = "all"),
      # mean
      data_comp_mean_se_all %>%
        summarize_at(y_variables, list(mean = mean)) %>%
        mutate(treatment_sport = "all"),
      by = "treatment_sport"
    )
    
    if (length(y_variables) > 1) {
      data_se_all <- data_se_all %>% 
        gather(-c(treatment_sport, num_obs), key = "variable", value = "value") %>%
        mutate(variable_2 = sub(".*\\_", "", variable),
               variable = sub('_[^_]*$', '', variable)) %>%
        spread(variable_2, value) %>%
        mutate(treatment_sport = as.character(treatment_sport)) %>%
        dplyr::select(variable, treatment_sport, num_obs, mean, se)
    } else {
      data_se_all <- data_se_all %>%
        mutate(treatment_sport = as.character(treatment_sport),
               variable = y_variables)
    }
    
    data_mean_se <- rbind(data_se, data_se_all) %>%
      mutate(
        cohort_prep = cohort_prep, treatment_repl = treatment_repl, 
        treatment_def = treatment_def, extra_act_save = extra_act, 
        time_stamp = Sys.time()
      )
    
    # calculate p-value and t-value
    df_ttest <- data.frame()
    for (i in 1:length(y_variables)) {
      y_variable <- y_variables[i]
      x_variable <- "treatment_sport"
      exp1 <- expr(!!ensym(y_variable) ~ !!ensym(x_variable))
      p_value = t.test(formula = eval(exp1), data = df)$p.value
      t_value = unname(t.test(formula = eval(exp1), data = df)$statistic)
      df_ttest_sub <- data.frame(
        variable = y_variable, p_value = p_value, t_value = t_value
      )
      df_ttest <- rbind(df_ttest, df_ttest_sub)
    }
    
    # combine: ttest and mean, se
    df_result <- left_join(data_mean_se, df_ttest, by = "variable") %>%
      mutate(
        p_value = case_when(treatment_sport == "all" ~ as.numeric(NA), TRUE ~ p_value),
        t_value = case_when(treatment_sport == "all" ~ as.numeric(NA), TRUE ~ t_value)
        ) %>% 
      rename_with(~ str_replace(., ".*_se$", "se")) %>%
      dplyr::select(variable, cohort_prep, treatment_repl, treatment_def, extra_act_save, 
                    treatment_sport, num_obs, ends_with("mean"), ends_with("se"), 
                    ends_with("p_value"), ends_with("t_value"), time_stamp, everything()) %>%
      as.data.frame() 
    
    return(df_result)
    
    
  #### MULTIVALUED TREATMENT SETTING ####
  #++++++++++++++++++++++++++++++++++++#
    
  } else if (treatment_setting == "multi") {
    
    # function for standard error
    func_se <- function(var, num_obs) {
      sd(var) / sqrt(num_obs)
    }
    
    # calculate mean and standard error across treatment groups
    data_comp_mean_se <- left_join(
      df %>% dplyr::select(treatment_sport_freq, all_of(y_variables)),
      df %>% group_by(treatment_sport_freq)  %>% summarize(num_obs = n()),
      by = "treatment_sport_freq"
    ) 
    
    data_comp_mean_se_all <- df %>% 
      dplyr::select(all_of(y_variables)) %>% 
      mutate(num_obs = df %>% summarize(num_obs = n()) %>% pull())
    
    # by group
    data_se <- left_join(
      # se
      data_comp_mean_se %>%
        group_by(treatment_sport_freq) %>%
        mutate(across(y_variables, ~ func_se(., num_obs))) %>%
        distinct() %>%
        rename_with(~ str_c(., "_se"), everything()) %>%
        rename(treatment_sport_freq = treatment_sport_freq_se, num_obs = num_obs_se),
      # mean
      data_comp_mean_se %>%
        group_by(treatment_sport_freq) %>%
        summarize_at(y_variables, list(mean = mean)),
      by = "treatment_sport_freq"
    )
    
    if (length(y_variables) > 1) {
      data_se <- data_se %>% 
        gather(-c(treatment_sport_freq, num_obs), key = "variable", value = "value") %>%
        mutate(variable_2 = sub(".*\\_", "", variable),
               variable = sub('_[^_]*$', '', variable)) %>%
        spread(variable_2, value) %>%
        mutate(treatment_sport_freq = as.character(treatment_sport_freq)) %>%
        dplyr::select(variable, treatment_sport_freq, num_obs, mean, se)
    } else {
      data_se <- data_se %>%
        mutate(treatment_sport_freq = as.character(treatment_sport_freq),
               variable = y_variables)
    }
    
    
    # all
    data_se_all <- left_join(
      # se
      data_comp_mean_se_all %>%
        dplyr::select(y_variables, num_obs) %>%
        mutate(across(y_variables, ~ func_se(., num_obs))) %>%
        distinct() %>%
        rename_with(~ str_c(., "_se"), everything()) %>%
        rename(num_obs = num_obs_se) %>%
        mutate(treatment_sport_freq = "all"),
      # mean
      data_comp_mean_se_all %>%
        summarize_at(y_variables, list(mean = mean)) %>%
        mutate(treatment_sport_freq = "all"),
      by = "treatment_sport_freq"
    )
    
    if (length(y_variables) > 1) {
      data_se_all <- data_se_all %>% 
        gather(-c(treatment_sport_freq, num_obs), key = "variable", value = "value") %>%
        mutate(variable_2 = sub(".*\\_", "", variable),
               variable = sub('_[^_]*$', '', variable)) %>%
        spread(variable_2, value) %>%
        mutate(treatment_sport_freq = as.character(treatment_sport_freq)) %>%
        dplyr::select(variable, treatment_sport_freq, num_obs, mean, se)
    } else {
      data_se_all <- data_se_all %>%
        mutate(treatment_sport_freq = as.character(treatment_sport_freq),
               variable = y_variables)
    }
    
    data_mean_se <- rbind(data_se, data_se_all) %>%
      mutate(
        cohort_prep = cohort_prep, treatment_repl = treatment_repl, 
        treatment_def = treatment_def, extra_act_save = extra_act, 
        time_stamp = Sys.time()
      )
    
    # calculate p-value and t-value
    df_ttest <- data.frame()
    
    for (i in 1:length(y_variables)) {
      
      # extract y-variable
      y_variable <- y_variables[i]
      x_variable <- "treatment_sport_freq"
      
      # generate formula
      exp1 <- expr(!!ensym(y_variable) ~ !!ensym(x_variable))
      
      df_multi_ttest_1 <- df %>% filter(treatment_sport_freq != "never")
      multi_ttest_1 <- t.test(formula = eval(exp1), data = df_multi_ttest_1)
      
      df_multi_ttest_2 <- df %>% filter(treatment_sport_freq != "weekly_atleast")
      multi_ttest_2 <- t.test(formula = eval(exp1), data = df_multi_ttest_2)
      
      df_multi_ttest_3 <- df %>% filter(treatment_sport_freq != "monthly_less")
      multi_ttest_3 <- t.test(formula = eval(exp1), data = df_multi_ttest_3)
      
      df_ttest_sub <- data.frame(
        variable = rep(y_variable, 4),
        treatment_sport_freq = c("weekly_atleast", "monthly_less", "never", "all"),
        t_value_daily = c(NA, unname(multi_ttest_1$statistic), unname(multi_ttest_3$statistic), NA),
        t_value_monthly = c(unname(multi_ttest_1$statistic), NA, unname(multi_ttest_2$statistic), NA),
        p_value_daily = c(NA, multi_ttest_1$p.value, multi_ttest_3$p.value, NA),
        p_value_monthly = c(multi_ttest_1$p.value, NA, multi_ttest_2$p.value, NA)
      ) 
    
      df_ttest <- rbind(df_ttest, df_ttest_sub)
    }
    
    # combine: ttest and mean, se
    df_result <- left_join(data_mean_se, df_ttest, by = c("treatment_sport_freq", "variable")) %>%
      rename_with(~ str_replace(., ".*_se$", "se")) %>%
      dplyr::select(variable, cohort_prep, treatment_repl, treatment_def, extra_act_save, 
                    treatment_sport_freq, num_obs, ends_with("mean"), ends_with("se"), 
                    ends_with("p_value"), ends_with("t_value"), time_stamp, everything()) %>%
      as.data.frame() 
    
    return(df_result)
    
  } else {
    stop("treatment_setting can only take on the values binary and multi.")
  }
  
}