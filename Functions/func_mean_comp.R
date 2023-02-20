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
      df %>% 
        select(treatment_sport, all_of(y_variables)),
      df %>% group_by(treatment_sport)  %>% summarize(num_obs = n()),
      by = "treatment_sport"
    ) 
    
    
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
        select(variable, treatment_sport, num_obs, mean, se)
    } else {
      data_se <- data_se %>%
        mutate(treatment_sport = as.character(treatment_sport),
               variable = y_variables)
    }
    
    
    # all
    data_se_all <- left_join(
      # se
      data_comp_mean_se %>%
        select(y_variables, num_obs) %>%
        mutate(across(y_variables, ~ func_se(., num_obs))) %>%
        distinct() %>%
        rename_with(~ str_c(., "_se"), everything()) %>%
        rename(num_obs = num_obs_se) %>%
        mutate(treatment_sport = "all"),
      # mean
      data_comp_mean_se %>%
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
        select(variable, treatment_sport, num_obs, mean, se)
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
      select(variable, cohort_prep, treatment_repl, treatment_def, extra_act_save, everything()) %>%
      as.data.frame() 
    
    return(df_result)
    
    
  #### MULTIVALUED TREATMENT SETTING ####
  #++++++++++++++++++++++++++++++++++++#
    
  } else if (treatment_setting == "multi") {
    df_result <- data.frame()
    return(df_result)
  } else {
    stop("treatment_setting can only take on the values binary and multi.")
  }
  
 
}