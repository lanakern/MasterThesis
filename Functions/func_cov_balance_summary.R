#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: COVARIATE BALANCE SUMMARY STATISTICS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function calculates summary statistics for the ASMDs in the multivalued
# treatment setting. The results can be used to asses covariata balance.
# This function is only applied for the robustness checks (thus, no binary 
# treatment setting is considered).
#+++
# Inputs:
# -> "list_estimation": estimation results in list format, as contained in the folder: DML/Estimation
# -> "outcome": outcome variable (grades or bigfive_*)
# -> "mice_num": number of mice iterations
# Outcome: data frame containing summary statistics (min, mean, median, max and percentages)
#+++

func_cov_balance_summary <- function(list_estimation, outcome, mice_num) {
  
  df_smd_sum_multi_rc <- data.frame()
  df_smd_all_multi_rc <- data.frame()
  df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
  if (str_detect(str_to_lower(outcome), "grade") | str_detect(str_to_lower(outcome), "gpa")) {
    outcome <- "grades"
  } else {
    outcome <- outcome
  }

  df_smd_cov_func_all_multi_rc <- data.frame()
  df_smd_cov_func_all_detail_multi_rc <- data.frame()
  for (mice_sel in 1:mice_num) { # iterate over MICE data sets
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetitions
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over folds
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
          
        # predictions
        df_pred_cov_func <- list_estimation[[mice_sel]]$pred %>% 
          filter(Repetition == rep_sel, Fold == iter_sel) %>%
          mutate(D_1 = ifelse(treatment == 1, 1, 0), D_2 = ifelse(treatment == 2, 1, 0), D_3 = ifelse(treatment == 3, 1, 0))
          
        # control variables
        df_controls_multi_rc <- list_estimation[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
          
        # control variables
        if (outcome == "grades") {
          x <- df_controls_multi_rc %>% dplyr::select(-all_of(
            c("outcome_grade", "treatment_sport_freq", "treatment_sport_freq_na", 
              "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
              "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
          } else {
            x <- df_controls_multi_rc %>% dplyr::select(-all_of(
              c(paste0("outcome_bigfive_", outcome), "treatment_sport_freq", "treatment_sport_freq_na", 
                "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
                "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
          }
          
        # calculate covariate balance
        df_smd_cov_func_all <- list()
        for (treatment_var_sel in c("D_1", "D_2", "D_3")) { # iterate over treatments
            D <- df_pred_cov_func %>% pull(treatment_var_sel) %>% as.character() %>% as.numeric() 
            m <- df_pred_cov_func %>% pull(str_replace(treatment_var_sel, "D_", "m")) %>% as.character() %>% as.numeric() 
            
            # calculate weights
            weights_multi_rc <- func_weights("binary", df_pred_cov_func %>% mutate(treatment = D, m = m), x)
            
            # calculate ASMD
            balance <- bal.tab(
              as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights_multi_rc, method = "weighting",
              s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
              disp.v.ratio = TRUE, disp.ks = TRUE, 
              un = TRUE, # display statistics also for before DML
              continuous = "std", binary = "std" # also standardized multi covariates
            )
            
            # prepare data frame
            df_smd_cov_func <- balance$Balance %>% 
              dplyr::select(Diff.Un, Diff.Adj) %>% 
              mutate(SD_before = abs(Diff.Un), SD_after = abs(Diff.Adj)) %>%
              dplyr::select(-c(Diff.Un, Diff.Adj)) %>%
              mutate(Rep = rep_sel, Fold = iter_sel, MICE = mice_sel) %>%
              mutate(control_var = rownames(.)) %>%
              rename(!!rlang::sym(paste0("SD_before_", treatment_var_sel)) := SD_before,
                     !!rlang::sym(paste0("SD_after_", treatment_var_sel)) := SD_after)
            rownames(df_smd_cov_func) <- 1:nrow(df_smd_cov_func)
            
            # save data frame in list
            df_smd_cov_func_all[[treatment_var_sel]] <- df_smd_cov_func
          } # close iteration over treatment variables
          
          # append ASMD across treatments in one data frame
          df_smd_cov_func_all <- left_join(
            df_smd_cov_func_all[["D_1"]], df_smd_cov_func_all[["D_2"]], by = c("control_var", "Rep", "Fold", "MICE")
          ) %>%
            left_join(df_smd_cov_func_all[["D_3"]], by = c("control_var", "Rep",  "Fold", "MICE"))
          
          df_smd_cov_func_all_detail_multi_rc <- rbind(df_smd_cov_func_all_detail_multi_rc, df_smd_cov_func_all)
          
          # aggregate and append
          df_smd_cov_func_all <- df_smd_cov_func_all %>%
            ungroup() %>%
            group_by(control_var, Rep, Fold, MICE) %>%
            # take maximum!
            summarize(SD_before = max(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                      SD_after = max(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
            ungroup()
          
          df_smd_cov_func_all_multi_rc <- rbind(df_smd_cov_func_all_multi_rc, df_smd_cov_func_all)
        } # close for loop over folds
      } # close iteration over repetitions
    } # close iteration over for mice data sets
    
    # summarize across controls: MICE, repetitions and folds
    df_smd_cov_func_all_multi_rc <- df_smd_cov_func_all_multi_rc %>%
      ungroup() %>%
      dplyr::select(-c("Rep", "MICE", "Fold")) %>%
      group_by(control_var) %>%
      summarize(SD_before = mean(SD_before), SD_after = mean(SD_after)) %>% 
      mutate(outcome = outcome)
    
    df_smd_cov_func_all_detail_multi_rc <- df_smd_cov_func_all_detail_multi_rc %>%
      ungroup() %>%
      dplyr::select(-c("Rep", "MICE", "Fold")) %>%
      group_by(control_var) %>%
      summarize(SD_before_D_1 = mean(SD_before_D_1), SD_before_D_2 = mean(SD_before_D_2), 
                SD_before_D_3 = mean(SD_before_D_3)) %>% 
      mutate(outcome = outcome)
    
    # summary statistics
    df_smd_cov_func_sum_multi_rc <- 
      data.frame(
        "treatment_setting" = rep("multi", 2),
        "adjustment" = c("before", "after"),
        "min" = c(min(df_smd_cov_func_all_multi_rc$SD_before),  min(df_smd_cov_func_all_multi_rc$SD_after, na.rm = T)),
        "mean" = c(mean(df_smd_cov_func_all_multi_rc$SD_before), mean(df_smd_cov_func_all_multi_rc$SD_after, na.rm = T)),
        "median" = c(median(df_smd_cov_func_all_multi_rc$SD_before), median(df_smd_cov_func_all_multi_rc$SD_after, na.rm = T)),
        "max" = c(max(df_smd_cov_func_all_multi_rc$SD_before), max(df_smd_cov_func_all_multi_rc$SD_after, na.rm = T)),
        "num_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.25), 
                             sum(df_smd_cov_func_all_multi_rc$SD_after > 0.25, na.rm = T)), 
        "num_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.1), 
                             sum(df_smd_cov_func_all_multi_rc$SD_after > 0.1, na.rm = T)), 
        "num_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.05), 
                            sum(df_smd_cov_func_all_multi_rc$SD_after > 0.05, na.rm = T)), 
        "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.25) / length(df_smd_cov_func_all_multi_rc$SD_before), 
                              sum(df_smd_cov_func_all_multi_rc$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_multi_rc %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
        "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.1) / length(df_smd_cov_func_all_multi_rc$SD_before), 
                              sum(df_smd_cov_func_all_multi_rc$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_multi_rc %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
        "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_rc$SD_before > 0.05) / length(df_smd_cov_func_all_multi_rc$SD_before), 
                             sum(df_smd_cov_func_all_multi_rc$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_multi_rc %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
      )
    
    df_smd_cov_func_sum_multi_rc <- df_smd_cov_func_sum_multi_rc %>% mutate(outcome = outcome)
    df_smd_cov_func_all_multi_rc <- df_smd_cov_func_all_multi_rc %>% mutate(outcome = outcome)
    
    # append to overall data frame
    df_smd_sum_multi_rc <- rbind(df_smd_sum_multi_rc, df_smd_cov_func_sum_multi_rc)
    df_smd_all_multi_rc <- rbind(df_smd_all_multi_rc, df_smd_cov_func_all_multi_rc)
    
  df_smd_sum_multi_rc <- df_smd_sum_multi_rc %>% dplyr::select(
    outcome, treatment_setting, adjustment, everything()
  )
  
  df_smd_all_multi_rc <- df_smd_all_multi_rc %>% dplyr::select(
    outcome, control_var, SD_before, SD_after
  )
  
  # aggregate over personality
  df_smd_sum_multi_rc <- rbind(
    df_smd_sum_multi_rc,
    df_smd_sum_multi_rc %>% 
      filter(outcome != "grades") %>%
      mutate(outcome = "personality") %>%
      group_by(outcome, treatment_setting, adjustment) %>%
      summarize_all(max)
  )
  
  return(df_smd_sum_multi_rc)
}