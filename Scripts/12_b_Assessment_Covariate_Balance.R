#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ASSESSMENT OF COVARIATE BALANCE AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# by Lana Kern
#++ 
# In this file, covariate balancing and the main drivers of selection are
# assessed by calculating mean standardized differences following Yang et al. (2016)
# and Knaus (2018). A detailed analysis is provided for the main model. 
# For the robustness checks only ASMDs are calculated. 
#++ 
# Sources:
# -> https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# -> https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
# -> Thoemmes and Kim (2011)
# -> Knaus (2018)
# -> Yang et al. (2016)
#++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Prepare Data ####
#++++++++++++++++++++#

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CALCULATION ABSOLUTE MEAN STANDARDIZED DIFFERENCES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
#++++++++++++++#
#### Binary ####
#++++++++++++++#

df_smd_sum_binary <- data.frame()
df_smd_all_binary <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))

# iterate over outcome variables
print("BINARY TREATMENT SETTING:")
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Grades/binary_", outcome_var_sel, "_postlasso_all_controlssameoutcome_",
                     treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  } else {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Personality/binary_", outcome_var_sel, "_postlasso_all_controlssameoutcome_", 
                     treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  }
  
  df_smd_cov_func_all_binary <- data.frame()
  for (mice_sel in 1:5) { # iterate over mice
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetition
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over fold
        
        # extract predictions and covariates
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
        df_pred_cov_func <- dml_result_all[[mice_sel]]$pred %>% filter(Repetition == rep_sel, Fold == iter_sel)
        df_controls_binary <- dml_result_all[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
        
        # treatment and covariates
        D <- df_pred_cov_func$treatment %>% as.character() %>% as.numeric() 
        
        if (str_detect(outcome_var_sel, "grade")) {
          x <- df_controls_binary %>% dplyr::select(-all_of(c("outcome_grade", "treatment_sport", "Fold", "Repetition", "group")))
        } else {
          x <- df_controls_binary %>% 
            dplyr::select(-all_of(c(paste0("outcome_bigfive_", outcome_var_sel), "treatment_sport", "Fold", "Repetition", "group")))
        }
        
        
        # calculate weights
        skip_to_next <- FALSE
        tryCatch(weights <- func_weights("binary", df_pred_cov_func, x) , error = function(e) { skip_to_next <<- TRUE})
        if(skip_to_next) {next}    
        
        # calculate ASMD before and after DML
        balance <- bal.tab(
          as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights, method = "weighting",
          s.d.denom = "pooled", 
          disp.v.ratio = TRUE, disp.ks = TRUE, 
          un = TRUE,
          continuous = "std", binary = "std" 
        )
        
        # prepare data
        df_smd_cov_func <- balance$Balance %>% 
          dplyr::select(Diff.Un, Diff.Adj) %>% 
          mutate(SD_before = abs(Diff.Un), SD_after = abs(Diff.Adj)) %>%
          dplyr::select(-c(Diff.Un, Diff.Adj)) %>%
          mutate(Fold = iter_sel, Rep = rep_sel, MICE = mice_sel) %>%
          mutate(control_var = rownames(.))
        rownames(df_smd_cov_func) <- 1:nrow(df_smd_cov_func)
        df_smd_cov_func_all_binary <- rbind(df_smd_cov_func_all_binary, df_smd_cov_func)
      } # close iteration over iter_sel
    } # close iteration over rep_sel
  } # close iteration over mice
  
  # aggregate acrosss mice, repetition and fold
  df_smd_cov_func_all_binary <- df_smd_cov_func_all_binary %>%
    dplyr::select(-c("Fold", "Rep", "MICE")) %>%
    group_by(control_var) %>%
    summarize(SD_before = max(SD_before), SD_after = max(SD_after))
  
  # summary statisticst
  df_smd_cov_func_sum_binary <- 
    data.frame(
      "treatment_setting" = rep("binary", 2),
      "adjustment" = c("before", "after"),
      "min" = c(min(df_smd_cov_func_all_binary$SD_before),  min(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
      "mean" = c(mean(df_smd_cov_func_all_binary$SD_before), mean(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
      "median" = c(median(df_smd_cov_func_all_binary$SD_before), median(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
      "max" = c(max(df_smd_cov_func_all_binary$SD_before), max(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
      "num_cov_smd_25" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.25), 
                           sum(df_smd_cov_func_all_binary$SD_after > 0.25, na.rm = T)), 
      "num_cov_smd_10" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.1), 
                           sum(df_smd_cov_func_all_binary$SD_after > 0.1, na.rm = T)), 
      "num_cov_smd_5" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.05), 
                          sum(df_smd_cov_func_all_binary$SD_after > 0.05, na.rm = T)), 
      "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.25) / length(df_smd_cov_func_all_binary$SD_before), 
                            sum(df_smd_cov_func_all_binary$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.1) / length(df_smd_cov_func_all_binary$SD_before), 
                            sum(df_smd_cov_func_all_binary$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_binary$SD_before > 0.05) / length(df_smd_cov_func_all_binary$SD_before), 
                           sum(df_smd_cov_func_all_binary$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
    )
  
  df_smd_cov_func_sum_binary <- df_smd_cov_func_sum_binary %>% mutate(outcome = outcome_var_sel)
  df_smd_cov_func_all_binary <- df_smd_cov_func_all_binary %>% mutate(outcome = outcome_var_sel)
  
  # append to overall data frame
  df_smd_sum_binary <- rbind(df_smd_sum_binary, df_smd_cov_func_sum_binary)
  df_smd_all_binary <- rbind(df_smd_all_binary, df_smd_cov_func_all_binary)
  
}

df_smd_sum_binary <- df_smd_sum_binary %>% dplyr::select(
  outcome, treatment_setting, adjustment, everything()
)

df_smd_all_binary <- df_smd_all_binary %>% dplyr::select(
  outcome, control_var, SD_before, SD_after
)

# aggregate over personality by taking the maximum
df_smd_sum_binary <- rbind(
  df_smd_sum_binary,
  df_smd_sum_binary %>% 
  filter(outcome != "grades") %>%
  mutate(outcome = "personality") %>%
  group_by(outcome, treatment_setting, adjustment) %>%
  summarize_all(max)
)


# save
saveRDS(df_smd_sum_binary, "Output/DML/Covariate_Balancing/covariate_balancing_summary_binary.rds")
saveRDS(df_smd_all_binary, "Output/DML/Covariate_Balancing/covariate_balancing_asdm_binary.rds")



#+++++++++++++#
#### Multi ####
#+++++++++++++#

df_smd_sum_multi <- data.frame()
df_smd_all_multi <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))

# iterate over outcome variables
print("MULTIVALUED TREATMENT SETTING")
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  # load data 
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all_multi <- 
      readRDS(paste0("Output/DML/Estimation/Grades/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_", treatment_def, 
                     "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  } else {
    dml_result_all_multi <- 
      readRDS(paste0("Output/DML/Estimation/Personality/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_", treatment_def, 
                     "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  }
  
  df_smd_cov_func_all_multi <- data.frame()
  df_smd_cov_func_all_detail_multi <- data.frame()
  for (mice_sel in 1:5) { # iterate over MICE data sets
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetitions
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over folds
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
        
        # predictions
        df_pred_cov_func <- dml_result_all_multi[[mice_sel]]$pred %>% 
          filter(Repetition == rep_sel, Fold == iter_sel) %>%
          mutate(D_1 = ifelse(treatment == 1, 1, 0), D_2 = ifelse(treatment == 2, 1, 0), D_3 = ifelse(treatment == 3, 1, 0))
        
        # control variables
        df_controls_multi <- dml_result_all_multi[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
        
        # control variables
        if (outcome_var_sel == "grades") {
          x <- df_controls_multi %>% dplyr::select(-all_of(
            c("outcome_grade", "treatment_sport_freq", "treatment_sport_freq_na", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
              "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
        } else {
          x <- df_controls_multi %>% dplyr::select(-all_of(
            c(paste0("outcome_bigfive_", outcome_var_sel), "treatment_sport_freq", "treatment_sport_freq_na", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
              "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
        }

        
        # calculate covariate balance
        df_smd_cov_func_all <- list()
        for (treatment_var_sel in c("D_1", "D_2", "D_3")) { # iterate over treatments
          D <- df_pred_cov_func %>% pull(treatment_var_sel) %>% as.character() %>% as.numeric() 
          m <- df_pred_cov_func %>% pull(str_replace(treatment_var_sel, "D_", "m")) %>% as.character() %>% as.numeric() 
          
          # calculate weights
          weights_multi <- func_weights("binary", df_pred_cov_func %>% mutate(treatment = D, m = m), x)
          
          # calculate ASMD
          balance <- bal.tab(
            as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights_multi, method = "weighting",
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
        
        df_smd_cov_func_all_detail_multi <- rbind(df_smd_cov_func_all_detail_multi, df_smd_cov_func_all)
        
        # aggregate and append
        df_smd_cov_func_all <- df_smd_cov_func_all %>%
          ungroup() %>%
          group_by(control_var, Rep, Fold, MICE) %>%
          # take maximum!
          summarize(SD_before = max(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                    SD_after = max(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
          ungroup()
        
        df_smd_cov_func_all_multi <- rbind(df_smd_cov_func_all_multi, df_smd_cov_func_all)
      } # close for loop over folds
    } # close iteration over repetitions
  } # close iteration over for mice data sets
  
  # summarize across controls: MICE, repetitions and folds
  df_smd_cov_func_all_multi <- df_smd_cov_func_all_multi %>%
    ungroup() %>%
    dplyr::select(-c("Rep", "MICE", "Fold")) %>%
    group_by(control_var) %>%
    summarize(SD_before = max(SD_before), SD_after = max(SD_after)) %>% 
    mutate(outcome = outcome_var_sel)
  
  df_smd_cov_func_all_detail_multi <- df_smd_cov_func_all_detail_multi %>%
    ungroup() %>%
    dplyr::select(-c("Rep", "MICE", "Fold")) %>%
    group_by(control_var) %>%
    summarize(SD_before_D_1 = max(SD_before_D_1), SD_before_D_2 = max(SD_before_D_2), 
              SD_before_D_3 = max(SD_before_D_3)) %>% 
    mutate(outcome = outcome_var_sel)
  
  # summary statistics
  df_smd_cov_func_sum_multi <- 
    data.frame(
      "treatment_setting" = rep("multi", 2),
      "adjustment" = c("before", "after"),
      "min" = c(min(df_smd_cov_func_all_multi$SD_before),  min(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
      "mean" = c(mean(df_smd_cov_func_all_multi$SD_before), mean(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
      "median" = c(median(df_smd_cov_func_all_multi$SD_before), median(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
      "max" = c(max(df_smd_cov_func_all_multi$SD_before), max(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
      "num_cov_smd_25" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.25), 
                           sum(df_smd_cov_func_all_multi$SD_after > 0.25, na.rm = T)), 
      "num_cov_smd_10" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.1), 
                           sum(df_smd_cov_func_all_multi$SD_after > 0.1, na.rm = T)), 
      "num_cov_smd_5" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.05), 
                          sum(df_smd_cov_func_all_multi$SD_after > 0.05, na.rm = T)), 
      "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.25) / length(df_smd_cov_func_all_multi$SD_before), 
                            sum(df_smd_cov_func_all_multi$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.1) / length(df_smd_cov_func_all_multi$SD_before), 
                            sum(df_smd_cov_func_all_multi$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_multi$SD_before > 0.05) / length(df_smd_cov_func_all_multi$SD_before), 
                           sum(df_smd_cov_func_all_multi$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
    )
  
  df_smd_cov_func_sum_multi <- df_smd_cov_func_sum_multi %>% mutate(outcome = outcome_var_sel)
  df_smd_cov_func_all_multi <- df_smd_cov_func_all_multi %>% mutate(outcome = outcome_var_sel)
  
  # append to overall data frame
  df_smd_sum_multi <- rbind(df_smd_sum_multi, df_smd_cov_func_sum_multi)
  df_smd_all_multi <- rbind(df_smd_all_multi, df_smd_cov_func_all_multi)
  
} # close loop over outcome_var_sel()

df_smd_sum_multi <- df_smd_sum_multi %>% dplyr::select(
  outcome, treatment_setting, adjustment, everything()
)

df_smd_all_multi <- df_smd_all_multi %>% dplyr::select(
  outcome, control_var, SD_before, SD_after
)

# aggregate over personality
df_smd_sum_multi <- rbind(
  df_smd_sum_multi,
  df_smd_sum_multi %>% 
    filter(outcome != "grades") %>%
    mutate(outcome = "personality") %>%
    group_by(outcome, treatment_setting, adjustment) %>%
    summarize_all(max)
)


# save
saveRDS(df_smd_sum_multi, "Output/DML/Covariate_Balancing/covariate_balancing_summary_multi.rds")
saveRDS(df_smd_all_multi, "Output/DML/Covariate_Balancing/covariate_balancing_asdm_multi.rds")
saveRDS(df_smd_cov_func_all_detail_multi, "Output/DML/Covariate_Balancing/covariate_balancing_asdm_multi_separate.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Covariate Balancing: Plot ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_smd_all_binary_plot <- df_smd_all_binary %>%
  mutate(outcome = case_when(outcome != "grades" ~ "personality", TRUE ~ "grades")) %>%
  group_by(outcome, control_var) %>%
  summarize(SD_before = max(SD_before), SD_after = max(SD_after)) 
  
# Binary
plot_cov_bal_binary <- list()
for (outcome_var_sel in c("grades", "personality")) {
  df_smd_plot_grades <- df_smd_all_binary_plot %>% filter(outcome == outcome_var_sel)
  plot_cov_bal_binary[[outcome_var_sel]] <- ggplot() +
    geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_before)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_grades)),
              aes(x = var_num, y = SD_before, fill = "Before DML")) +
    geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_after)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_grades)),
              aes(x = var_num, y = SD_after, fill = "After DML")) +
    scale_fill_manual(" ", values = c('Before DML' = "grey70", 'After DML' = "grey20")) +
    scale_x_continuous(breaks = c(0, 200, 400), limits = c(0, 500), expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0.2, 0.4, 0.6), limits = c(0, 0.65), expand = c(0, 0)) +
    ylab("ASMD\n") + xlab("\n ASMD Rank \n") +
    ggtitle(str_to_title(outcome_var_sel)) + 
    theme_bw() + 
    theme(legend.position = "right", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 48),
          axis.text = element_text(size = 46), axis.title = element_text(size = 46),
          legend.text = element_text(size = 46), legend.title = element_text(size = 46)) +
    guides(fill = guide_legend(title = "ASMD:")) 
}


# Multi
plot_cov_bal_multi <- list()
df_smd_all_multi_plot <- df_smd_all_multi %>% mutate(
  outcome = case_when(outcome == "grades" ~ "grades", TRUE ~ "personality")
) %>% group_by(outcome, control_var) %>% summarize_all(max)
  

for (outcome_var_sel in c("grades", "personality")) {
  df_smd_plot_multi <- df_smd_all_multi_plot %>% filter(outcome == outcome_var_sel)
  plot_cov_bal_multi[[outcome_var_sel]] <- ggplot() +
    geom_area(data = df_smd_plot_multi %>% arrange(desc(SD_before)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_multi)),
              aes(x = var_num, y = SD_before, fill = "Before DML")) +
    geom_area(data = df_smd_plot_multi %>% arrange(desc(SD_after)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_multi)),
              aes(x = var_num, y = SD_after, fill = "After DML")) + 
    scale_fill_manual(" ", values = c('Before DML' = "grey70", 'After DML' = "grey20")) +
    scale_x_continuous(breaks = c(0, 200, 400), limits = c(0, 500), expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0.2, 0.4, 0.6), limits = c(0, 0.65), expand = c(0, 0)) +
    ylab("ASMD\n") + xlab("\n ASMD Rank \n") +
    ggtitle(paste("\n", str_to_title(outcome_var_sel))) + 
    theme_bw() + 
    theme(legend.position = "right", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 48),
          axis.text = element_text(size = 46), axis.title = element_text(size = 46),
          legend.text = element_text(size = 46), legend.title = element_text(size = 46)) +
    guides(fill = guide_legend(title = "ASMD:")) 
}


# Arrange
plot_cov_bal_save <- 
  ggarrange(plot_cov_bal_binary$grades + ggtitle("GPA Binary"), 
            plot_cov_bal_multi$grades + ggtitle("GPA Multi") + rremove("ylab") +
              theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()), 
            plot_cov_bal_binary$personality + ggtitle("Personality Binary") + rremove("ylab") +
              theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
            plot_cov_bal_multi$personality + ggtitle("Personality Multi") + rremove("ylab") +
              theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
            nrow = 1, ncol = 4, common.legend = TRUE, legend = "bottom", align = "h")

# ggsave("Output/DML/Covariate_Balancing/plot_cov_balance.png", plot_cov_bal_save,
#        width = 40, height = 15, dpi = 300, units = "in", device = 'png')
pdf("Output/DML/Covariate_Balancing/plot_cov_balance.pdf",
    width = 30, height = 12, pointsize = 25, family = "Helvetica")
print(plot_cov_bal_save)
dev.off()

# Multi: detail

## WEEKLY ##
df_smd_cov_func_all_detail_multi <- df_smd_cov_func_all_detail_multi %>%
  mutate(control_var = str_sub(control_var, 1, 30))
pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_weekly.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% arrange(desc(SD_before_D_1)) %>% 
    dplyr::select(control_var, SD_before_D_1) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_1), x = SD_before_D_1)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + 
    scale_x_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme_bw() + ggtitle("\nWeekly") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 46), axis.title = element_text(size = 46), 
          plot.title = element_text(size = 48, hjust = 0.5))
)
dev.off()

pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_weekly_notext.pdf",
    width = 6, height = 10, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% 
    arrange(desc(SD_before_D_1)) %>% 
    dplyr::select(control_var, SD_before_D_1) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_1), x = SD_before_D_1)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + theme_bw() + ggtitle("\nWeekly") + 
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(size = 28), 
          axis.ticks.length.y = unit(.25, "cm"), axis.ticks.y = element_line(linewidth = 1),
          axis.title = element_text(size = 28), plot.title = element_text(size = 30, hjust = 0.5))
)
dev.off()

## MONTHLY ##
pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_monthly.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% 
    arrange(desc(SD_before_D_2)) %>% 
    dplyr::select(control_var, SD_before_D_2) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_2), x = SD_before_D_2)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + theme_bw() + ggtitle("\nMonthly") + 
    scale_x_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 46), axis.title = element_text(size = 46), 
          plot.title = element_text(size = 48, hjust = 0.5))
)
dev.off()

pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_monthly_notext.pdf",
    width = 6, height = 10, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% 
    arrange(desc(SD_before_D_2)) %>% 
    dplyr::select(control_var, SD_before_D_2) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_2), x = SD_before_D_2)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + theme_bw() + ggtitle("\nMonthly") + 
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(size = 28), 
          axis.ticks.length.y = unit(.25, "cm"), axis.ticks.y = element_line(linewidth = 1),
          axis.title = element_text(size = 28), plot.title = element_text(size = 30, hjust = 0.5))
)
dev.off()

## NEVER ##
pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_never.pdf",
    width = 20, height = 15, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% 
    arrange(desc(SD_before_D_3)) %>% 
    dplyr::select(control_var, SD_before_D_3) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_3), x = SD_before_D_3)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + theme_bw() + ggtitle("\nNever") + 
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text = element_text(size = 46), axis.title = element_text(size = 46), 
          plot.title = element_text(size = 48, hjust = 0.5))
)
dev.off()


pdf("Output/DML/Covariate_Balancing/plot_asdm_main_drivers_never_notext.pdf",
    width = 6, height = 10, pointsize = 25, family = "Helvetica")
print(
  df_smd_cov_func_all_detail_multi %>% 
    arrange(desc(SD_before_D_3)) %>% 
    dplyr::select(control_var, SD_before_D_3) %>% 
    head(10) %>%
    ggplot(aes(y = reorder(control_var, SD_before_D_3), x = SD_before_D_3)) +
    geom_bar(fill = "black", width = 0.3, stat = "identity") +
    xlab("\nASMD before DML\n") + ylab("") + theme_bw() + ggtitle("\nNever") + 
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits = c(0, 0.65), expand = c(0,0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(size = 28), 
          axis.ticks.length.y = unit(.25, "cm"), axis.ticks.y = element_line(linewidth = 1),
          axis.title = element_text(size = 28), plot.title = element_text(size = 30, hjust = 0.5))
)
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Main Drivers of selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# extract 50 variables with highest ASMD before DML
df_main_drivers <- df_smd_all_multi %>%
  filter(outcome == "grades") %>% dplyr::select(-outcome) %>%
  arrange(-SD_before) %>%
  group_by(control_var) %>% 
  summarize_all(max) %>% # TAKE MAXIMUM!
  arrange(desc(SD_before)) %>%
  head(50) 


saveRDS(df_main_drivers %>% as.data.frame(), "Output/DML/Covariate_Balancing/main_drivers.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ROBUSTNESS CHECKS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

df_covval_summary_rc <- data.frame()

# no extracurricular activity (keeping inactive students)
print("No extracurricular activity:")
postlasso_grades_noextra <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_noextra  <- func_cov_balance_summary(postlasso_grades_noextra, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "inactive")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_noextra)

# extracurricular activity within uni
print("No extracurricular activity within university:")
postlasso_grades_extrauni <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extrauni_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_extrauni  <- func_cov_balance_summary(postlasso_grades_extrauni, "grades", 5) %>% mutate(rc = "extrauni")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_extrauni)


# control variables before treatment and outcome
print("Control variables before treatment and outcome:")
postlasso_grades_controls_bef_all <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlsbefall_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_befall  <- func_cov_balance_summary(postlasso_grades_controls_bef_all, "grades", 5) %>% mutate(rc = "controls_bef_all")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_befall)

# no endogeneous variables
print("No endogeneous variables:")
postlasso_grades_noendog <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogno_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_noendog  <- func_cov_balance_summary(postlasso_grades_noendog, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "noendog")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_noendog)

# only lags
print("Only lags:")
postlasso_grades_onlylags <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_all_onlylags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_onlylags  <- func_cov_balance_summary(postlasso_grades_onlylags, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "onlylag")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_onlylags)

# polysc
print("Adding polynominals:")
postlasso_grades_polys <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_allpoly_controlssameoutcome_", treatment_def, 
                 "_down_extradrop_allpoly_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
df_covbal_summary_polys  <- func_cov_balance_summary(postlasso_grades_polys, "grades", 4) %>% 
  filter(adjustment == "after") %>% mutate(rc = "poly")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_polys)


# trimming
print("Trimming:")
postlasso_grades_trimming001 <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 "0.01", "_K4-2_Rep5_", "covbal.rds"))
df_covbal_summary_trimming001  <- func_cov_balance_summary(postlasso_grades_trimming001, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "trimming_001")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_trimming001)


postlasso_grades_trimming001minmax <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 "min-max_001", "_K4-2_Rep5_", "covbal.rds"))
df_covbal_summary_trimming001minmax  <- func_cov_balance_summary(postlasso_grades_trimming001minmax, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "trimming_001-minmax")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_trimming001minmax)

postlasso_grades_trimming01 <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 "0.1", "_K4-2_Rep5_", "covbal.rds"))
df_covbal_summary_trimming01  <- func_cov_balance_summary(postlasso_grades_trimming01, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "trimming_01")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_trimming01)


postlasso_grades_trimmingno <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 "no", "_K4-2_Rep5_", "covbal.rds"))
df_covbal_summary_trimmingno  <- func_cov_balance_summary(postlasso_grades_trimmingno, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "trimming_no")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_trimmingno)

# penalty choice
print("Penalty choice:")
df_covbal_summary_hyper1SE <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5_", "1SE", "_covbal.rds"))
df_covbal_summary_1SE  <- func_cov_balance_summary(df_covbal_summary_hyper1SE, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "hyper_1SE")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_1SE)

df_covbal_summary_hyper1SEplus <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_all_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5_", "1SE_plus", "_covbal.rds"))
df_covbal_summary_1SEplus  <- func_cov_balance_summary(df_covbal_summary_hyper1SEplus, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "hyper_1SEplus")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_1SEplus)
 
# multiclass classification
print("Multiclass classification:")
postlasso_grades_nonsep <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5_nonseparate", cov_balance_save, ".rds"))
df_covbal_summary_nonsep  <- func_cov_balance_summary(postlasso_grades_nonsep, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "multiclass")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_nonsep)


# not normalizing treatment probs
print("Not normalizing treatment probabilities:")
postlasso_grades_nonnorm <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_grades_", "postlasso", 
                 "_all_controlssameoutcome_", treatment_def, "_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming", 
                 model_trimming, "_K4-2_Rep5", cov_balance_save, "_nonorm.rds"))
df_covbal_summary_nonorm  <- func_cov_balance_summary(postlasso_grades_nonnorm, "grades", 5) %>% 
  filter(adjustment == "after") %>% mutate(rc = "nonorm")
df_covval_summary_rc <- rbind(df_covval_summary_rc, df_covbal_summary_nonorm)


saveRDS(df_covval_summary_rc %>% dplyr::select(rc, outcome, treatment_setting, adjustment, everything()), 
        "Output/DML/Covariate_Balancing/covariate_balancing_summary_rc.rds")
