#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ASSESSMENT OF COVARIATE BALANCE AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# by Lana Kern
#++ 
# In this file, covariate balancing and the main drivers of selection are
# assessed by calculating mean standardized differences following Yang et al. (2016)
# and Knaus (2018).
# This analysis is only done in the main model, but for both the binary and multivalued
# treatment setting. 
#++ 
# Sources:
# -> https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# -> https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
# -> Thoemmes and Kim (2011)
# -> Knaus (2018)
#++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Prepare Data ####
#++++++++++++++++++++#

if (cov_balance == "yes") {
  cov_balance_save <- "_covbal"
} else {
  cov_balance_save <- ""
}

# load all MICE data sets and append them
# data_all_mice_grades <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", 
#                       cov_balance_save, "_mice", mice_data_sel, ".rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
#   data_all_mice_grades <- rbind(data_all_mice_grades, data_all_mice_sub)
# }
# 
# data_all_mice_personality <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", 
#                       cov_balance_save, "_mice", mice_data_sel, "_personality.rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
#   data_all_mice_personality <- rbind(data_all_mice_personality, data_all_mice_sub)
# }
# 
# data_all_mice_grades_multi <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop", 
#                       cov_balance_save, "_mice", mice_data_sel, ".rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
#   data_all_mice_grades_multi <- rbind(data_all_mice_grades_multi, data_all_mice_sub)
# }
# 
# data_all_mice_personality_multi <- data.frame()
# for (mice_data_sel in 1:5) {
#   data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop", 
#                       cov_balance_save, "_mice", mice_data_sel, "_personality.rds")
#   data_all_mice_sub <- readRDS(data_load)
#   data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
#   data_all_mice_personality_multi <- rbind(data_all_mice_personality_multi, data_all_mice_sub)
# }


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
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Grades/binary_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  } else {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Personality/binary_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
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
    summarize(SD_before = mean(SD_before), SD_after = mean(SD_after))
  
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

# aggregate over personality
df_smd_sum_binary <- rbind(
  df_smd_sum_binary,
  df_smd_sum_binary %>% 
  filter(outcome != "grades") %>%
  mutate(outcome = "personality") %>%
  group_by(outcome, treatment_setting, adjustment) %>%
  summarize_all(mean)
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
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  # load data 
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all_multi <- 
      readRDS(paste0("Output/DML/Estimation/Grades/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  } else {
    dml_result_all_multi <- 
      readRDS(paste0("Output/DML/Estimation/Personality/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
  }
  
  df_smd_cov_func_all_multi <- data.frame()
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
        
        # aggregate and append
        df_smd_cov_func_all <- df_smd_cov_func_all %>%
          ungroup() %>%
          group_by(control_var, Rep, Fold, MICE) %>%
          summarize(SD_before = mean(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                    SD_after = mean(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
          ungroup()
        
        df_smd_cov_func_all_multi <- rbind(df_smd_cov_func_all_multi, df_smd_cov_func_all)
      } # close for loop over iterations
    } # close iteration over repetitions
  } # close iteration over for mice data sets
  
  # summarize across controls: MICE, repetitions and folds
  df_smd_cov_func_all_multi <- df_smd_cov_func_all_multi %>%
    ungroup() %>%
    dplyr::select(-c("Rep", "MICE", "Fold")) %>%
    group_by(control_var) %>%
    summarize(SD_before = mean(SD_before), SD_after = mean(SD_after)) %>% 
    mutate(outcome = "GPA")
  
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
    summarize_all(mean)
)


# save
saveRDS(df_smd_sum_multi, "Output/DML/Covariate_Balancing/covariate_balancing_summary_multi.rds")
saveRDS(df_smd_all_multi, "Output/DML/Covariate_Balancing/covariate_balancing_asdm_multi.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Covariate Balancing: Plot ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_smd_all_binary_plot <- df_smd_all_binary %>%
  mutate(outcome = case_when(outcome != "grades" ~ "personality", TRUE ~ "grades")) %>%
  group_by(outcome, control_var) %>%
  summarize(SD_before = mean(SD_before), SD_after = mean(SD_after)) 
  
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
    ylab("ASMD\n") + xlab("\nRank from highest to lowest ASMD\n") +
    ggtitle(str_to_title(outcome_var_sel)) + 
    theme_bw() + 
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0.5, size = 45),
          axis.text = element_text(size = 32), axis.title = element_text(size = 38),
          legend.text = element_text(size = 40), legend.title = element_text(size = 40)) +
    guides(fill = guide_legend(title = "ASMD:")) 
}


# plot_cov_bal_binary_save <- ggarrange(
#   plot_cov_bal_binary[["grades"]] + ggtitle("GPA"),
#   plot_cov_bal_binary[["personality"]] + ylab(""),
#   nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom"
# )
# 
# ggsave("Output/DML/Covariate_Balancing/plot_cov_balance_binary.png", plot_cov_bal_binary_save,
#        width = 20, height = 15, dpi = 300, units = "in", device = 'png')


# Multi
plot_cov_bal_multi <- list()
df_smd_all_multi_plot <- df_smd_all_multi %>% mutate(
  outcome = case_when(outcome == "grades" ~ "grades", TRUE ~ "personality")
) %>% group_by(outcome, control_var) %>% summarize_all(mean)
  

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
    ylab("ASMD\n") + xlab("\n Rank from highest to lowest ASMD \n") +
    ggtitle(paste("\n", str_to_title(outcome_var_sel))) + 
    theme_bw() + 
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0.5, size = 45),
          axis.text = element_text(size = 32), axis.title = element_text(size = 38),
          legend.text = element_text(size = 40), legend.title = element_text(size = 40)) +
    guides(fill = guide_legend(title = "ASMD:")) 
}


# Arrange
plot_cov_bal_save <- 
  ggarrange(plot_cov_bal_binary$grades + ggtitle("GPA Binary") + ylim(0, 0.5), 
            plot_cov_bal_binary$personality + ggtitle("GPA Multi") + rremove("ylab") + ylim(0, 0.5), 
            plot_cov_bal_multi$grades + ggtitle("Personality Binary") + rremove("ylab") + ylim(0, 0.5), 
            plot_cov_bal_multi$personality + ggtitle("Personality Multi") + rremove("ylab") + ylim(0, 0.5),
            nrow = 1, ncol = 4, common.legend = TRUE, legend = "bottom", align = "h")

ggsave("Output/DML/Covariate_Balancing/plot_cov_balance.png", plot_cov_bal_save,
       width = 40, height = 15, dpi = 300, units = "in", device = 'png')



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Main Drivers of selection ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# extract 50 variables with highest ASMD before DML
df_main_drivers <- df_smd_all_binary %>%
  arrange(-SD_before) %>%
  group_by(control_var) %>% 
  summarize_all(mean) %>% 
  arrange(desc(SD_before)) %>%
  head(50)


saveRDS(df_main_drivers %>% as.data.frame(), "Output/DML/Covariate_Balancing/main_drivers.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: NO COVARIATE BALANCE SAMPLE ADJUSTMENT ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## BINARY ##
df_smd_sum_binary_check <- data.frame()
df_smd_all_binary_check <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))

# iterate over outcome variables
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Grades/binary_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", ".rds"))
  } else {
    dml_result_all <- 
      readRDS(paste0("Output/DML/Estimation/Personality/binary_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", ".rds"))
  }
  
  df_smd_cov_func_all_binary_check <- data.frame()
  for (mice_sel in 1:5) { # iterate over mice
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetition
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over fold
        
        # extract predictions and covariates
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
        df_pred_cov_func <- dml_result_all[[mice_sel]]$pred %>% filter(Repetition == rep_sel, Fold == iter_sel)
        df_controls_binary_check <- dml_result_all[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
        
        # treatment and covariates
        D <- df_pred_cov_func$treatment %>% as.character() %>% as.numeric() 
        
        if (str_detect(outcome_var_sel, "grade")) {
          x <- df_controls_binary_check %>% dplyr::select(-all_of(c("outcome_grade", "treatment_sport", "Fold", "Repetition", "group")))
        } else {
          x <- df_controls_binary_check %>% 
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
        df_smd_cov_func_all_binary_check <- rbind(df_smd_cov_func_all_binary_check, df_smd_cov_func)
      } # close iteration over iter_sel
    } # close iteration over rep_sel
  } # close iteration over mice
  
  # aggregate acrosss mice, repetition and fold
  df_smd_cov_func_all_binary_check <- df_smd_cov_func_all_binary_check %>%
    dplyr::select(-c("Fold", "Rep", "MICE")) %>%
    group_by(control_var) %>%
    summarize(SD_before = mean(SD_before), SD_after = mean(SD_after))
  
  # summary statisticst
  df_smd_cov_func_sum_binary_check <- 
    data.frame(
      "treatment_setting" = rep("binary", 2),
      "adjustment" = c("before", "after"),
      "min" = c(min(df_smd_cov_func_all_binary_check$SD_before),  min(df_smd_cov_func_all_binary_check$SD_after, na.rm = T)),
      "mean" = c(mean(df_smd_cov_func_all_binary_check$SD_before), mean(df_smd_cov_func_all_binary_check$SD_after, na.rm = T)),
      "median" = c(median(df_smd_cov_func_all_binary_check$SD_before), median(df_smd_cov_func_all_binary_check$SD_after, na.rm = T)),
      "max" = c(max(df_smd_cov_func_all_binary_check$SD_before), max(df_smd_cov_func_all_binary_check$SD_after, na.rm = T)),
      "num_cov_smd_25" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.25), 
                           sum(df_smd_cov_func_all_binary_check$SD_after > 0.25, na.rm = T)), 
      "num_cov_smd_10" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.1), 
                           sum(df_smd_cov_func_all_binary_check$SD_after > 0.1, na.rm = T)), 
      "num_cov_smd_5" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.05), 
                          sum(df_smd_cov_func_all_binary_check$SD_after > 0.05, na.rm = T)), 
      "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.25) / length(df_smd_cov_func_all_binary_check$SD_before), 
                            sum(df_smd_cov_func_all_binary_check$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_binary_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.1) / length(df_smd_cov_func_all_binary_check$SD_before), 
                            sum(df_smd_cov_func_all_binary_check$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_binary_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_binary_check$SD_before > 0.05) / length(df_smd_cov_func_all_binary_check$SD_before), 
                           sum(df_smd_cov_func_all_binary_check$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_binary_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
    )
  
  df_smd_cov_func_sum_binary_check <- df_smd_cov_func_sum_binary_check %>% mutate(outcome = outcome_var_sel)
  df_smd_cov_func_all_binary_check <- df_smd_cov_func_all_binary_check %>% mutate(outcome = outcome_var_sel)
  
  # append to overall data frame
  df_smd_sum_binary_check <- rbind(df_smd_sum_binary_check, df_smd_cov_func_sum_binary_check)
  df_smd_all_binary_check <- rbind(df_smd_all_binary_check, df_smd_cov_func_all_binary_check)
  
}

df_smd_sum_binary_check <- df_smd_sum_binary_check %>% dplyr::select(
  outcome, treatment_setting, adjustment, everything()
)

df_smd_all_binary_check <- df_smd_all_binary_check %>% dplyr::select(
  outcome, control_var, SD_before, SD_after
)

# aggregate over personality
df_smd_sum_binary_check <- rbind(
  df_smd_sum_binary_check,
  df_smd_sum_binary_check %>% 
    filter(outcome != "grades") %>%
    mutate(outcome = "personality") %>%
    group_by(outcome, treatment_setting, adjustment) %>%
    summarize_all(mean)
)



## MULTI ## 
df_smd_sum_multi_check <- data.frame()
df_smd_all_multi_check <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))

# iterate over outcome variables
for (outcome_var_sel in c("grades", "agreeableness", "extraversion", "conscientiousness", "neuroticism", "openness")) {
  
  print(paste("Start Outcome:", str_to_title(outcome_var_sel)))
  
  # load data 
  if (str_detect(outcome_var_sel, "grade")) {
    dml_result_all_multi_check <- 
      readRDS(paste0("Output/DML/Estimation/Grades/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", ".rds"))
  } else {
    dml_result_all_multi_check <- 
      readRDS(paste0("Output/DML/Estimation/Personality/multi_", outcome_var_sel, 
                     "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                     model_trimming, "_K4-2_Rep5", ".rds"))
  }
  
  df_smd_cov_func_all_multi_check <- data.frame()
  for (mice_sel in 1:5) { # iterate over MICE data sets
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetitions
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over folds
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
        
        # predictions
        df_pred_cov_func <- dml_result_all_multi_check[[mice_sel]]$pred %>% 
          filter(Repetition == rep_sel, Fold == iter_sel) %>%
          mutate(D_1 = ifelse(treatment == 1, 1, 0), D_2 = ifelse(treatment == 2, 1, 0), D_3 = ifelse(treatment == 3, 1, 0))
        
        # control variables
        df_controls_multi_check <- dml_result_all_multi_check[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
        
        # control variables
        if (outcome_var_sel == "grades") {
          x <- df_controls_multi_check %>% dplyr::select(-all_of(
            c("outcome_grade", "treatment_sport_freq", "treatment_sport_freq_na", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
              "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
        } else {
          x <- df_controls_multi_check %>% dplyr::select(-all_of(
            c(paste0("outcome_bigfive_", outcome_var_sel), "treatment_sport_freq", "treatment_sport_freq_na", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
              "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
        }
        
        
        # calculate covariate balance
        df_smd_cov_func_all <- list()
        for (treatment_var_sel in c("D_1", "D_2", "D_3")) { # iterate over treatments
          D <- df_pred_cov_func %>% pull(treatment_var_sel) %>% as.character() %>% as.numeric() 
          m <- df_pred_cov_func %>% pull(str_replace(treatment_var_sel, "D_", "m")) %>% as.character() %>% as.numeric() 
          
          # calculate weights
          weights_multi_check <- func_weights("binary", df_pred_cov_func %>% mutate(treatment = D, m = m), x)
          
          # calculate ASMD
          balance <- bal.tab(
            as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights_multi_check, method = "weighting",
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
        
        # aggregate and append
        df_smd_cov_func_all <- df_smd_cov_func_all %>%
          ungroup() %>%
          group_by(control_var, Rep, Fold, MICE) %>%
          summarize(SD_before = mean(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                    SD_after = mean(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
          ungroup()
        
        df_smd_cov_func_all_multi_check <- rbind(df_smd_cov_func_all_multi_check, df_smd_cov_func_all)
      } # close for loop over iterations
    } # close iteration over repetitions
  } # close iteration over for mice data sets
  
  # summarize across controls: MICE, repetitions and folds
  df_smd_cov_func_all_multi_check <- df_smd_cov_func_all_multi_check %>%
    ungroup() %>%
    dplyr::select(-c("Rep", "MICE", "Fold")) %>%
    group_by(control_var) %>%
    summarize(SD_before = mean(SD_before), SD_after = mean(SD_after)) %>% 
    mutate(outcome = "GPA")
  
  # summary statistics
  df_smd_cov_func_sum_multi_check <- 
    data.frame(
      "treatment_setting" = rep("multi", 2),
      "adjustment" = c("before", "after"),
      "min" = c(min(df_smd_cov_func_all_multi_check$SD_before),  min(df_smd_cov_func_all_multi_check$SD_after, na.rm = T)),
      "mean" = c(mean(df_smd_cov_func_all_multi_check$SD_before), mean(df_smd_cov_func_all_multi_check$SD_after, na.rm = T)),
      "median" = c(median(df_smd_cov_func_all_multi_check$SD_before), median(df_smd_cov_func_all_multi_check$SD_after, na.rm = T)),
      "max" = c(max(df_smd_cov_func_all_multi_check$SD_before), max(df_smd_cov_func_all_multi_check$SD_after, na.rm = T)),
      "num_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.25), 
                           sum(df_smd_cov_func_all_multi_check$SD_after > 0.25, na.rm = T)), 
      "num_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.1), 
                           sum(df_smd_cov_func_all_multi_check$SD_after > 0.1, na.rm = T)), 
      "num_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.05), 
                          sum(df_smd_cov_func_all_multi_check$SD_after > 0.05, na.rm = T)), 
      "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.25) / length(df_smd_cov_func_all_multi_check$SD_before), 
                            sum(df_smd_cov_func_all_multi_check$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_multi_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.1) / length(df_smd_cov_func_all_multi_check$SD_before), 
                            sum(df_smd_cov_func_all_multi_check$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_multi_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_check$SD_before > 0.05) / length(df_smd_cov_func_all_multi_check$SD_before), 
                           sum(df_smd_cov_func_all_multi_check$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_multi_check %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
    )
  
  df_smd_cov_func_sum_multi_check <- df_smd_cov_func_sum_multi_check %>% mutate(outcome = outcome_var_sel)
  df_smd_cov_func_all_multi_check <- df_smd_cov_func_all_multi_check %>% mutate(outcome = outcome_var_sel)
  
  # append to overall data frame
  df_smd_sum_multi_check <- rbind(df_smd_sum_multi_check, df_smd_cov_func_sum_multi_check)
  df_smd_all_multi_check <- rbind(df_smd_all_multi_check, df_smd_cov_func_all_multi_check)
  
} # close loop over outcome_var_sel()


df_smd_sum_multi_check <- df_smd_sum_multi_check %>% dplyr::select(
  outcome, treatment_setting, adjustment, everything()
)

df_smd_all_multi_check <- df_smd_all_multi_check %>% dplyr::select(
  outcome, control_var, SD_before, SD_after
)

# aggregate over personality
df_smd_sum_multi_check <- rbind(
  df_smd_sum_multi_check,
  df_smd_sum_multi_check %>% 
    filter(outcome != "grades") %>%
    mutate(outcome = "personality") %>%
    group_by(outcome, treatment_setting, adjustment) %>%
    summarize_all(mean)
)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### RC: PENALTY CHOICE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

## MULTI ## 
df_smd_sum_multi_rc_hyper <- data.frame()
df_smd_all_multi_rc_hyper <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))

# iterate over outcome variables
for (hyperparam_var_sel in c("1SE", "1SE_plus")) {
  
  print(paste("Start Hyperparameter:", hyperparam_var_sel))
  
  # load data 
  dml_result_all_multi_rc_hyper <- 
    readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                   "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                   model_trimming, "_K4-2_Rep5_", hyperparam_var_sel, "_covbal.rds"))
  
  
  df_smd_cov_func_all_multi_rc_hyper <- data.frame()
  for (mice_sel in 1:5) { # iterate over MICE data sets
    print(paste("Data Set:", mice_sel))
    for (rep_sel in 1:5) { # iterate over repetitions
      df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
      for (iter_sel in 1:nrow(df_iterate_sel)) { # iterate over folds
        df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
        
        # predictions
        df_pred_cov_func <- dml_result_all_multi_rc_hyper[[mice_sel]]$pred %>% 
          filter(Repetition == rep_sel, Fold == iter_sel) %>%
          mutate(D_1 = ifelse(treatment == 1, 1, 0), D_2 = ifelse(treatment == 2, 1, 0), D_3 = ifelse(treatment == 3, 1, 0))
        
        # control variables
        df_controls_multi_rc_hyper <- dml_result_all_multi_rc_hyper[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
        
        # control variables
        x <- df_controls_multi_rc_hyper %>% dplyr::select(-all_of(
          c("outcome_grade", "treatment_sport_freq", "treatment_sport_freq_na", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
            "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
        
        # calculate covariate balance
        df_smd_cov_func_all <- list()
        for (treatment_var_sel in c("D_1", "D_2", "D_3")) { # iterate over treatments
          D <- df_pred_cov_func %>% pull(treatment_var_sel) %>% as.character() %>% as.numeric() 
          m <- df_pred_cov_func %>% pull(str_replace(treatment_var_sel, "D_", "m")) %>% as.character() %>% as.numeric() 
          
          # calculate weights
          weights_multi_rc_hyper <- func_weights("binary", df_pred_cov_func %>% mutate(treatment = D, m = m), x)
          
          # calculate ASMD
          balance <- bal.tab(
            as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights_multi_rc_hyper, method = "weighting",
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
        
        # aggregate and append
        df_smd_cov_func_all <- df_smd_cov_func_all %>%
          ungroup() %>%
          group_by(control_var, Rep, Fold, MICE) %>%
          summarize(SD_before = mean(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                    SD_after = mean(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
          ungroup()
        
        df_smd_cov_func_all_multi_rc_hyper <- rbind(df_smd_cov_func_all_multi_rc_hyper, df_smd_cov_func_all)
      } # close for loop over iterations
    } # close iteration over repetitions
  } # close iteration over for mice data sets
  
  # summarize across controls: MICE, repetitions and folds
  df_smd_cov_func_all_multi_rc_hyper <- df_smd_cov_func_all_multi_rc_hyper %>%
    ungroup() %>%
    dplyr::select(-c("Rep", "MICE", "Fold")) %>%
    group_by(control_var) %>%
    summarize(SD_before = mean(SD_before), SD_after = mean(SD_after)) %>% 
    mutate(outcome = "GPA")
  
  # summary statistics
  df_smd_cov_func_sum_multi_rc_hyper <- 
    data.frame(
      "treatment_setting" = rep("multi", 2),
      "adjustment" = c("before", "after"),
      "min" = c(min(df_smd_cov_func_all_multi_rc_hyper$SD_before),  min(df_smd_cov_func_all_multi_rc_hyper$SD_after, na.rm = T)),
      "mean" = c(mean(df_smd_cov_func_all_multi_rc_hyper$SD_before), mean(df_smd_cov_func_all_multi_rc_hyper$SD_after, na.rm = T)),
      "median" = c(median(df_smd_cov_func_all_multi_rc_hyper$SD_before), median(df_smd_cov_func_all_multi_rc_hyper$SD_after, na.rm = T)),
      "max" = c(max(df_smd_cov_func_all_multi_rc_hyper$SD_before), max(df_smd_cov_func_all_multi_rc_hyper$SD_after, na.rm = T)),
      "num_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.25), 
                           sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.25, na.rm = T)), 
      "num_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.1), 
                           sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.1, na.rm = T)), 
      "num_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.05), 
                          sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.05, na.rm = T)), 
      "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.25) / length(df_smd_cov_func_all_multi_rc_hyper$SD_before), 
                            sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.25, na.rm = T) / df_smd_cov_func_all_multi_rc_hyper %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.1) / length(df_smd_cov_func_all_multi_rc_hyper$SD_before), 
                            sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.1, na.rm = T) / df_smd_cov_func_all_multi_rc_hyper %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
      "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_multi_rc_hyper$SD_before > 0.05) / length(df_smd_cov_func_all_multi_rc_hyper$SD_before), 
                           sum(df_smd_cov_func_all_multi_rc_hyper$SD_after > 0.05, na.rm = T) / df_smd_cov_func_all_multi_rc_hyper %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
    )
  
  df_smd_cov_func_sum_multi_rc_hyper <- df_smd_cov_func_sum_multi_rc_hyper %>% mutate(outcome = "GPA", hyperparam = hyperparam_var_sel)
  df_smd_cov_func_all_multi_rc_hyper <- df_smd_cov_func_all_multi_rc_hyper %>% mutate(outcome = "GPA", hyperparam = hyperparam_var_sel)
  
  # append to overall data frame
  df_smd_sum_multi_rc_hyper <- rbind(df_smd_sum_multi_rc_hyper, df_smd_cov_func_sum_multi_rc_hyper)
  df_smd_all_multi_rc_hyper <- rbind(df_smd_all_multi_rc_hyper, df_smd_cov_func_all_multi_rc_hyper)
  
} # close loop over hyperparam_sel()


df_smd_sum_multi_rc_hyper <- df_smd_sum_multi_rc_hyper %>% dplyr::select(
  outcome, hyperparam, treatment_setting, adjustment, everything()
) %>% filter(adjustment == "after")

df_smd_all_multi_rc_hyper <- df_smd_all_multi_rc_hyper %>% dplyr::select(
  outcome, hyperparam, control_var, SD_before, SD_after
)

saveRDS(df_smd_sum_multi_rc_hyper, "Output/DML/Covariate_Balancing/covariate_balancing_summary_rc_hyper.rds")