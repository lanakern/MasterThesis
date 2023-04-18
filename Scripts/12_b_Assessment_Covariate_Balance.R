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
data_all_mice_grades <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", 
                      cov_balance_save, "_mice", mice_data_sel, ".rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_grades <- rbind(data_all_mice_grades, data_all_mice_sub)
}

data_all_mice_personality <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", 
                      cov_balance_save, "_mice", mice_data_sel, "_personality.rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_personality <- rbind(data_all_mice_personality, data_all_mice_sub)
}

data_all_mice_grades_multi <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop", 
                      cov_balance_save, "_mice", mice_data_sel, ".rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_grades_multi <- rbind(data_all_mice_grades_multi, data_all_mice_sub)
}

data_all_mice_personality_multi <- data.frame()
for (mice_data_sel in 1:5) {
  data_load <- paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop", 
                      cov_balance_save, "_mice", mice_data_sel, "_personality.rds")
  data_all_mice_sub <- readRDS(data_load)
  data_all_mice_sub <- data_all_mice_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  data_all_mice_personality_multi <- rbind(data_all_mice_personality_multi, data_all_mice_sub)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#cov_bal_method <- "separate"
# cov_bal_method <- "union" # otherwise plot does not make sense
# 
# df_smd_binary_all_outcome <- data.frame()
# df_smd_binary_sum_outcome <- data.frame()
# 
# for (outcome_var_sel in c("grades")) {
# 
#   # load covariate balance results from post-lasso (MICE can be identified via the list)
#   dml_result_all <- 
#     readRDS(paste0("Output/DML/Estimation/Grades/binary_", outcome_var_sel, 
#                    "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
#                    model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
#   
#   
#   # iterate over mice data frames
#   df_smd_binary_all <- data.frame()
#   df_smd_all_binary_all <- data.frame()
#   for (mice_data_sel in 1:5) {
#     
#     # load and standardize original data
#     data_dml_binary <- 
#       readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop", 
#                      cov_balance_save, "_mice", mice_data_sel, ".rds"))
#     data_dml_binary <- data_dml_binary %>%
#       recipe(.) %>%
#       update_role(treatment_sport, new_role = "outcome") %>%
#       step_normalize(all_of(data_dml_binary %>% dplyr::select(-c("treatment_sport")) %>% colnames())) %>%
#       prep() %>%
#       bake(new_data = NULL)
#     
#     # decide on method: union of variables or separately across folds
#     
#     #### UNION ####
#     #%%%%%%%%%%%%%#
#     
#     if (cov_bal_method == "union") {
#       
#       # control variables
#       controls_sel <- unique(dml_result_all[[mice_data_sel]]$coef$term)
#       controls_sel <- controls_sel[!controls_sel  %in% "(Intercept)"]
#       controls_sel <- str_replace(sort(controls_sel), "`friends_study_share_(almost)half`", "friends_study_share_.almost.half")
#       
#       # predictions
#       df_pred_binary_all <- dml_result_all[[mice_data_sel]]$pred
#       
#       # data with all columns in controls_sel
#       df_controls_binary_all <- data.frame()
#       df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
#       for (rep_fold in 1:nrow(df_iterate)) {
#         
#         # extract covariates
#         df_iterate_sel <- df_iterate[rep_fold, ]
#         df_controls_binary <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$controls
#         controls_sel <- colnames(df_controls_binary)[colnames(df_controls_binary) %in% controls_sel]
#         df_controls_binary <- df_controls_binary %>% dplyr::select(all_of(controls_sel))
#         df_controls_binary_all <- rbind(df_controls_binary_all, df_controls_binary)
#         
#       }
#       
#       
#       ## Calculate Weights ##
#       #+++++++++++++++++++++#
#       
#       weights <- func_weights("binary", df_pred_binary_all, df_controls_binary_all)
#       
#       
#       ## ASDM: Apply Own Function ##
#       #++++++++++++++++++++++++++++#
#       
#       list_smd_binary <- func_cov_balance("binary", data_dml_binary, controls_sel, weights)
#       df_smd_binary <- list_smd_binary$smd_summary %>% mutate(MICE = mice_data_sel)
#       df_smd_binary_all <- rbind(df_smd_binary_all, df_smd_binary)
#       df_smd_all_binary <- list_smd_binary$smd_values %>% mutate(MICE = mice_data_sel)
#       df_smd_all_binary_all <- rbind(df_smd_all_binary_all, df_smd_all_binary)
#       
#       
#     #### SEPARATE CALCULATIONS PER FOLD ####
#     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#       
#     } else {
#       
#       # iteration
#       df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
#       
#       for (iter_sel in 1:nrow(df_iterate)) {
#         
#         df_iterate_sel <- df_iterate[iter_sel, ]
#         
#         # control variables
#         controls_sel <- dml_result_all[[mice_data_sel]]$coef %>%
#           filter(Repetition == df_iterate_sel$Rep & Fold == df_iterate_sel$Fold) %>%
#           pull(term) %>%
#           unique()
#         controls_sel <- controls_sel[!controls_sel %in% "(Intercept)"]
#         controls_sel <- str_replace(sort(controls_sel), "`friends_study_share_(almost)half`", "friends_study_share_.almost.half")
#         
#         # predictions
#         df_pred_binary <- dml_result_all[[mice_data_sel]]$pred %>% 
#           filter(Repetition == df_iterate_sel$Rep & Fold == df_iterate_sel$Fold)
# 
#         # extract covariates
#         df_controls_binary <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$controls
#         controls_sel <- colnames(df_controls_binary)[colnames(df_controls_binary) %in% controls_sel]
#         df_controls_binary <- df_controls_binary %>% dplyr::select(all_of(controls_sel))
#         
#         # calculate weights
#         weights <- func_weights("binary", df_pred_binary, df_controls_binary)
#         
#         # ASDM
#         list_smd_binary <- func_cov_balance("binary", data_dml_binary, controls_sel, weights)
#         df_smd_binary <- list_smd_binary$smd_summary %>% mutate(Fold = df_iterate_sel$Fold, Rep = df_iterate_sel$Rep, MICE = mice_data_sel)
#         df_smd_binary_all <- rbind(df_smd_binary_all, df_smd_binary)
#         df_smd_all_binary <- list_smd_binary$smd_values %>% mutate(Fold = df_iterate_sel$Fold, Rep = df_iterate_sel$Rep, MICE = mice_data_sel)
#         df_smd_all_binary_all <- rbind(df_smd_all_binary_all, df_smd_all_binary)
#         
#       } # close iteration over repetitions and folds
#       
#     } # close else()
#     
#   } # close iteration over MICE data sets
#   
#   if (colnames(df_smd_binary_all) %in% c("Fold", "Rep")) {
#     df_smd_binary_all <- df_smd_binary_all %>%
#       dplyr::select(-c(Fold, Rep))
#   }
#   df_smd_binary_final <- df_smd_binary_all %>%
#     dplyr::select(-MICE) %>%
#     group_by(treatment_setting, adjustment, controls) %>% 
#     summarise_all(mean) %>%
#     ungroup()
#   
#   # for all outcomes in one data frame
#   df_smd_binary_sum_outcome <- rbind(df_smd_binary_sum_outcome, 
#                                      df_smd_binary_final %>% mutate(outcome = outcome_var_sel))
#   
#   df_smd_binary_all_outcome <- rbind(df_smd_binary_all_outcome, 
#                                      df_smd_all_binary_all %>% mutate(outcome = outcome_var_sel))
# }
# 
# df_smd_binary_all_outcome_save <- 
#   df_smd_binary_all_outcome %>%
#   dplyr::select(-MICE) %>%
#   group_by(outcome, control_var) %>%
#   summarize_all(mean)
# 
# 
# df_smd_binary_sum_outcome_save <- 
#   data.frame(
#     "treatment_setting" = rep("binary", 3),
#     "adjustment" = c("before", "after", "after"),
#     "controls" = c("all", "all", "selected"),
#     "min" = c(min(df_smd_binary_all_outcome_save$SD_before), min(df_smd_binary_all_outcome_save$SD_after_all), min(df_smd_binary_all_outcome_save$SD_after, na.rm = T)),
#     "mean" = c(mean(df_smd_binary_all_outcome_save$SD_before), mean(df_smd_binary_all_outcome_save$SD_after_all), mean(df_smd_binary_all_outcome_save$SD_after, na.rm = T)),
#     "median" = c(median(df_smd_binary_all_outcome_save$SD_before), median(df_smd_binary_all_outcome_save$SD_after_all), median(df_smd_binary_all_outcome_save$SD_after, na.rm = T)),
#     "max" = c(max(df_smd_binary_all_outcome_save$SD_before), max(df_smd_binary_all_outcome_save$SD_after_all), max(df_smd_binary_all_outcome_save$SD_after, na.rm = T)),
#     "num_cov_smd_20" = c(sum(df_smd_binary_all_outcome_save$SD_before > 20), 
#                          sum(df_smd_binary_all_outcome_save$SD_after_all > 20), 
#                          sum(df_smd_binary_all_outcome_save$SD_after > 20, na.rm = T)), 
#     "num_cov_smd_10" = c(sum(df_smd_binary_all_outcome_save$SD_before > 10), 
#                          sum(df_smd_binary_all_outcome_save$SD_after_all > 10), 
#                          sum(df_smd_binary_all_outcome_save$SD_after > 10, na.rm = T)), 
#     "num_cov_smd_5" = c(sum(df_smd_binary_all_outcome_save$SD_before > 5), 
#                         sum(df_smd_binary_all_outcome_save$SD_after_all > 5), 
#                         sum(df_smd_binary_all_outcome_save$SD_after > 5, na.rm = T)), 
#     "perc_cov_smd_20" = c(sum(df_smd_binary_all_outcome_save$SD_before > 20) / length(df_smd_binary_all_outcome_save$SD_before), 
#                           sum(df_smd_binary_all_outcome_save$SD_after_all > 20) / length(df_smd_binary_all_outcome_save$SD_after_all), 
#                           sum(df_smd_binary_all_outcome_save$SD_after > 20, na.rm = T) / df_smd_binary_all_outcome_save %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
#     "perc_cov_smd_10" = c(sum(df_smd_binary_all_outcome_save$SD_before > 10) / length(df_smd_binary_all_outcome_save$SD_before), 
#                           sum(df_smd_binary_all_outcome_save$SD_after_all > 10) / length(df_smd_binary_all_outcome_save$SD_after_all), 
#                           sum(df_smd_binary_all_outcome_save$SD_after > 10, na.rm = T) / df_smd_binary_all_outcome_save %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
#     "perc_cov_smd_5" = c(sum(df_smd_binary_all_outcome_save$SD_before > 5) / length(df_smd_binary_all_outcome_save$SD_before), 
#                          sum(df_smd_binary_all_outcome_save$SD_after_all > 5) / length(df_smd_binary_all_outcome_save$SD_after_all), 
#                          sum(df_smd_binary_all_outcome_save$SD_after > 5, na.rm = T) / df_smd_binary_all_outcome_save %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
#   )
# 
# 
# saveRDS(df_smd_binary_sum_outcome_save, "Output/DML/Covariate_Balancing/summary_covariate_balancing.rds")
# saveRDS(df_smd_binary_all_outcome_save, "Output/DML/Covariate_Balancing/asdm_covariate_balancing.rds")




#### Covariate Balancing: Cobalt Function ####
#++++++++++++++++++++++++++++++++++++++++++++#
  
## GRADES ##
#----------#

# load covariate balance results from post-lasso (MICE can be identified via the list)
dml_result_all <- 
  readRDS(paste0("Output/DML/Estimation/Grades/binary_", "grades", 
                 "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# Before matching: "Un"; after matching: "Adj"
df_smd_cov_func_all_binary <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
for (mice_sel in 1:5) {
  for (rep_sel in 1:5) {
    df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
    
    for (iter_sel in 1:nrow(df_iterate_sel)) {
      df_pred_cov_func <- dml_result_all[[mice_sel]]$pred %>% filter(Repetition == rep_sel, Fold == iter_sel)
      df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
      df_controls_binary <- dml_result_all[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
      
      D <- df_pred_cov_func$treatment %>% as.character() %>% as.numeric() 
      x <- df_controls_binary %>% dplyr::select(-all_of(c("outcome_grade", "treatment_sport", "Fold", "Repetition", "group")))
      weights <- func_weights("binary", df_pred_cov_func, x) # davor df_controls_cov_func
      balance <- bal.tab(
        as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights, method = "weighting",
        s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
        disp.v.ratio = TRUE, disp.ks = TRUE, 
        un = TRUE, # display statistics also for before DML
        continuous = "std", binary = "std" # also standardized binary covariates
        # which.treat = .all # for multivalued treatment: pairwise comparisons
      )
      df_smd_cov_func <- balance$Balance %>% 
        dplyr::select(Diff.Un, Diff.Adj) %>% 
        mutate(SD_before = abs(Diff.Un), SD_after = abs(Diff.Adj)) %>%
        dplyr::select(-c(Diff.Un, Diff.Adj)) %>%
        mutate(Fold = iter_sel, Rep = rep_sel, MICE = mice_sel) %>%
        mutate(control_var = rownames(.))
      rownames(df_smd_cov_func) <- 1:nrow(df_smd_cov_func)
      df_smd_cov_func_all_binary <- rbind(df_smd_cov_func_all_binary, df_smd_cov_func)
    }
    
  }
}

df_smd_cov_func_all_binary <- df_smd_cov_func_all_binary %>%
  dplyr::select(-c("Rep", "MICE")) %>%
  group_by(control_var) %>%
  summarize(SD_before = mean(SD_before), SD_after = mean(SD_after))

df_smd_cov_func_sum_binary <- 
  data.frame(
    "treatment_setting" = rep("binary", 2),
    "adjustment" = c("before", "after"),
    "min" = c(min(df_smd_cov_func_all_binary$SD_before),  min(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
    "mean" = c(mean(df_smd_cov_func_all_binary$SD_before), mean(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
    "median" = c(median(df_smd_cov_func_all_binary$SD_before), median(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
    "max" = c(max(df_smd_cov_func_all_binary$SD_before), max(df_smd_cov_func_all_binary$SD_after, na.rm = T)),
    "num_cov_smd_25" = c(sum(df_smd_cov_func_all_binary$SD_before > 25), 
                         sum(df_smd_cov_func_all_binary$SD_after > 25, na.rm = T)), 
    "num_cov_smd_10" = c(sum(df_smd_cov_func_all_binary$SD_before > 10), 
                         sum(df_smd_cov_func_all_binary$SD_after > 10, na.rm = T)), 
    "num_cov_smd_5" = c(sum(df_smd_cov_func_all_binary$SD_before > 5), 
                        sum(df_smd_cov_func_all_binary$SD_after > 5, na.rm = T)), 
    "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_binary$SD_before > 25) / length(df_smd_cov_func_all_binary$SD_before), 
                          sum(df_smd_cov_func_all_binary$SD_after > 25, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
    "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_binary$SD_before > 10) / length(df_smd_cov_func_all_binary$SD_before), 
                          sum(df_smd_cov_func_all_binary$SD_after > 10, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
    "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_binary$SD_before > 5) / length(df_smd_cov_func_all_binary$SD_before), 
                         sum(df_smd_cov_func_all_binary$SD_after > 5, na.rm = T) / df_smd_cov_func_all_binary %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
  )

df_smd_cov_func_all_binary <- df_smd_cov_func_all_binary %>% mutate(outcome = "GPA")

saveRDS(df_smd_cov_func_sum_binary, "Output/DML/Covariate_Balancing/summary_covariate_balancing_binary.rds")
saveRDS(df_smd_cov_func_all_binary, "Output/DML/Covariate_Balancing/asdm_covariate_balancing_binary.rds")


## Personality ##
#---------------#



#### Covariate Balancing: Plot ####
#+++++++++++++++++++++++++++++++++#

#df_smd_plot <- df_smd_binary_all_outcome_save
df_smd_plot <- df_smd_cov_func_all_binary

# create plots and store them in list: First for SD_after
plot_cov_bal_all_sel <- list()
i <- 0
for (outcome_var_sel in c("GPA")) {
  df_smd_plot_grades <- df_smd_plot %>% filter(outcome == outcome_var_sel)
  i <- i + 1
  plot_cov_bal_all_sel[[outcome_var_sel]] <- ggplot() +
    geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_before)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_grades)),
              aes(x = var_num, y = SD_before, fill = "Before DML")) +
    geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_after)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_grades)),
              aes(x = var_num, y = SD_after, fill = "After DML")) +
    scale_fill_manual(" ", values = c('Before DML' = "grey70", 'After DML' = "grey20")) +
    ylab("ASDM") + xlab("Rank from highest to lowest ASDM") +
    ggtitle(outcome_var_sel) + 
    theme_bw() + 
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0.5, size = 22),
          axis.text = element_text(size = 18), axis.title = element_text(size = 20),
          legend.text = element_text(size = 18), legend.title = element_text(size = 20)) +
    guides(fill = guide_legend(title = "ASDM")) 
}

plot_cov_bal_final_grades <- plot_cov_bal_all_sel[["GPA"]] 

ggsave("Output/DML/Covariate_Balancing/plot_cov_balance_grades.png", plot_cov_bal_final_grades)


# # For SD_after_all
# plot_cov_bal_al <- list()
# i <- 0
# for (outcome_var_sel in c("grades")) {
#   df_smd_plot_grades <- df_smd_binary_all_outcome_save %>% filter(outcome == outcome_var_sel)
#   i <- i + 1
#   plot_cov_bal_al[[i]] <- ggplot() +
#     geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_before)) %>% 
#                 mutate(var_num = 1:nrow(df_smd_plot_grades)),
#               aes(x = var_num, y = SD_before, fill = "Before")) +
#     geom_area(data = df_smd_plot_grades %>% arrange(desc(SD_after_all)) %>% 
#                 mutate(var_num = 1:nrow(df_smd_plot_grades)),
#               aes(x = var_num, y = SD_after_all, fill = "After All")) + 
#     scale_fill_manual(" ", values = c(Before = "grey20", After = "grey90", `After All` = "grey95")) +
#     ylab("ASDM") + xlab("Rank from highest to lowest ASDM") +
#     ggtitle(str_to_title(outcome_var_sel)) + 
#     theme_bw() + 
#     theme(legend.position = "right", 
#           plot.title = element_text(hjust = 0.5, size = 22),
#           axis.text = element_text(size = 18), axis.title = element_text(size = 20),
#           legend.text = element_text(size = 18), legend.title = element_text(size = 20)) +
#     guides(fill = guide_legend(title = "ASDM")) 
# }
# 
# ggsave("Output/DML/Covariate_Balancing/plot_cov_balance_grades_binary.png", plot_multi_cov_bal_al$grades)



#### Main Drivers of selection ####
#+++++++++++++++++++++++++++++++++#

# extract 50 variables with highest ASDM 
#df_main_drivers <- df_smd_binary_all_outcome_save %>% 
df_main_drivers_binary <- df_smd_cov_func_all_binary %>%
  #dplyr::select(-MICE) %>%
  arrange(-SD_before) %>%
  group_by(outcome, control_var) %>% 
  summarize_all(mean) %>% 
  arrange(desc(SD_before)) %>% 
  head(50)


# descriptive statistics for them
cols_binary <- colnames(data_all_mice_grades)[colnames(data_all_mice_grades) %in% df_main_drivers_binary$control_var]
cols_multi <- colnames(data_all_mice_grades_multi)[colnames(data_all_mice_grades_multi) %in% df_main_drivers_binary$control_var]

df_descr_binary <- data_all_mice_grades %>% dplyr::select(treatment_sport, all_of(cols_binary))
df_descr_multi <- data_all_mice_grades_multi %>% dplyr::select(treatment_sport_freq, all_of(cols_multi))

df_main_drivers_binary <- left_join(
  df_main_drivers_binary, 
  df_descr_binary %>%
    group_by(treatment_sport) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport, value = mean) %>%
    rename(mean_0 = "0", mean_1 = "1"),
  by = "control_var"
)

df_main_drivers_multi <- left_join(
  df_main_drivers, 
  df_descr_multi %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean) %>%
    rename(mean_weekly = "1", mean_monthly = "2", mean_never = "3"),
  by = "control_var"
)

df_main_drivers_all <- inner_join(df_main_drivers_binary, df_main_drivers_multi)

# save
saveRDS("Output/DML/Covariate_Balancing/main_drivers.rds", df_main_drivers_all)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# # iterate over mice data frames
# df_smd_multi_all_outcome <- data.frame()
# df_smd_multi_sum_outcome <- data.frame()
# 
# df_smd_multi_all <- data.frame()
# df_smd_all_multi_all_before <- data.frame()
# df_smd_all_multi_all_after <- data.frame()
# df_smd_all_multi_all_after_all <- data.frame()
# 
# for (outcome_var_sel in c("grades")) {
#   # load covariate balance results from post-lasso (MICE can be identified via the list)
#   dml_result_all <- 
#     readRDS(paste0("Output/DML/Estimation/Grades/multi_", outcome_var_sel, 
#                    "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
#                    model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))
#   
#   # iterate over mice data frames
#   df_smd_multi_all <- data.frame()
#   df_smd_all_multi_all <- data.frame()
#   for (mice_data_sel in 1:5) {
#     
#     # load and standardize original data
#     data_dml_multi <- 
#       readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_weekly_down_extradrop", cov_balance_save, "_mice", mice_data_sel, ".rds"))
#     data_dml_multi <- data_dml_multi %>%
#       recipe(.) %>%
#       update_role(treatment_sport_freq, new_role = "outcome") %>%
#       step_normalize(all_of(data_dml_multi %>% dplyr::select(-treatment_sport_freq) %>% colnames())) %>%
#       prep() %>%
#       bake(new_data = NULL)
#     
#     # control variables
#     controls_sel <- unique(dml_result_all[[mice_data_sel]]$coef$term)
#     controls_sel <- controls_sel[!controls_sel  %in% "(Intercept)"]
#     controls_sel <- str_replace(sort(controls_sel), "`friends_study_share_(almost)half`", "friends_study_share_.almost.half")
#     
#     # predictions
#     df_pred_multi_all <- dml_result_all[[mice_data_sel]]$pred
#     
#     # data with all columns in controls_sel
#     df_controls_multi_all <- data.frame()
#     df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
#     
#     for (rep_fold in 1:nrow(df_iterate)) {
#       # extract covariates
#       df_iterate_sel <- df_iterate[rep_fold, ]
#       df_controls_multi <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$controls
#       controls_sel <- colnames(df_controls_multi)[colnames(df_controls_multi) %in% controls_sel]
#       df_controls_multi <- df_controls_multi %>% dplyr::select(all_of(controls_sel))
#       df_controls_multi_all <- rbind(df_controls_multi_all, df_controls_multi)
#     }
#     
#     ## Calculate Weights ##
#     #+++++++++++++++++++++#
#     
#     weights <- func_weights("multi", df_pred_multi_all, df_controls_multi_all)
#     
#     
#     ## ASDM: Apply Own Function ##
#     #++++++++++++++++++++++++++++#
#     
#     list_smd_multi <- func_cov_balance("multi", data_dml_multi, controls_sel, weights)
#     
#     # summary
#     # df_smd_multi <- list_smd_multi$smd_summary %>% mutate(MICE = mice_data_sel)
#     # df_smd_multi_all <- rbind(df_smd_multi_all, df_smd_multi)
#     
#     # all
#     df_smd_all_multi_before <- list_smd_multi$smd_values$before %>% mutate(MICE = mice_data_sel)
#     df_smd_all_multi_all_before <- rbind(df_smd_all_multi_all_before, df_smd_all_multi_before)
#     
#     df_smd_all_multi_after <- list_smd_multi$smd_values$after_sel %>% mutate(MICE = mice_data_sel)
#     df_smd_all_multi_all_after <- rbind(df_smd_all_multi_all_after, df_smd_all_multi_after)
#     
#     df_smd_all_multi_after_all <- list_smd_multi$smd_values$after_all %>% mutate(MICE = mice_data_sel)
#     df_smd_all_multi_all_after_all <- rbind(df_smd_all_multi_all_after_all, df_smd_all_multi_after_all)
#     
#   } # close iteration over MICE data sets
#   
#   
#   # aggregate summary
#   # if (colnames(df_smd_multi_all) %in% c("Fold", "Rep")) {
#   #   df_smd_multi_all <- df_smd_multi_all %>%
#   #     dplyr::select(-c(Fold, Rep))
#   # }
#   # df_smd_multi_final <- df_smd_multi_all %>%
#   #   dplyr::select(-MICE) %>%
#   #   group_by(treatment_setting, adjustment, controls) %>% 
#   #   summarise_all(mean) %>%
#   #   ungroup()
#   
#   
#   # aggregate individual
#   df_smd_all_multi_all <- left_join(
#     df_smd_all_multi_before %>% rowwise() %>% mutate(SD_before = mean(c(SD_1, SD_2, SD_3), na.rm = TRUE)) %>% dplyr::select(-c(SD_1, SD_2, SD_3)),
#     df_smd_all_multi_after_all %>% rowwise() %>% mutate(SD_after_all = mean(c(SD_1, SD_2, SD_3), na.rm = TRUE)) %>% dplyr::select(-c(SD_1, SD_2, SD_3)),
#     by = c("control_var", "MICE")
#   ) %>% left_join(
#     df_smd_all_multi_after %>% rowwise() %>% mutate(SD_after = mean(c(SD_1, SD_2, SD_3), na.rm = TRUE)) %>% dplyr::select(-c(SD_1, SD_2, SD_3)),
#     by = c("control_var", "MICE")
#   )
#   
#   # for all outcomes in one data frame
#   # df_smd_multi_sum_outcome <- rbind(df_smd_multi_sum_outcome, 
#   #                                   df_smd_multi_final %>% mutate(outcome = outcome_var_sel))
#   
#   df_smd_multi_all_outcome <- rbind(df_smd_multi_all_outcome, 
#                                     df_smd_all_multi_all %>% mutate(outcome = outcome_var_sel))
# }
#   
# 
# # detailed asdm
# df_smd_multi_all_outcome <- df_smd_multi_all_outcome %>% as.data.frame()
# 
# # summary
# df_smd_multi_sum_outcome <- 
#   data.frame(
#     "treatment_setting" = rep("multi", 3),
#     "adjustment" = c("before", "after", "after"),
#     "controls" = c("all", "all", "selected"),
#     "min" = c(min(df_smd_multi_all_outcome$SD_before), min(df_smd_multi_all_outcome$SD_after_all), min(df_smd_multi_all_outcome$SD_after, na.rm = T)),
#     "mean" = c(max(df_smd_multi_all_outcome$SD_before), max(df_smd_multi_all_outcome$SD_after_all), max(df_smd_multi_all_outcome$SD_after, na.rm = T)),
#     "median" = c(max(df_smd_multi_all_outcome$SD_before), max(df_smd_multi_all_outcome$SD_after_all), max(df_smd_multi_all_outcome$SD_after, na.rm = T)),
#     "max" = c(max(df_smd_multi_all_outcome$SD_before), max(df_smd_multi_all_outcome$SD_after_all), max(df_smd_multi_all_outcome$SD_after, na.rm = T)),
#     "num_cov_smd_20" = c(sum(df_smd_multi_all_outcome$SD_before > 20), 
#                             sum(df_smd_multi_all_outcome$SD_after_all > 20), 
#                             sum(df_smd_multi_all_outcome$SD_after > 20, na.rm = T)), 
#     "num_cov_smd_10" = c(sum(df_smd_multi_all_outcome$SD_before > 10), 
#                             sum(df_smd_multi_all_outcome$SD_after_all > 10), 
#                             sum(df_smd_multi_all_outcome$SD_after > 10, na.rm = T)), 
#     "num_cov_smd_5" = c(sum(df_smd_multi_all_outcome$SD_before > 5), 
#                            sum(df_smd_multi_all_outcome$SD_after_all > 5), 
#                            sum(df_smd_multi_all_outcome$SD_after > 5, na.rm = T)), 
#     "perc_cov_smd_20" = c(sum(df_smd_multi_all_outcome$SD_before > 20) / length(df_smd_multi_all_outcome$SD_before), 
#                              sum(df_smd_multi_all_outcome$SD_after_all > 20) / length(df_smd_multi_all_outcome$SD_after_all), 
#                              sum(df_smd_multi_all_outcome$SD_after > 20, na.rm = T) / df_smd_multi_all_outcome %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
#     "perc_cov_smd_10" = c(sum(df_smd_multi_all_outcome$SD_before > 10) / length(df_smd_multi_all_outcome$SD_before), 
#                              sum(df_smd_multi_all_outcome$SD_after_all > 10) / length(df_smd_multi_all_outcome$SD_after_all), 
#                              sum(df_smd_multi_all_outcome$SD_after > 10, na.rm = T) / df_smd_multi_all_outcome %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
#     "perc_cov_smd_5" = c(sum(df_smd_multi_all_outcome$SD_before > 5) / length(df_smd_multi_all_outcome$SD_before), 
#                             sum(df_smd_multi_all_outcome$SD_after_all > 5) / length(df_smd_multi_all_outcome$SD_after_all), 
#                             sum(df_smd_multi_all_outcome$SD_after > 5, na.rm = T) / df_smd_multi_all_outcome %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
#   )


## GRADES ##
#----------#


# load covariate balance results from post-lasso (MICE can be identified via the list)
dml_result_all_multi <- 
  readRDS(paste0("Output/DML/Estimation/Grades/multi_", "grades", 
                 "_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming",
                 model_trimming, "_K4-2_Rep5", cov_balance_save, ".rds"))


# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# Before matching: "Un"; after matching: "Adj"
df_smd_cov_func_all_multi <- data.frame()
df_iterate <- data.frame("Rep" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)), "Fold" = rep(c(1:4), 5))
for (mice_sel in 1:5) {
  for (rep_sel in 1:5) {
    df_iterate_sel <- df_iterate %>% filter(Rep == rep_sel)
    
    for (iter_sel in 1:nrow(df_iterate_sel)) {
      df_iterate_sel_2 <- df_iterate_sel[iter_sel, ]
      
      df_pred_cov_func <- dml_result_all_multi[[mice_sel]]$pred %>% 
        filter(Repetition == rep_sel, Fold == iter_sel) %>%
        mutate(D_1 = ifelse(treatment == 1, 1, 0), D_2 = ifelse(treatment == 2, 1, 0), D_3 = ifelse(treatment == 3, 1, 0))
      
      df_controls_multi <- dml_result_all_multi[[mice_sel]]$cov_balance[[mice_sel]][[df_iterate_sel_2$Rep]][[df_iterate_sel_2$Fold]]$controls
      
      # control variables
      x <- df_controls_multi %>% dplyr::select(-all_of(
        c("outcome_grade", "treatment_sport_freq", "treatment_sport_freq_monthly_less", "treatment_sport_freq_never",
          "treatment_sport_freq_weekly_atleast", "Fold", "Repetition", "group")))
      
      # calculate covariate balance
      df_smd_cov_func_all <- list()
      for (treatment_var_sel in c("D_1", "D_2", "D_3")) {
        D <- df_pred_cov_func %>% pull(treatment_var_sel) %>% as.character() %>% as.numeric() 
        m <- df_pred_cov_func %>% pull(str_replace(treatment_var_sel, "D_", "m")) %>% as.character() %>% as.numeric() 
        
        # calculate weights
        weights_multi <- func_weights("binary", df_pred_cov_func %>% mutate(treatment = D, m = m), x)
        
        balance <- bal.tab(
          as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights_multi, method = "weighting",
          s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
          disp.v.ratio = TRUE, disp.ks = TRUE, 
          un = TRUE, # display statistics also for before DML
          continuous = "std", binary = "std" # also standardized multi covariates
        )
        
        df_smd_cov_func <- balance$Balance %>% 
          dplyr::select(Diff.Un, Diff.Adj) %>% 
          mutate(SD_before = abs(Diff.Un), SD_after = abs(Diff.Adj)) %>%
          dplyr::select(-c(Diff.Un, Diff.Adj)) %>%
          mutate(Rep = rep_sel, Fold = iter_sel, MICE = mice_sel) %>%
          mutate(control_var = rownames(.)) %>%
          rename(!!rlang::sym(paste0("SD_before_", treatment_var_sel)) := SD_before,
                 !!rlang::sym(paste0("SD_after_", treatment_var_sel)) := SD_after)
        rownames(df_smd_cov_func) <- 1:nrow(df_smd_cov_func)
        df_smd_cov_func_all[[treatment_var_sel]] <- df_smd_cov_func
      } # close iteration over treatment variables
      
      # append in one data frame
      df_smd_cov_func_all <- left_join(
        df_smd_cov_func_all[["D_1"]], df_smd_cov_func_all[["D_2"]], by = c("control_var", "Rep", "Fold", "MICE")
      ) %>%
        left_join(df_smd_cov_func_all[["D_3"]], by = c("control_var", "Rep",  "Fold", "MICE"))
      
      # aggregate
      df_smd_cov_func_all <- df_smd_cov_func_all %>%
        ungroup() %>%
        group_by(control_var, Rep, Fold, MICE) %>%
        summarize(SD_before = mean(c(SD_before_D_1, SD_before_D_2, SD_before_D_3)),
                  SD_after = mean(c(SD_after_D_1, SD_after_D_2, SD_after_D_3))) %>%
        ungroup()
      
      df_smd_cov_func_all_multi <- rbind(df_smd_cov_func_all_multi, df_smd_cov_func_all)
    } # close for loop over iterations
  } # close iteration over repetitions
} # close iteration over for loop

df_smd_cov_func_all_multi <- df_smd_cov_func_all_multi %>%
  ungroup() %>%
  dplyr::select(-c("Rep", "MICE", "Fold")) %>%
  group_by(control_var) %>%
  summarize(SD_before = mean(SD_before), SD_after = mean(SD_after))

df_smd_cov_func_sum_multi <- 
  data.frame(
    "treatment_setting" = rep("multi", 2),
    "adjustment" = c("before", "after"),
    "min" = c(min(df_smd_cov_func_all_multi$SD_before),  min(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
    "mean" = c(mean(df_smd_cov_func_all_multi$SD_before), mean(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
    "median" = c(median(df_smd_cov_func_all_multi$SD_before), median(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
    "max" = c(max(df_smd_cov_func_all_multi$SD_before), max(df_smd_cov_func_all_multi$SD_after, na.rm = T)),
    "num_cov_smd_25" = c(sum(df_smd_cov_func_all_multi$SD_before > 25), 
                         sum(df_smd_cov_func_all_multi$SD_after > 25, na.rm = T)), 
    "num_cov_smd_10" = c(sum(df_smd_cov_func_all_multi$SD_before > 10), 
                         sum(df_smd_cov_func_all_multi$SD_after > 10, na.rm = T)), 
    "num_cov_smd_5" = c(sum(df_smd_cov_func_all_multi$SD_before > 5), 
                        sum(df_smd_cov_func_all_multi$SD_after > 5, na.rm = T)), 
    "perc_cov_smd_25" = c(sum(df_smd_cov_func_all_multi$SD_before > 25) / length(df_smd_cov_func_all_multi$SD_before), 
                          sum(df_smd_cov_func_all_multi$SD_after > 25, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
    "perc_cov_smd_10" = c(sum(df_smd_cov_func_all_multi$SD_before > 10) / length(df_smd_cov_func_all_multi$SD_before), 
                          sum(df_smd_cov_func_all_multi$SD_after > 10, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow()), 
    "perc_cov_smd_5" = c(sum(df_smd_cov_func_all_multi$SD_before > 5) / length(df_smd_cov_func_all_multi$SD_before), 
                         sum(df_smd_cov_func_all_multi$SD_after > 5, na.rm = T) / df_smd_cov_func_all_multi %>% dplyr::select(SD_after) %>% na.omit() %>% nrow())
  )

df_smd_cov_func_all_multi <- df_smd_cov_func_all_multi %>% mutate(outcome = "GPA")

saveRDS(df_smd_cov_func_sum_multi, "Output/DML/Covariate_Balancing/summary_covariate_balancing_multi.rds")
saveRDS(df_smd_cov_func_all_multi, "Output/DML/Covariate_Balancing/asdm_covariate_balancing_multi.rds")


#### Covariate Balancing: Plot ####
#+++++++++++++++++++++++++++++++++#

plot_multi_cov_bal_al <- list()
i <- 0
for (outcome_var_sel in c("grades")) {
  df_smd_plot_multi <- df_smd_cov_func_all_multi %>% filter(outcome == outcome_var_sel)
  i <- i + 1
  plot_multi_cov_bal_al[[outcome_var_sel]] <- ggplot() +
    geom_area(data = df_smd_plot_multi %>% arrange(desc(SD_before)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_multi)),
              aes(x = var_num, y = SD_before, fill = "Before")) +
    geom_area(data = df_smd_plot_multi %>% arrange(desc(SD_after)) %>% 
                mutate(var_num = 1:nrow(df_smd_plot_multi)),
              aes(x = var_num, y = SD_after_all, fill = "After All")) + 
    scale_fill_manual(" ", values = c(Before = "grey20", After = "grey90", `After All` = "grey95")) +
    ylab("ASDM") + xlab("Rank from highest to lowest ASDM") +
    ggtitle(str_to_title(outcome_var_sel)) + 
    theme_bw() + 
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0.5, size = 22),
          axis.text = element_text(size = 18), axis.title = element_text(size = 20),
          legend.text = element_text(size = 18), legend.title = element_text(size = 20)) +
    guides(fill = guide_legend(title = "ASDM")) 
}

ggsave("Output/DML/Covariate_Balancing/plot_cov_balance_grades_multi.png", plot_multi_cov_bal_al$grades)


#### Main Drivers of selection ####
#+++++++++++++++++++++++++++++++++#

# 30 variables with highest ASDM are reported
df_main_drivers_multi <- df_smd_cov_func_all_multi %>% 
  arrange(desc(SD_before)) %>% 
  head(50)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



