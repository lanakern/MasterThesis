#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ASSESSMENT OF COVARIATE BALANCE AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# by Lana Kern
#++ 
# In this file, covariate balancing and the main drivers of selection are
# assessed by calculating mean standardized differences following Yang et al (2016)
# and Knaus (2018).
#++ 
# Sources:
# -> https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# -> https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html
# -> Thoemmes and Kim (2011)
# -> Knaus (2018)
#++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Prepare Data ####
#++++++++++++++++++++#

# load covariate balance results from post-lasso (MICE can be identified via the list)
dml_result_all <- 
  readRDS("Output/DML/binary_postlasso_all_controlssameoutcome_weekly_down_extradrop.rds")
#dml_result_all[["MICE"]]$cov_balance[["MICE"]][["REP"]][["FOLD"]]


# iterate over mice data frames
df_smd_binary_all <- data.frame()
df_smd_all_binary_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  # load and standardize original data
  data_dml_binary <- 
    readRDS(paste0("Data/Prep_11/prep_11_dml_binary_all_stand_weekly_down_extradrop_mice", mice_data_sel, ".rds"))
  data_dml_binary <- data_dml_binary %>%
    recipe(.) %>%
    update_role(treatment_sport, new_role = "outcome") %>%
    step_normalize(all_of(data_dml_binary %>% dplyr::select(-c("treatment_sport")) %>% colnames())) %>%
    prep() %>%
    bake(new_data = NULL)
  
  
  # append predictions across repetitions and fold in dplyr::selected mice data frame
  df_pred_binary_all <- data.frame()
  df_iterate <- data.frame("Rep" = c(1, 1, 2, 2), "Fold" = c(1, 2, 1, 2))
  for (rep_fold in 1:nrow(df_iterate)) {
    df_iterate_sel <- df_iterate[rep_fold, ]
    df_pred_binary <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$pred
    df_pred_binary_all <- rbind(df_pred_binary_all, df_pred_binary)
  }
  
  # append columns: only columns in all data sets are used
  df_controls_binary_all <- data.frame()
  controls_sel_binary_all <- c()
  for (rep_fold in 1:nrow(df_iterate)) {
    
    # extract covariates
    df_iterate_sel <- df_iterate[rep_fold, ]
    df_controls_binary <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$controls
    
    # keep only covariates which are in all data frames
    controls_sel_binary <- colnames(df_controls_binary)
    if (rep_fold != 1) {
      controls_sel_binary_all <- controls_sel_binary[controls_sel_binary %in% controls_sel_binary_all]
    } else {
      controls_sel_binary_all <- controls_sel_binary
    }
    df_controls_binary <- df_controls_binary[, controls_sel_binary_all]
    if (rep_fold != 1) {
      df_controls_binary_all <- df_controls_binary_all[, controls_sel_binary_all]
    } else {
      df_controls_binary_all <- df_controls_binary_all
    }
    df_controls_binary_all <- rbind(df_controls_binary_all, df_controls_binary)
  }
  
  controls_sel <- colnames(df_controls_binary_all)
  
  
  #### Calculate weights ####
  #+++++++++++++++++++++++++#
  
  func_weights_normalize <- function(w) {
    w <- w / sum(w) * length(w)
    return(w)
  }
  
  
  # extract and prepare information
  prob_score <- as.matrix(df_pred_binary_all$m)
  prob_score <- cbind(1 - prob_score, prob_score)
  y <- as.matrix(df_pred_binary_all$outcome, ncol = 1) 
  t <- df_pred_binary_all$treatment %>% as.character() %>% as.numeric() 
  t <- cbind(1 - t, t)
  x <- df_controls_binary_all %>% dplyr::select(-c(Fold, Repetition)) %>% mutate(intercept = 1) %>% as.matrix()
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
  
  # Calculate weight matrix
  w_mat <- w_ipw + w_ols - w_adj
  weights <- rowSums(w_mat)
  
  
  ## ASDM: Apply Own Function ##
  #++++++++++++++++++++++++++++#
  
  list_smd_binary <- func_cov_balance("binary", data_dml_binary, controls_sel, weights)
  df_smd_binary <- list_smd_binary$smd_summary %>% mutate(MICE = mice_data_sel)
  df_smd_binary_all <- rbind(df_smd_binary_all, df_smd_binary)
  df_smd_all_binary <- list_smd_binary$smd_values %>% mutate(MICE = mice_data_sel)
  df_smd_all_binary_all <- rbind(df_smd_all_binary_all, df_smd_all_binary)
}


#### Covariate Balancing: Summary ####
#++++++++++++++++++++++++++++++++++++#

df_smd_binary_all %>% 
  group_by(treatment_setting, adjustment, controls) %>% 
  summarize_all(mean) %>% 
  dplyr::select(-MICE)
  

#### Covariate Balancing: Cobalt Function ####
#++++++++++++++++++++++++++++++++++++++++++++#
  
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# Before matching: "Un"; after matching: "Adj"
D <- df_pred_binary_all$treatment %>% as.character() %>% as.numeric() 
balance <- bal.tab(
  as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights, method = "weighting",
  s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
  disp.v.ratio = TRUE, disp.ks = TRUE, 
  un = TRUE, # display statistics also for before DML
  continuous = "std", binary = "std" # also standardized binary covariates
  # which.treat = .all # for multivalued treatment: pairwise comparisons
)
df_smd_after_binary <- balance$Balance %>% 
  dplyr::select(Diff.Un, Diff.Adj) %>% 
  mutate(SD_before = abs(Diff.Un)*100, SD_after = abs(Diff.Adj)*100) %>%
  dplyr::select(-c(Diff.Un, Diff.Adj))

df_smd_after_binary %>% summarize_all(mean) 
  
  
#### Covariate Balancing: Plot ####
#+++++++++++++++++++++++++++++++++#
  
ggplot() +
  geom_area(data = df_smd_all_binary %>% arrange(desc(SD_before)) %>% 
              mutate(var_num = 1:nrow(df_smd_all_binary)),
            aes(x = var_num, y = SD_before, fill = "Before")) +
  geom_area(data = df_smd_all_binary %>% arrange(desc(SD_after)) %>% 
              mutate(var_num = 1:nrow(df_smd_all_binary)),
            aes(x = var_num, y = SD_after, fill = "After")) +
  scale_fill_manual(" ", values = c(Before = "grey20", After = "grey90")) +
  ylab("ASDM") + xlab("Rank from highest to lowest ASDM") +
  theme_bw() + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "ASDM")) 
  
  
#### Main Drivers of selection ####
#+++++++++++++++++++++++++++++++++#
  
# extract 30 variables with highest ASDM 
df_main_drivers_binary <- df_smd_all_binary_all %>% 
  dplyr::select(-MICE) %>%
  group_by(control_var) %>% 
  summarize_all(mean) %>% 
  arrange(desc(SD_before)) %>% 
  head(20)


# descriptive statistics for them
df_descr <- readRDS("Data/Prep_8/prep_8_plausi_weekly_down_extradrop_mice1.rds")
df_descr_binary <- df_descr %>% dplyr::select(treatment_sport, all_of(df_main_drivers_binary$control_var))

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






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


dml_result_all <- 
  readRDS("Output/DML/multi_postlasso_all_controlssameoutcome_weekly_down_extradrop.rds")
#dml_result_all[["MICE"]]$cov_balance[["MICE"]][["REP"]][["FOLD"]]


# iterate over mice data frames
df_smd_multi_all <- data.frame()
df_smd_all_multi_all_before <- data.frame()
df_smd_all_multi_all_after <- data.frame()
df_smd_all_multi_all_after_all <- data.frame()
for (mice_data_sel in 1:5) {
  
  # load and standardize original data
  data_dml_multi <- 
    readRDS(paste0("Data/Prep_11/prep_11_dml_multi_all_stand_weekly_down_extradrop_mice", mice_data_sel, ".rds"))
  data_dml_multi <- data_dml_multi %>%
    recipe(.) %>%
    update_role(treatment_sport_freq, new_role = "outcome") %>%
    step_normalize(all_of(data_dml_multi %>% dplyr::select(-treatment_sport_freq) %>% colnames())) %>%
    prep() %>%
    bake(new_data = NULL)
  
  
  # append predictions across repetitions and fold in dplyr::selected mice data frame
  df_pred_multi_all <- data.frame()
  df_iterate <- data.frame("Rep" = c(1, 1, 2, 2), "Fold" = c(1, 2, 1, 2))
  for (rep_fold in 1:nrow(df_iterate)) {
    df_iterate_sel <- df_iterate[rep_fold, ]
    df_pred_multi <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$pred
    df_pred_multi_all <- rbind(df_pred_multi_all, df_pred_multi)
  }
  
  # append columns: only columns in all data sets are used
  df_controls_multi_all <- data.frame()
  controls_sel_multi_all <- c()
  for (rep_fold in 1:nrow(df_iterate)) {
    
    # extract covariates
    df_iterate_sel <- df_iterate[rep_fold, ]
    df_controls_multi <- dml_result_all[[mice_data_sel]]$cov_balance[[mice_data_sel]][[df_iterate_sel$Rep]][[df_iterate_sel$Fold]]$controls
    
    # keep only covariates which are in all data frames
    controls_sel_multi <- colnames(df_controls_multi)
    if (rep_fold != 1) {
      controls_sel_multi_all <- controls_sel_multi[controls_sel_multi %in% controls_sel_multi_all]
    } else {
      controls_sel_multi_all <- controls_sel_multi
    }
    df_controls_multi <- df_controls_multi[, controls_sel_multi_all]
    if (rep_fold != 1) {
      df_controls_multi_all <- df_controls_multi_all[, controls_sel_multi_all]
    } else {
      df_controls_multi_all <- df_controls_multi_all
    }
    df_controls_multi_all <- rbind(df_controls_multi_all, df_controls_multi)
  }
  
  controls_sel <- colnames(df_controls_multi_all)
  
  
  #### Calculate weights ####
  #+++++++++++++++++++++++++#
  
  func_weights_normalize <- function(w) {
    w <- w / sum(w) * length(w)
    return(w)
  }
  
  
  # extract and prepare information
  prob_score <- as.matrix(df_pred_multi_all %>% dplyr::select(m1, m2, m3))
  y <- as.matrix(df_pred_multi_all$outcome, ncol = 1) 
  t <- df_pred_multi_all$treatment %>% as.character() %>% as.numeric() 
  t <- cbind(ifelse(t == 1, 1, 0), ifelse(t == 2, 1, 0), ifelse(t == 3, 1, 0))
  x <- df_controls_multi_all %>% dplyr::select(-c(Fold, Repetition)) %>% mutate(intercept = 1) %>% as.matrix()
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
  
  # Calculate weight matrix
  w_mat <- w_ipw + w_ols - w_adj
  weights <- rowSums(w_mat)
  
  
  ## ASDM: Apply Own Function ##
  #++++++++++++++++++++++++++++#
  
  list_smd_multi <- func_cov_balance("multi", data_dml_multi, controls_sel, weights)
  
  # summary
  df_smd_multi <- list_smd_multi$smd_summary %>% mutate(MICE = mice_data_sel)
  df_smd_multi_all <- rbind(df_smd_multi_all, df_smd_multi)
  
  # all
  df_smd_all_multi_before <- list_smd_multi$smd_values$before %>% mutate(MICE = mice_data_sel)
  df_smd_all_multi_all_before <- rbind(df_smd_all_multi_all_before, df_smd_all_multi_before)
  
  df_smd_all_multi_after <- list_smd_multi$smd_values$after_sel %>% mutate(MICE = mice_data_sel)
  df_smd_all_multi_all_after <- rbind(df_smd_all_multi_all_after, df_smd_all_multi_after)
  
  df_smd_all_multi_after_all <- list_smd_multi$smd_values$after_all %>% mutate(MICE = mice_data_sel)
  df_smd_all_multi_all_after_all <- rbind(df_smd_all_multi_all_after_all, df_smd_all_multi_after_all)
}


#### Covariate Balancing: Summary ####
#++++++++++++++++++++++++++++++++++++#

df_smd_multi_all %>% 
  group_by(treatment_setting, adjustment, controls) %>% 
  summarize_all(mean) %>% 
  dplyr::select(-MICE)


#### Covariate Balancing: Cobalt Function ####
#++++++++++++++++++++++++++++++++++++++++++++#

# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# Before matching: "Un"; after matching: "Adj"
D <- df_pred_multi_all$treatment %>% as.character() %>% as.numeric() 
balance <- bal.tab(
  as.data.frame(x), treat = D, stats = "mean.diffs", weights = weights, method = "weighting",
  s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
  disp.v.ratio = TRUE, disp.ks = TRUE, 
  un = TRUE, # display statistics also for before DML
  continuous = "std", binary = "std" # also standardized binary covariates
  # which.treat = .all # for multivalued treatment: pairwise comparisons
)
df_smd_after_multi <- balance$Balance %>% 
  dplyr::select(Diff.Adj) %>% 
  mutate(SD_after = abs(Diff.Adj)*100) %>%
  dplyr::select(-c(Diff.Adj))

df_smd_after_multi %>% summarize_all(mean) 


#### Covariate Balancing: Plot ####
#+++++++++++++++++++++++++++++++++#

df_smd_all_multi_before <- df_smd_all_multi_before %>%
  mutate(SD = pmax(SD_1, SD_2, SD_3))

df_smd_all_multi_after_all <- df_smd_all_multi_after_all %>%
  mutate(SD = pmax(SD_1, SD_2, SD_3))

ggplot() +
  geom_area(data = df_smd_all_multi_before %>% arrange(desc(SD)) %>% 
              mutate(var_num = 1:nrow(df_smd_all_multi_before)),
            aes(x = var_num, y = SD, fill = "Before")) +
  geom_area(data = df_smd_all_multi_after_all %>% arrange(desc(SD)) %>% 
              mutate(var_num = 1:nrow(df_smd_all_multi_after_all)),
            aes(x = var_num, y = SD, fill = "After")) +
  scale_fill_manual(" ", values = c(Before = "grey20", After = "grey90")) +
  xlab("ASDM") + ylab("Rank from highest to lowest ASDM") +
  theme_bw() + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "ASDM")) 


#### Main Drivers of selection ####
#+++++++++++++++++++++++++++++++++#

# 30 variables with highest ASDM are reported
df_main_drivers_multi <- df_smd_all_multi_before %>% 
  dplyr::select(-MICE) %>%
  group_by(control_var) %>% 
  summarize_all(mean) %>% 
  arrange(desc(SD)) %>% 
  head(30)

df_descr_multi <- df_descr %>% dplyr::select(treatment_sport_freq, all_of(df_main_drivers_multi$control_var))

df_main_drivers_binary <- left_join(
  df_main_drivers_multi, 
  df_descr_multi %>%
    group_by(treatment_sport_freq) %>%
    summarize_all(mean) %>%
    gather(-treatment_sport_freq, key = "control_var", value = "mean") %>%
    spread(key = treatment_sport_freq, value = mean),
  by = "control_var"
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



