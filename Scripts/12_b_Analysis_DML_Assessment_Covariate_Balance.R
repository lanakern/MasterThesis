#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ASSESSMENT OF COVARIATE BALANCE AND MAIN DRIVERS OF SELECTION ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# by Lana Kern
#++ 
# In this file, covariate balancing and the main drivers of selection are
# assessed by calculating standardized differences following Yang et al (2016)
# and Knaus (2018).
# -> Standardization factor is the same before and after matching to ensure
# changes in the mean difference are not confounded by changes in the 
# standard deviation of the covariate. 
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
    step_normalize(all_of(data_dml_binary %>% select(-treatment_sport) %>% colnames())) %>%
    prep() %>%
    bake(new_data = NULL)
  
  
  # append predictions across repetitions and fold in selected mice data frame
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
  x <- df_controls_binary_all %>% select(-c(Fold, Repetition)) %>% mutate(intercept = 1) %>% as.matrix()
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
    XXtX <- x[t[,i] == 1,] %*% inv(XtX)
    
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
  select(-MICE)
  

#### Covariate Balancing: Cobalt Function ####
#++++++++++++++++++++++++++++++++++++++++++++#
  
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html
# Before matching: "Un"; after matching: "Adj"
D <- df_pred_binary_all$treatment %>% as.character() %>% as.numeric() 
balance <- bal.tab(
  as.data.frame(x), treat = D, weights = weights, method = "weighting",
  s.d.denom = "pooled", # pooled standard deviation (most appropriate for ATE; for ATTE: "treated")
  disp.v.ratio = TRUE, disp.ks = TRUE, 
  un = TRUE, # display statistics also for before DML
  binary = "std" # also standardized binary covariates
  # which.treat = .all # for multivalued treatment: pairwise comparisons
)
df_smd_after_binary <- balance$Balance %>% select(Diff.Adj) %>% mutate(SD = abs(Diff.Adj))
  
  
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
  xlab("ASDM") + ylab("Rank from highest to lowest ASDM") +
  theme_bw() + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "ASDM")) 
  
  
#### Main Drivers of Selection ####
#+++++++++++++++++++++++++++++++++#
  
# 20 variables with highest ASDM are reported
df_smd_all_binary_all %>% 
  select(-MICE) %>%
  group_by(control_var) %>% 
  summarize_all(mean) %>% 
  arrange(desc(SD_before)) %>% 
  head(20)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Prepare Data ####
#++++++++++++++++++++#

# load covariate balance results from post-lasso (MICE can be identified via the list)
dml_result_all <- 
  readRDS("Output/DML/multi_postlasso_all_controlssameoutcome_weekly_down_extradrop.rds")
#dml_result_all[["MICE"]]$cov_balance[["MICE"]][["REP"]][["FOLD"]]

# append predictions across repetitions and fold in selected mice data frame
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
    controls_sel_multi_all <- controls_sel_binary[controls_sel_binary %in% controls_sel_multi_all]
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
prob_score <- as.matrix(df_pred_binary_all$m)
prob_score <- cbind(1 - prob_score, prob_score)
y <- as.matrix(df_pred_binary_all$outcome, ncol = 1) 
t <- df_pred_binary_all$treatment %>% as.character() %>% as.numeric() 
t <- cbind(1 - t, t)
x <- df_controls_binary_all %>% select(-c(Fold, Repetition)) %>% mutate(intercept = 1) %>% as.matrix()
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
  XXtX <- x[t[,i] == 1,] %*% inv(XtX)
  
  for (r in 1:n) {
    w_ol <- matrix(0, n, 1)
    XXtXX <- XXtX %*% xx[r,]
    w_ol[t[,i] == 1,] <- unname(XXtXX[, 1])
    w_ols[,i] <- w_ols[,i] + w_ol
    w_adj[,i] <- w_adj[,i] + w_ol * w_ipw[r,i]
  }
} # End t

# Calculate weight matrix
w_mat <- w_ipw + w_ols - w_adj
weights <- rowSums(w_mat)




## MULTIVALUED TREATMENT SETTING ##
#+++++++++++++++++++++++++++++++++#

data_dml_multi <- readRDS("Output/DML/multi_postlasso_all_controlssameoutcome_weekly_down_extradrop.rds")
controls_drop <- data_dml_multi %>% select(starts_with("treatment"), starts_with("outcome"), group) %>% colnames()
#controls_drop <- controls_drop[!str_detect(controls_drop, "na")]
controls_drop <- controls_drop[!str_detect(controls_drop, "treatment_sport_freq$")]

# calculate absolute difference in means
df_diff_1 <- abs(diff(as.matrix(
  data_dml_multi %>% 
    group_by(treatment_sport_freq) %>%
  summarise(across(everything(), mean)) %>%
    filter(treatment_sport_freq != 1)  %>%
  select(-c(all_of(controls_drop), "treatment_sport_freq"))
))) %>% as.matrix()

df_diff_2 <- abs(diff(as.matrix(
  data_dml_multi %>% 
    group_by(treatment_sport_freq) %>%
    summarise(across(everything(), mean)) %>%
    filter(treatment_sport_freq != 2)  %>%
    select(-c(all_of(controls_drop), "treatment_sport_freq"))
))) %>% as.matrix()

df_diff_3 <- abs(diff(as.matrix(
  data_dml_multi %>% 
    group_by(treatment_sport_freq) %>%
    summarise(across(everything(), mean)) %>%
    filter(treatment_sport_freq != 3)  %>%
    select(-c(all_of(controls_drop), "treatment_sport_freq"))
))) %>% as.matrix()


# calculate factor
df_meanvar <- data_dml_multi %>% 
  group_by(treatment_sport_freq) %>%
  summarise(across(everything(), var)) %>%
  select(-c(all_of(controls_drop), "treatment_sport_freq")) %>%
  summarise(across(everything(), mean)) %>% 
  summarise(across(everything(), sqrt)) %>%
  as.matrix()


# fraction
df_smd_1 <- (df_diff_1 / df_meanvar) * 100
df_smd_2 <- (df_diff_2 / df_meanvar) * 100
df_smd_3 <- (df_diff_3 / df_meanvar) * 100
df_smd_bef_multi <- rbind(df_smd_1, df_smd_2)
df_smd_bef_multi <- rbind(df_smd_bef_multi, df_smd_3)
df_smd_bef_multi <- apply(df_smd_bef_multi, 2, function(x) max(x, na.rm = TRUE)) # maximum SD 
df_smd_bef_multi <- data.frame("SD" = df_smd_bef_multi)

# descriptives
num_cols <- ncol(data_dml_multi) - length(controls_drop)
df_smd_all <- rbind(df_smd_all, data.frame(
  "treatment_setting" = "multi", "adjustment" = "before", 
  "min" = min(df_smd_bef_multi$SD), "max" = max(df_smd_bef_multi$SD), 
  "mean" = mean(df_smd_bef_multi$SD), "median" = median(df_smd_bef_multi$SD),
  "num_cov_smd_20" = sum(df_smd_bef_multi > 20), 
  "num_cov_smd_10" = sum(df_smd_bef_multi > 10), 
  "num_cov_smd_5" = sum(df_smd_bef_multi > 5),
  "perc_cov_smd_20" = sum(df_smd_bef_multi > 20) / num_cols, 
  "perc_cov_smd_10" = sum(df_smd_bef_multi > 10) / num_cols, 
  "perc_cov_smd_5" = sum(df_smd_bef_multi > 5) / num_cols
))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_cov_bal_binary <- readRDS("TEST_COV_BAL.rds")
data_cov_bal_binary <- data_cov_bal_binary[[1]]

func_weights_normalize <- function(w) {
  w <- w / sum(w) * length(w)
  return(w)
}


# extract and prepare information
prob_score <- as.matrix(data_cov_bal_binary$pred$m)
prob_score <- cbind(1 - prob_score, prob_score)
y <- as.matrix(data_cov_bal_binary$pred$outcome, ncol = 1) 
t <- data_cov_bal_binary$pred$treatment %>% as.character() %>% as.numeric() 
t <- cbind(1 - t, t)
x <- data_cov_bal_binary$controls %>% select(-c(Fold, Repetition)) %>% as.matrix()
n <- nrow(t)
num_t <- ncol(t)

# Predict y's
w_ipw <- matrix(0, n, num_t)
w_ols <- matrix(0, n, num_t)
w_adj <- matrix(0, n, num_t)

for (i in 1:num_t) {
  xx <- add_intercept(x)

  # IPW weights
  w_ipw[,i] <- as.matrix(t[,i] / prob_score[,i], ncol = 1)
  w_ipw[,i] <- func_weights_normalize(w_ipw[,i])
  
  # Get X'X for treated
  XtX <- crossprod(xx[t[,i] == 1,])
  # Get X(X'X)-1 for treated
  XXtX <- xx[t[,i] == 1,] #%*% inv(XtX)
  
  for (r in 1:n) {
    w_ol <- matrix(0, n, 1)
    XXtXX <- XXtX %*% xx[r,]
    w_ol[t[,i] == 1,] <- unname(XXtXX[, 1])
    w_ols[,i] <- w_ols[,i] + w_ol
    w_adj[,i] <- w_adj[,i] + w_ol * w_ipw[r,i]
  }
} # End t

# Calculate weight matrix
w_mat <- w_ipw + w_ols - w_adj
weights <- rowSums(w_mat)




D <- data_cov_bal_binary$pred$treatment %>% as.character() %>% as.numeric() 
balance <- bal.tab(as.data.frame(x), treat = D, weights = weights, method = "weighting",
                   s.d.denom = "pooled", disp.v.ratio = TRUE, disp.ks = TRUE, un = TRUE)
df_smd_after_binary <- balance$Balance %>% select(Diff.Adj) %>% mutate(SD = abs(Diff.Adj))

num_cols <- ncol(x)
df_smd_all <- rbind(df_smd_all, data.frame(
  "treatment_setting" = "binary", "adjustment" = "after", 
  "min" = min(df_smd_after_binary$SD), "max" = max(df_smd_after_binary$SD), 
  "mean" = mean(df_smd_after_binary$SD), "median" = median(df_smd_after_binary$SD),
  "num_cov_smd_20" = sum(df_smd_after_binary$SD > 20), 
  "num_cov_smd_10" = sum(df_smd_after_binary$SD > 10), 
  "num_cov_smd_5" = sum(df_smd_after_binary$SD > 5),
  "perc_cov_smd_20" = sum(df_smd_after_binary$SD > 20) / num_cols, 
  "perc_cov_smd_10" = sum(df_smd_after_binary$SD > 10) / num_cols, 
  "perc_cov_smd_5" = sum(df_smd_after_binary$SD > 5) / num_cols
))



# IPW weights
w_ipw <- as.matrix(t[,1] / prob_score[, 1], ncol = 1)
w_ipw <- func_weights_normalize(w_ipw)

# X'X or treated
XtX <- crossprod(x[t[,1] == 1,])
# Get X(X'X)-1 for treated
XXtX <- x[t[,i] == 1,] %*% inv(XtX)

for (r in 1:n) {
  if (cs_i[r] == TRUE) {
    w_ol <- matrix(0,n,1)
    w_ol[t[,1] == 1,] <- XXtX %*% x[r,]
    w_ols[,1] <- w_ols[,1] + w_ol
    w_adj[,1] <- w_adj[,1] + w_ol * w_ipw[r,1]
  }
}

# Calculate weight matrix
w_mat <- w_ipw + w_ols - w_adj



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIVALUED TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# for first MICE data set and first repetition 
  ## data frame containing outcome, prediction, and treatment
df_dml_load_multi <- 
  readRDS(paste0("Output/DML/multi_postlasso_all_stand_controlssameoutcome_weekly_down_extradrop.rds"))
df_pred <- df_dml_load_multi[[1]]$pred %>% filter(Repetition == 1)
p_mat <- df_pred %>% select(m1, m2, m3)

  ## load control variables
load_data <- paste0(
  "Data/Prep_11/prep_11_dml_multi_all_weekly_down_yes_mice1.rds", model_type, "_", 
  "_", treatment_def, "_", treatment_repl, extra_act_save, "_mice", mice_data_sel, ".rds"
)

load_data <- str_replace(load_data, "_level", "") # drop level

x <- data_dml %>% select(-all_of(controls_drop)) %>% mutate(intercept = 1) %>% as.matrix()

# list with covariates used
# ADJUST MODEL: m1, m2, m3
df_coef <- df_dml_load_multi[[1]]$coef %>% filter(Repetition == 1, term != "(Intercept)")
cov_list <- list()
cov_list[[1]] <- df_coef %>% filter(model == "m") %>% pull(term)
cov_list[[2]] <- as.list(df_coef %>% filter(model == "m") %>% pull(term))
cov_list[[3]] <- as.list(df_coef %>% filter(model == "m") %>% pull(term))

# extract and prepare information
y <- as.matrix(df_pred$outcome, ncol=1) 
t <- data_dml %>% 
  select(treatment_sport_freq_weekly_atleast, treatment_sport_freq_monthly_less, treatment_sport_freq_never) %>%
  as.matrix()
n <- nrow(x)
num_t <- length(unique(df_pred$treatment))
y <- as.matrix(y,ncol=1)
if (is.null(cs_i)) rep(TRUE,n)

# Predict y's
w_ipw <- matrix(0,n,num_t)
w_ols <- matrix(0,n,num_t)
w_adj <- matrix(0,n,num_t)


# IPW weights
w_ipw[cs_i,i] <- as.matrix(t[cs_i,i] / p_mat[cs_i,i],ncol=1)
w_ipw[cs_i,i] <- norm_w_to_n(w_ipw[cs_i,i,drop=F])

# X'X or treated
XtX <- crossprod(x[t[,i]==1,])
# Get X(X'X)-1 for treated
XXtX <- x[t[,i]==1,] %*% solvex(XtX)


for (i in 1:num_t) {
  xx <- add_intercept(x[,nm_list[[i]],drop=FALSE])
  # Remove multicollinear
  coef <- stats::glm.fit(xx[t[,i]==1,],y[t[,i]==1,])$coefficients
  # Check for NA coefficients
  if (any(is.na(coef)) == TRUE) {
    xx <- xx[,!is.na(coef),drop=F]
  }
  
  # IPW weights
  w_ipw[cs_i,i] <- as.matrix(t[cs_i,i] / p_mat[cs_i,i],ncol=1)
  w_ipw[cs_i,i] <- norm_w_to_n(w_ipw[cs_i,i,drop=F])
  
  # Get X'X for treated
  XtX <- crossprod(xx[t[,i]==1,])
  # Get X(X'X)-1 for treated
  XXtX <- xx[t[,i]==1,] %*% inv(XtX)
  
  for (r in 1:n) {
    if (cs_i[r]==TRUE) {
      w_ol <- matrix(0,n,1)
      w_ol[t[,i]==1,] <- XXtX %*% x[r,]
      w_ols[,i] <- w_ols[,i] + w_ol
      w_adj[,i] <- w_adj[,i] + w_ol * w_ipw[r,i]
    }
  }
} # End t

# Calculate weight matrix
w_mat <- w_ipw + w_ols - w_adj