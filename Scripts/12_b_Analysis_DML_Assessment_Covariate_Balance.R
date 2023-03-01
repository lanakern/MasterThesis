#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ASSESSMENT OF COVARIATE BALANCE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load data
data_dml_binary <- readRDS("Data/Prep_11/prep_11_dml_binary_all_stand_weekly_down_extradrop_mice1.rds")

# calculate weights #
#+++++++++++++++++++#

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
  XXtX <- xx[t[,i] == 1,] %*% inv(XtX)
  
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


## Apply function ##
#++++++++++++++++++#

df_smd_binary <- func_cov_balance("binary", data_dml_binary, weights)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



# ## WITH STANDARDIZED VARIABLES ##
# 
# # EXACTLY SAME RESULT
# 
# data_dml_binary <- data_dml_binary %>%
#   recipe(.) %>%
#   update_role(treatment_sport, new_role = "outcome") %>%
#   step_normalize(all_of(data_dml_binary %>% select(-treatment_sport) %>% colnames())) %>%
#   prep() %>%
#   bake(new_data = NULL)
# 
# 
# # calculate absolute difference in means
# df_diff <- abs(diff(as.matrix(
#   data_dml_binary %>% 
#     group_by(treatment_sport) %>%
#     summarise(across(everything(), mean)) %>%
#     select(-all_of(controls_drop))
# ))) %>% 
#   as.matrix()
# 
# 
# # calculate factor
# df_meanvar <- data_dml_binary %>% 
#   group_by(treatment_sport) %>%
#   summarise(across(everything(), var)) %>%
#   select(-all_of(controls_drop)) %>%
#   summarise(across(everything(), mean)) %>% 
#   summarise(across(everything(), sqrt)) %>%
#   as.matrix()
# 
# 
# # fraction
# df_smd <- (df_diff / df_meanvar) * 100
# 
# # descriptives
# max(df_smd) # maximum SD
# mean(df_smd) # mean SD
# 
# sum(df_smd > 10) # number of variables with SD > 10
# sum(df_smd > 5) # number of variables with SD > 5



## MULTIVALUED TREATMENT SETTING ##
#+++++++++++++++++++++++++++++++++#

data_dml_multi <- readRDS("Data/Prep_11/prep_11_dml_multi_all_stand_weekly_down_extradrop_mice1.rds")
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