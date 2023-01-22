#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the estimation samples are established. For instance,
# for estimation the ID_t needs to be removed and different confounding
# factors are used for different models-
#+++
# -> The resulting data frames are final data sets used for DML
#+++


#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#

# load data
if (cohort_prep == "controls_same_outcome") {
  data_load <- paste0("Data/Prep_9/prep_9_descr_", treatment_def, 
                      "_", treatment_repl, ".rds")
} else {
  data_load <- paste0("Data/Prep_9/prep_9_descr_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
}

data_final <- readRDS(data_load)

# correct data types
data_final <- data_final %>% type.convert(as.is = TRUE)

# drop ID_t, interview_date, etc. which is not used in the estimation
data_final <- data_final %>% 
  # instead of id enumerator is established which corresponds to which
  # group the observation belongs; this info is only used for sample splitting
  # in the cross-fitting procedure
  mutate(group = as.integer(factor(id_t,levels = unique(id_t))))  %>%
  select(-c(id_t, starts_with("interview_date"), treatment_period))

# ensure all character variables are dropped
data_final <- data_final[, !sapply(data_final, is.character)]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### BINARY TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_binary <- data_final 


## XGBoost ##
#+++++++++++#

# use only baseline variables
data_xgboost_raw <- eval(parse(text = paste('data_binary %>%', vars_baseline)))

# select on outcome (standardized vs. level)
data_xgboost <- data_xgboost_raw %>% select(-c(starts_with("outcome_grade_stand")))
data_xgboost_stand <- data_xgboost_raw %>% select(-c("outcome_grade", "outcome_grade_lag"))
                                        
# save data frame
saveRDS(data_xgboost, "Data/Prep_11/prep_11_final_data_binary_xgboost.rds")
saveRDS(data_xgboost_stand, "Data/Prep_11/prep_11_final_data_binary_xgboost_stand.rds")



## LASSO ##
#+++++++++#

## BASELINE ##

# select variables
data_lasso_base_raw <- eval(parse(text = paste('data_binary %>%', vars_baseline)))

# scale
cols_scale <- data_lasso_base_raw %>% select(-c(outcome_grade, outcome_grade_stand, treatment_sport)) %>% colnames()
data_lasso_base_raw <- data_lasso_base_raw %>%
  mutate(across(cols_scale, ~ scale(.)[, 1]))
data_lasso_base <- data_lasso_base_raw %>% select(-c(starts_with("outcome_grade_stand")))
data_lasso_base_stand <- data_lasso_base_raw %>% select(-c("outcome_grade", "outcome_grade_lag"))


# save data frames
saveRDS(data_lasso_base, "Data/Prep_11/prep_11_final_data_binary_lasso_base.rds")
saveRDS(data_lasso_base_stand, "Data/Prep_11/prep_11_final_data_binary_lasso_stand_base.rds")

## ALL ##
data_lasso <- data_binary %>% select(-c(starts_with("outcome_grade_stand")))
data_lasso_stand <- data_binary %>% select(-c("outcome_grade", "outcome_grade_lag"))

saveRDS(data_lasso, "Data/Prep_11/prep_11_final_data_binary_lasso.rds")
saveRDS(data_lasso_stand, "Data/Prep_11/prep_11_final_data_binary_lasso_stand.rds")

## ALL + INTERACTION + POLYNOMIALS ##

# create new data frame 
df_lasso_all <- data_binary 

#-- INTERACTION TERMS --#

# NO CENTERING
# interaction terms: to generate interaction terms all numeric variables
# need to be centered to mitigate multicollinearity (NOT ALL SOURCES AGREE)
  ## identify variables to be centered
  ## 1.) all numeric variables
cols_numeric_all <- names(unlist(lapply(df_lasso_all, class)[lapply(df_lasso_all, class) == "numeric"]))
  ## 2.) all numeric variables that are already centered from PCA (have mean zero)
cols_numeric_drop <- c()
for (cols_numeric_sel in cols_numeric_all) {
  mean_cols_numeric_sel <- 
    df_lasso_all %>% select(all_of(cols_numeric_sel)) %>% pull() %>% mean()
  
  if (round(mean_cols_numeric_sel) == 0) {
    cols_numeric_drop <- c(cols_numeric_drop, cols_numeric_sel)
  }
}
  ## 3.) keep only variables which need to be centered
cols_numeric <- cols_numeric_all[!cols_numeric_all %in% cols_numeric_drop]
  ## center variables
# df_lasso_all <- df_lasso_all %>%
#   mutate(across(all_of(cols_numeric), ~ . - mean(.)))

# generate interaction terms
  ## name vector with column names
lasso_all_names <- colnames(df_lasso_all)
names(lasso_all_names) <- paste0("V", 1:length(lasso_all_names))
names_interaction <- names(lasso_all_names) 
  ## generate interactions
df_interaction <- t(apply(df_lasso_all %>% select(-c(starts_with("treatment_sport") & starts_with("outcome_grade"))), 
                          1, combn, 2, prod))
  ## add column names
colnames(df_interaction) <- paste("V", combn(1:length(names_interaction), 2, paste, collapse = ":V"), sep = "")
df_interaction <- data.frame(df_interaction)
  ## rename
for (names_interaction_sel in names_interaction) {
  colnames(df_interaction) <- gsub(names_interaction_sel, unname(test_names[names(test_names) == names_interaction_sel]), colnames(test2))
}

df_lasso_all <- cbind(df_lasso_all, df_interaction)


#-- POLYNOMIALS --#
df_lasso_all <- df_lasso_all %>%
  mutate(across(all_of(cols_numeric_all), .fns = list("order2" = ~ .^2))) %>%
  mutate(across(all_of(cols_numeric_all), .fns = list("order3" = ~ .^3))) %>%
  mutate(across(all_of(cols_numeric_all), .fns = list("order4" = ~ .^4)))



# save data frames
saveRDS(df_lasso_all, "Data/Prep_11/prep_11_final_data_binary_lasso_all.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MULTIPLE TREATMENT SETTING ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

