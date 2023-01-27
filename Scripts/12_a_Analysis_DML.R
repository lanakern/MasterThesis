#%%%%%%%%%%%%%%%%#
#### DoubleML ####
#%%%%%%%%%%%%%%%%#

# set seed for reproducible results
set.seed(1234)

# load
data <- readRDS("Data/Prep_11/prep_11_final_data_binary_lasso_base.rds")
data <- data %>% select(-c(ends_with("_lag")))


#%%%%%%%%%%%#
#### APE ####
#%%%%%%%%%%%#

# https://docs.doubleml.org/stable/examples/R_double_ml_pension.html
# (unconditional) Average Predictive Effect (APE) of doing sports on grades.
# This effect corresponds to the ATE if sport participation would be assigned to 
# individuals in an entirely randomized way.
# APE is a naive estimate of the ATE and biased since it does not account for 
# endogeneity of participation.
ape <- data %>% filter(treatment_sport == 1) %>% pull(outcome_grade) %>% mean() -
  data %>% filter(treatment_sport == 0) %>% pull(outcome_grade) %>% mean()
print(ape)


#%%%%%%%%%%%#
#### APO ####
#%%%%%%%%%%%#

# Average Potential Outcome (APO)
# "What is the expected outcome if everybody receives treatment?"



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 1.) No Control Variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_no_controls <- data %>% select(outcome_grade, treatment_sport, group)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 2.) Baseline Regressions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Baseline regression: 
# -> Small data sets (with small amount of predictors)
# -> binary treatment setting with weekly sport definition and downward replacement
# -> lasso (parameter tuning of lambda with 10-fold CV) 
# -> K = 5, S = 100, 

K_base <- 2
K_tuning_base <- 2
S_base <- 2
mlalgo_base <- "lasso"
trimming_base <- 0.01

dml_baseline_lasso_all <- list()

# iterate over the 5 MICE data sets
for (mice_data_sel in 1:2) {
  
  print(paste("Data Set", mice_data_sel))
  
  # load data
  data_lasso <- readRDS(paste0("Data/Prep_11/prep_11_dml_binary_base", "_mice", mice_data_sel, ".rds"))
  data_lasso <- data_lasso %>% select(-c(ends_with("_lag")))
  
  # run DML
  dml_baseline_lasso <- func_dml(
    data = data_lasso, outcome = "outcome_grade", treatment = "treatment_sport", group = "group", 
    K = K_base, K_tuning = K_tuning_base, S = S_base, mlalgo = mlalgo_base, trimming = trimming_base
  )
  
  # append to full data frame
  dml_baseline_lasso_all <- append(dml_baseline_lasso_all, list(dml_baseline_lasso))
}


# calculate pooled estimate
func_dml_pool_mice(dml_baseline_lasso_all, nrow(data_lasso), 2)




## Function ##
#++++++++++++#

data_func <- as.data.table(data)
colnames(data_func) <- str_replace_all(colnames(data_func), "\\(", "")
colnames(data_func) <- str_replace_all(colnames(data_func), "\\)", "")

data_func_model <- double_ml_data_from_data_frame(
  data_func, y_col = "outcome_grade", d_cols = "treatment_sport",
  x_cols = data_func %>% select(-c(outcome_grade, starts_with("treatment"), group)) %>% colnames()
  )
print(data_func_model)


ml_g <- lrn("regr.cv_glmnet", nfolds = 5, s = "lambda.min")
ml_m <- lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")

dml_irm_atte <- DoubleMLIRM$new(
  data_func_model, ml_g, ml_m, score = "ATTE", 
  dml_procedure = "dml1", n_folds = 5, n_rep = 2
  ) 
dml_irm_atte$fit()
print(dml_irm_atte)


dml_irm_atte$coef
dml_irm_atte$all_coef
median(dml_irm_atte$all_coef)

dml_irm_atte$all_se
dml_irm_atte$all_se[1,1]^2 + (dml_irm_atte$all_coef[1,1] - dml_irm_atte$coef)^2
dml_irm_atte$all_se[1,2]^2 + (dml_irm_atte$all_coef[1,2] - dml_irm_atte$coef)^2
sqrt(mean(c(0.0001312643 , 0.0001293884)))
dml_irm_atte$se


sqrt(var(dml_irm_atte$psi)/N)


#%%%%%%%%%%%%#
#### CATE ####
#%%%%%%%%%%%%#

# Conditional Average Treatment Effect (CATE)
# "What is the expected treatment effect for somebody with characteristics X = x?"
# Heterogeneous treatment effects?