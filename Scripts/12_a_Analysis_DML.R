#%%%%%%%%%%%%%%%%#
#### DoubleML ####
#%%%%%%%%%%%%%%%%#


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
# APE is biased since it does not account for endogeneity of participation.

ape <- data %>% filter(treatment_sport == 1) %>% pull(outcome_grade) %>% mean() -
  data %>% filter(treatment_sport == 0) %>% pull(outcome_grade) %>% mean()
print(ape)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### 1.) Baseline Regressions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Baseline regression is used for (post)-lasso and xgboost
set.seed(1234)
baseline_post_lasso <-
  func_double_ml(data = data, outcome = "outcome_grade", treatment = "treatment_sport",
                 group = "group", K = 5, S = 2, mlalgo = "postlasso")



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