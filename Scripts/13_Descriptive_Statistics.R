#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# FOR MOST IMPORTANT PREDICTORS?!?! Because union are too many.

#+++
# by Lana Kern
#+++
# In this file, descriptive statistics are made for all predictors that are 
# not set to 0 in the lasso regression, i.e., those that are used in the
# post-lasso model (-> union over lasso treatment & outcome predictors).
# Those descriptives are only made for the main model.
#+++


# load data
dml_postlasso <- 
  readRDS("Output/DML/Estimation/Grades/binary_grades_postlasso_all_controlssameoutcome_weekly_down_extradrop_all_notreatmentoutcomelags_endogyes_trimming0.01_K4-2_Rep5.rds")

# extract predictors
postlasso_coef <- c()
for (mice_sel in 1:5) {
  postlasso_coef <- c(postlasso_coef, dml_postlasso[[mice_sel]]$coef$term)
}
postlasso_coef <- unique(postlasso_coef)
postlasso_coef <- postlasso_coef[!postlasso_coef %in% "(Intercept)"]

