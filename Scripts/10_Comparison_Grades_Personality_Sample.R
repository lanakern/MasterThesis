#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Comparison between Grades and Personality Sample ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the GPA and personality samples are compared as well as
# number of students, observations, and controls are presented (also for
# the robustness checks)
#+++

cov_bal <- "_covbal"


#### IDENTICAL IDs IN GPA AND PERSONALITY ####
#++++++++++++++++++++++++++++++++++++++++++++#

# number of individuals who are only in one of the samples
data_grades_1 <- readRDS(paste0("Data/Grades/Prep_10/COMPARE_ID_GRADES.rds"))
data_personality_1 <- readRDS(paste0("Data/Personality/Prep_10/COMPARE_ID_PERSONALITY.rds"))

id_grades <- unique(data_grades_1$id_t)
num_id_grades <- length(id_grades)
id_pers <- unique(data_personality_1$id_t)
num_id_pers <- length(id_pers)

# number of individuals who are only in one sample
outersect <- function(x, y) {
  unique(sort(c(setdiff(x, y),setdiff(y, x))))
}
length(outersect(id_grades, id_pers))
length(outersect(id_grades, id_pers)) / num_id_grades
1 - length(outersect(id_grades, id_pers)) / num_id_grades # 81% identical
length(setdiff(unique(data_grades_1$id_t), unique(data_personality_1$id_t)))
length(setdiff(unique(data_personality_1$id_t), unique(data_grades_1$id_t)))

remove(data_grades_1)
remove(data_personality_1)
gc()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Comparison Students: Grades, Personality, Robustness Checks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# main grades
data_main_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_binary_all_all_down_extradrop", cov_bal, "_mice1.rds"))
print("MAIN GRADES:")
data_main_grades %>% group_by(treatment_sport) %>% count() %>% adorn_totals("row")
length(unique(data_main_grades$group))

data_main_grades_multi <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop", cov_bal, "_mice1.rds"))
print("MAIN GRADES:")
data_main_grades_multi %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")


# main personality
data_main_pers <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_all_down_extradrop", cov_bal, "_mice1_personality.rds"))
print("MAIN PERSONALITY:")
data_main_pers %>% group_by(treatment_sport) %>% count() %>% adorn_totals("row")

data_main_pers_multi <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_all_down_extradrop", cov_bal, "_mice1_personality.rds"))
print("MAIN PERSONALITY:")
data_main_pers_multi %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_main_pers_multi$group))

# robustness checks
data_rc1_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_no_extradrop", cov_bal, "_mice1.rds"))
print("RC1 GRADES - only first treatment period:")
data_rc1_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")

data_rc2_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down", cov_bal, "_mice1.rds"))
print("RC2 GRADES - no extra. act. required:")
data_rc2_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_rc2_grades$group))

data_rc5_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop", cov_bal, "_robustcheck_mice1.rds"))
print("RC5 GRADES - treatment and controls before outcome:")
data_rc5_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")



#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Number Predictors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# MAIN GRADES
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_all_down_extradrop_covbal_mice1.rds")
ncol_mice1 <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(-c(outcome_grade, treatment_sport, starts_with("outcome_grade_lag"), starts_with("treatment_sport_lag"), group))
ncol_mice1_all <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
ncol_mice1_int <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(contains(":")) %>% ncol()
ncol_mice1_polys <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(contains("_order")) %>% ncol()
ncol_mice1_nointpolys <- ncol_mice1_all - ncol_mice1_int - ncol_mice1_polys

print("MAIN MODEL")
print(paste("Total number of variables:", ncol_mice1))
print(paste("Total number of control variables:", ncol_mice1_all))
print(paste("Total number of interaction effects:", ncol_mice1_int))
print(paste("Total number of polynominals:", ncol_mice1_polys))
print(paste("Total number of control variables without interactions and polynominals:", ncol_mice1_nointpolys))
print(paste("Total number of control variables without interactions, polynominals, and lags:", 
            ncol_mice1_nointpolys - length(colnames(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>% 
                                                      dplyr::select(-contains(":"), -contains("order")) %>%
                                                      dplyr::select(ends_with("_lag"))))))
print(paste("Number of endogeneous variables without interactions:",
            round(mean(
              eval(parse(text =  paste0("prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%", vars_endogenous))) %>% 
                dplyr::select(-contains(":")) %>% dplyr::select(-contains("_order")) %>% colnames() %>% length()
            ))))
print(paste("Number of endogeneous variables with interactions:",
            round(mean(
              eval(parse(text =  paste0("prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%", vars_endogenous))) %>% 
                colnames() %>% length()
            ))
))



# RC 5 Grades
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_weekly_down_extradrop_robustcheck_mice1.rds")
ncol_mice1 <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(-c(outcome_grade, treatment_sport, starts_with("outcome_grade_lag"), starts_with("treatment_sport_lag"), group))
ncol_mice1_all <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
ncol_mice1_int <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(contains(":")) %>% ncol()
ncol_mice1_polys <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(contains("_order")) %>% ncol()
ncol_mice1_nointpolys <- ncol_mice1_all - ncol_mice1_int - ncol_mice1_polys


print("CONTROLS BEFORE OUTCOME")
print(paste("Total number of variables:", round(mean(c(ncol_mice1)))))
print(paste("Total number of control variables:", round(mean(c(ncol_mice1_all)))))
print(paste("Total number of interaction effects:", round(mean(c(ncol_mice1_int)))))
print(paste("Total number of polynominals:", round(mean(c(ncol_mice1_polys)))))
print(paste("Total number of control variables without interactions and polynominals:", ncol_mice1_nointpolys))
print(paste("Total number of control variables without interactions, polynominals, and lags:", 
            ncol_mice1_nointpolys - length(colnames(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>% 
                                                      dplyr::select(-contains(":"), -contains("order")) %>%
                                                      dplyr::select(ends_with("_lag"))))
))


# RC 1: No LVCF
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_all_no_extradrop_covbal_mice1.rds")
ncol_mice1 <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(-c(outcome_grade, treatment_sport, starts_with("outcome_grade_lag"), starts_with("treatment_sport_lag"), group))
ncol_mice1_all <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)

print("NO LVCF")
print(paste("Total number of variables:", round(mean(c(ncol_mice1)))))
print(paste("Total number of control variables:", round(mean(c(ncol_mice1_all)))))


# PERSONALITY
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- readRDS("Data/Personality/Prep_10/prep_10_dml_binary_all_all_down_extradrop_covbal_mice1_personality.rds")
ncol_mice1 <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1)
prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 <- prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1 %>%
  dplyr::select(-c(bigfive_neuroticism, treatment_sport, starts_with("treatment_sport_lag"), group))
ncol_mice1_all <- ncol(prep_10_dml_binary_allintpoly_weekly_down_extradrop_mice1) 

print(paste("Total number of variables:", round(mean(c(ncol_mice1)))))
print(paste("Total number of control variables:", round(mean(c(ncol_mice1_all)))))


