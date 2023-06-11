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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### IDENTICAL IDs IN GPA AND PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
1 - length(outersect(id_grades, id_pers)) / num_id_grades # 82% identical
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
length(unique(data_main_grades_multi$group))

# main personality
data_main_pers <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_binary_all_all_down_extradrop", cov_bal, "_mice1_personality.rds"))
print("MAIN PERSONALITY:")
data_main_pers %>% group_by(treatment_sport) %>% count() %>% adorn_totals("row")

data_main_pers_multi <- readRDS(paste0("Data/Personality/Prep_10/prep_10_dml_multi_all_all_down_extradrop", cov_bal, "_mice1_personality.rds"))
print("MAIN PERSONALITY:")
data_main_pers_multi %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_main_pers_multi$group))

# robustness checks
data_rc1a_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down", cov_bal, "_mice1.rds"))
print("RC1a GRADES - no extra. act. required:")
data_rc1a_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_rc1a_grades$group))

data_rc1b_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extrauni", cov_bal, "_mice1.rds"))
print("RC1b GRADES - active within uni:")
data_rc1b_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_rc1b_grades$group))

data_rc1c_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop", cov_bal, "_robustcheck_mice1.rds"))
print("RC1c GRADES - treatment and controls before outcome:")
data_rc1c_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_rc1c_grades$group))

data_rc1d_grades <- readRDS(paste0("Data/Grades/Prep_10/prep_10_dml_multi_all_all_down_extradrop", 
                                  cov_bal, "_robustcheck_controls_bef_all_mice1.rds"))
print("RC1d GRADES - controls before treatment and outcome:")
data_rc1d_grades %>% group_by(treatment_sport_freq) %>% count() %>% adorn_totals("row")
length(unique(data_rc1d_grades$group))



#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Number Predictors ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

# MAIN GRADES
data_main_grades_allintpoly <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_allintpoly_all_down_extradrop_covbal_mice1.rds")
ncol_mice1 <- ncol(data_main_grades_allintpoly)
data_main_grades_allintpoly <- data_main_grades_allintpoly %>%
  dplyr::select(-c(outcome_grade, treatment_sport, starts_with("outcome_grade_lag"), starts_with("treatment_sport_lag"), group))
ncol_mice1_all <- ncol(data_main_grades_allintpoly)
ncol_mice1_int <- data_main_grades_allintpoly %>%
  dplyr::select(contains(":")) %>% ncol()
ncol_mice1_polys <- data_main_grades_allintpoly %>%
  dplyr::select(contains("_order")) %>% ncol()
ncol_mice1_nointpolys <- ncol_mice1_all - ncol_mice1_int - ncol_mice1_polys

print("MAIN MODEL")
print(paste("Total number of variables:", ncol_mice1))
print(paste("Total number of control variables:", ncol_mice1_all))
print(paste("Total number of interaction effects:", ncol_mice1_int))
print(paste("Total number of polynominals:", ncol_mice1_polys))
print(paste("Total number of control variables without interactions and polynominals:", ncol_mice1_nointpolys))
print(paste("Total number of control variables without interactions, polynominals, and lags:", 
            ncol_mice1_nointpolys - length(colnames(data_main_grades_allintpoly %>% 
                                                      dplyr::select(-contains(":"), -contains("order")) %>%
                                                      dplyr::select(ends_with("_lag"))))))
print(paste("Number of endogeneous variables without interactions:",
            round(mean(
              eval(parse(text =  paste0("data_main_grades_allintpoly %>%", vars_endogenous))) %>% 
                dplyr::select(-contains(":")) %>% dplyr::select(-contains("_order")) %>% colnames() %>% length()
            ))))
print(paste("Number of endogeneous variables with interactions:",
            round(mean(
              eval(parse(text =  paste0("data_main_grades_allintpoly %>%", vars_endogenous))) %>% 
                colnames() %>% length()
            ))
))


# PERSONALITY
ncol_mice1 <- ncol(data_main_pers_multi)
data_main_pers_multi <- data_main_pers_multi %>%
  dplyr::select(-c(bigfive_neuroticism, "treatment_sport_freq", starts_with("treatment_sport_freq_lag"), group,
                   "treatment_sport_freq_never", "treatment_sport_freq_monthly_less", "treatment_sport_freq_weekly_atleast",
                   starts_with("treatment_sport_freq_source")))
ncol_mice1_all <- ncol(data_main_pers_multi) 
print("PERSONALITY SAMPLE:")
print(paste("Total number of variables:", round(mean(c(ncol_mice1)))))
print(paste("Total number of control variables:", round(mean(c(ncol_mice1_all)))))


# RCs
data_rc1c_grades %>% dplyr::select(-c(
  starts_with("outcome_grade"), "treatment_sport_freq", starts_with("treatment_sport_freq_lag"), group,
  "treatment_sport_freq_never", "treatment_sport_freq_monthly_less", "treatment_sport_freq_weekly_atleast",
  starts_with("treatment_sport_freq_source"))) %>%
  colnames() %>% length()

data_rc1d_grades %>% dplyr::select(-c(
  starts_with("outcome_grade"), "treatment_sport_freq", starts_with("treatment_sport_freq_lag"), group,
  "treatment_sport_freq_never", "treatment_sport_freq_monthly_less", "treatment_sport_freq_weekly_atleast",
  starts_with("treatment_sport_freq_source"))) %>%
  colnames() %>% length()

