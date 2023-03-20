#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Comparison between Grades and Personality Sample ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# number of individuals who are only in one of the samples
data_grades_1 <- readRDS("Data/Grades/Prep_8/prep_8_plausi_weekly_down_extradrop_mice1.rds")
data_personality_1 <- readRDS("Data/Personality/Prep_8/prep_8_plausi_weekly_down_extradrop_mice1_personality.rds")

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
1 - length(outersect(id_grades, id_pers)) / num_id_grades


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Comparison Data Sets ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_main_mice1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice1.rds")
data_main_mice2 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice2.rds")
data_main_mice3 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice3.rds")
data_main_mice4 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice4.rds")
data_main_mice5 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice5.rds")

setdiff(colnames(data_main_mice1), colnames(data_main_mice2))
setdiff(colnames(data_main_mice2), colnames(data_main_mice3))
setdiff(colnames(data_main_mice4), colnames(data_main_mice5))


# check all variables
summary(data_main_mice1$outcome_grade_lag)
summary(data_main_mice1$treatment_sport_lag)
data_main_mice1 %>% dplyr::select(starts_with("outcome")) %>% colnames()
data_main_mice1 %>% dplyr::select(starts_with("treatment")) %>% colnames()

data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("outcome"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("bigfive"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("treatment"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("interview"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("birth"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("health"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("educ"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("opinion_educ"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("academic"), starts_with("helpless"), starts_with("motivation"), starts_with("prof_expected"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("uni"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("extracurricular"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("interest"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("comp"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("emp"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("father"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("mother"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("parents"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("friends"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("personality"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("sibling"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("partner"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("child"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("living"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-starts_with("prof_expected"))
data_main_mice1 <- data_main_mice1 %>% dplyr::select(-c(starts_with("gender"), starts_with("age"), bilingual, kindergarden,
                                                migration, starts_with("religion"),
                                                academic, starts_with("helpless_grades"), starts_with("motivation_degree"),
                                                starts_with("satisfaction"), starts_with("social_integr"), starts_with("stress"),
                                                starts_with("status_prof"), starts_with("place_residence"), starts_with("na")))

