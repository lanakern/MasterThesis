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
