#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Comparison Descriptives for Active and Inactive Individuals ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, descriptive statistics are made for three groups: sport participants,
# participants in any other extracurricular activity, and inactive individuals.
# This is done to find out if active and inactive individuals differ more in
# observable characteristics than sport participants and participants in any
# other extracurricular activity.
#+++

# Load data for main model
# empty data frame where all five MICE data sets are appended
data_descr <- data.frame()

for (mice_data_sel in 1:5) {
  # load data 
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, "_mice", mice_data_sel, ".rds")
  } else {
    data_load <- paste0("Data/Grades/Prep_8/prep_8_plausi_", treatment_def, "_", 
                        treatment_repl, extra_act_save, "_robustcheck", "_mice", 
                        mice_data_sel, ".rds")
  }
  
  data_descr_sub <- readRDS(data_load)
  data_descr_sub <- data_descr_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  
  # keep only columns that are in all data frame
  colnames_sub <- colnames(data_descr_sub)
  colnames_all <- colnames(data_descr)
  
  if (length(colnames_all) > 1) {
    colnames_both <- intersect(colnames_sub, colnames_all)
    
    data_descr_sub <- data_descr_sub %>% dplyr::select(all_of(colnames_both))
    data_descr <- data_descr %>% dplyr::select(all_of(colnames_both))
  }
  
  data_descr <- rbind(data_descr, data_descr_sub)
}


# Generate dummy for group
data_descr <- data_descr %>% mutate(active = case_when(
  treatment_sport == 1 ~ "sport", extracurricular_num == 0 ~ "inactive",
  extracurricular_num > 0 ~ "active", TRUE ~ "NA"
))
table(data_descr$active)


## MAKE DESCRIPTIVE STATISTICS ##
#+++++++++++++++++++++++++++++++#

# keep only numeric variables
data_descr_num <- cbind(
  data_descr %>% ungroup() %>% dplyr::select_if(~ is.numeric(.)), data_descr %>% dplyr::select(active)
) %>% dplyr::select(-contains("_cat_"), -ends_with("lag"))

data_descr_comp_mean <- left_join(
  data_descr_num %>%
    filter(active == "sport") %>%
    dplyr::select(outcome_grade, gender_male, age, migration, childhood_biological_parents, kindergarden, 
                  starts_with("health") & !ends_with("lag"), 
                  starts_with("bigfive"), starts_with("personality"), starts_with("satisfaction"), starts_with("stress"),
                  educ_school_grade_final, educ_years_school, educ_years_uni, educ_years_total, educ_school_degree_general,
                  starts_with("uni_major"), uni_ects_current, starts_with("uni_time"), starts_with("interest"), starts_with("comp"),
                  starts_with("mother"), starts_with("father"), starts_with("parents"), sibling_total, partner_current) %>%
    summarize_all(mean) %>%
    gather(key = "variable", value = "sport"),
  data_descr %>%
    filter(active == "active") %>%
    dplyr::select(outcome_grade, gender_male, age, migration, childhood_biological_parents, kindergarden, 
                  starts_with("health") & !ends_with("lag"), 
                  starts_with("bigfive"), starts_with("personality"), starts_with("satisfaction"), starts_with("stress"),
                  educ_school_grade_final, educ_years_school, educ_years_uni, educ_years_total, educ_school_degree_general,
                  starts_with("uni_major"), uni_ects_current, starts_with("uni_time"), starts_with("interest"), starts_with("comp"),
                  starts_with("mother"), starts_with("father"), starts_with("parents"), sibling_total, partner_current) %>%
    summarize_all(mean) %>%
    gather(key = "variable", value = "active"),
  by = "variable"
) %>% left_join(
  data_descr %>%
    filter(active == "inactive") %>%
    dplyr::select(outcome_grade, gender_male, age, migration, childhood_biological_parents, kindergarden, 
                  starts_with("health") & !ends_with("lag"), 
                  starts_with("bigfive"), starts_with("personality"), starts_with("satisfaction"), starts_with("stress"),
                  educ_school_grade_final, educ_years_school, educ_years_uni, educ_years_total, educ_school_degree_general,
                  starts_with("uni_major"), uni_ects_current, starts_with("uni_time"), starts_with("interest"), starts_with("comp"),
                  starts_with("mother"), starts_with("father"), starts_with("parents"), sibling_total, partner_current) %>%
    summarize_all(mean) %>%
    gather(key = "variable", value = "inactive"),
  by = "variable"
) %>% left_join(
  data_descr %>%
    filter(active != "inactive") %>%
    dplyr::select(outcome_grade, gender_male, age, migration, childhood_biological_parents, kindergarden, 
                  starts_with("health") & !ends_with("lag"), 
                  starts_with("bigfive"), starts_with("personality"), starts_with("satisfaction"), starts_with("stress"),
                  educ_school_grade_final, educ_years_school, educ_years_uni, educ_years_total, educ_school_degree_general,
                  starts_with("uni_major"), uni_ects_current, starts_with("uni_time"), starts_with("interest"), starts_with("comp"),
                  starts_with("mother"), starts_with("father"), starts_with("parents"), sibling_total, partner_current) %>%
    summarize_all(mean) %>%
    gather(key = "variable", value = "sport+active"),
  by = "variable"
)






