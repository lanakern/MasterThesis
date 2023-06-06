#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE COMPETENCE DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the information about competence measures is prepared.
# Competence measure tests are only conducted in wave 1 (2010/2011 (CATI+competencies)), 
# wave 5 (2013 (CATI+competencies)), and wave 12 (2017 (CATI)). Moreover, different 
# competence measures are assessed across waves.
#++++
# 1.) Drop respondents:
# -> Respondents who have missing values in all competence measures are dropped.
# -> Respondents who are not in cohort profile are dropped.
#++++
# 2.) The competence measure data set is in wide-format, i.e., one row per
# respondent. I restructure the data set in long-format, i.e., one row per
# respondent + wave. This is necessary for the merge with cohort profile and
# for the further analysis.
#++++
# 3.) Merge with cohort profile so that only respondents who are in both data
# sets are kept. However, all waves in cohort profile are kept in order to 
# downward replace the results of the competence measure tests to further surveys 
# in step 3.).
#++++
# 4.) Handle missing values: 
# 4.1) Downward replacement of missing values. For example, respondent has info
# on competence measure mathematics in wave 1. The next competence measure test
# on mathematics is conducted in wave 12. Hence, from wave 2 to wave 11, the
# values from wave 1 are used.
# 4.2) Domain-general competencies are paper-, computer-based or online. Missing
# values are replaced across those survey methods.
#++++
# --> FINAL DATA FRAME IS A PANEL DATA SET (one row for each respondent-wave combination).
#++++


#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%%#

# competencies
data_competencies <- readRDS("Data/Personality/Prep_1/prep_1_competencies_personality.rds")
num_id_comp <- length(unique(data_competencies$ID_t))

# cohort profile
# cohort data based on selection
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality.rds") %>%
    filter(wave_2 == "CATI") %>%
    dplyr::select(ID_t, wave, interview_date)
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile <- readRDS("Data/Personality/Prep_2/prep_2_cohort_profile_personality_robustcheck.rds") %>%
    filter(wave_2 == "CATI") %>%
    dplyr::select(ID_t, wave, interview_date)
}

num_id_cohort <- length(unique(data_cohort_profile$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Drop Respondents ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# I am only interested in wave1 and wave5; hence first I drop all wave12 columns
data_competencies <- data_competencies %>% dplyr::select(-ends_with("12"))

# Drop respondents who have missing values in all competence measures
data_competencies$sum_NA <- apply(data_competencies, 1, function(x) sum(is.na(x)))
num_drop <- data_competencies %>% dplyr::select(-c(ID_t, starts_with("wave"), "sum_NA")) %>% colnames() %>% length()
id_drop <- data_competencies %>% filter(sum_NA == num_drop) %>% pull(ID_t) %>% unique()
num_id_drop <- length(id_drop)
data_competencies <- data_competencies %>% subset(!ID_t %in% id_drop)
num_id_comp_adj_1 <- length(unique(data_competencies$ID_t))

# Drop respondents who are not in cohort profile
id_cohort_comp <- intersect(
  unique(data_competencies$ID_t), unique(data_cohort_profile$ID_t)
)

data_competencies <- data_competencies %>% subset(ID_t %in% id_cohort_comp)
num_id_comp_adj_2 <- length(unique(data_competencies$ID_t))

data_cohort_profile <- data_cohort_profile %>% subset(ID_t %in% id_cohort_comp)
num_id_cohort_adj_1 <- length(unique(data_cohort_profile$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Restructure Competencies Data Set ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# competence data set is in wide-format; I want to have it in long format
  ## vector containing waves
  ## 2017 is not used because I do not have a lot of observations for
  ## individuals studying after 2017
wave_comp <- c("2010/2011 (CATI+competencies)", "2013 (CATI+competencies)") # , "2017 (CATI)")
  ## iterator to extract respective wave information from wave_comp
iter_num <- 0
  ## data set for merge (includes only ID and missing wave variable)
data_competencies_long <- data.frame(ID_t = unique(data_competencies$ID_t)) 
data_competencies_long$w1 <- "2010/2011 (CATI+competencies)"
data_competencies_long$w2 <- "2013 (CATI+competencies)"
data_competencies_long <- data_competencies_long %>% 
  pivot_longer(!ID_t, names_to = "drop", values_to = "wave") %>%
  dplyr::select(-drop)

# loop over wave 1, and wave 5
for (wave_sel in c("w1", "w5")) {
  
  # adjust iterator
  iter_num <- iter_num + 1
  
  # create data set for wave selected by wave_sel
  data_competencies_sub_wave <- 
    data_competencies %>%
      # keep only individuals who participated in competence measures in wave 1
      filter((!!sym(paste0("wave_", wave_sel))) == 1) %>%
      # keep only wave 1 variables
      dplyr::select(ID_t, ends_with(wave_sel)) %>%
      # omit individuals with at least one missing value
      # na.omit() %>%
      # adjust wave variable
      rename(wave = paste0("wave_", wave_sel)) %>%
      mutate(wave = wave_comp[iter_num]) %>%
      # drop suffix of variables
      rename_with(~ str_remove(., paste0("_", wave_sel)))
  
  # merge
  data_competencies_long <- 
    left_join(
      data_competencies_long, data_competencies_sub_wave, by = c("ID_t", "wave")
    ) 
}

length(unique(data_competencies_long$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge with Cohort Profile ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# left join: keeps all information in cohort profile
# this ensures that I can downward replace competence measures across waves
data_competencies_final <- left_join(
  data_cohort_profile, data_competencies_long %>% rename(wave_comp = wave), 
  by = c("ID_t")
)
length(unique(data_competencies_final$ID_t)) 

# ensure that information is up to-date
data_competencies_final <- data_competencies_final %>%
  # generate year variable for wave cohort and wave competencies
  mutate(year_cohort = str_sub(wave, 1, 4), year_comp = str_sub(wave_comp, 1, 4)) %>%
  # year of competence measures must be smaller or equal than year of cohort
  filter(year_comp <= year_cohort) %>%
  # in case of duplicates keep newest competence measures
  group_by(ID_t, wave) %>%
  filter(year_comp == max(year_comp))

# check that there are no duplicates (-> zero rows)
data_competencies_final[duplicated(data_competencies_final %>% ungroup %>% dplyr::select(ID_t, wave)),]

# remove variables not needed anymore
data_competencies_final <- data_competencies_final %>%
  ungroup() %>%
  arrange(ID_t, year_cohort) %>%
  dplyr::select(-c(starts_with("wave"), starts_with("year")))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Handle Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## 1.) Replace missing values downwards ##
#++++++++++++++++++++++++++++++++++++++++#

colSums(is.na(data_competencies_final))
data_competencies_final <- 
  data_competencies_final %>% 
  group_by(ID_t) %>%
  fill(data_competencies_final %>% dplyr::select(-c(ID_t)) %>% colnames(), 
       .direction = "down")
colSums(is.na(data_competencies_final))



## 2.) domain-general competencies ##
#+++++++++++++++++++++++++++++++++++#

# can be paper-based, computer-based or online.
# hence, missing values are replaced across those survey possibilities

colSums(is.na(data_competencies_final))
data_competencies_final <- data_competencies_final %>%
  mutate(
    # reasoning
    comp_reasoning_sum = ifelse(is.na(comp_reasoning_paper_sum), comp_reasoning_comp_sum, comp_reasoning_paper_sum),
    comp_reasoning_sum = ifelse(is.na(comp_reasoning_sum), comp_reasoning_online_sum, comp_reasoning_sum), 
    # perceptual speed
    comp_percspeed_sum = ifelse(is.na(comp_percspeed_paper_sum), comp_percspeed_comp_sum, comp_percspeed_paper_sum),
    comp_percspeed_sum = ifelse(is.na(comp_reasoning_sum),comp_percspeed_online_sum, comp_reasoning_sum)
  ) %>%
  dplyr::select(-c(matches(".*_comp_.*"), matches(".*_paper_.*"), matches(".*_online_.*")))
colSums(is.na(data_competencies_final))



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# keep only wle estimates
data_competencies_final <- data_competencies_final %>%
  dplyr::select(ID_t, interview_date, ends_with("_wle"))

# check for duplicates
sum(duplicated(data_competencies_final))
sum(duplicated(data_competencies_final[, c("ID_t", "interview_date")]))

# sort data frame 
data_competencies_final <- data_competencies_final %>%
  arrange(ID_t, interview_date) 
 
# sample size 
print(paste("Number of respondents with competence measures before data preparation:", num_id_comp))
print(paste("Number of respondents with at least one non-missing competence measure:", num_id_comp_adj_1))
print(paste("Number of respondents after merge with cohort_profile:", num_id_comp_adj_2))


print(paste("The final sample size includes", 
            length(unique(data_competencies_final$ID_t)),
            "respondents."))
# number of rows and columns
print(paste("Number of rows", nrow(data_competencies_final)))
print(paste("Number of columns", ncol(data_competencies_final)))

# save prepared competence data
if (cohort_prep == "controls_same_outcome") {
  data_comp_save <- "Data/Personality/Prep_3/prep_3_competencies_personality.rds"
} else if (cohort_prep == "controls_bef_outcome") {
  data_comp_save <- "Data/Personality/Prep_3/prep_3_competencies_personality_robustcheck.rds"
}

saveRDS(data_competencies_final, data_comp_save)



