#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics for Personality as Outcome ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, descriptive statistics including differences-in-means for the 
# big five personality traits are made for both the binary and 
# multivalued treatment setting. 
# Everything is done for the main model and the five MICE data set. Those are
# appended row-wise,
#+++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Binary Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#

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
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl,
                        extra_act_save, "_mice", mice_data_sel, "_personality.rds")
  } else {
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl, 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel, "_personality.rds")
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

id_num <- length(unique(data_descr$id_t))
obs_num <- nrow(data_descr %>% filter(MICE == 1))

# extract data from one mice data set: used for variables that have no missing
# values like outcome, treatment, etc.
# otherwise standard errors are downward biased
data_descr_mice1 <- data_descr %>% filter(MICE == 1)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Treatment and Control Group ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Outcome: Big Five Personality Traits ##
#++++++++++++++++++++++++++++++++++++++++#


# calculate mean-standardized differences
vars_personality <- c(
  "bigfive_conscientiousness", "bigfive_extraversion", "bigfive_neuroticism",
  "bigfive_openness", "bigfive_agreeableness"
)

data_outcome_descr_all <- func_mean_comp(data_descr_mice1, vars_personality,  "binary") %>%
  dplyr::select(-median)


data_outcome_descr_all

# calculate minimum, maximum, median etc.
data_outcome_descr_all_2 <- data.frame()
for (vars_personality_sel in vars_personality) {
  data_outcome_descr <- func_summary_stats(data_descr_mice1, "treatment_sport", vars_personality_sel)
  data_outcome_descr_all_2 <- rbind(data_outcome_descr_all_2, data_outcome_descr)
}
data_outcome_descr_all_2

# merge
data_outcome_descr_all <- left_join(
  data_outcome_descr_all, data_outcome_descr_all_2, by = c("variable", "treatment_sport")
) %>%
  dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                treatment_sport, num_obs, min, max, median, mean, everything())


# save
if (file.exists("Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_BINARY.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_BINARY.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_all) %>%
    group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_BINARY.rds")
} else {
  saveRDS(data_outcome_descr_all, "Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_BINARY.rds")
}


# # histogram
# plot_outcome_treatment_binary_1 <- 
#   data_descr_mice1 %>%
#   mutate(treatment_label = ifelse(treatment_sport == 1, "Sport Participatiopn", "No Sport Participation")) %>%
#   ggplot(aes(x = bigfive_conscientiousness)) +
#   geom_histogram(aes(y = ..density.., fill = as.factor(treatment_label)),
#                  alpha = 0.8) +
#   xlab("Degree of Conscientiousness") + ylab("Density") +
#   scale_fill_manual(values = c("grey88", "grey68")) +
#   ggtitle("Conscientiousness") +
#   theme_bw() +
#   guides(fill = guide_legend(title = "Treatment Group"))
# 
# plot_outcome_treatment_binary_2 <- 
#   data_descr_mice1 %>%
#   mutate(treatment_label = ifelse(treatment_sport == 1, "Sport Participatiopn", "No Sport Participation")) %>%
#   ggplot(aes(x = bigfive_extraversion)) +
#   geom_histogram(aes(y = ..density.., fill = as.factor(treatment_label)),
#                  alpha = 0.8) +
#   xlab("Degree of Extraversion") + ylab("Density") +
#   scale_fill_manual(values = c("grey88", "grey68")) +
#   ggtitle("Extraversion") +
#   theme_bw() +
#   guides(fill = guide_legend(title = "Treatment Group"))
# 
# 
#   #geom_density()
# ggsave("Output/Descriptives/Personality/PLOT_OUTCOME_TREATMENT_BINARY.png", 
#        plot_outcome_treatment_binary)



## Sport-participation vs. non-participation ##
#+++++++++++++++++++++++++++++++++++++++++++++#

# number of observations
nrow(data_descr_mice1)
data_descr_mice1 %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_obs = n())

obs_num_treatment <- data_descr_mice1 %>% filter(treatment_sport == 1) %>% nrow()
obs_num_control <- data_descr_mice1 %>% filter(treatment_sport == 0) %>% nrow()
obs_num_treatment + obs_num_control # must equal obs_num

# number of individuals
data_descr_mice1 %>%
  ungroup() %>% group_by(id_t, treatment_sport) %>%
  summarize(number_obs = n()) %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_id = n())


id_num_treatment <- data_descr_mice1 %>% filter(treatment_sport == 1) %>% pull(id_t) %>% unique() %>% length()
id_num_control <- data_descr_mice1 %>% filter(treatment_sport == 0) %>% pull(id_t) %>% unique() %>% length()
id_num_treatment + id_num_control # not id_num because individuals may change from treatment to control group


## Lags ##
#++++++++#

# check for how many observations lag and current variable is different
data_descr %>%
  dplyr::select(treatment_sport, treatment_sport_lag) %>%
  filter(treatment_sport != treatment_sport_lag) %>%
  nrow()


data_descr %>%
  dplyr::select(bigfive_agreeableness, bigfive_agreeableness_lag) %>%
  filter(bigfive_agreeableness != bigfive_agreeableness_lag) %>%
  nrow()

data_descr %>%
  dplyr::select(bigfive_openness, bigfive_openness_lag) %>%
  filter(bigfive_openness != bigfive_openness_lag) %>%
  nrow()

data_descr %>%
  filter(bigfive_extraversion != bigfive_extraversion_lag) %>%
  nrow()

data_descr %>%
  filter(bigfive_conscientiousness != bigfive_conscientiousness_lag) %>%
  nrow()

data_descr %>%
  filter(bigfive_neuroticism != bigfive_neuroticism_lag) %>%
  nrow()


## LENGTH TREATMENT PERIOD ##
#+++++++++++++++++++++++++++#

# general
summary(data_descr$treatment_period_length)

# differences between treatment and control group
data_descr %>%
  filter(treatment_sport == 1) %>%
  dplyr::select(treatment_period_length) %>%
  summary(.)
  

data_descr %>%
  filter(treatment_sport == 0) %>%
  dplyr::select(treatment_period_length) %>%
  summary(.)



## NUMBER TREATMENT PERIODS ##
#++++++++++++++++++++++++++++#

# general
summary(data_descr$treatment_period)

# differences between treatment and control group
data_descr %>%
  filter(treatment_sport == 1) %>%
  dplyr::select(treatment_period) %>% summary(.)


data_descr %>%
  filter(treatment_sport == 0) %>%
  dplyr::select(treatment_period) %>% summary(.)



#%%%%%%%%%%%%%%%%%%%%#
#### COMPETENCIES ####
#%%%%%%%%%%%%%%%%%%%%#

# differs across MICE data sets as they are imputed

# competence variables
vars_ability <- data_descr %>% dplyr::select(starts_with("comp_")) %>% colnames()

# apply operation
data_comp_descr <- func_mean_comp(data_descr, vars_ability,  "binary")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")) {
#   data_comp_descr_hist <- readRDS("Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
#   data_comp_descr_save <- rbind(data_comp_descr_hist, data_comp_descr) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_comp_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# } else {
#   saveRDS(data_comp_descr, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# }




#%%%%%%%%%%%%%%%%%%#
#### MOTIVATION ####
#%%%%%%%%%%%%%%%%%%#

vars_motivation <- data_descr %>% dplyr::select(starts_with("motivation_degree")) %>% colnames()
data_motiv_descr <- func_mean_comp(data_descr, vars_motivation, "binary")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")) {
#   data_outcome_descr_hist <- readRDS("Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
#   data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_motiv_descr) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# } else {
#   saveRDS(data_motiv_descr, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# }




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITIES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_extra_descr <- func_mean_comp(data_descr, "extracurricular_num", "binary")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")) {
#   data_outcome_descr_hist <- readRDS("Output/Descriptives/OUTCOME_TREATMENT_BINARY.rds")
#   data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_extra_descr) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# } else {
#   saveRDS(data_extra_descr, "Output/Descriptives/Personality/OUTCOME_TREATMENT_BINARY.rds")
# }

# do sport participants also participate in other extracurricular activities
summary(data_descr$extracurricular_num)

data_descr %>%
  filter(treatment_sport == 1) %>%
  dplyr::select(extracurricular_num) %>% summary(.)

data_descr %>%
  filter(treatment_sport == 0) %>%
  dplyr::select(extracurricular_num) %>% summary(.)


# frequency
table(data_descr$extracurricular_freq)

table(data_descr %>% filter(treatment_sport == 1) %>% dplyr::select(extracurricular_freq))
table(data_descr %>% filter(treatment_sport == 0) %>% dplyr::select(extracurricular_freq))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DESCRIPTIVES MOST IMPROTANT PREDICTORS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_descr_sub <- data_descr


## frequency table for categorical variables ##
#+++++++++++++++++++++++++++++++++++++++++++++#

vars_categoric <- data_descr_sub %>% ungroup() %>% dplyr::select_if(~ is.character(.)) %>% colnames()
## differentiated by treatment
data_descr_cat <-
  data_descr_sub %>%
  dplyr::select(treatment_sport, all_of(vars_categoric)) %>%
  pivot_longer(
    cols = -treatment_sport, 
    names_pattern = "([A-z]+)", names_to = c("variable")
  ) %>%
  group_by(treatment_sport, variable) %>%
  count(value) %>%
  arrange(variable, value) %>%
  mutate(n_rel = case_when(treatment_sport == 0 ~ as.double(n / obs_num_control), 
                           treatment_sport == 1 ~ as.double(n / obs_num_treatment),
                           TRUE ~ as.double(n))) %>%
  rename(n_abs = n)
data_descr_cat
## not differentiated by treatment
data_descr_cat_all <- 
  data_descr_cat %>%
  group_by(variable, value) %>%
  mutate(n_abs = sum(n_abs)) %>%
  dplyr::select(-c(treatment_sport)) %>% distinct() %>%
  mutate(n_rel = n_abs / obs_num)
data_descr_cat_all



## summary for numeric variables ##
#+++++++++++++++++++++++++++++++++#

# extract numeric columns
vars_numeric <- data_descr_sub %>% ungroup() %>% dplyr::select_if(~ is.numeric(.))
vars_numeric_drop_expr <- paste("vars_numeric %>% dplyr::select(-c(", 
                                paste0("starts_with('", vars_categoric, "')", collapse = "|"), 
                                "))")
vars_numeric <- eval(parse(text = vars_numeric_drop_expr)) %>% colnames()


data_descr_sub %>%
  dplyr::select(all_of(vars_numeric)) %>%
  summary(.)

data_descr_sub %>%
  filter(treatment_sport == 1) %>%
  dplyr::select(all_of(vars_numeric)) %>%
  summary(.)

data_descr_sub %>%
  filter(treatment_sport == 0) %>%
  dplyr::select(all_of(vars_numeric)) %>%
  summary(.)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Multivalued Treatment Setting ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#

# empty data frame where all five MICE data sets are appended
data_descr_multi <- data.frame()

for (mice_data_sel in 1:5) {
  # load data 
  if (extra_act == "yes") {
    extra_act_save <- "_extradrop"
  } else {
    extra_act_save <- ""
  }
  
  if (cohort_prep == "controls_same_outcome") {
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl,
                        extra_act_save, "_mice", mice_data_sel, "_personality.rds")
  } else {
    data_load <- paste0("Data/Personality/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl, 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel, "_personality.rds")
  }
  
  data_descr_sub <- readRDS(data_load)
  data_descr_sub <- data_descr_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  
  # keep only columns that are in all data frame
  colnames_sub <- colnames(data_descr_sub)
  colnames_all <- colnames(data_descr_multi)
  
  if (length(colnames_all) > 1) {
    colnames_both <- intersect(colnames_sub, colnames_all)
    
    data_descr_sub <- data_descr_sub %>% dplyr::select(all_of(colnames_both))
    data_descr_multi <- data_descr_multi %>% dplyr::select(all_of(colnames_both))
  }
  
  data_descr_multi <- rbind(data_descr_multi, data_descr_sub)
}

id_num_multi <- length(unique(data_descr_multi$id_t))
obs_num_multi <- nrow(data_descr_multi %>% filter(MICE == 1))


# data set only for first MICE data set
data_descr_multi_mice1 <- data_descr_multi %>% filter(MICE == 1)


#%%%%%%%%%%%%%%%#
#### OUTCOME ####
#%%%%%%%%%%%%%%%#

# calculate mean-standardized differences
data_outcome_descr_multi_all <- func_mean_comp(data_descr_multi_mice1, vars_personality,  "multi") %>%
  dplyr::select(-median)


# calculate minimum, maximum, median etc.
data_outcome_descr_multi_all_2 <- data.frame()
for (vars_personality_sel in vars_personality) {
  data_outcome_descr <- func_summary_stats(data_descr_multi_mice1, "treatment_sport_freq", vars_personality_sel)
  data_outcome_descr_multi_all_2 <- rbind(data_outcome_descr_multi_all_2, data_outcome_descr)
}
data_outcome_descr_multi_all_2

# merge
data_outcome_descr_multi_all <- left_join(
  data_outcome_descr_multi_all, data_outcome_descr_multi_all_2, by = c("variable", "treatment_sport_freq")
) %>%
  dplyr::select(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save,
                treatment_sport_freq, num_obs, min, max, median, mean, everything())


# save
if (file.exists("Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_MULTI.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_MULTI.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_multi_all) %>%
    group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_MULTI.rds")
} else {
  saveRDS(data_outcome_descr_multi_all, "Output/Descriptives/Personality/PERSONALITY_OUTCOME_TREATMENT_MULTI.rds")
}


#%%%%%%%%%%%%%%%%%%%%#
#### COMPETENCIES ####
#%%%%%%%%%%%%%%%%%%%%#

# differs across MICE data sets as they are imputed

# competence variables
vars_ability <- data_descr_multi %>% dplyr::select(starts_with("comp_")) %>% colnames()

# apply operation
data_comp_descr_multi <- func_mean_comp(data_descr_multi, vars_ability,  "multi")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")) {
#   data_comp_descr_hist <- readRDS("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
#   data_comp_descr_save <- rbind(data_comp_descr_hist, data_comp_descr_multi) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_comp_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# } else {
#   saveRDS(data_comp_descr_multi, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# }




#%%%%%%%%%%%%%%%%%%#
#### MOTIVATION ####
#%%%%%%%%%%%%%%%%%%#

vars_motivation <- data_descr_multi %>% dplyr::select(starts_with("motivation_degree")) %>% colnames()
data_motiv_descr_multi <- func_mean_comp(data_descr_multi, vars_motivation, "multi")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")) {
#   data_outcome_descr_hist <- readRDS("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
#   data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_motiv_descr_multi) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# } else {
#   saveRDS(data_motiv_descr_multi, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# }




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITIES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_extra_descr_multi <- func_mean_comp(data_descr_multi, "extracurricular_num", "multi")

# if (file.exists("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")) {
#   data_outcome_descr_hist <- readRDS("Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
#   data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_extra_descr_multi) %>%
#     group_by(variable, cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
#     filter(time_stamp == max(time_stamp)) %>%
#     distinct() %>% ungroup() %>% data.frame()
#   saveRDS(data_outcome_descr_save, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# } else {
#   saveRDS(data_extra_descr_multi, "Output/Descriptives/Personality/OUTCOME_TREATMENT_MULTI.rds")
# }


