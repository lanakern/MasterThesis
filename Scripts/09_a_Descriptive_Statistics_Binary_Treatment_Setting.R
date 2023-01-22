#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#


# load data 
if (cohort_prep == "controls_same_outcome") {
  data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, 
                      "_", treatment_repl, ".rds")
} else {
  data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
}

data_descr <- readRDS(data_load)
data_descr <- data_descr %>% ungroup()
id_num <- length(unique(data_descr$id_t))
obs_num <- nrow(data_descr)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Treatment and Control Group ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Sport-participation vs. non-participation ##
#+++++++++++++++++++++++++++++++++++++++++++++#

# number of observations
data_descr %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_obs = n())

obs_num_treatment <- data_descr %>% filter(treatment_sport == 1) %>% nrow()
obs_num_control <- data_descr %>% filter(treatment_sport == 0) %>% nrow()
obs_num_treatment + obs_num_control # must equal obs_num

# number of individuals
data_descr %>%
  ungroup() %>% group_by(id_t, treatment_sport) %>%
  summarize(number_obs = n()) %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_id = n())


id_num_treatment <- data_descr %>% filter(treatment_sport == 1) %>% pull(id_t) %>% unique() %>% length()
id_num_control <- data_descr %>% filter(treatment_sport == 0) %>% pull(id_t) %>% unique() %>% length()
id_num_treatment + id_num_control # not id_num because individuals may change from treatment to control group


## Lags ##
#++++++++#

# check for how many observations lag and current variable is different
data_descr %>%
  select(treatment_sport, treatment_sport_lag) %>%
  filter(treatment_sport != treatment_sport_lag) %>%
  nrow()


data_descr %>%
  select(outcome_grade, outcome_grade_lag) %>%
  filter(outcome_grade != outcome_grade_lag) %>%
  nrow()



## LENGTH TREATMENT PERIOD ##
#+++++++++++++++++++++++++++#

# general
data_descr <- data_descr %>%
  mutate(treatment_period_length = as.numeric(difftime(interview_date_end, interview_date_start, units = "days")) / 30)

summary(data_descr$treatment_period_length)

# differences between treatment and control group
data_descr %>%
  filter(treatment_sport == 1) %>%
  select(treatment_period_length) %>%
  summary(.)
  

data_descr %>%
  filter(treatment_sport == 0) %>%
  select(treatment_period_length) %>%
  summary(.)



## NUMBER TREATMENT PERIODS ##
#++++++++++++++++++++++++++++#

# general
summary(data_descr$treatment_period)

# differences between treatment and control group
data_descr %>%
  filter(treatment_sport == 1) %>%
  select(treatment_period) %>% summary(.)


data_descr %>%
  filter(treatment_sport == 0) %>%
  select(treatment_period) %>% summary(.)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### EXTRACURRICULAR ACTIVITIES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# do sport participants also participate in other extracurricular activities
summary(data_descr$extracurricular_num)

data_descr %>%
  filter(treatment_sport == 1) %>%
  select(extracurricular_num) %>% summary(.)

data_descr %>%
  filter(treatment_sport == 0) %>%
  select(extracurricular_num) %>% summary(.)


# frequency
table(data_descr$extracurricular_freq)

table(data_descr %>% filter(treatment_sport == 1) %>% select(extracurricular_freq))
table(data_descr %>% filter(treatment_sport == 0) %>% select(extracurricular_freq))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DESCRIPTIVES MOST IMPROTANT PREDICTORS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_descr_sub <- eval(parse(text = paste('data_descr %>%', vars_baseline)))
data_descr_sub %>% colnames()


## frequency table for categorical variables ##
#+++++++++++++++++++++++++++++++++++++++++++++#

vars_categoric <- data_descr_sub %>% ungroup() %>% select_if(~ is.character(.)) %>% colnames()
  ## differentiated by treatment
data_descr_cat <-
  data_descr_sub %>%
  select(treatment_sport, all_of(vars_categoric)) %>%
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
  select(-c(treatment_sport)) %>% distinct() %>%
  mutate(n_rel = n_abs / obs_num)
data_descr_cat_all



## summary for numeric variables ##
#+++++++++++++++++++++++++++++++++#

# extract numeric columns
vars_numeric <- data_descr_sub %>% ungroup() %>% select_if(~ is.numeric(.))
vars_numeric_drop_expr <- paste("vars_numeric %>% select(-c(", 
                                paste0("starts_with('", vars_categoric, "')", collapse = "|"), 
                                "))")
vars_numeric <- eval(parse(text = vars_numeric_drop_expr)) %>% colnames()


data_descr_sub %>%
  select(all_of(vars_numeric)) %>%
  summary(.)

data_descr_sub %>%
  filter(treatment_sport == 1) %>%
  select(all_of(vars_numeric)) %>%
  summary(.)

data_descr_sub %>%
  filter(treatment_sport == 0) %>%
  select(all_of(vars_numeric)) %>%
  summary(.)



#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# drop remaining variables not needed for estimation
vars_categoric_drop <- readRDS("Data/Prep_7/prep_7_variables_drop_cat.rds")
vars_categoric_drop <- vars_categoric_drop[vars_categoric_drop %in% colnames(data_descr)]
data_descr_final <- data_descr %>%
  select(-c(all_of(vars_categoric_drop)))


# number of respondents, rows, and columns
print(paste("Number of respondents:", length(unique(data_descr$id_t))))
print(paste("Number of rows:", nrow(data_descr)))
print(paste("Number of columns:", ncol(data_descr)))

# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_save <- paste0("Data/Prep_9/prep_9_descr_", treatment_def, 
                      "_", treatment_repl, ".rds")
} else {
  data_save <- paste0("Data/Prep_9/prep_9_descr_", treatment_def, 
                      "_", treatment_repl, "_robustcheck.rds")
}

saveRDS(data_descr_final, data_save)

