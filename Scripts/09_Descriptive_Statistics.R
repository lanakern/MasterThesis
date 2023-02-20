#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, descriptive statistics are made for both the binary and 
# multivalued treatment setting. For the outcome, ability, and personality
# variables, differences-in-means are calculated. 
# Everything is done for the main model and the five MICE data set. Those are
# appended row-wise
#+++

# define inputs
cohort_prep <- main_cohort_prep
treatment_repl <- main_treatment_repl
treatment_def <- main_treatment_def
extra_act <- main_extra_act


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
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl,
                        extra_act_save, "_mice", mice_data_sel, ".rds")
  } else {
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl, 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel, ".rds")
  }
  
  data_descr_sub <- readRDS(data_load)
  data_descr_sub <- data_descr_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  
  # keep only columns that are in all data frame
  colnames_sub <- colnames(data_descr_sub)
  colnames_all <- colnames(data_descr)
  
  if (length(colnames_all) > 1) {
    colnames_both <- intersect(colnames_sub, colnames_all)
    
    data_descr_sub <- data_descr_sub %>% select(all_of(colnames_both))
    data_descr <- data_descr %>% select(all_of(colnames_both))
  }

  data_descr <- rbind(data_descr, data_descr_sub)
}

id_num <- length(unique(data_descr$id_t))
obs_num <- nrow(data_descr %>% filter(MICE == 1))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Treatment and Control Group ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Outcome ##
#+++++++++++#

data_descr_outcome_binary <- data_descr %>% filter(MICE == 1) 

# difference-in-means: outcome variable
# same across data frames because outcome variable is not imputed!
# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
data_outcome_descr <-
  data_descr_outcome_binary %>%
  group_by(treatment_sport) %>%
  summarize(
    num_obs = n(), 
    mean_grades = mean(outcome_grade),
    se_grades = sd(outcome_grade) / sqrt(num_obs)
  # add total
  ) %>% rbind(
    data_descr_outcome_binary %>%
      summarize(
        num_obs = n(), 
        mean_grades = mean(outcome_grade),
        se_grades = sd(outcome_grade) / sqrt(num_obs)
      ) %>%
      mutate(treatment_sport = "all") %>%
      select(treatment_sport, everything())
  )
data_outcome_descr

# the difference-in-means of the outcome variable is statistically significant at 
# any conventional significance level
outcome_treatment_ttest <- with(data_descr_outcome_binary, t.test(outcome_grade ~ treatment_sport))

# save result
data_outcome_descr <- data_outcome_descr %>%
  mutate(
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, 
    treatment_def = treatment_def, extra_act_save = extra_act, 
    t_value = unname(outcome_treatment_ttest$statistic), 
    p_value = outcome_treatment_ttest$p.value,
    time_stamp = Sys.time()
  ) %>%
  select(cohort_prep, treatment_repl, treatment_def, extra_act_save, everything()) %>%
  as.data.frame()

if (file.exists("Output/Descriptives/OUTCOME_TREATMENT_BINARY.rds")) {
  data_outcome_descr_hist <- readRDS("Output/Descriptives/OUTCOME_TREATMENT_BINARY.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr) %>%
    group_by(cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/Descriptives/OUTCOME_TREATMENT_BINARY.rds")
} else {
  saveRDS(data_outcome_descr, "Output/Descriptives/OUTCOME_TREATMENT_BINARY.rds")
}

# histogram
plot_outcome_treatment_binary <- 
  data_descr %>%
  mutate(treatment_label = ifelse(treatment_sport == 1, "Sport Participatiopn", "No Sport Participation")) %>%
  ggplot(aes(x = outcome_grade)) +
  geom_histogram(aes(y = ..density.., fill = as.factor(treatment_label)),
                 alpha = 0.8, binwidth = 0.1) +
  xlab("Current Average Grade") + ylab("Density") +
  scale_fill_manual(values = c("grey88", "grey68")) +
  theme_bw() +
  guides(fill = guide_legend(title = "Treatment Group"))
  #geom_density()
ggsave("Output/Descriptives/PLOT_OUTCOME_TREATMENT_BINARY.png")



## Sport-participation vs. non-participation ##
#+++++++++++++++++++++++++++++++++++++++++++++#

# number of observations
data_descr_outcome_binary %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_obs = n())

obs_num_treatment <- data_descr_outcome_binary %>% filter(treatment_sport == 1) %>% nrow()
obs_num_control <- data_descr_outcome_binary %>% filter(treatment_sport == 0) %>% nrow()
obs_num_treatment + obs_num_control # must equal obs_num

# number of individuals
data_descr_outcome_binary %>%
  ungroup() %>% group_by(id_t, treatment_sport) %>%
  summarize(number_obs = n()) %>%
  ungroup() %>% group_by(treatment_sport) %>%
  summarize(number_id = n())


id_num_treatment <- data_descr_outcome_binary %>% filter(treatment_sport == 1) %>% pull(id_t) %>% unique() %>% length()
id_num_control <- data_descr_outcome_binary %>% filter(treatment_sport == 0) %>% pull(id_t) %>% unique() %>% length()
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


#%%%%%%%%%%%%%%%%%%%#
#### PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%#

vars_personality <- c(
  "bigfive_conscientiousness", "bigfive_extraversion", "bigfive_neuroticism",
  "bigfive_openness", "bigfive_agreeableness"
)

data_descr__personality <-
  data_descr_sub %>%
  select(treatment_sport, all_of(vars_personality)) %>%
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
data_descr_personality


#%%%%%%%%%%%%%%%#
#### ABILITY ####
#%%%%%%%%%%%%%%%#

vars_ability <- data_descr %>% select(starts_with("comp_")) %>% colnames()
data_descr %>%
  filter(treatment_sport == 1) %>% 
  select(all_of(vars_ability)) %>%
  summary()
data_descr %>%
  filter(treatment_sport == 0) %>% 
  select(all_of(vars_ability)) %>%
  summary()



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
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl,
                        extra_act_save, "_mice", mice_data_sel, ".rds")
  } else {
    data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl, 
                        extra_act_save, "_robustcheck", "_mice", mice_data_sel, ".rds")
  }
  
  data_descr_sub <- readRDS(data_load)
  data_descr_sub <- data_descr_sub %>% ungroup() %>% mutate(MICE = mice_data_sel)
  
  # keep only columns that are in all data frame
  colnames_sub <- colnames(data_descr_sub)
  colnames_all <- colnames(data_descr_multi)
  
  if (length(colnames_all) > 1) {
    colnames_both <- intersect(colnames_sub, colnames_all)
    
    data_descr_sub <- data_descr_sub %>% select(all_of(colnames_both))
    data_descr_multi <- data_descr_multi %>% select(all_of(colnames_both))
  }
  
  data_descr_multi <- rbind(data_descr_multi, data_descr_sub)
}

id_num_multi <- length(unique(data_descr_multi$id_t))
obs_num_multi <- nrow(data_descr_multi %>% filter(MICE == 1))


#### Outcome ####
#%%%%%%%%%%%%%%%#

data_descr_outcome_multi <- data_descr_multi %>% filter(MICE == 1) %>%
  mutate(
    treatment_sport_freq = case_when(
      treatment_sport_freq == "daily" ~ "daily + weekly",
      treatment_sport_freq == "weekly" ~ "daily + weekly",
      treatment_sport_freq == "monthly" ~ "monthly + less",
      treatment_sport_freq == "less frequently" ~ "monthly + less",
      TRUE ~ "never"
    ))

# difference-in-means: outcome variable
# same across data frames because outcome variable is not imputed!
# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
data_outcome_descr_multi <-
  data_descr_outcome_multi %>%
  group_by(treatment_sport_freq) %>%
  summarize(
    num_obs = n(), 
    mean_grades = mean(outcome_grade),
    se_grades = sd(outcome_grade) / sqrt(num_obs)
    # add total
  ) %>% rbind(
    data_descr_outcome_multi %>%
      summarize(
        num_obs = n(), 
        mean_grades = mean(outcome_grade),
        se_grades = sd(outcome_grade) / sqrt(num_obs)
      ) %>%
      mutate(treatment_sport_freq = "all") %>%
      select(treatment_sport_freq, everything())
  )
data_outcome_descr_multi

# the difference-in-means of the outcome variable is statistically significant at 
# any conventional significance level
data_descr_outcome_multi_ttest_1 <- data_descr_outcome_multi %>%
  filter(treatment_sport_freq != "never")
outcome_treatment_ttest_1 <- with(data_descr_outcome_multi_ttest_1, t.test(outcome_grade ~ treatment_sport_freq))

data_descr_outcome_multi_ttest_2 <- data_descr_outcome_multi %>%
  filter(treatment_sport_freq != "daily + weekly")
outcome_treatment_ttest_2 <- with(data_descr_outcome_multi_ttest_2, t.test(outcome_grade ~ treatment_sport_freq))

data_descr_outcome_multi_ttest_3 <- data_descr_outcome_multi %>%
  filter(treatment_sport_freq != "monthly + less")
outcome_treatment_ttest_3 <- with(data_descr_outcome_multi_ttest_3, t.test(outcome_grade ~ treatment_sport_freq))

data_outcome_descr_multi <- data_outcome_descr_multi %>%
  mutate(p_value_daily = c(NA, outcome_treatment_ttest_1$p.value, outcome_treatment_ttest_3$p.value, NA)) %>%
  mutate(p_value_monthly = c(outcome_treatment_ttest_1$p.value, NA, outcome_treatment_ttest_2$p.value, NA))


# save result
data_outcome_descr_multi <- data_outcome_descr_multi %>%
  mutate(
    cohort_prep = cohort_prep, treatment_repl = treatment_repl, 
    treatment_def = treatment_def, extra_act_save = extra_act, 
    time_stamp = Sys.time()
  ) %>%
  select(cohort_prep, treatment_repl, treatment_def, extra_act_save, everything()) %>%
  as.data.frame()

if (file.exists("Output/OUTCOME_TREATMENT_MULTI.rds")) {
  data_outcome_descr_hist <- readRDS("Output/OUTCOME_TREATMENT_MULTI.rds")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr_multi) %>%
    group_by(cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  saveRDS(data_outcome_descr_save, "Output/OUTCOME_TREATMENT_MULTI.rds")
} else {
  saveRDS(data_outcome_descr_multi, "Output/OUTCOME_TREATMENT_MULTI.rds")
}


#### Personality ####
#%%%%%%%%%%%%%%%%%%%#


#### Ability ####
#%%%%%%%%%%%%%%%#


