#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Descriptive Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#


# load data 
if (extra_act == "yes") {
  extra_act_save <- "_extradrop"
} else {
  extra_act_save <- ""
}

mice_data_sel <- 1

if (cohort_prep == "controls_same_outcome") {
  data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl,
                      extra_act_save, "_mice", mice_data_sel, ".rds")
} else {
  data_load <- paste0("Data/Prep_8/prep_8_plausi_", treatment_def, "_", treatment_repl, 
                      extra_act_save, "_robustcheck", "_mice", mice_data_sel, ".rds")
}

data_descr <- readRDS(data_load)
data_descr <- data_descr %>% ungroup()
id_num <- length(unique(data_descr$id_t))
obs_num <- nrow(data_descr)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Treatment and Control Group ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Outcome ##
#+++++++++++#

# difference-in-means: outcome variable
# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
data_outcome_descr <-
  data_descr %>%
  group_by(treatment_sport) %>%
  summarize(
    num_obs = n(), 
    mean_grades = mean(outcome_grade),
    se_grades = sd(outcome_grade) / sqrt(num_obs)
  # add total
  ) %>% rbind(
    data_descr %>%
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
outcome_treatment_ttest <- with(data_descr, t.test(outcome_grade ~ treatment_sport))

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

if (file.exists("Output/OUTCOME_TREATMENT.xlsx")) {
  data_outcome_descr_hist <- read.xlsx("Output/OUTCOME_TREATMENT.xlsx", sheetName = "Sheet1")
  data_outcome_descr_save <- rbind(data_outcome_descr_hist, data_outcome_descr) %>%
    group_by(cohort_prep , treatment_repl, treatment_def, extra_act_save) %>%
    filter(time_stamp == max(time_stamp)) %>%
    distinct() %>% ungroup() %>% data.frame()
  write.xlsx(data_outcome_descr_save, "Output/OUTCOME_TREATMENT.xlsx", sheetName = "Sheet1",
             row.names = FALSE, append = FALSE, showNA = FALSE)
} else {
  write.xlsx(data_outcome_descr, "Output/OUTCOME_TREATMENT.xlsx", row.names = FALSE)
}

# histogram
ggplot(data = data_descr, aes(x = outcome_grade)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(binwidth = 0.1)



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





