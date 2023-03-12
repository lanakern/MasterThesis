#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MERGE & PREPARE EPISODE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this script, the episode data is appended to the CATI + CAWI data set.
# In addition, further data preparations are conducted. Both steps are identical
# for both cohort profile data preparation steps.
#++++
# 1.) Subset episode data: Only respondents who are also in CATI&CAWI are kept
#++++
# 2.) Subset on uni spells: From the episode data, a data frame is created which
# includes only the university episodes. Next, I check that the interview dates
# take place within the university episode.
#++++
# 3.) Calculation of spell length
# -> for the calculation of the spell length the interview date closest to the
# end of the treatment period is used. That is for "controls_same_outcome" the
# start of the treatment period and for "controls_bef_outcome" the CATI interview
# date.
# -> cumulated spell length is of interest
# -> data frame with only uni spells is created
#++++
# 4.) Current employment
# -> Respondents who work during study are identified using current_emp as indicator
# which is 0 for respondents who do not work during their study. 
#++++
# -> Panel data set
#++++

#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # install packages if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to manipulate data, e.g. replace_na, spread() etc.
# 
# if (!require("sqldf")) install.packages("sqldf")
# library(sqldf)  # for sql syntax
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # for excel file
# 
# # define inputs
#   ## selection on cohort preparation
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"
#   ## treatment replacement
# treatment_repl <- "downup" # (only used for saving)



#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# CATI and CAWI
if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi <- readRDS(paste0("Data/Grades/Prep_4/prep_4_merge_cati_cawi_treat", 
                                   treatment_repl, ".rds"))
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi <- readRDS(paste0("Data/Grades/Prep_4/prep_4_merge_cati_cawi_treat", 
                                   treatment_repl, "_robustcheck.rds"))
}

# Episode data
data_life_course <- readRDS("Data/Grades/Prep_2/prep_2_life_course.rds")


# number of respondents in different data sets
num_id_cati_cawi <- length(unique(data_cati_cawi$ID_t))
num_id_eps <- length(unique(data_life_course$ID_t))


# number of respondents in both data sets
  ## all respondents in CATI_CAWI have at least one observation in the episode data set
id_keep_caticawi_eps <- intersect(unique(data_cati_cawi$ID_t), unique(data_life_course$ID_t))
sum(id_keep_caticawi_eps %in% unique(data_cati_cawi$ID_t))
  ## keep only the respondents in both data sets
data_life_course <- data_life_course %>%
  subset(ID_t %in% id_keep_caticawi_eps)
num_id_adj_1 <- length(unique(data_life_course$ID_t))



#%%%%%%%%%%%%%%%%%%#
#### UNI SPELLS ####
#%%%%%%%%%%%%%%%%%%#

# Next, the uni spells are identified. I am only interested in episodes in
# which the student studies. Hence, from data_cati_cawi, I keep only rows for which 
# the interview dates are within the uni spell (from the episode data).
# To do so, a data frame with only the university spells is generated
# so only observations are kept for which the respondent currently studies.
data_uni <- data_life_course %>%
  filter(educ_study == 1)
length(unique(data_uni$ID_t)) # all individuals are kept because all have university episodes


# show example for merging
data_uni %>% dplyr::select(ID_t, start_date, end_date, sptype_2) %>% 
  subset(ID_t %in% c(7001969, 7002033, 7019370))
data_cati_cawi %>% 
  dplyr::select(ID_t, treatment_period, starts_with("interview_date")) %>% 
  subset(ID_t %in% c(7001969, 7002033, 7019370))

# 1.) Check if interview date from outcome (interview_date_end in data_cati_cawi)
# is within uni spell, that is between start_date and end_date of data_uni
# 2.) Check if control variables are also asked within university interval, that is
# d1.interview_date_start is also between start_date and end_date
data_check_uni_spell <- 
  sqldf(
    "select d1.ID_t, treatment_period
    from data_cati_cawi AS d1
    inner join data_uni AS d2 on d1.ID_t = d2.ID_t and 
    d1.interview_date_end between d2.start_date and d2.end_date and
    d1.interview_date_start between d2.start_date and d2.end_date"
  ) %>%
  distinct()
data_check_uni_spell %>% subset(ID_t %in% c(7001969, 7002033, 7019370, 7017362))
  ## drop rows where outcome interview is not inside uni spell
  ## this is done via an inner join
data_cati_cawi_eps <- data_cati_cawi %>%
  inner_join(data_check_uni_spell, by = c("ID_t", "treatment_period"))

# check number of respondents
num_id_adj_2 <- length(unique(data_cati_cawi_eps$ID_t)) 
id_drop_uni_spell <- num_id_adj_1 - num_id_adj_2  




#%%%%%%%%%%%%%%%%%%%%#
#### Spell length ####
#%%%%%%%%%%%%%%%%%%%%#

# spell length is calculated here, that is for example, the years of schooling,
# previous years of employment and previous years of study


## MAKE PREPARATIONS FOR CALCULATION ##
#+++++++++++++++++++++++++++++++++++++#

# first, the date closest to the interview_end_date is identified
# this date is used to calculate (or rather adjust) the spell length)
  ## for "controls_bef_outcome" this is interview_date_CATI
  ## for "controls_same_outcome" this is interview_date_start
if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi_eps <- data_cati_cawi_eps %>%
    mutate(interview_date_spell = interview_date_CATI)
} else if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi_eps <- data_cati_cawi_eps %>%
    mutate(interview_date_spell = interview_date_start)
}


# life course data is duplicated for each treatment period
# thus, number of rows for each respondent is nrow(data_life_course)*num_treatment_periods
data_cati_cawi_eps %>%
  dplyr::select(ID_t, treatment_period, interview_date_start, interview_date_spell, interview_date_end) %>%
  subset(ID_t == 7001984)

data_cati_cawi_eps_all <- left_join(data_cati_cawi_eps, data_life_course, by = "ID_t")

data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, sptype_2, start_date, end_date, 
                interview_date_start, interview_date_spell, interview_date_end) %>%
  head(20)


# adjust date to calculate spell length
  ## if end_date of spell is larger than the interview date AND the interview
  ## date inside the episode, the spell length needs to be adjusted because spell 
  ## is not finished
  ## to simplify this process, I create a new date variable
  ## this variable equals the real end date if the interview date takes place
  ## after the end of the period, otherwise, the interview date
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(end_date_adj = case_when(
    end_date > interview_date_spell & interview_date_spell > start_date ~ interview_date_spell,
    TRUE ~ end_date
  ))


data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, sptype_2, start_date, end_date, end_date_adj, 
                interview_date_start, interview_date_spell, interview_date_end) %>%
  head(20)



## CALCULATE SPELL LENGTH ##
#++++++++++++++++++++++++++#


# spell length is only calculated if it is relevant for the treatment period,
# i.e. only for current and previous episodes but not for future one
# indicator = 1 if episode is relevant for treatment, i.e., if interview date
# is larger than start_date of episode
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>% 
  mutate(eps_rel = if_else(interview_date_spell > start_date, 1, 0))

data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, sptype_2, start_date, end_date, end_date_adj, 
                interview_date_start, interview_date_spell, interview_date_end, eps_rel) %>%
  head(20)

# spell length in years: start_date - end_date
# is only calculated if episode is relevant for the respective treatment period
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(
    spell_length_years = case_when(
      eps_rel == 1 ~ as.numeric(difftime(end_date_adj, start_date, units = "weeks")) / 52.5, 
      TRUE ~ as.double(NA)
    )
  ) 


data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, sptype_2, start_date, end_date, end_date_adj, 
                interview_date_start, interview_date_spell, interview_date_end, eps_rel,
                spell_length_years) %>%
  head(20)


## CUMULATED SPELL LENGTH ##
#++++++++++++++++++++++++++#

# cumulated years spent in education type
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  # group by ID_t and education type
  group_by(ID_t, treatment_period, sptype_2) %>%
  # calculate cumulative sum 
  mutate(spell_length_cum_years = cumsum(spell_length_years))


data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, sptype_2, start_date, end_date, end_date_adj, 
                interview_date_start, interview_date_spell, interview_date_end, eps_rel,
                spell_length_years, spell_length_cum_years) %>%
  head(20)


# restructure so that one variable for each education type
# fill missing values 
data_cati_cawi_eps_all <- 
  data_cati_cawi_eps_all %>%
  group_by(ID_t) %>%
  mutate(row = row_number()) %>% # to avoid error message
  mutate(uni_spell = ifelse(sptype_2 == "Uni", 1, 0)) %>%
  pivot_wider(names_from = sptype_2, values_from = spell_length_cum_years,
              names_prefix = "spell_length_cum_") %>%
  dplyr::select(-row) %>%
  fill(starts_with("spell_length_cum_"), .direction = "down")


data_cati_cawi_eps_all %>%
  subset(ID_t == 7001984) %>%
  dplyr::select(ID_t, treatment_period, start_date, end_date, end_date_adj, 
         interview_date_start, interview_date_spell, interview_date_end, eps_rel, 
         starts_with("spell_length_cum_")) %>%
  head(20)


# all missing values in spell_length variables are replaced by 0
# missings indicate that person has no observation for spell type, i.e., length is 0
data_cati_cawi_eps_all <- data_cati_cawi_eps_all %>%
  mutate(across(starts_with("spell_"),  ~replace_na(., 0)))


## UNI SPELL DATA SET ##
#++++++++++++++++++++++#

# keep only uni spells
length(unique(data_cati_cawi_eps_all$ID_t))
data_cati_cawi_eps_all_2 <- data_cati_cawi_eps_all %>% 
  filter(uni_spell == 1 & eps_rel == 1) %>%
  distinct()
length(unique(data_cati_cawi_eps_all_2$ID_t)) 

# there may be duplicates as in this example because during interviews
# respondent has two uni spells within one treatment period
data_cati_cawi_eps_all_2 %>% 
  dplyr::select(ID_t, treatment_period, uni_spell) %>%
  group_by(ID_t, treatment_period) %>%
  count(uni_spell) %>% filter(n > 1)
data_cati_cawi_eps_all_2 %>% subset(ID_t == 7001992) %>%
  dplyr::select(ID_t, treatment_period, starts_with("interview_date"), start_date, end_date)

# in this case, the last (most recent) uni spell is kept
data_cati_cawi_eps_all_2 <- data_cati_cawi_eps_all_2 %>%
  ungroup() %>%
  arrange(ID_t, treatment_period, start_date) %>%
  group_by(ID_t, treatment_period) %>%
  dplyr::slice(n())
data_cati_cawi_eps_all_2 %>% subset(ID_t == 7001992) %>%
  dplyr::select(ID_t, treatment_period, starts_with("interview_date"), start_date, end_date)  


## LENGTH CURRENT STUDY ##
#++++++++++++++++++++++++#

# calculate length of current study: subtracting interview_spell with start_date
# note: the cumulated variable is the cumulated sum of previous study time (
# including the current one
data_cati_cawi_eps_all_2 <- data_cati_cawi_eps_all_2 %>%
  mutate(spell_length_current_Uni = case_when(
    uni_spell == 1 & eps_rel == 1 ~ as.numeric(difftime(interview_date_spell, start_date, units = "weeks")) / 52.5,
    TRUE ~ as.double(NA)
  ))


# check operations
check <- data_cati_cawi_eps_all_2 %>% 
  dplyr::select(
    ID_t, start_date, end_date, end_date_adj, treatment_period, interview_date_start,
    interview_date_spell, interview_date_end, starts_with("spell_length_cum_"), 
    spell_length_current_Uni
    )
  ## check if all spell_length are positive
check %>% ungroup() %>% dplyr::select(starts_with("spell_length_cum_")) %>% summarize(sum(. < 0))
check %>% ungroup() %>% dplyr::select(starts_with("spell_length_cum_")) %>% summarize(summary(.))

# drop variables which are not of interest anymore
# order data frame
data_cati_cawi_unispell <- data_cati_cawi_eps_all_2 %>%
  dplyr::select(-c(spell_length_years, uni_spell, eps_rel, sptype, start_date, end_date)) %>%
  dplyr::select(ID_t, treatment_period, starts_with("interview_"), starts_with("sport"),
         starts_with("grade"), starts_with("spell_"), starts_with("educ_"),
         everything()) %>%
  ungroup()
length(unique(data_cati_cawi_unispell$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### CURRENT EMPLOYMENT ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


# of interest is if student is working during study
# to identify this, I need to come back to the previous data frame
# I only keep respondents who are previous data frame
id_keep_emp <- unique(data_cati_cawi_unispell$ID_t)
data_emp <- data_life_course %>% 
  subset(ID_t %in% id_keep_emp) %>%
  filter(sptype_2 == "Emp") %>%
  dplyr::select(ID_t, sptype_2, start_date, end_date, matches("emp"))

length(unique(data_emp$ID_t)) # number of students who work / have worked at any time

# employment start needs to be smaller than interview date
# employment ends needs to be larger than the interview date
# -> Only in this case employment is during university study
# the created data frame identifies the employment spells during the university study
data_emp_uni <- data_cati_cawi_unispell %>% 
  subset(ID_t %in% unique(data_emp$ID_t)) %>% 
  dplyr::select(ID_t, interview_date_spell)
length(unique(data_emp_uni$ID_t)) # number of students who work / have worked at any time

  ## example
data_emp_uni %>% subset(ID_t == 7003179)
data_emp %>% subset(ID_t == 7003179)
  
data_check_emp <-
  sqldf(
    "SELECT c.ID_t, start_date, interview_date_spell, emp_prof_pos, emp_student_job, 
    emp_student_job_type, emp_student_job_rel, emp_net_income, emp_act_work_hours, emp
    FROM data_emp AS e
    INNER JOIN data_emp_uni AS c ON e.ID_t = c.ID_t AND
    e.start_date <= c.interview_date_spell AND e.end_date >= c.interview_date_spell
    ")
data_check_emp %>% subset(ID_t == 7003179)
length(unique(data_check_emp$ID_t)) # number of student who work during study


# calculate length of current employment 
data_check_emp <- data_check_emp %>%
 mutate(spell_length_current_Emp = as.numeric(difftime(interview_date_spell, start_date, units = "weeks")) / 52.5) %>%
  dplyr::select(-start_date)


# add prefix for emp_ variables: all starts with emp_current
data_check_emp <- cbind(
  data_check_emp %>%
    dplyr::select(starts_with("emp")) %>%
    rename_with(~ gsub(.x, pattern = "emp", replacement = "emp_current")),
  data_check_emp %>%
    dplyr::select(-starts_with("emp"))
)

# check for duplicates (that is two employment spells within one treatment period)
data_check_emp %>%
  group_by(ID_t, interview_date_spell) %>%
  count() %>% filter(n > 1)
data_check_emp %>% subset(ID_t == 7001970) %>% dplyr::select(ID_t, interview_date_spell, everything())
  ## in case of duplicated keep the employment spell with the longer duration
data_check_emp <- data_check_emp %>%
  arrange(ID_t, interview_date_spell, spell_length_current_Emp) %>%
  group_by(ID_t, interview_date_spell) %>%
  dplyr::slice(n())
data_check_emp %>% subset(ID_t == 7001970) %>% dplyr::select(ID_t, interview_date_spell, everything())

# add employment to respective treatment period
data_cati_cawi_unispell_emp <- left_join(
  data_cati_cawi_unispell, data_check_emp, by = c("ID_t", "interview_date_spell")
)

# set emp_current indicator to 0 for individuals who do not work during their study
table(data_cati_cawi_unispell_emp$emp_current, useNA = "always")
data_cati_cawi_unispell_emp <- data_cati_cawi_unispell_emp %>%
  mutate(emp_current = ifelse(is.na(emp_current), 0, emp_current))
table(data_cati_cawi_unispell_emp$emp_current, useNA = "always")

# drop the employment variables not needed anymore
cols_emp_drop <- data_cati_cawi_unispell_emp %>% dplyr::select(starts_with("emp")) %>% colnames()
cols_emp_drop <- cols_emp_drop[!str_detect(cols_emp_drop, "current")]
data_cati_cawi_unispell_emp <- data_cati_cawi_unispell_emp %>%
  dplyr::select(-all_of(cols_emp_drop))


# number of respondents
length(unique(data_cati_cawi_unispell_emp$ID_t)) # unchanged


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# show reduction of sample size
print(paste("Number of respondents in episode data before data merge and preparation:", num_id_eps))
print(paste("Number of respondents in CATI&CAWI before data merge and preparation:", num_id_cati_cawi))

print(paste("Number of respondents after merge:", num_id_adj_1))
print(paste("Number of respondents after identifying treatment periods within uni spells:", num_id_adj_2))


# number of respondents, rows and columns in final data frame
print(paste("Number of respondents after merge process:", length(unique(data_cati_cawi_unispell_emp$ID_t))))
print(paste("Number of rows after merge process:", nrow(data_cati_cawi_unispell_emp)))
print(paste("Number of columns after merge process:", ncol(data_cati_cawi_unispell_emp)))


# save data frame
if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi_eps_save <- paste0("Data/Grades/Prep_4/prep_4_merge_cati_cawi_eps_treat", 
                                    treatment_repl, ".rds") 
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi_eps_save <- paste0("Data/Grades/Prep_4/prep_4_merge_cati_cawi_eps_treat", 
                                    treatment_repl, "_robustcheck.rds")  
}
saveRDS(data_cati_cawi_unispell_emp, data_cati_cawi_eps_save)

# save in excel
df_excel_save <- data.frame(
  "data_prep_step" = "merge_cati_cawi_eps",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = NA, 
  "num_id" = length(unique(data_cati_cawi_unispell_emp$ID_t)), 
  "num_rows" = nrow(data_cati_cawi_unispell_emp),
  "num_cols" = ncol(data_cati_cawi_unispell_emp),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "grade")

