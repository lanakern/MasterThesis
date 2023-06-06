#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Interview Participation: Personality ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, a data frame is generated which contains information about
# the participation in each interview. This data set is used to determine the 
# start and end of the treatment period for each respondent. 
#++++
# 1.) Only respondents who are also included in the episode data are kept.
#++++
# 2.) Handle missing values in interview date
# -> year: year from wave is inserted
# -> month & day: previous month and day from respective survey (CATI or CAWI)
# is inserted
# -> For 2011 (CAWI) no previous CAWI waves exist, so that date from CATI is
# taken + 6 months (this is the usual difference between those interviews).
#++++
# 3.) Generate indices for treatment period start and end (depends on user
# selection on how to do so). 
# -> "controls_same_outcome" (main analysis): CAWI CATI
# -> "controls_bef_outcome" (robustness check): CATI CAWI CATI (Not applied)
#++++
# 4.) Subset on length of treatment period
# -> "controls_same_outcome": 12 months
# -> "controls_bef_outcome" : 24 months (not applied)
#++++
# --> Results in a panel data frame: one row for each respondent-wave
# combination with information about start and end of treatment period 
# (enumerated). 
#++++


#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%#


# load data 
data_cohort_profile_raw <- readRDS("Data/Personality/Prep_1/prep_1_cohort_profile_personality.rds")

# number of respondents
num_id_start <- length(unique(data_cohort_profile_raw$ID_t)) # 17,909

# keep only respondents who have observations in episode data
data_life_course <- readRDS("Data/Personality/Prep_2/prep_2_life_course_personality.rds")
id_keep <- unique(data_life_course$ID_t) # 17,740

data_cohort_profile_raw <- data_cohort_profile_raw %>%
  subset(ID_t %in% id_keep)
num_id_adj_1 <- length(unique(data_cohort_profile_raw$ID_t)) # 17,740


#%%%%%%%%%%%%%%%%%%%%%%#
#### Interview Date ####
#%%%%%%%%%%%%%%%%%%%%%#

data_cohort_profile <- data_cohort_profile_raw

# check for missing values in interview day, month, and year
# (there are also many missings in competence month and year, but not relevant
# because competence measures are only surveyed infrequently)
  ## 1551 each
sum(is.na(data_cohort_profile$interview_day))
sum(is.na(data_cohort_profile$interview_month))
sum(is.na(data_cohort_profile$interview_year))
  ## missing for same respondent?: 
  ## yes all have all date info missing
data_cohort_profile %>% 
  dplyr::select(ID_t, starts_with("interview_")) %>%
  group_by(ID_t) %>%
  # count number of NAs for each interview_* column
  summarise_all(~sum(is.na(.))) %>% 
  # take the row sum
  transmute(ID_t, sumNA = rowSums(.[-1])) %>%
  # check unique values: 0, 3, 6 
  dplyr::select(sumNA) %>% pull() %>% unique()
  ## in which wave are the missing values?
  ## 2013 and 2017 CATI as well as 2011 CAWI
data_cohort_profile[is.na(data_cohort_profile$interview_day), "wave"] %>% unique()


## note that even for CATI wave 2010/2011, some interviews are from 2012 
#data_cohort_profile %>% filter(wave == "2010/2011 (CATI+competencies)") %>% pull(interview_year) %>% unique()

# handle missing values
  ## for missing year insert year from wave
year_wave <- substr(data_cohort_profile[is.na(data_cohort_profile$interview_year), "wave"], 1, 4)
data_cohort_profile[is.na(data_cohort_profile$interview_year), "interview_year"] <- year_wave
sum(is.na(data_cohort_profile$interview_year)) # no missing values are left
  ## CATI missings: insert day and month from previous CATI interview (year is different)
  ## do the same for CAWI
id_miss <- data_cohort_profile[is.na(data_cohort_profile$interview_day), "ID_t"] 
data_cohort_profile <- 
  rbind(
    ## CATI
    data_cohort_profile %>% 
      arrange(ID_t, wave) %>%
      subset(ID_t %in% id_miss) %>%
      filter(grepl("CATI", wave)) %>%
      group_by(ID_t) %>%
      fill(c(interview_day, interview_month), .direction = "down"),
    ## CAWI
    data_cohort_profile %>% 
      arrange(ID_t, wave) %>%
      subset(ID_t %in% id_miss) %>%
      filter(!grepl("CATI", wave)) %>%
      group_by(ID_t) %>%
      fill(c(interview_day, interview_month), .direction = "down")
  ) %>%
  ## add data for respondents without missings
  rbind(
    data_cohort_profile %>% 
      subset(!ID_t %in% id_miss)
  )


# there are some missing values left, but first dates are generated
sum(is.na(data_cohort_profile$interview_day)) # 23
sum(is.na(data_cohort_profile$interview_month)) # 23
  ## check for competence date if everytime month and year is missing
sum(is.na(data_cohort_profile$competence_month))
sum(is.na(data_cohort_profile$competence_year))
data_cohort_profile %>% 
  dplyr::select(ID_t, competence_month, competence_year) %>%
  group_by(ID_t) %>%
  # count number of NAs for each interview_* column
  summarise_all(~sum(is.na(.))) %>% 
  # take the row sum
  transmute(ID_t, sumNA = rowSums(.[-1])) %>%
  # check unique values: 0, 3, 6 
  dplyr::select(sumNA) %>% pull() %>% unique()
  ## create date variable for interview and competence
data_cohort_profile <- data_cohort_profile %>%
  mutate(
    interview_date = case_when(
      !is.na(interview_month) & !is.na(interview_day) ~  mdy(paste(paste(interview_month, interview_day), interview_year, sep = ",")),
      TRUE ~ as.Date(NA)
    ),
    competence_date = case_when(
      !is.na(competence_month) & !is.na(competence_year) ~ mdy(paste(paste(competence_month, 1), competence_year, sep = ",")),
      TRUE ~ as.Date(NA))
  )
sum(is.na(data_cohort_profile$interview_date)) # as expected 23 missing values are left
sum(is.na(data_cohort_profile$competence_date))


  ## for all other missing values in interview date take previous date + 6 months 
  ## this is realistic as only CAWI interview dates in 2011 are missing which
  ## usually take place 6 months after CATI
data_cohort_profile[is.na(data_cohort_profile$interview_date), "wave"] %>% unique()
data_cohort_profile <- data_cohort_profile %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  mutate(
    interview_date = case_when(
      is.na(interview_date) ~ as.Date(lag(interview_date) %m+% months(6)), 
      TRUE ~ as.Date(interview_date)),
  ) %>%
  ungroup()


# check if all missing values in interview_date are gone
sum(is.na(data_cohort_profile$interview_date))

# keep only variables needed for the upcoming analysis
data_cohort_profile <- data_cohort_profile %>%
  dplyr::select(ID_t, wave, interview_date, competence_date) %>%
  ungroup()



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Indices ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# generate second wave variable to easily identify if wave is from CATI or cAWI
data_cohort_profile <- data_cohort_profile %>% 
  mutate(wave_2 = ifelse(grepl("CAWI", wave), "CAWI", "CATI"))

# generate new data frame
data_cohort_profile_prep_1 <- data_cohort_profile


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Controls are taken before outcome: search for "CATI CAWI CATI" combinations #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

if (cohort_prep == "controls_bef_outcome") {
  
  # End of treatment period: treatment always ends with CATI interview, but
  # a CAWI and CATI interview has to be taken place before
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>% 
    group_by(ID_t) %>%
    mutate(
      wave_2_lag = lag(wave_2),
      CATI_imp = case_when(wave_2 == "CATI" & wave_2 != wave_2_lag ~ 1, TRUE ~ 0)
      ) %>%
    mutate(treatment_ends = ifelse(CATI_imp == 1, cumsum(CATI_imp), NA)) %>%
    #mutate(treatment_ends = treatment_ends - 1) %>%
    replace_na(list(treatment_ends = 0))
  
  
  # Start of treatment period: treatment period always starts with CATI
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(treatment_ends = na_if(treatment_ends, 0)) %>%
    mutate(treatment_starts = treatment_ends) %>% 
    fill(treatment_starts, .direction = "up") %>%
    mutate(treatment_starts = ifelse(!is.na(treatment_ends), treatment_ends + 1, treatment_starts)) %>%
    ungroup()
  
  
  # Handle CAWI duplicates 
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(case_when(
      wave_2 == "CAWI" ~ row_number() == n()
    )) %>%
    rbind(data_cohort_profile_prep_1 %>% filter(wave_2 == "CATI")) %>% 
    arrange(ID_t, wave, treatment_starts, treatment_ends) %>%
    ungroup()
  

  # Handle CATI duplicates 
  data_cohort_profile_prep_1 <- 
    data_cohort_profile_prep_1 %>%
    ungroup() %>% group_by(ID_t) %>%
    mutate(wave_2_lead = lead(wave_2)) %>%
    mutate(treatment_starts = ifelse(
      wave_2 == wave_2_lead, NA, treatment_starts
    ))
  
  
  # Drop individuals without any treatment period 
    ## identify individuals 
  id_drop <- 
    data_cohort_profile_prep_1 %>%
    group_by(ID_t) %>%
    filter(all(is.na(treatment_ends)) & all(is.na(treatment_starts))) %>%
    pull(ID_t) %>% unique()
  id_drop_num <- length(id_drop)
    ## drop individuals
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    subset(!ID_t %in% id_drop)
  
  
  # Drop rows not belonging to a treatment period 
  
  # drop also rows for individuals who have treatment periods but missing values
  # in both variables: treatment_starts and treatment_ends; those rows are not
  # useful as they are not used.
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter_at(vars(treatment_starts, treatment_ends), any_vars(!is.na(.)))
  
  # following CATI surveys
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(row_number() == n())

  # new number of individuals
  num_id_adj_2 <- length(unique(data_cohort_profile_prep_1$ID_t))
  
  # drop variables not needed anymore
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    dplyr::select(-c(wave_2_lag, wave_2_lead, CATI_imp))
  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Controls are taken at same time then outcome: search for "CATI CAWI" combinations #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

} else if (cohort_prep == "controls_same_outcome") {
  
  # treatment always starts with CAWI interview
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    arrange(ID_t, interview_date) %>%
    group_by(ID_t, wave_2) %>%
    mutate(treatment_starts = ifelse(grepl("CAWI", wave_2), row_number(), NA)) %>%
    ungroup() %>% group_by(ID_t) %>%
    mutate(treatment_starts_help = treatment_starts) %>% 
    fill(treatment_starts_help, .direction = "down")
  
  # treatment always ends with CAWI interview
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, wave_2, treatment_starts_help) %>%
    mutate(treatment_ends = NA) %>% 
    mutate(treatment_ends = ifelse(
      wave_2 == "CATI", replace(treatment_ends, 1,treatment_starts_help), NA
    )) %>%
    ungroup()
  
  # drop rows with NA in treatment_starts and treatment_ends
    ## to do so generate merge data frame for treatment period numbers kept
  df_keep_period <- data_cohort_profile_prep_1 %>% 
    ungroup() %>%
    filter(treatment_starts_help == treatment_ends) %>%
    dplyr::select(ID_t, treatment_starts_help)
    ## merge
  data_cohort_profile_prep_1 <- inner_join(
    data_cohort_profile_prep_1, df_keep_period, by = c("ID_t", "treatment_starts_help")
  ) %>% dplyr::select(-treatment_starts_help) %>% ungroup()
  
  # drop rows where treatment_starts & treatmend_ends have missing values
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter(!(is.na(treatment_starts) & is.na(treatment_ends)))
  
  # adjust numbering of treatment start and treatment end so that it always starts
  # with 1 and is then enumerated correctly
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    arrange(ID_t, interview_date) %>%
    group_by(ID_t, wave_2) %>%
    mutate(treatment_starts = ifelse(grepl("CAWI", wave_2) & !is.na(treatment_starts), row_number(), NA)) %>%
    ungroup() %>%
    arrange(ID_t, interview_date) %>%
    group_by(ID_t, wave_2) %>%
    mutate(treatment_ends = ifelse(grepl("CATI", wave_2) & !is.na(treatment_ends), row_number(), NA))
  
  # number of respondents
  num_id_adj_2 <- length(unique(data_cohort_profile_prep_1$ID_t))
  
  # checks
    ## treatment start always have treatment end -> result is 0
  data_cohort_profile_prep_1 %>%
    group_by(ID_t) %>%
    mutate(
      max1 = max(treatment_starts, na.rm = TRUE), max2 = max(treatment_ends, na.rm = TRUE)
    ) %>%
    dplyr::select(ID_t, max1, max2) %>% distinct() %>%
    summarize(check = sum(max1 != max2)) %>% summarize(sum(check))
    ## treatment start is always CATI, treatment end always CAWI
    ## -> result is CATI and CAWI
  data_cohort_profile_prep_1 %>%
    dplyr::select(ID_t, wave_2, treatment_starts) %>%
    na.omit() %>% pull(wave_2) %>% unique()
  
  data_cohort_profile_prep_1 %>%
    dplyr::select(ID_t, wave_2, treatment_ends) %>%
    na.omit() %>% pull(wave_2) %>% unique()
  
} else {
  stop("Please select preparation method.")
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Length of Treatment Period ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


if (cohort_prep == "controls_bef_outcome") {
  # for this approach length is maximum 2 years
  # always starts with CATI and ends with CATI, hence only this is needed
  df_check_length <- data_cohort_profile_prep_1 %>% ungroup() %>% filter(wave_2 == "CATI") 
  
  # extract start date of treatment period
  df_check_length_start <- df_check_length %>% 
    dplyr::select(ID_t, interview_date, treatment_starts) %>% 
    na.omit() %>%
    rename(interview_date_start = interview_date)
  
  # extract end date of treatment period
  df_check_length_end <- df_check_length %>% 
    dplyr::select(ID_t, interview_date, treatment_ends) %>% 
    na.omit() %>%
    rename(interview_date_end = interview_date)
  
  # join start and end date of treatment period and calculate length of treatment
  # period in years
  df_check_length <- inner_join(
    df_check_length_start, df_check_length_end, by = c("ID_t", "treatment_starts" = "treatment_ends")
  ) %>%
    mutate(treatment_length = as.numeric(interview_date_end - interview_date_start) / 365) %>%
    rename(treatment_period = treatment_starts)
  
  summary(df_check_length$treatment_length)
  
  # identify rows to keep and drop
  df_check_length_keep <- df_check_length %>%
    filter(treatment_length <= 2) %>%
    dplyr::select(ID_t, treatment_period) %>%
    mutate(keep_row = TRUE) 
  
  df_check_length_drop <- df_check_length %>%
    filter(treatment_length > 2) %>%
    dplyr::select(ID_t, treatment_period) %>%
    mutate(end_NA = TRUE) 
  
  # starts are dropped and ends are set NA
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    left_join(df_check_length_keep, by = c("ID_t", "treatment_starts" = "treatment_period")) %>%
    left_join(df_check_length_keep, by = c("ID_t", "treatment_ends" = "treatment_period")) %>%
    left_join(df_check_length_drop, by = c("ID_t", "treatment_ends" = "treatment_period")) %>%
    mutate(keep_row = ifelse(is.na(keep_row.x), keep_row.y, keep_row.x))
    ## keep only if keep_row = TRUE
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter(keep_row == TRUE) %>% dplyr::select(-(starts_with("keep_row")))
    ## set treatment_ends = NA if end_NA = TRUE
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(treatment_ends = case_when(end_NA == TRUE ~ as.double(NA), TRUE ~ treatment_ends)) %>%
    dplyr::select(-end_NA)
  
  # adjust number of respondents
  num_id_adj_3 <- length(unique(data_cohort_profile_prep_1$ID_t))
  id_drop_num_length <- num_id_adj_2 - num_id_adj_3
  
} else if (cohort_prep == "controls_same_outcome") {
  # for this approach length is maximum 1 year
  # always starts with CATI and ends with CAWI, hence only this is needed
  df_check_length <- data_cohort_profile_prep_1 %>% ungroup()
  
  # extract start date of treatment period
  df_check_length_start <- df_check_length %>% 
    dplyr::select(ID_t, interview_date, treatment_starts) %>% 
    na.omit() %>%
    rename(interview_date_start = interview_date)
  
  # extract end date of treatment period
  df_check_length_end <- df_check_length %>% 
    dplyr::select(ID_t, interview_date, treatment_ends) %>% 
    na.omit() %>%
    rename(interview_date_end = interview_date)
  
  # join start and end date of treatment period and calculate length of treatment
  # period in years
  df_check_length <- inner_join(
    df_check_length_start, df_check_length_end, by = c("ID_t", "treatment_starts" = "treatment_ends")
  ) %>%
    mutate(treatment_length = as.numeric(interview_date_end - interview_date_start) / 365) %>%
    rename(treatment_period = treatment_starts)
  
  summary(df_check_length$treatment_length)
  
  # merge length of treatment period to data frame
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(treatment_period = case_when(
      is.na(treatment_starts) ~ treatment_ends, TRUE ~ treatment_starts
    )) %>% 
    left_join(
      df_check_length %>% dplyr::select(ID_t, treatment_period, treatment_length),
      by = c("ID_t", "treatment_period")
    )
  
  # subset data frame to only keep observations with treatment period below 2 years
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>% 
    filter(treatment_length <= 1) %>%
    dplyr::select(-c(treatment_period, treatment_length))
  
  # adjust number of respondents
  num_id_adj_3 <- length(unique(data_cohort_profile_prep_1$ID_t))
  id_drop_num_length <- num_id_adj_2 - num_id_adj_3
  } else {
    stop("Please select preparation method.")
}
  



#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# check for duplicates
sum(duplicated(data_cohort_profile_prep_1))

# check missing values
colSums(is.na(data_cohort_profile_prep_1))

# check interview dates
min(data_cohort_profile_prep_1$interview_date) 
max(data_cohort_profile_prep_1$interview_date) 

# maximum number of treatment periods
max(data_cohort_profile_prep_1$treatment_starts, na.rm = T)

# ungroup
data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>% ungroup()

# number of respondents, number of rows and columns
print(paste("Number of respondents before data preparation:", num_id_start))
print(paste("Number of respondents after subsetting on episode data:", num_id_adj_1))
print(paste("Number of respondents after data preparation:", length(unique(data_cohort_profile_prep_1$ID_t))))
print(paste("Number of rows", nrow(data_cohort_profile_prep_1)))
print(paste("Number of columns", ncol(data_cohort_profile_prep_1)))
  

# save data
  ## generate prefix for robustness check
if (cohort_prep == "controls_same_outcome") {
  data_cohort_profile_save <- "Data/Personality/Prep_2/prep_2_cohort_profile_personality.rds"
} else {
  data_cohort_profile_save <- "Data/Personality/Prep_2/prep_2_cohort_profile_personality_robustcheck.rds"
}

saveRDS(data_cohort_profile_prep_1, data_cohort_profile_save)


# save number of rows, columns, and respondents in excel file
  ## create data frame with information
df_excel_save <- data.frame(
  "data_prep_step" = "cohort_profile",
  "data_prep_choice_cohort" = cohort_prep,
  "num_id" = length(unique(data_cohort_profile_prep_1$ID_t)), 
  "num_rows" = nrow(data_cohort_profile_prep_1),
  "num_cols" = ncol(data_cohort_profile_prep_1),
  "time_stamp" = Sys.time()
)
  ## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "personality")

  