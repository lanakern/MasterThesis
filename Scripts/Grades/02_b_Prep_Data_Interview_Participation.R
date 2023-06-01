#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Interview Participation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
# taken + 6 months (this is the usual different between those interviews).
#++++
# 3.) Generate indices for treatment period start and end (depends on user
# selection on how to do so). 
# -> "controls_same_outcome" (main analysis): the control variables, treatment
# and outcome are taken from the same interview, i.e., treatment periods
# are constructed so that they start with CATI and end with next CAWI.
# respondents are dropped for who no treatment periods can be identified that
# is 1.) because they only take part in CATI interviews and 2.) because they
# take only part in first CAWI interview which takes places BEFORE firsr CATI interview.
# -> "controls_bef_outcome" (used as robustness check): the control variables
# are tried as good as possible to be measured before treatment to account
# for endogeneity. The drawback of this approach is that the treatment periods
# are very long since treatment periods start with CAWI, follow by CATI, and 
# end with CAWI interview.
# respondents are dropped because 1.) they only participate in the CATI survey
# 2.) they only participate in one CAWI survey 3.) they only participate in
# first CATI but no other cATI survey -> CAWI CATI CAWI cannot be generated.
#++++
# 4.) Subset on length of treatment period
# -> "controls_same_outcome": 12 months
# -> "controls_bef_outcome" : 24 months
#++++
# --> Results in a panel data frame: one row for each respondent-wave
# combination with information about start and end of treatment period 
# (enumerated). 
#++++


#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%#


# load data 
data_cohort_profile_raw <- readRDS("Data/Grades/Prep_1/prep_1_cohort_profile.rds")

# number of respondents
num_id_start <- length(unique(data_cohort_profile_raw$ID_t)) # 17,909

# keep only respondents who have observations in episode data
data_life_course <- readRDS("Data/Grades/Prep_2/prep_2_life_course.rds")
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
# Controls are taken before outcome: search for "cAWI CATI CAWI" combinations #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

if (cohort_prep %in% c("controls_bef_outcome", "controls_bef_all")) {
  
  # End of treatment period: treatment always ends with CAWI interview, but
  # a CATI and CAWI interview has to be taken place before
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>% 
    group_by(ID_t) %>%
    mutate(
      wave_2_lag = lag(wave_2),
      CAWI_imp = ifelse(wave_2 == "CAWI" & wave_2 != wave_2_lag, 1, 0)
      ) %>%
    mutate(treatment_ends = ifelse(CAWI_imp == 1, cumsum(CAWI_imp), NA)) %>%
    mutate(treatment_ends = treatment_ends - 1) %>%
    replace_na(list(treatment_ends = 0))
  
  
  # Start of treatment period: treatment period always starts with CAWI
  # both CAWI & CATI before treatment_end CAWI have the same number as they
  # are in the same treatment period
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(treatment_ends = na_if(treatment_ends, 0)) %>%
    mutate(treatment_starts = treatment_ends) %>% 
    fill(treatment_starts, .direction = "up") %>%
    mutate(treatment_starts = ifelse(!is.na(treatment_ends), treatment_ends + 1, treatment_starts)) %>%
    ungroup()
  
  
  # Handle CATI duplicates 
    # drop CATI observations not needed to generate treatment period
    # that is, for CATI duplicates keep latest CATI survey
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(case_when(
      wave_2 == "CATI" ~ row_number() == n()
    )) %>%
    rbind(data_cohort_profile_prep_1 %>% filter(wave_2 == "CAWI")) %>% 
    arrange(ID_t, wave, treatment_starts, treatment_ends) %>%
    ungroup()
  

  # Handle CAWI duplicates 
    ## set treatment_starts variable NA is subsequent wave is also CAWI
    ## in this case only second CAWI wave should be kept
  data_cohort_profile_prep_1 <- 
    data_cohort_profile_prep_1 %>%
    ungroup() %>% group_by(ID_t) %>%
    mutate(wave_2_lead = lead(wave_2)) %>%
    mutate(treatment_starts = ifelse(
      wave_2 == wave_2_lead, NA, treatment_starts
    ))
  
  
  # Drop individuals without any treatment period 
  
  # not for all individuals it is possible to generate treatment periods
  # this is for instance if the individual only participated in one 
  # or even non CAWI survey
  # In this case, the treatment_starts and treatment_ends variables only contain
  # missing values 
  data_cohort_profile_prep_1 %>% subset(ID_t == 7007385)
  
  # identify individuals (5770 individuals)
  id_drop <- 
    data_cohort_profile_prep_1 %>%
    group_by(ID_t) %>%
    filter(all(is.na(treatment_ends)) & all(is.na(treatment_starts))) %>%
    pull(ID_t) %>% unique()
  id_drop_num <- length(id_drop)
  
  # analyse dropped individuals
  check_drop <- data_cohort_profile_prep_1 %>% subset(ID_t %in% id_drop)
    ## number of individuals only in CATI survey
  id_drop_num_CATI <- length(unique(check_drop$ID_t)) - check_drop %>% filter(wave_2 == "CAWI") %>% pull(ID_t) %>% unique() %>% length()
    ## look at individuals also in cawi survey
  id_drop_CAWI <- check_drop %>% filter(wave_2 == "CAWI") %>% pull(ID_t) %>% unique()
  df_check <- data_cohort_profile_prep_1 %>% subset(ID_t %in% id_drop_CAWI) %>% arrange(ID_t, wave)
    ## most of the remaining only take part in one CAWI interview
  df_check %>% group_by(ID_t) %>% count(wave_2) %>% filter(wave_2 == "CAWI") %>% pull(n) %>% table()
  data_cohort_profile_prep_1 %>% subset(ID_t == 7002266)
  data_cohort_profile_prep_1 %>% subset(ID_t == 7002964)
    ## all only take part in one CATI
  df_check %>% group_by(ID_t) %>% count(wave_2) %>% filter(wave_2 == "CATI") %>% pull(n) %>% table()
  id_drop_num_only_one <- id_drop_num - id_drop_num_CATI
  
  # drop individuals
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    subset(!ID_t %in% id_drop)
  
  
  # Drop rows not belonging to a treatment period 
  
  # drop also rows for individuals who have treatment periods but missing values
  # in both variables: treatment_starts and treatment_ends; those rows are not
  # useful as they are not used.
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter_at(vars(treatment_starts, treatment_ends), any_vars(!is.na(.)))
  
  # following CAWI surveys
  
  # if two CAWI surveys appear sequentially, that is for instance CAWI12 CAWI13 CATI13 CAWI14
  # the second CAWI row is kept so that CAWI13 CATI13 CAWI14
  # individual example: 7003396
  # if two CATI survey appear sequentially, the same is done 
  # individual example: 7001969
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(row_number() == n())
  
  
  # new number of individuals
  num_id_adj_2 <- length(unique(data_cohort_profile_prep_1$ID_t))
  
  # drop variables not needed anymore
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    dplyr::select(-c(wave_2_lag, wave_2_lead, CAWI_imp))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Controls are taken before treatment and then outcome: search for "CAWI CATI CAWI CAWI" combinations #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
} else if (cohort_prep == "controls_treatment_outcome") {
  
  # End of treatment period: treatment always ends with CAWI interview, but
  # a CATI and CAWI interview has to be taken place before
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    as.data.frame() %>%
    group_by(ID_t) %>%
    mutate(
      first_CATI = as.numeric(wave_2 == "CATI" & !duplicated(wave_2 == "CATI")),
      first_CAWI = as.numeric(wave_2 == "CAWI" & !duplicated(wave_2 == "CAWI")),
      first_CAWI_fill = ifelse(as.numeric(first_CAWI) == 0, NA, as.numeric(first_CAWI)),
      wave_3 = ifelse(first_CAWI == 1 & wave_2 == "CAWI", "USED", wave_2)
      )  %>%
    fill(first_CAWI_fill, .direction = "down") %>%
    mutate(
      first_CAWI_fill = ifelse(first_CAWI == 1 & first_CAWI_fill == 1, NA, first_CAWI_fill), 
      second_CAWI = as.numeric(wave_3 == "CAWI" & first_CAWI_fill == 1 & !duplicated(wave_3 == "CAWI")),
      second_CAWI_fill = ifelse(as.numeric(second_CAWI) == 0, NA, as.numeric(second_CAWI)),
      wave_4 = ifelse(second_CAWI == 1 & wave_3 == "CAWI", "USED", wave_3)
    ) %>% 
    fill(second_CAWI_fill, .direction = "down") %>%
    mutate(
      second_CAWI_fill = ifelse(second_CAWI == 1 & second_CAWI_fill == 1, NA, second_CAWI_fill), 
      third_CAWI = as.numeric(wave_4 == "CAWI" & second_CAWI_fill == 1 & !duplicated(wave_4 == "CAWI")),
    ) %>%
    dplyr::select(ID_t, wave, wave_2, interview_date, competence_date, first_CATI, first_CAWI, second_CAWI, third_CAWI) %>%
    mutate(
      second_CAWI = ifelse(second_CAWI == 1, 2, second_CAWI), third_CAWI = ifelse(third_CAWI == 1, 3, third_CAWI),
      drop_col = ifelse(first_CATI == 0 & first_CAWI == 0 & second_CAWI == 0 & third_CAWI == 0, 1, 0)
    ) %>% 
    filter(drop_col != 1)
  
  
  data_cohort_profile_prep_1$first_CATI_help <- 
    data_cohort_profile_prep_1 %>% ungroup() %>% dplyr::select(contains("_CAWI")) %>% rowSums() 
  data_cohort_profile_prep_1[data_cohort_profile_prep_1$wave_2 == "CAWI","first_CATI_help"] <- data_cohort_profile_prep_1[data_cohort_profile_prep_1$wave_2 == "CAWI","first_CATI_help"] + 1
  data_cohort_profile_prep_1[data_cohort_profile_prep_1$wave_2 == "CATI","first_CATI_help"] <- data_cohort_profile_prep_1[data_cohort_profile_prep_1$wave_2 == "CATI","first_CATI"]
  
  
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(
      treatment_starts = ifelse(first_CATI_help %in% c(1,2,3), 1, NA),
      treatment_ends = ifelse(first_CATI_help == 4, 1, NA)
    )
  
  
  # Drop individuals without any treatment period 
  
  # not for all individuals it is possible to generate treatment periods
  # this is for instance if the individual only participated in one 
  # or even non CAWI survey
  # In this case, the treatment_starts and treatment_ends variables only contain
  # missing values 
  data_cohort_profile_prep_1 %>% subset(ID_t == 7007385)
  
  # identify individuals (7528 individuals)
  id_drop <- 
    data_cohort_profile_prep_1 %>%
    group_by(ID_t) %>%
    filter(all(is.na(treatment_ends))) %>%
    pull(ID_t) %>% unique()
  id_drop_num <- length(id_drop)
  
  # drop individuals
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    subset(!ID_t %in% id_drop)
  
  # Drop rows not belonging to a treatment period 
  
  # drop also rows for individuals who have treatment periods but missing values
  # in both variables: treatment_starts and treatment_ends; those rows are not
  # useful as they are not used.
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter_at(vars(treatment_starts, treatment_ends), any_vars(!is.na(.)))
  
  # new number of individuals
  num_id_adj_2 <- length(unique(data_cohort_profile_prep_1$ID_t))
  
  # drop variables not needed anymore
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    mutate(
      controls = ifelse(first_CATI_help %in% c(1,2), 1, 0),
      treatment = ifelse(first_CATI_help == 3, 1, 0),
      outcome = ifelse(first_CATI_help == 4, 1, 0)
    ) %>%
    dplyr::select(ID_t, wave, interview_date, competence_date, treatment_starts, treatment_ends,
                  controls, treatment, outcome, first_CATI_help)
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Controls are taken at same time then outcome: search for "CATI CAWI" combinations #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
} else if (cohort_prep == "controls_same_outcome") {
  
  # treatment always starts with CATI interview
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    arrange(ID_t, interview_date) %>%
    group_by(ID_t, wave_2) %>%
    mutate(treatment_starts = ifelse(grepl("CATI", wave_2), row_number(), NA)) %>%
    ungroup() %>% group_by(ID_t) %>%
    mutate(treatment_starts_help = treatment_starts) %>% 
    fill(treatment_starts_help, .direction = "down")
  
  # treatment always ends with CAWI interview
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    group_by(ID_t, wave_2, treatment_starts_help) %>%
    mutate(treatment_ends = NA) %>% 
    mutate(treatment_ends = ifelse(
      wave_2 == "CAWI", replace(treatment_ends, 1,treatment_starts_help), NA
    )) %>%
    ungroup()
  
  # show examples
  df_check_1 <- data_cohort_profile_prep_1 %>% 
    subset(ID_t %in% c(7036384, 7011375, 7003857, 7019155, 7001968, 
                       7001972, 7013481, 7013310, 7015624, 7036384,
                       7001969, 7016717, 7004031, 7003396, 7001968,
                       7001988, 7002011, 7002025, 7002058))
  
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
    mutate(treatment_starts = ifelse(grepl("CATI", wave_2) & !is.na(treatment_starts), row_number(), NA)) %>%
    ungroup() %>%
    arrange(ID_t, interview_date) %>%
    group_by(ID_t, wave_2) %>%
    mutate(treatment_ends = ifelse(grepl("CAWI", wave_2) & !is.na(treatment_ends), row_number(), NA))
  
  # show examples
  df_check_2 <- data_cohort_profile_prep_1 %>% 
    subset(ID_t %in% c(7036384, 7011375, 7003857, 7019155, 7001968, 
                       7001972, 7013481, 7013310, 7015624, 7036384,
                       7001969, 7016717, 7004031, 7003396, 7001968,
                       7001988, 7002011, 7002025, 7002058))
  
  
  # number of respondents
  num_id_adj_2 <- length(unique(data_cohort_profile_prep_1$ID_t))
  
  # look at individuals who get dropped
  id_dropped <- setdiff(
    unique(data_cohort_profile$ID_t), unique(data_cohort_profile_prep_1$ID_t)
  )
  id_dropped_num <- length(id_dropped)
    ## they only participate in CATI interviews or
    ## in first CAWI but this takes place before the CATI interview
  df_check_3 <- data_cohort_profile %>%
    filter(ID_t %in% id_dropped)
  df_check_3 %>% pull(wave_2) %>% unique()
    ## 74 also participate in CAWI but CAWI is before first CATI
  id_dropped_num_CAWI_bef_CATI <- df_check_3 %>% filter(wave_2 == "CAWI") %>% pull(ID_t) %>% unique() %>% length()
  id_dropped_num_CAWI_bef_CATI
  id_dropped_num_CATI <- id_dropped_num - id_dropped_num_CAWI_bef_CATI
  
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
  # always starts with CAWI and ends with CAWI, hence only this is needed
  df_check_length <- data_cohort_profile_prep_1 %>% ungroup() %>% filter(wave_2 == "CAWI") 
  
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
} else if (cohort_prep == "controls_treatment_outcome") {

  # extract start date of treatment period
  df_check_length_start <- data_cohort_profile_prep_1 %>% 
    filter(first_CATI_help == 1) %>% 
    dplyr::select(ID_t, interview_date) %>% 
    rename(interview_date_start = interview_date)
  
  # extract end date of treatment period
  df_check_length_end <- data_cohort_profile_prep_1 %>% 
    filter(first_CATI_help == 4) %>% 
    dplyr::select(ID_t, interview_date) %>% 
    rename(interview_date_end = interview_date)
  
  # join start and end date of treatment period and calculate length of treatment
  # period in years
  df_check_length <- inner_join(
    df_check_length_start, df_check_length_end, by = c("ID_t")
  ) %>%
    mutate(treatment_length = as.numeric(interview_date_end - interview_date_start) / 365) 
  
  summary(df_check_length$treatment_length)
  
  # identify rows to keep and drop
  id_drop <- df_check_length %>%
    filter(treatment_length > 4) %>%
    dplyr::select(ID_t) %>% pull() 
  
  # starts are dropped and ends are set NA
  data_cohort_profile_prep_1 <- data_cohort_profile_prep_1 %>%
    filter(!ID_t %in% id_drop)
  
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
max(data_cohort_profile_prep_1$interview_date) # 2018 (IMPORTANT)

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
  data_cohort_profile_save <- "Data/Grades/Prep_2/prep_2_cohort_profile.rds"
} else if (cohort_prep == "controls_bef_outcome") {
  data_cohort_profile_save <- "Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck.rds"
} else if (cohort_prep == "controls_treatment_outcome") {
  data_cohort_profile_save <- paste0("Data/Grades/Prep_2/prep_2_cohort_profile_robustcheck_", cohort_prep, ".rds")
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
func_save_sample_reduction(df_excel_save, "grade")

  