#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Interview Participation ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, a data frame is generated which contains information about
# the participation in each interview.
# This data set is used to determine the start and end of the treatment period
# for each respondent. 
#++++
# 1.) Handle missing values in interview date. For missing year, year from
# wave is inserted while for missing day and month previous values are used.
# Then interview date and competence measure test date are generated.
#++++
# 2.) Generate indices for treatment period start and end. 
#++++
# 3.) Subset Data Frame
# - Individuals for which no treatment periods are identified are dropped.
# - Rows are dropped which do not belong to a treatment period anymore (for
# instance, because no CAWI interview is conducted anymore)
# - If two CAWI or CATI interviews take place sequentially, the second one
# is kept because this information is more up-to-date. 
#++++
# --> Results in a panel data frame: one row for each respondent-wave
# combination with information about start end end of treatment period 
# (enumerated). 
#++++



#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages; if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("readstata13")) install.packages("readstata13")
library(readstata13)  # to import stata (.dta) file into R (see data manual why this function is used)

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to transform time data and work with dates

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to work with missing values


# set language for dates and times to German, since the NEPS month names
# are written in German; otherwise date/time functions are not working
# for German language
Sys.setlocale("LC_TIME", "German")


cohort <- "test"



#%%%%%%%%%%%%%%%%%#
#### Load Data ####
#%%%%%%%%%%%%%%%%#


# load data 
data_cohort_profile <- readRDS("Data/Prep_1/prep_1_cohort_profile.rds")

# number of respondents
length(unique(data_cohort_profile$ID_t)) # 17,909



#%%%%%%%%%%%%%%%%%%%%%%#
#### Interview Date ####
#%%%%%%%%%%%%%%%%%%%%%#


# check for missing values in interview day, month, and year
# (there are also many missings in competence month and year, but not relevant
# because competence measures are only surveyed infrequently)
  ## 1564 each
sum(is.na(data_cohort_profile$interview_day))
sum(is.na(data_cohort_profile$interview_month))
sum(is.na(data_cohort_profile$interview_year))
  ## missing for same respondent?: 
  ## yes all have all date info missing
data_cohort_profile %>% 
  select(ID_t, starts_with("interview_")) %>%
  group_by(ID_t) %>%
  # count number of NAs for each interview_* column
  summarise_all(~sum(is.na(.))) %>% 
  # take the row sum
  transmute(ID_t, sumNA = rowSums(.[-1])) %>%
  # check unique values: 0, 3, 6 
  select(sumNA) %>% pull() %>% unique()
  ## in which wave are the missing values?
  ## 2013 and 2017 CATI as well as 2011 CAWI
data_cohort_profile[is.na(data_cohort_profile$interview_day), "wave"] %>% unique()


# handle missing values
  ## for missing year insert year from wave
year_wave <- substr(data_cohort_profile[is.na(data_cohort_profile$interview_year), "wave"], 1, 4)
data_cohort_profile[is.na(data_cohort_profile$interview_year), "interview_year"] <- year_wave
  ## CATI missings: insert day and month from previous CATI interview
id_miss <- data_cohort_profile[is.na(data_cohort_profile$interview_day), "ID_t"] 
data_cohort_profile <- 
  rbind(
    data_cohort_profile %>% 
      arrange(ID_t, wave) %>%
      subset(ID_t %in% id_miss) %>%
      filter(grepl("CATI", wave)) %>%
      group_by(ID_t) %>%
      fill(c(interview_day, interview_month), .direction = "down"),
    data_cohort_profile %>% 
      arrange(ID_t, wave) %>%
      subset(ID_t %in% id_miss) %>%
      filter(!grepl("CATI", wave)) %>%
      group_by(ID_t) %>%
      fill(c(interview_day, interview_month), .direction = "down")
  ) %>%
  rbind(
    data_cohort_profile %>% 
      subset(!ID_t %in% id_miss)
  )
  ## for all other missings just take previous
data_cohort_profile <- data_cohort_profile %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  fill(c(interview_day, interview_month), .direction = "down")


# check if all missing values are gone
sum(is.na(data_cohort_profile %>% select(starts_with("interview"))))


# create date variable for interview and competence
data_cohort_profile <- data_cohort_profile %>%
  mutate(
    interview_date = mdy(paste(paste(interview_month, interview_day), interview_year, sep = ",")),
    competence_date = mdy(paste(paste(competence_month, 1), competence_year, sep = ","))
  )
unique(data_cohort_profile$interview_date)


# keep only variables needed for the upcoming analysis
data_cohort_profile <- data_cohort_profile %>%
  select(ID_t, wave, interview_date, competence_date, 
         competence_available_plausible_student) %>%
  ungroup()



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate Indices ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

if (cohort == "test") {
  
  # End of treatment period #
  #+++++++++++++++++++++++++#
  
  data_cohort_profile <- data_cohort_profile %>% 
    mutate(wave_2 = ifelse(grepl("CAWI", wave), "CAWI", "CATI")) %>%
    group_by(ID_t) %>%
    mutate(
      wave_2_lag = lag(wave_2),
      CAWI_imp = ifelse(wave_2 == "CAWI" & wave_2 != wave_2_lag, 1, 0)
      ) %>%
    mutate(treatment_ends = ifelse(CAWI_imp == 1, cumsum(CAWI_imp), NA)) %>%
    mutate(treatment_ends = treatment_ends - 1) %>%
    replace_na(list(treatment_ends = 0))
  
  
  # Start of treatment period #
  #+++++++++++++++++++++++++++#
  
  data_cohort_profile <- data_cohort_profile %>%
    mutate(treatment_ends = na_if(treatment_ends, 0)) %>%
    mutate(treatment_starts = treatment_ends) %>% 
    fill(treatment_starts, .direction = "up") %>%
    mutate(treatment_starts = ifelse(!is.na(treatment_ends), treatment_ends + 1, treatment_starts)) %>%
    ungroup()
  
  
  # Time-invariant wave: always kept #
  #++++++++++++++++++++++++++++++++++#
  
  # identify time-invariant control variables
  # data_cohort_profile <- 
  #   # create time invariant variable indicator for CATI and CAWI
  #   rbind(
  #     # select first CATI interview
  #     data_cohort_profile %>%
  #       arrange(ID_t, wave) %>%
  #       group_by(ID_t) %>%
  #       filter(grepl("CATI", wave)) %>% 
  #       filter(row_number() == 1) %>%
  #       mutate(controls_invariant = 1) %>%
  #       select(ID_t, wave, controls_invariant),
  #     # select first CAWI interview
  #     data_cohort_profile %>%
  #       arrange(ID_t, wave) %>%
  #       group_by(ID_t) %>%
  #       filter(grepl("CAWI", wave)) %>% 
  #       filter(row_number() == 1) %>%
  #       mutate(controls_invariant = 1) %>%
  #       select(ID_t, wave, controls_invariant)
  #   ) %>%
  #   # merge this to full data frame (right join to keep everything in full df)
  #   right_join(
  #     data_cohort_profile, 
  #     by = c("ID_t", "wave")
  #   ) %>%
  #   # sort data frame
  #   arrange(ID_t, wave) %>%
  #   ungroup()
  
  
  # Handle CATI duplicates #
  #+++++++++++++++++++++++#
  
  data_cohort_profile <- data_cohort_profile %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(case_when(
      wave_2 == "CATI" ~ row_number() == n()
    )) %>%
    rbind(data_cohort_profile %>% filter(wave_2 == "CAWI")) %>% 
    #rbind(data_cohort_profile %>% filter(wave_2 == "CAWI" | (wave_2 == "CATI" & controls_invariant == 1))) %>%
    arrange(ID_t, wave, treatment_starts, treatment_ends) %>%
    ungroup()
  

  # Handle CAWI duplicates #
  #+++++++++++++++++++++++#
  
  data_cohort_profile <- 
    data_cohort_profile %>%
    ungroup() %>% group_by(ID_t) %>%
    mutate(wave_2_lead = lead(wave_2)) %>%
    mutate(treatment_starts = ifelse(
      wave_2 == wave_2_lead, NA, treatment_starts
    ))
  
} else {
  # OLD VERSION
  
  # Next, indices are generated which indicate the treatment period for each
  # respondent and from which wave, control, outcome and treatment variables
  # are taken.
  
  # time-invariant control variables are taken from the first two interviews
  # first CATI and first CAWI
  data_cohort_profile <- 
    # create time invariant variable indicator for CATI and CAWI
    rbind(
      # select first CATI interview
      data_cohort_profile %>%
        arrange(ID_t, wave) %>%
        group_by(ID_t) %>%
        filter(grepl("CATI", wave)) %>% 
        filter(row_number() == 1) %>%
        mutate(controls_invariant = 1),
      # select first CAWI interview
      data_cohort_profile %>%
        arrange(ID_t, wave) %>%
        group_by(ID_t) %>%
        filter(grepl("CAWI", wave)) %>% 
        filter(row_number() == 1) %>%
        mutate(controls_invariant = 1)
    ) %>%
    # merge this to full data frame (right join to keep everything in full df)
    right_join(
      data_cohort_profile, 
      by = c("ID_t", "wave", "interview_date", "competence_date", "competence_available_plausible_student")
    ) %>%
    # sort data frame
    arrange(ID_t, wave)
  
  # for some individuals the control variables are taken from later waves
  data_cohort_profile[, c("wave", "controls_invariant")] %>% distinct() %>% na.omit()
  
  
  # count CATI and CAWI
  data_cohort_profile <- data_cohort_profile %>%
    # generate cati and cawi variable
    mutate(wave_2 = ifelse(grepl("CAWI", wave), "CAWI", "CATI")) %>%
    # sort
    arrange(ID_t, wave) %>%
    # group by ID_t and cati/cawi and count (-> running number)
    group_by(ID_t, wave_2) %>%
    mutate(count = row_number()) %>%
    # create a separate column for CATI and cAWI running number
    pivot_wider(values_from = count, names_from = wave_2, names_prefix = "count_") %>%
    # fill missing values up
    fill(c(count_CATI), .direction = "down") %>%
    # for CAWI replace missing values with zero (otherwise next computation makes troubles)
    replace_na(list(count_CAWI = 0))
  
  
  
  ## END OF TREATMENT PERIOD ##
  #+++++++++++++++++++++++++++#
  
  # treatment periods ends: for all CAWI
  ## first treatment period ends when count_CAWI = 2 -> second condition is
  ## that at least one CATI information is available at this time
  ## second treatment period ends when count_CAWI = 3 -> second condition is
  ## that student also participated in second CATI period.
  i_start <- 1
  i_stop <- max(data_cohort_profile$count_CATI) - 1
  data_cohort_profile$treatment_ends <- NA
  for (i in i_start:i_stop) {
    data_cohort_profile[data_cohort_profile$count_CAWI >= (i + 1) & # before: "==" not working
                          data_cohort_profile$count_CATI >= i,
                        "treatment_ends"] <- i
    i <- i + 1
  }
  ## if two treatment_ends with same number keep first one and set other NA
  ## show example
  #test <- data_cohort_profile %>% subset(ID_t %in% c(7001969, 7016717, 7004031, 7003396, 7001968))
  data_cohort_profile <- data_cohort_profile %>%
    group_by(ID_t, treatment_ends) %>%
    mutate(treatment_ends = ifelse(duplicated(treatment_ends), NA, treatment_ends))
  ## adjust enumerator for treatment_ends variable
  ## for some respondents where two CAWI surveys are conducted in a row
  ## (yields a gap of 1 in the treatment_ends variable)
  data_cohort_profile <-
    rbind(
      data_cohort_profile %>% filter(is.na(treatment_ends)),
      data_cohort_profile %>%
        group_by(ID_t) %>%
        filter(!is.na(treatment_ends)) %>%
        mutate(treatment_ends = row_number())
    ) %>%
    arrange(ID_t, wave, interview_date)
  
  
  
  ## START OF TREATMENT PERIOD ##
  #+++++++++++++++++++++++++++++#
  
  # treatment period starts: 
  ## first one starts in first row per group
  ## from the second one, they start when the outer finish
  data_cohort_profile <- data_cohort_profile %>%
    arrange(ID_t, wave) %>%
    group_by(ID_t) %>%
    # first treatment start
    mutate(treatment_starts = ifelse(row_number() == 1, 1, NA)) %>%
    # further treatment start
    mutate(treatment_starts = ifelse(!is.na(treatment_ends), 
                                     treatment_ends + 1, treatment_starts)) %>%
    # fill up missings to ensure that control variables can be taken from
    # respective wave according to treatment_starts variable
    fill(treatment_starts, .direction = "down")
  ## replace last treatment start with NA when no further treatment ends
  # extract maximum treatment end for each ID
  sub <-  data_cohort_profile %>% 
    group_by(ID_t) %>% 
    summarise(max_tr_ends = max(treatment_ends, na.rm = TRUE))
  # merge variable with maximum to data
  data_cohort_profile <- left_join(data_cohort_profile, sub, by = "ID_t")
  # replace treatment_starts with NA if treatment_starts is larger than maximum
  # number of treatment ends as this implies that no new treatment period starts
  # because no ending observations are given
  data_cohort_profile <- data_cohort_profile %>%
    mutate(treatment_starts = ifelse(treatment_starts > max_tr_ends, NA, treatment_starts))
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Subset Data Frame ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#


## Drop individuals without any treatment period ##
#+++++++++++++++++++++++++++++++++++++++++++++++++#

# not for all individuals it is possible to generate treatment periods
# this is for instance if the individual only participated in one 
# or even non CAWI survey
# In this case, the treatment_starts and treatment_ends variables only contain
# missing values 
data_cohort_profile %>% subset(ID_t == 7007385)

# identify individuals (5239 individuals)
id_drop <- 
  data_cohort_profile %>%
  group_by(ID_t) %>%
  filter(all(is.na(treatment_ends)) & all(is.na(treatment_starts))) %>%
  pull(ID_t) %>% unique()

# drop individuals
data_cohort_profile <- data_cohort_profile %>%
  subset(!ID_t %in% id_drop)



## Drop rows not belonging to a treatment period ##
#+++++++++++++++++++++++++++++++++++++++++++++++++#

# drop also rows for individuals who have treatment periods but missing values
# in both variables: treatment_starts and treatment_ends; those rows are not
# useful as they are not used.
data_cohort_profile <- data_cohort_profile %>%
  filter_at(vars(treatment_starts, treatment_ends), any_vars(!is.na(.)))

length(unique(data_cohort_profile$ID_t))

# following surveys
  ## create wave variable if not "test" run
if (cohort == "test") {
  data_cohort_profile <- data_cohort_profile
} else {
  data_cohort_profile <- data_cohort_profile %>%
    mutate(wave_2 = ifelse(grepl("CAWI", wave), "CAWI", "CATI"))
  ## if two CAWI surveys appear sequentially, that is for instance CAWI12 CAWI13 CATI13 CAWI14
  ## the second CAWI row is kept so that CAWI13 CATI13 CAWI14
  ## individual example: 7003396
  ## if two CATI survey appear sequentially, the same is done 
  ## individual example: 7001969
  data_cohort_profile <- data_cohort_profile %>%
    group_by(ID_t, treatment_starts, wave_2) %>% 
    filter(row_number() == n())
}


# # there are mistakes in generating treatment periods: FIX THIS 
# data_cohort_profile %>% subset(ID_t == 7002011) # example (see period 3)
# data_cohort_profile %>% subset(ID_t == 7002044) # example (see period 3)
# 
#   ## keep only rows with duplicates in ID_t and treatment_starts
# data_cohort_profile %>%
#   group_by(ID_t, treatment_starts) %>%
#   filter(n() == 1 & !is.na(treatment_starts))
# 
# data_cohort_profile[(!(duplicated(data_cohort_profile$treatment_starts) | 
#                          duplicated(data_cohort_profile$treatment_starts, fromLast = TRUE))) & 
#                       !is.na(data_cohort_profile$treatment_starts), "treatment_starts"] <- NA



# number of individuals left
print(
  paste(length(id_drop), "individuals are dropped because they only participate at", 
        "least once in the CAWI survey. This leaves a sample size of",
        length(unique(data_cohort_profile$ID_t)), "students.")
)


#+++++++++++++++++++#
#### Final Steps ####
#+++++++++++++++++++#

# keep only variables of interest
data_cohort_profile <- data_cohort_profile %>%
  ungroup() %>%
  select(ID_t, wave, interview_date, competence_date, competence_available_plausible_student,  
         treatment_ends, treatment_starts)

# check computations based on examples
check_1 <- data_cohort_profile %>%
  subset(ID_t %in% c(7001969, 7016717, 7004031, 7003396, 7001968)) %>%
  select(ID_t, wave, treatment_starts, treatment_ends)

check_2 <- data_cohort_profile %>% 
  subset(ID_t %in% c(7001988, 7002011, 7002025, 7002058)) %>%
  select(ID_t, wave, treatment_starts, treatment_ends)

# number of respondents, number of rows and columns
print(paste("Number of respondents:", length(unique(data_cohort_profile$ID_t))))
print(paste("Number of rows", nrow(data_cohort_profile)))
print(paste("Number of columns", ncol(data_cohort_profile)))
  
# save data
saveRDS(data_cohort_profile, "Data/Prep_2/prep_2_cohort_profile.rds")
  

