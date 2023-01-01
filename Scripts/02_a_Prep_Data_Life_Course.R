#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE EPISODE DATA: LIFE COURSE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, for each respondent, the full life course data is generated.
# Hence, in the final data frame of this file, one can gain knowledge of the
# exact periods spend in schooling, higher education, employment, unemployment etc.
#++++
# 1.) School and higher education episodes are merged with biography data.
# This is done because the biography data contains more reliable start and end
# dates as they are smoothed and corrected.
#++++
# 2.) All other data sets are merged. These include: internship, gap, higher
# education break and preparation, military, education (containing highest degree), 
# and employment.
#++++
# 3.) Only main spells are kept because this is what I am interested in.
# For instance, if individual states he/she is employed (main spell) and
# undertakes an internship (side spell), I am only interested in employment.
#++++
# 4.) Start and end date
# -> Generation of start and end date from month and year variables; as start
# day 1 is used and as end day 28 (otherwise problems with February).
# Moreover, end dates are adjusted for overlapping spells.
# -> Replacements and corrections for school and higher education period
#++++
# 5.) Missing Values
# -> For internship, military, employment, vocational preparation and breaks
# missing values are set to 0 for individuals who do not participate.
# -> Downward replacement for schooling and employment variables because later
# only university episode is kept but this information should also be
# easily accessible.
#++++
# 6.) Remove duplicates
#++++
# -> Final data frame contains detailed educational and employment history of
# each respondents. Thus, more rows than respondents because one rows is for
# one episode of each respondent.
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#

# clear workspace
rm(list = ls())

# # install packages; if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("readstata13")) install.packages("readstata13")
# library(readstata13)  # to import stata (.dta) file into R (see data manual why this function is used)
# 
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate)  # to transform time data and work with dates
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to work with missing values
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # for saving and loading excel
# 
# 
# # set language for dates and times to German, since the NEPS month names
# # are written in German; otherwise date/time functions are not working
# # for German language
# Sys.setlocale("LC_TIME", "German")



#%%%%%%%%%%%%%%%%%#
#### Load data ####
#%%%%%%%%%%%%%%%%%#

# load already prepared data sets from file 01
data_bio <- readRDS("Data/Prep_1/prep_1_biography.rds")
data_school <- readRDS("Data/Prep_1/prep_1_school.rds")
data_education <- readRDS("Data/Prep_1/prep_1_educ.rds")
data_highereduc_prep <- readRDS("Data/Prep_1/prep_1_vocprep.rds")
data_highereduc <- readRDS("Data/Prep_1/prep_1_voctrain.rds")
data_highereduc_break <- readRDS("Data/Prep_1/prep_1_vocbreak.rds")
data_military <- readRDS("Data/Prep_1/prep_1_military.rds")
data_internship <- readRDS("Data/Prep_1/prep_1_internship.rds")
data_gap <- readRDS("Data/Prep_1/prep_1_gap.rds")
data_emp <- readRDS("Data/Prep_1/prep_1_emp.rds")

# extract number of respondents (to ensure that no respondents get lost
# during data analysis)
id_num <- length(unique(data_bio$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge school and higher education with biography ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# THIS IS HELPFUL TO HAVE FULL HISTORY IN ONE DATA SET
# Start and end dates in biography are used since those are smoothed and
# corrected. As a result, the life course data is more consistent and only
# completed, harmonized, and right-censored episodes are contained. 


## MERGE BIOGRAPHY WITH SCHOOL ##
#+++++++++++++++++++++++++++++++#

# merge biography (x) and school (y): generate indicator from which data set info
# is coming
# -> master: biography, using: school
data_school_bio <- transform(merge(
  x = cbind(data_bio, source_school = "master"),
  y = cbind(data_school, source_school = "using"),
  all.x = TRUE, by = c("ID_t", "splink")),
  source_school = ifelse(!is.na(source_school.x) & !is.na(source_school.y), "both",
                  # in the merged dataset, source = "both" if the observations is in x AND in y
                  ifelse(!is.na(source_school.x), "master", "using")),
  # the columns "source" in x and y are deleted
  source_school.x = NULL,
  source_school.y = NULL
)
# delete duplicated wave and spms columns
data_school_bio$wave.y = NULL
data_school_bio$spms.y = NULL

# check: all school spells are finished
data_school_bio %>%
  subset(sptype == "School") %>%
  select(splast) %>%
  unique()


## ADD HIGHER EDUCATION ##
#++++++++++++++++++++++++#

# add voctrain to biography were already school data is appended
data_educ_all <- transform(merge(
  x = cbind(data_school_bio, source_voc = "master"),
  y = cbind(data_highereduc, source_voc = "using"),
  all.x = TRUE, by = c("ID_t", "splink")),
  source_voc = ifelse(!is.na(source_voc.x) & !is.na(source_voc.y), "both",
                  # in the merged dataset, source = "both" if the observations is in x AND in y
                  ifelse(!is.na(source_voc.x), "master", "using")),
  # the columns "source" in x and y are deleted
  source_voc.x = NULL,
  source_voc.y = NULL
)

# delete duplicated wave and spms columns
data_educ_all$wave.y = NULL
data_educ_all$spms.y = NULL

# rename wave variable
data_educ_all <- data_educ_all %>% rename(wave = wave.x) %>% select(-source_voc)


#### Merge other episode data ####
#++++++++++++++++++++++++++++++++#


## ADD EDUCATION ##
data_life_course <- left_join(
  data_educ_all, data_education , by = c("ID_t", "splink")
)

## ADD VOCBREAKS AND PREP ##
data_life_course <- left_join(
  data_life_course, data_highereduc_break, by = c("ID_t", "splink")
)

data_life_course <- left_join(
  data_life_course, data_highereduc_prep, by = c("ID_t", "splink")
)

## ADD INTERNSHIP ##
data_life_course <- left_join(
  data_life_course, data_internship, by = c("ID_t", "splink")
)

## ADD GAP ##
data_life_course <- left_join(
  data_life_course, data_gap, by = c("ID_t", "splink")
)

## ADD MILITARY ##
data_life_course <- left_join(
  data_life_course, data_military, by = c("ID_t", "splink")
)

## ADD EMPLOYMENT ##
data_life_course <- left_join(
  data_life_course, data_emp, by = c("ID_t", "splink")
)

# copy for missing value comparison below
data_life_course_raw <- data_life_course 


#%%%%%%%%%%%%%%%%%%%#
#### Main Spells ####
#%%%%%%%%%%%%%%%%%%%#

length(unique(data_life_course$ID_t))
nrow(data_life_course)

# keep only main spells
data_life_course <- data_life_course %>%
  filter(spms != "Side spell")

length(unique(data_life_course$ID_t))
nrow(data_life_course)
       


#%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Start and End Date ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Generate start and end date of spells ##
#+++++++++++++++++++++++++++++++++++++++++#

  # check for missings
data_life_course %>% select(startm, starty, endm, endy) %>% is.na() %>% colSums()
  # for start date, the first of the month is used
  # for end date the 28 of the month (for simplicity; with other value problems with february)
data_life_course <- data_life_course %>%
  mutate(
    start_date = mdy(paste(paste(startm, 1), starty, sep = ",")),
    end_date = mdy(paste(paste(endm, 28), endy, sep = ",")),
  ) %>%
  # drop variables not needed anymore: for episode date, wave should not be
  # considered because it relates to the interview date which might not 
  # even overlap with the reported start and end date of the episode
  # drop all other variables corresponding to dates
  select(
    -c(wave, splink, starts_with("source_"), 
       matches('_m$|_y$'), startm, starty, endm, endy)
  ) %>%
  # reorder columns
  select(ID_t, start_date, end_date, everything()) %>%
  # sort data frame appropriately
  arrange(ID_t, start_date, end_date)
  ## ensure that no missing values were generated
sum(is.na(data_life_course$start_date))
sum(is.na(data_life_course$end_date))



## Adjust start and end date ##
#+++++++++++++++++++++++++++++#

## 1.) Within spells ##
#$$

# start and end date is adjusted for "School" and "VocTrain" if spells are within spells
  ## store original start and end date variables
data_life_course <- data_life_course %>%
  mutate(
    start_date_orig = start_date, end_date_orig = end_date
  )
  ## show example for school spell
data_life_course %>%
  subset(ID_t == 7003000) %>%
  select(ID_t, start_date, end_date, sptype)

  ## create example for testing a variety of possibilities
# test_ex <- data_life_course %>% 
#   subset(ID_t == 7003000) %>%
#   select(ID_t, start_date, end_date, sptype) %>%
#   filter(sptype %in% c("School", "VocTrain", "Emp")) %>%
#   rbind(
#     data.frame(
#       ID_t = c(7003000, 7003000), start_date = c("2015-06-01", "2018-12-01"),
#       end_date = c("2017-05-28", "2021-07-28"), sptype = c("VocTrain", "Emp")
#     )
#   )
  ## adjust as long as adjustment are made
i <- 0
end_date_replace_1 <- 1
end_date_replace_2 <- 1
while (sum(end_date_replace_1) > 0 | sum(end_date_replace_2) > 0) {
  i <- i + 1
  print(paste("Iteration", i))
  
  data_life_course <-
    data_life_course %>%
    # sort and group data frame 
    arrange(ID_t, start_date) %>%
    group_by(ID_t, sptype) %>% 
    # generating leading start date to make the comparison
    mutate(
      start_date_lead = lead(start_date),
      end_date_lag = lag(end_date)
    ) %>%
    # adjust end date...
    ## indicator for replacement
    mutate(
      ## 1.) replace end_date for first period if start date of second period is within
      # the interval
      end_date_replace_1 = ifelse(
        start_date_lead < end_date & start_date < start_date_lead & !is.na(start_date_lead) &
          sptype %in% c("School", "VocTrain"), 1, 0
      ),
      ## lag (for next identification)
      end_date_replace_1_lag = lag(end_date_replace_1),
      ## 2.) replace end_date for second period if end_date of first period exceeds second and 
      # replacement in first period has been made
      end_date_replace_2 = case_when(
        end_date_replace_1_lag == 1 & end_date_lag > end_date & !is.na(end_date_lag) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    ## end_date adjustments for first episode
    mutate(
      end_date = case_when(end_date_replace_1 == 1 ~ start_date_lead, TRUE ~ end_date)
    ) %>%
    ## end_date adjustments for following episode
    mutate(
      end_date = case_when(end_date_replace_2 == 1 ~ end_date_lag, TRUE ~ end_date)
    )
  
  
  # adjust vectors for while-loop
  end_date_replace_1 <- data_life_course$end_date_replace_1
  end_date_replace_2 <- data_life_course$end_date_replace_2
  
}
  ## arrange and sort
data_life_course <- data_life_course %>%
  ungroup() %>%
  arrange(ID_t, start_date, end_date) %>%
  select(-c(start_date_lead, end_date_lag, starts_with("end_date_replace")))

  ## show result
data_life_course %>%
  subset(ID_t == 7003000) %>%
  select(ID_t, start_date, end_date, start_date_orig, end_date_orig, sptype) 
  ## ensure that start_date is always before end_date
sum(data_life_course$start_date >= data_life_course$end_date) # should be zero


## 2.) same start date ##
#$$

# check for two episodes with same start date and same episode type
# replacements are also only made for School and VocTrain
data_life_course %>%
  subset(ID_t == 7033988) %>%
  select(ID_t, start_date, end_date, start_date_orig, end_date_orig, sptype)

data_life_course %>%
  subset(ID_t == 7006709) %>%
  select(ID_t, start_date, end_date, start_date_orig, end_date_orig, sptype)

# make replacements as long as it is necessary
i <- 0
while (i < 10) {
  i <- i + 1
  print(paste("Iteration", i))
  data_life_course <- data_life_course %>%
    ungroup() %>%
    arrange(ID_t, start_date) %>%
    group_by(ID_t, sptype) %>%
    # create lags
    mutate(
      start_date_lag = lag(start_date),
      end_date_lag = lag(end_date)
    ) %>%
    # adjust start date
    mutate(
      start_date = case_when(
        start_date == start_date_lag & end_date_lag < end_date & 
          sptype %in% c("School", "VocTrain") ~ end_date_lag, 
        TRUE ~ start_date
      )
    ) %>%
    select(-c(start_date_lag, end_date_lag)) 
}

data_life_course <- data_life_course %>% arrange(ID_t, start_date)

  ## show result
data_life_course %>%
  subset(ID_t == 7033988) %>%
  select(ID_t, start_date, end_date, start_date_orig, end_date_orig, sptype)

data_life_course %>%
  subset(ID_t == 7006709) %>%
  select(ID_t, start_date, end_date, start_date_orig, end_date_orig, sptype)

  ## ensure that start_date is always before end_date
sum(data_life_course$start_date >= data_life_course$end_date) # should be zero



#%%%%%%%%%%%%%%%%%%%%%%%%#
#### University spell ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


## identify university periods ##
#+++++++++++++++++++++++++++++++#

# an individual studies if either "educ_uni_type_inst" includes "University"
# or "educ_uni_quali" includes any university degree
university_degree <- c("Bachelor", "Master", "state examination", "doctorate",
                       "Magister", "Diploma", "university", "Habilitation")
data_life_course <- data_life_course %>%
  mutate(
    educ_study = case_when(
      grepl("University", educ_uni_type_inst) | grepl(paste(university_degree, collapse = "|"), educ_uni_quali) ~ 1,
      is.na(educ_uni_type_inst) & is.na(educ_uni_quali) ~ as.double(NA),
      TRUE ~ 0
    )
  )


# replace education type "voctrain" with "Uni" if educ_study equals 1
# this allows to distinguish university and apprenticeship episodes
data_life_course <- data_life_course %>%
  mutate(sptype = as.character(sptype)) %>%
  mutate(sptype_2 = case_when(
    educ_study == 1 ~ "Uni", TRUE ~ sptype
  )) 



## create running indicator for university study period ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

data_life_course <- data_life_course %>% 
  ungroup() %>%
  arrange(ID_t, start_date) %>% 
  group_by(ID_t, educ_study) %>% 
  # enumerate university episode
  mutate(
    educ_study_num = row_number(),
    educ_study_num = ifelse(educ_study == 1, educ_study_num, NA)
  )



## create indicator for university start ##
#+++++++++++++++++++++++++++++++++++++++++#

# the really first study period may be before WT 2010
# hence two indicators are created:
  ## educ_uni_start: really first study episode
  ## educ_uni_start_WT10: first study episode of WT 2010
table(data_life_course$educ_uni_first_eps)

data_life_course <- data_life_course %>%
  mutate(
    educ_uni_start = ifelse(educ_study_num == 1, 1, 0), 
    educ_uni_start_WT10 = case_when(
      educ_uni_first_eps == "Episode is 1st study episode WT 2010 (start of study)" ~ 1,
      educ_study == 1 & (educ_uni_first_eps != "Episode is 1st study episode WT 2010 (start of study)" | is.na(educ_uni_first_eps)) ~ 0,
      TRUE ~ as.double(NA)
    ) 
  )

table(data_life_course$educ_uni_start)
table(data_life_course$educ_uni_start_WT10)

# look at individuals who have no study start in WT10
id_no_start_WT10 <- setdiff(
  data_life_course %>% filter(educ_uni_start == 1) %>% pull(ID_t) %>% unique(),
  data_life_course %>% filter(educ_uni_start_WT10 == 1) %>% pull(ID_t) %>% unique()
)
  ## some of them really have no study start, but some have
data_life_course %>% subset(ID_t == id_no_start_WT10[1]) %>% select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)
data_life_course %>% subset(ID_t == id_no_start_WT10[10]) %>% select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)
data_life_course %>% subset(ID_t == id_no_start_WT10[15]) %>% select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)
  ## set educ_uni_first_eps for those who start study in the academic year 2010 (WT 10/11 and SS 11)
data_life_course <- data_life_course %>%
  mutate(
    educ_uni_start_WT10_adj = ID_t %in% id_no_start_WT10,
    educ_uni_start_WT10 = ifelse(
      educ_uni_start_WT10_adj == TRUE & educ_study_num == 1 & start_date >= "2010-10-01" & start_date <= "2011-10-01", 1, educ_uni_start_WT10
    )
  ) %>% select(-educ_uni_start_WT10_adj)
  ## some persons are left who really did not start their study in WT 10/11
id_no_start_WT10 <- setdiff(
  data_life_course %>% filter(educ_uni_start == 1) %>% pull(ID_t) %>% unique(),
  data_life_course %>% filter(educ_uni_start_WT10 == 1) %>% pull(ID_t) %>% unique()
)
data_life_course %>% subset(ID_t == id_no_start_WT10[1]) %>% select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)
data_life_course %>% subset(ID_t == id_no_start_WT10[2]) %>% select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)


## Drop no first-year students in academic year 2010 ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++#

# drop individuals who do not have a study episode
id_keep <- data_life_course %>% filter(educ_study == 1) %>% pull(ID_t) %>% unique()
data_life_course <- data_life_course %>% subset(ID_t %in% id_keep)
id_num_adj_1 <- length(unique(data_life_course$ID_t))
id_num - id_num_adj_1 # 117

# drop individuals who do not start their study in academic year 2010
id_keep <- data_life_course %>% filter(educ_uni_start_WT10 == 1) %>% pull(ID_t) %>% unique()
data_life_course <- data_life_course %>% subset(ID_t %in% id_keep)
id_num_adj_2 <- length(unique(data_life_course$ID_t))
id_num_adj_1 - id_num_adj_2 # 11 (from id_no_start_WT10)

# check if all students start their study in academic year 2010
data_check <- 
  data_life_course %>%
  ungroup() %>%
  select(ID_t, start_date, educ_uni_start_WT10) %>%
  filter(educ_uni_start_WT10 == 1) %>%
  distinct() %>%
  mutate(
    month = month(start_date),
    year = year(start_date)
  )
## there are some individuals starting earlier or after the academic year
## 2010 -> those are dropped
table(data_check$year)
id_drop <- data_check %>%
  filter(!year %in% c(2010, 2011)) %>%
  select(ID_t) %>% pull() %>% unique()
length(id_drop) # 41 students are dropped
## drop those students
data_life_course <- data_life_course %>%
  subset(!(ID_t %in% id_drop))
## adjust sample size
id_num_adj_3 <- length(unique(data_life_course$ID_t))

# total drop
id_num_adj_2 - id_num_adj_3
id_num - id_num_adj_3 # 169

# check
  ## values should all be the same
length(unique(data_life_course$ID_t))
data_life_course %>% filter(educ_study == 1) %>% pull(ID_t) %>% unique() %>% length()
data_life_course %>% filter(educ_uni_start == 1) %>% pull(ID_t) %>% unique() %>% length()
data_life_course %>% filter(educ_uni_start_WT10 == 1) %>% pull(ID_t) %>% unique() %>% length()

# check if every individual has only one university start
  ## yes no results for general university start
data_life_course %>% ungroup() %>% select(ID_t, educ_uni_start) %>% filter(educ_uni_start == 1) %>%
  group_by(ID_t) %>% count() %>% filter(n > 1)
  ## but two results for first study episode in WT 2010
data_life_course %>% ungroup() %>% select(ID_t, educ_uni_start_WT10) %>% filter(educ_uni_start_WT10 == 1) %>%
  group_by(ID_t) %>% count() %>% filter(n > 1)
data_life_course %>% subset(ID_t == 7033988) %>% 
  select(ID_t, start_date, end_date, sptype, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10, educ_uni_first_eps)
  ## this is manually adjusted
data_life_course[data_life_course$ID_t == "7033988" & data_life_course$start_date == "2011-10-01", "educ_uni_start_WT10"] <- 0



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Handle Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## NA replacement for dummies ##
#++++++++++++++++++++++++++++++#

# Dummy variables for activity
# for military, intern and vocational training preparation & break, I already have dummy variables
table(data_life_course$military, useNA = "always")
table(data_life_course$intern, useNA = "always")
table(data_life_course$educ_voc_prep, useNA = "always")
table(data_life_course$educ_uni_break, useNA = "always")
table(data_life_course$emp, useNA = "always")

data_life_course <- data_life_course %>%
  mutate_at(c('military','intern', 'educ_voc_prep', 'educ_uni_break', 'emp'), ~ replace_na(.,0))

table(data_life_course$military, useNA = "always")
table(data_life_course$intern, useNA = "always")
table(data_life_course$educ_voc_prep, useNA = "always")
table(data_life_course$educ_uni_break, useNA = "always")
table(data_life_course$emp, useNA = "always")

# internship
  ## replace all 0 or "no internship" for individuals who do not undertake an internship, that is intern = 0
table(data_life_course$intern_type,  useNA = "always")
table(data_life_course$intern_study_rel,  useNA = "always")
data_life_course <- 
  data_life_course %>%
  mutate(intern_type = case_when(intern == 0 ~ "no internship", TRUE ~ intern_type),
         intern_study_rel = ifelse(intern == 0, 0, intern_study_rel))
table(data_life_course$intern_type,  useNA = "always")
table(data_life_course$intern_study_rel,  useNA = "always")


# higher education break
  ## all three variables are zero for respondents who did not undertake a break
  ## identified by educ_uni_break == 0
table(data_life_course$educ_uni_break_term_off,  useNA = "always")
table(data_life_course$educ_uni_break_deregist_temp,  useNA = "always")
table(data_life_course$educ_uni_break_deregist_nform,  useNA = "always")

data_life_course <- 
  data_life_course %>%
  mutate(
    educ_uni_break_term_off = ifelse(educ_uni_break == 0, 0, educ_uni_break_term_off),
    educ_uni_break_deregist_temp = ifelse(educ_uni_break == 0, 0, educ_uni_break_deregist_temp),
    educ_uni_break_deregist_nform = ifelse(educ_uni_break == 0, 0, educ_uni_break_deregist_nform)
  )

table(data_life_course$educ_uni_break_term_off,  useNA = "always")
table(data_life_course$educ_uni_break_deregist_temp,  useNA = "always")
table(data_life_course$educ_uni_break_deregist_nform,  useNA = "always")


# employment
  ## drop gap: is not so interesting in my analysis, because most just were on
  ## vacation
table(data_life_course$gap_type, useNA = "always")
data_life_course <- data_life_course %>% select(-starts_with("gap"))


## Downward replacement ##
#++++++++++++++++++++++++#

# missing values are replaced downward because later only university spells
# are kept, but other information like schooling should also be available
# easily
cols_fill <- data_life_course %>% ungroup() %>%
  select(starts_with("educ_school"), starts_with("educ_highest"), starts_with("emp"),
         starts_with("intern"), "military", "educ_voc_prep", 
         starts_with("educ_uni_break")) %>% colnames()

data_life_course <- data_life_course %>%
  # fill up rows downwards by ID
  arrange(ID_t, start_date, end_date) %>%
  group_by(ID_t) %>%
  fill(all_of(cols_fill), .direction = "down") 



#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Handle duplicates ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#


## COMPLETE DUPLICATES ##

# check for complete duplicates
sum(duplicated(data_life_course))

# remove them
data_life_course <- data_life_course %>% distinct()
sum(duplicated(data_life_course))



## PARTIAL DUPLICATES ##

# there are duplicates in ID_t, start_date, end_date, sptype_2
# in this case, some other variable differ
# However, since I am interested in spell lengths, I only keep the last duplicate
sum(duplicated(data_life_course[c("ID_t","start_date", "end_date", "sptype_2")]))

data_life_course <- data_life_course[!duplicated(
  data_life_course[c("ID_t","start_date", "end_date", "sptype_2")], 
  fromLast = TRUE), ]


sum(duplicated(data_life_course[c("ID_t","start_date", "end_date", "sptype_2")]))


# there are spells which have the same start date, but another end date
# this may be realistic for employment (two jobs at the same time)
# but unrealistic for education spells (two studies at two different universities
# is possible but unlikely)
sum(duplicated(data_life_course[c("ID_t","start_date", "sptype_2")]))
data_life_course[duplicated(data_life_course[c("ID_t","start_date", "sptype_2")]), "ID_t"]

  ## drop duplicates
data_life_course <- data_life_course[!duplicated(
  data_life_course[c("ID_t","start_date", "sptype_2")], fromLast = TRUE), ]
sum(duplicated(data_life_course[c("ID_t","start_date", "sptype_2")]))



#%%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# drop episodes I am not interested in: gap, Unemp, Data edition gap, ParLeave
unique(data_life_course$sptype)
data_life_course <- data_life_course %>% 
  filter(!sptype %in% c("Gap", "Unemp", "ParLeave", "Data edition gap"))
unique(data_life_course$sptype)

# check for missing values (replaced later)
  ## compare now and before
colSums(is.na(data_life_course_raw))
colSums(is.na(data_life_course))

# check for duplicates
sum(duplicated(data_life_course))

# check if all start dates are smaller than end dates
sum(data_life_course$start_date >= data_life_course$end_date)

# drop variables not needed anymore
data_life_course <- data_life_course %>% select(-c(spms, splast, educ_uni_first_eps))

# arrange
data_life_course <- data_life_course %>%
  select(ID_t, sptype, sptype_2, educ_study, educ_study_num, educ_uni_start, educ_uni_start_WT10,
         start_date, end_date, start_date_orig, end_date_orig, everything())

# ungroup
data_life_course <- data_life_course %>% ungroup()

# number of respondents, rows, and columns in each data preparation step
print(paste("Number of respondents before data preparation:", id_num))
print(paste("Number of respondents after dropping those without valid study episode:", id_num_adj_1))
print(paste("Number of respondents after dropping those who do not start study in academic year 2010/11:", id_num_adj_3))
print(paste("Number of respondents after data preparation:", 
            length(unique(data_life_course$ID_t))))
print(paste("Number of rows", nrow(data_life_course)))
print(paste("Number of columns", ncol(data_life_course)))

# save data frame for further preparation in other files
saveRDS(data_life_course, "Data/Prep_2/prep_2_life_course.rds")

# save excel
df_excel_save <- data.frame(
  "data_prep_step" = "episode",
  "data_prep_choice_cohort" = NA, 
  "num_id" = length(unique(data_life_course$ID_t)), 
  "num_rows" = nrow(data_life_course),
  "num_cols" = ncol(data_life_course),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save)









