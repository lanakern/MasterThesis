#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE EPISODE DATA: LIFE COURSE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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
# education break, military, education (containing highest degree), and
# employment.
#++++
# 3.) Generation of start and end date from month and year variables; as start
# day 1 is used and as end day 28 (otherwise problems with february)
#++++
# 4.) Identify uni spells: First study episode is identified, and following
# episodes are enumerated. sptype is changed from VocTrain to Uni for study episodes.
#++++
# 5.) Fill missing values downwards for variables for which this makes sense,
# for example, school degree.
#++++
# 6.) Handle duplicates: for duplicated spells, the last reported spell
# is kept. Moreover, for duplicates in start date, education spells are adjusted
# accordingly.
#++++
# --> Episode data set has more rows than respondents; one row for each episode
# of each respondent.


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


#### Load data ####
#+++++++++++++++++#

# load already prepared data sets from file 01
data_bio <- readRDS("Data/Prep_1/prep_1_biography.rds")
data_school <- readRDS("Data/Prep_1/prep_1_school.rds")
data_highereduc <- readRDS("Data/Prep_1/prep_1_voctrain.rds")
data_education <- readRDS("Data/Prep_1/prep_1_educ.rds")
data_highereduc_break <- readRDS("Data/Prep_1/prep_1_vocbreak.rds")
data_military <- readRDS("Data/Prep_1/prep_1_military.rds")
data_internship <- readRDS("Data/Prep_1/prep_1_internship.rds")
data_gap <- readRDS("Data/Prep_1/prep_1_gap.rds")
data_emp <- readRDS("Data/Prep_1/prep_1_emp.rds")

# extract number of respondents (to ensure that no respondents get lost
# during data analysis)
id_num <- length(unique(data_bio$ID_t))


#### Merge school and higher education with biography ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# THIS IS HELPFUL TO HAVE FULL HISTORY IN ONE DATA SET
# Start and end dates in biography are used since those are smoothed and
# corrected. As a result, the life course data is more consistent and only
# completed, harmonized, and right-censored episodes are contained. 

## MERGE BIOGRAPHY WITH SCHOOL ##

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
data_educ_all <- data_educ_all %>% rename(wave = wave.x)


#### Merge other episode data ####
#++++++++++++++++++++++++++++++++#


## ADD EDUCATION ##

# education contains nice generated variables for the highest degree
data_life_course <- left_join(
  data_educ_all, data_education , by = c("ID_t", "splink")
)


## ADD VOCBREAKS ##
data_life_course <- left_join(
  data_life_course, data_highereduc_break, by = c("ID_t", "splink")
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


#### Main Spells ####
#+++++++++++++++++++#

# keep only main speels
data_life_course <- data_life_course %>%
  filter(spms != "Side spell")


#### Generate Start and End Date ####
#+++++++++++++++++++++++++++++++++++#

# Generate start and end date of spells
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


# handle missing values in date:
  ## there is one respondent who has three missing values in start
  ## of employment spell
data_life_course %>% 
  subset(ID_t == unique(data_life_course[which(is.na(data_life_course$start_date)), "ID_t"])) %>% 
  select(ID_t, start_date, end_date, sptype) 
  ## for simplicity those rows are dropped
data_life_course <- data_life_course %>% filter(!is.na(start_date))


#### Adjust start and end date ####
#+++++++++++++++++++++++++++++++++#

# start and end date is adjusted for school spell: sometimes spells are
# within spells
  ## show example
data_life_course %>%
  subset(ID_t == 7003000) %>%
  select(ID_t, start_date, end_date, sptype)
  ## adjust
data_life_course <-
  data_life_course %>%
  # sort and group data frame 
  arrange(ID_t, sptype, start_date) %>%
  group_by(ID_t, sptype) %>% 
  # generating leading start date to make the comparison
  mutate(
    start_date_lead = lead(start_date)
  ) %>%
  # adjust end date if leading start date is smaller than current end date
  mutate(
    end_date = case_when(
      start_date_lead < end_date & sptype %in% c("School", "VocTrain", "VocPrep") & 
        start_date < start_date_lead & !is.na(start_date_lead) ~ start_date_lead, 
      TRUE ~ end_date
    )
  ) %>%
  # sort again
  arrange(ID_t, start_date, end_date) %>%
  select(-start_date_lead)

data_life_course %>%
  subset(ID_t == 7003000) %>%
  select(ID_t, start_date, end_date, sptype)


#### University spell ####
#++++++++++++++++++++++++#


## create indicator for university start ##

# university starts
data_life_course <- data_life_course %>%
  mutate(
    educ_uni_start = ifelse(
      educ_uni_first_eps == "Episode is 1st study episode WT 2010 (start of study)", 
      1, 0
    ) 
  ) %>%
  replace_na(list(educ_uni_start = 0))

# check if every individual has only one university start
data_life_course %>% select(ID_t, educ_uni_start) %>% 
  distinct() %>% group_by(ID_t) %>% 
  count(educ_uni_start) %>% pull(n) %>% unique()

# # for one person there are two; seems like a mistake
# # replace second university start with zero
# data_life_course <- data_life_course %>%
#   arrange(ID_t, start_date, end_date)
# 
# data_life_course[!is.na(data_life_course$educ_uni_start), c("ID_t", "educ_uni_start")][
#   duplicated(data_life_course[!is.na(data_life_course$educ_uni_start), c("ID_t", "educ_uni_start")]),
#   "educ_uni_start"
# ] <- 0

# # check
# ## only ones, i.e., only one study start for each respondent
# data_life_course %>% select(ID_t, educ_uni_start) %>% distinct() %>% group_by(ID_t) %>% 
#   count(educ_uni_start) %>% arrange(-n) %>% select(n) %>% pull() %>% unique()


# there are some (exactly 8) individuals who do not have provided the information 
# that they start studying
id_no_start_uni <- 
  setdiff(
    unique(data_life_course$ID_t),
    data_life_course %>% filter(educ_uni_start == 1) %>% select(ID_t) %>% pull() %>% unique()
  )
length(id_no_start_uni)

# for those the episode where the first time the educ_uni_type_inst variable
# contains "Universität" or "Fachhochschule" is considered as the first
# study episode 
data_life_course <- rbind(
  # replace educ_uni_start variable only for specific IDs
  data_life_course %>%
    subset(ID_t %in% id_no_start_uni) %>%
    group_by(ID_t) %>%
    mutate(
      educ_uni_start = grepl("University", educ_uni_type_inst) & 
        !duplicated(grepl("University", educ_uni_type_inst))
      ),
  # rbind data frame with IDs where everything is correct
  data_life_course %>%
    subset(!(ID_t %in% id_no_start_uni))
)


# check if all students start their study in winter term 2010/2011
data_check <- 
  data_life_course %>%
  select(ID_t, start_date, educ_uni_start) %>%
  filter(educ_uni_start == 1) %>%
  distinct() %>%
  mutate(
    month = month(start_date),
    year = year(start_date)
  )
  ## there are some individuals starting earlier or in 2011
  ## students who start before 2010 are dropped
table(data_check$year)
id_drop <- data_check %>%
  filter(year < 2010) %>%
  select(ID_t) %>% pull() %>% unique()
length(id_drop) # 18 students are dropped
  ## drop those students
data_life_course <- data_life_course %>%
  subset(!(ID_t %in% id_drop))
  ## adjust sample size
id_num_adj_1 <- length(unique(data_life_course$ID_t))


## create running indicator for university spells ##

# university spells are identified by educ_uni_type_inst not NA
# data frame with university spells is combined with data frame without
data_life_course <- 
  rbind(
    data_life_course %>%
      # only keep observations where educ_uni_type_inst is not NA
      filter(!is.na(educ_uni_type_inst)) %>%
      # sort data frame by ID and date to ensure that number is correct
      arrange(ID_t, start_date, end_date) %>%
      group_by(ID_t) %>%
      mutate(educ_uni_eps_num = row_number()),
    data_life_course %>%
      filter(is.na(educ_uni_type_inst)) %>%
      mutate(educ_uni_eps_num = NA)
  ) %>%
  arrange(ID_t, start_date, end_date)



## identify uni spell ##

# replace education type "voctrain" with "Uni" if educ_uni_eps_num is not NA
# this allows to distinguish the years spent in higher education incl. vocational
# training and only spend in a higher education institution
  ## generate small function
recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}
  ## apply function to do recoding
data_life_course <- data_life_course %>%
  # sptype as character
  mutate(sptype = as.character(sptype)) %>%
  mutate(sptype_2 = recode_if(sptype, !is.na(educ_uni_eps_num), "VocTrain" = "Uni")) 

# create uni spell column
data_life_course <-
  data_life_course %>%
  group_by(ID_t) %>%
  #mutate(row = row_number()) %>% # to avoid error message
  mutate(uni_spell = ifelse(sptype_2 == "Uni", 1, 0))



#### Fill up missing rows for education information variables  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


# highest school degree, school leaving grades, country of school where
# certificate is obtained
# to identify this, last information given on this variable is copied 
# downwards
# it is very likely that this information is time-invariant. However, it may
# be the case that a student obtains a "Fachhochschulreife", studies at a
# university of applied science, goes back to school, and obtains a general
# university entrance qualification ("Abitur").
# Thus, information will be later extracted episode specific
cols_fill <- data_life_course %>% ungroup() %>% 
  select(starts_with("educ_school"), starts_with("educ_highest"), starts_with("emp"),
         starts_with("intern"), "gap_type", "military") %>% colnames()

data_life_course <- data_life_course %>%
  # fill up rows downwards by ID
  arrange(ID_t, start_date, end_date) %>%
  group_by(ID_t) %>%
  fill(all_of(cols_fill), .direction = "down") 



#### Handle duplicates ####
#+++++++++++++++++++++++++#


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
  ## example
data_life_course %>% subset(ID_t == 7004431) %>% select(ID_t, start_date, end_date, sptype_2)
  ## drop duplicates
data_life_course <- data_life_course[!duplicated(
  data_life_course[c("ID_t","start_date", "sptype_2")], fromLast = TRUE), ]
sum(duplicated(data_life_course[c("ID_t","start_date", "sptype_2")]))


#+++++++++++++++++++#
#### Final Steps ####
#+++++++++++++++++++#

# remove grouping
data_life_course <- data_life_course %>% ungroup()

# check for missing values
colSums(is.na(data_life_course))

# number of respondents, rows, and columns
print(paste("Number of respondents after data preparation:", 
            length(unique(data_life_course$ID_t))))
print(paste("Number of rows", nrow(data_life_course)))
print(paste("Number of columns", ncol(data_life_course)))

# save data frame for further preparation in other files
saveRDS(data_life_course, "Data/Prep_2/prep_2_life_course.rds")





