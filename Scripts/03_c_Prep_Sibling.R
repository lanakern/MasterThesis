#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### PREPARE SIBLING DATA ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# 1.) Merge birth month and year from CATI to sibling data. This is necessary
# to identify younger and older siblings.
#++++
# 2.) Create sibling variables: number of siblings, number of older siblings, 
# indicator for having a twin
#++++
# 3.) Identify two oldest or closest siblings for non missing values 
# -> Create variables for school degree etc. for them
#++++
# 4.) Missing Values
# -> Replaced later in 07
# -> For respondents with only one sibling sibling_2 dummies are set to 0. 
#++++
# --> Cross-sectional data set: one row for each respondent



#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
# rm(list = setdiff(ls(), c("cohort_prep", "treatment_repl", "treatment_def", "df_inputs", "prep_sel_num")))

# # install packages; if needed, load packages
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)  # to manipulate data
# 
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate)  # to transform time data and work with dates
# 
# if (!require("tidyr")) install.packages("tidyr")
# library(tidyr)  # to work with missing values
# 
# # set language for dates and times to German, since the NEPS month names
# # are written in German; otherwise date/time functions are not working
# # for German language
# Sys.setlocale("LC_TIME", "German")




#%%%%%%%%%%%%%%%%#
#### Load data ###
#%%%%%%%%%%%%%%%%#

# prepared sibling data from 01_Load_Data
data_sibling <- readRDS("Data/Prep_1/prep_1_sibling.rds")

# prepared cati data from 01_Load_Data (only needed for birth date)
data_target_cati <- readRDS("Data/Prep_1/prep_1_target_cati.rds")

# number of respondents
id_sib <- unique(data_sibling$ID_t)
id_num_sib <- length(id_sib) # 15,602 respondents have at least one sibling
id_num_sib
length(unique(data_target_cati$ID_t)) # 17,909 respondents are in CATI data

# ensure that sibling data is only evaluated in wave 1 (-> time-constant)
unique(data_sibling$wave)

# sibling data contains more rows than respondents; this is because some
# respondents have more than one sibling
id_num_sib == nrow(data_sibling)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Merge with CATI for Birth Date ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# From CATI I need to add the birth date of the respondent 
# This is necessary to calculate if the sibling is older or younger

# extract birth month and year
# for some respondents there are duplicates. In this case, we keep newest
# information on birth date
data_target_cati <- data_target_cati %>%
  arrange(ID_t, wave) %>%
  select(ID_t, birth_month, birth_year) %>%
  group_by(ID_t) %>%
  # to ensure that missing value is not kept
  fill(c(birth_month, birth_year), .direction = "downup") %>% ungroup() %>%
  distinct() %>%
  filter(duplicated(ID_t) == FALSE)

length(unique(data_target_cati$ID_t)) == nrow(data_target_cati)
sum(is.na(data_target_cati$birth_month))
sum(is.na(data_target_cati$birth_year))

# merge siblings and target_cati via ID_t
# left join to ensure that all rows in data_sibling are kept: the
# respondents birth month and year is appended for each row
data_sibling_adj <- inner_join(
  data_sibling, data_target_cati, by = "ID_t"
)

length(unique(data_sibling_adj$ID_t)) # 15,602 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Calculate sibling variables: number total, number older, twin ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# calculate birth date of sibling and respondent (needed to identify
# older siblings)
  ## load function
source("Functions/func_generate_date.R")
  ## birth date of sibling
sum(is.na(data_sibling_adj$sibling_birth_m))
sum(is.na(data_sibling_adj$sibling_birth_y))
data_sibling_adj_2 <- func_generate_date(
  data = data_sibling_adj,
  month = "sibling_birth_m", year = "sibling_birth_y",
  varname = "sibling_birth_date"
)
sum(is.na(data_sibling_adj_2$sibling_birth_y))
  ## birth date of respondent
data_sibling_adj_2 <- func_generate_date(
  data = data_sibling_adj_2,
  month = "birth_month", year = "birth_year",
  varname = "birth_date"
)

sum(is.na(data_sibling_adj_2$birth_date)) # no missing values


# create indicator for older sibling +
#++++++++++++++++++++++++++++++++++++#

# indicator is missing if birth date of sibling is unknown
data_sibling_adj_2$sibling_older <- rep(NA, times = length(data_sibling_adj_2$ID_t))

for (i in 1:length(data_sibling_adj_2$sibling_older)) {
  if (!is.na(data_sibling_adj_2$sibling_birth_date[i]) & !is.na(data_sibling_adj_2$birth_date[i]) &
      data_sibling_adj_2$sibling_birth_date[i] > data_sibling_adj_2$birth_date[i]) {
    data_sibling_adj_2$sibling_older[i] = 0
  } else {
    if (!is.na(data_sibling_adj_2$sibling_birth_date[i]) & !is.na(data_sibling_adj_2$birth_date[i]) &
        data_sibling_adj_2$sibling_birth_date[i] < data_sibling_adj_2$birth_date[i]) {
      data_sibling_adj_2$sibling_older[i] = 1
    } else {
      data_sibling_adj_2$sibling_older[i] = NA
    }
  }
}

# missing values
  ## do not coincide for NA in sibling_older and NA in sibling_birth_date
sum(is.na((data_sibling_adj_2$sibling_older)))
sum(is.na((data_sibling_adj_2$sibling_birth_date)))
  ## seems to be many twins: birth date of respondent and birth date of
  ## sibling is not the same
data_sibling_adj_2 %>%
  subset(is.na(sibling_older) & !is.na(sibling_birth_date)) %>%
  select(ID_t, wave, sibling_birth_date, birth_date, sibling_older)



# Identify twins #
#++++++++++++++++#

# generate indicator for twin and replace sibling_older for twin to 0
data_sibling_adj_2 <- data_sibling_adj_2 %>%
  mutate(
    sibling_older = ifelse(sibling_birth_date == birth_date, 0, sibling_older),
    sibling_twin = ifelse(sibling_birth_date == birth_date, 1, 0)
  )

# now missing values coincide
sum(is.na((data_sibling_adj_2$sibling_older)))
sum(is.na((data_sibling_adj_2$sibling_birth_date)))

table(data_sibling_adj_2$sibling_twin)
sum(is.na((data_sibling_adj_2$sibling_twin)))

  
# Number of siblings #
#++++++++++++++++++++#

# Important: Total number of siblings is easy as the number of siblings
# is directly given. However, to determine the number of older and younger
# siblings is difficult because sometimes the birth date is not given.
# Since it is likely that individuals model themselves on their older
# siblings, only the number of older siblings is calculated.

# total number of siblings: use previous data frame because rows where
# birth date of sibling is missing is dropped
data_sibling_adj_3 <- left_join(
  data_sibling_adj_2,
  data_sibling %>% group_by(ID_t) %>% summarise(sibling_total = max(sibling_num)),
  by = "ID_t"
)

# total number of older siblings
data_sibling_adj_3 <- data_sibling_adj_3 %>%
  mutate(
    sibling_older_total = ifelse(
      is.na(sibling_older), NA,
      ave(sibling_older, ID_t, FUN = function(x) sum(x, na.rm = TRUE))
    )
  )
  ## if respondent has not reported one sibling birth date but all other
  ## sibling_older_total variable is NA once but not for other rows
  ## hence, fill up missings
data_sibling_adj_3 <- data_sibling_adj_3 %>% group_by(ID_t) %>% fill(sibling_older_total, .direction = "downup")

# total number of twins (only dummy)
data_sibling_adj_3 <- left_join(
  data_sibling_adj_3,
  data_sibling_adj_3 %>% group_by(ID_t) %>% summarise(sibling_twin_sum = sum(sibling_twin, na.rm = TRUE)),
  by = "ID_t"
) %>%
  mutate(sibling_twin = ifelse(sibling_twin_sum >= 1, 1, sibling_twin_sum)) %>%
  mutate(sibling_twin = ifelse(is.na(sibling_birth_date), NA, sibling_twin)) %>%
  select(-sibling_twin_sum) %>% distinct()
  ## same with missings for twin variaböe
data_sibling_adj_3 <- data_sibling_adj_3 %>% 
  group_by(ID_t) %>% 
  fill(sibling_twin, .direction = "downup") %>%
  distinct()


sum(is.na(data_sibling_adj_3$sibling_twin))
summary(data_sibling_adj_3$sibling_twin)
data_sibling_adj_3 %>% subset(ID_t == 7002020) %>% select(ID_t, wave, sibling_num, sibling_twin, sibling_older, sibling_total)
 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Subset on two siblings ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# I will only keep information on two siblings
  ## sort by ID, sibling_num and sibling_school_degree
  ## and count number of non-missings for school degree
data_sibling_adj_4 <- data_sibling_adj_3 %>%
  left_join(
    data_sibling_adj_3 %>% group_by(ID_t) %>% 
      count(n_true_fale = !is.na(sibling_school_degree)) %>% 
      subset(n_true_fale == TRUE) %>%
      rename(sibling_school_degree_non_na_num = n) %>% 
      select(ID_t, sibling_school_degree_non_na_num) %>%
      distinct(),
    by = "ID_t"
  )


  ## if a respondent has more than two siblings, drop the rows where
  ## no informatuon on the school degree of the sibling is given
  ## however, this can only be done if at least two non-missing values
  ## in school degree are provides; otherwise all rows would be dropped
  ## 1.) identify ID_t 
id_drop <- data_sibling_adj_4 %>%
  subset(sibling_total > 2 & sibling_school_degree_non_na_num >= 2) %>%
  select(ID_t) %>% unique() %>% pull()
  ## 2.) Combine respondents where no rows are deleted with respondents where rows
  ## are deleted
data_sibling_adj_4 <- 
  rbind(
    data_sibling_adj_4 %>%
      subset(!ID_t %in% id_drop),
    data_sibling_adj_4 %>%
      subset(ID_t %in% id_drop) %>%
      drop_na(sibling_school_degree)
  )
  

  ## do the same for activity
data_sibling_adj_4 <- data_sibling_adj_4 %>%
  left_join(
    data_sibling_adj_4 %>% group_by(ID_t) %>% 
      count(n_true_fale = !is.na(sibling_activity)) %>% 
      subset(n_true_fale == TRUE) %>%
      rename(sibling_activity_non_na_num = n) %>% 
      select(ID_t, sibling_activity_non_na_num) %>%
      distinct(),
    by = "ID_t"
  )


id_drop <- data_sibling_adj_4 %>%
  subset(sibling_total > 2 & sibling_activity_non_na_num >= 2) %>%
  select(ID_t) %>% unique() %>% pull()
  ## 3.) Combine respondents where no rows are deleted with respondents where rows
  ## are deleted
data_sibling_adj_4 <- 
  rbind(
    data_sibling_adj_4 %>%
      subset(!ID_t %in% id_drop),
    data_sibling_adj_4 %>%
      subset(ID_t %in% id_drop) %>%
      drop_na(sibling_activity)
  )


  ## if still more than two siblings are present in the data, the two oldest
  ## siblings are kept; this is identified by the age difference in month
data_sibling_adj_4 <- data_sibling_adj_4 %>%
  mutate(
    age_diff_month = (year(birth_date) - year(sibling_birth_date)) * 12 + month(birth_date) - month(sibling_birth_date)
  ) %>%
  # replace NA in age_diff_month with 0 (results from not given birth date)
  mutate(age_diff_month = ifelse(is.na(age_diff_month), 0, age_diff_month))

# create counter for number of siblings and number of older
# siblings still kept in this data set
data_sibling_adj_4 <- data_sibling_adj_4 %>%
  left_join(
    data_sibling_adj_4 %>% count(ID_t) %>% rename(n_sib = n),
    by = "ID_t"
  ) %>%
  left_join(
    data_sibling_adj_4 %>%
      filter(age_diff_month > 0) %>%
      group_by(ID_t) %>% 
      count(ID_t) %>%
      rename(n_sib_old = n),
    by = "ID_t"
  )


length(unique(data_sibling_adj_4$ID_t))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generate other variables ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Generate school degree and activity variables for two siblings #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# students with only at the most two siblings do not need another preparation
id_less_two <- data_sibling_adj_4 %>%
  subset(n_sib <= 2) %>% select(ID_t) %>% unique() %>% pull()

# students with more than two siblings need special preparation
# more than two older
id_more_two_older <- data_sibling_adj_4 %>%
  subset(n_sib > 2 & n_sib_old >= 2) %>% select(ID_t) %>% unique() %>% pull()
  ## -> drop rows with negative age difference (younger) and keep the two rows
  ## with most closest oldest siblings
data_sub_1 <- data_sibling_adj_4 %>%
  subset(ID_t %in% id_more_two_older) %>%
  subset(age_diff_month > 0) %>%
  arrange(ID_t, age_diff_month) %>%
  group_by(ID_t) %>%
  slice_head(n = 2)

# one older: keep oldest and closest youngest
id_one_older <- data_sibling_adj_4 %>%
  subset(n_sib > 2 & n_sib_old == 1) %>% select(ID_t) %>% unique() %>% pull()
data_sub_2 <- rbind(
  data_sibling_adj_4 %>%
    subset(ID_t %in% id_one_older) %>%
    subset(age_diff_month > 0),
  data_sibling_adj_4 %>%
    subset(ID_t %in% id_one_older) %>%
    subset(age_diff_month <= 0) %>%
    arrange(ID_t, age_diff_month) %>%
    group_by(ID_t) %>%
    slice_head(n = 1)
)

# only younger: keep two closest
# NOTE: if birth dates are not given, sibling is considered as younger and
# random selection is made
id_younger <- data_sibling_adj_4 %>%
  subset(n_sib > 2 & is.na(n_sib_old)) %>% select(ID_t) %>% unique() %>% pull()
data_sub_3 <- 
  data_sibling_adj_4 %>%
  subset(ID_t %in% id_younger) %>%
  subset(age_diff_month <= 0) %>%
  arrange(ID_t, age_diff_month) %>%
  group_by(ID_t) %>%
  slice_head(n = 2)

# combine
data_sibling_final <- 
  data_sibling_adj_4 %>% subset(ID_t %in% id_less_two) %>%
  rbind(data_sub_1) %>%
  rbind(data_sub_2) %>%
  rbind(data_sub_3)

# generate new counter for siblings
data_sibling_final <- 
  data_sibling_final %>%
  group_by(ID_t) %>%
  mutate(sibling_num = row_number())
  
length(unique(data_sibling_final$ID_t)) # 15,602

# generate variable containing the highest school degree, employment status
# and if unemployed the activity in WT 10/11
  ## for employment first aggregate the categories:
  ## employed: Vollzeit erwerbstätig, Teilzeit erwerbstätig, nebenher erwerbstätig
  ## var equals 1 if employed, otherwise 0 (including missings)
data_sibling_final <- data_sibling_final %>%
  mutate(sibling_employed = recode(
    sibling_employed,
    "full-time employed" = 1,
    "part-time employed" = 1,
    "in a side job" = 1,
    .default = 0
  )) #%>%
  #replace_na(list(sibling_employed = 0))
  ## also for school degree only consider if sibling has a university entrance
  ## qualification:
  ## -> "General/subject-related university entrance quali\032cation (Abitur(/12th grade EOS)"
  ## -> "Fachhochschulreife/ leaving certificate of a Fachoberschule"
data_sibling_final <- data_sibling_final %>%
  mutate(sibling_uni_entrance_quali = ifelse(
    grepl("Fachhochschulreife", sibling_school_degree) | grepl("university", sibling_school_degree), 1, 
    ifelse(is.na(sibling_school_degree), NA, 0)
  )) #%>%
  #replace_na(list(sibling_uni_entrance_quali = 0))
  ## indicator if the sibling studies in WT 10/11: "Studium", "Promotion"
data_sibling_final <- data_sibling_final %>%
  mutate(sibling_study = recode(
    sibling_activity,
    "course of study" = 1,
    "doctorate" = 1,
    .default = 0
  )) %>%
  # all employe people do not study
  mutate(sibling_study = ifelse(sibling_employed == 1 & is.na(sibling_study), 0, sibling_study)) #%>%
  #replace_na(list(sibling_study = 0))
  ## spread() data frame
data_sibling_adj_sub_1 <- data_sibling_final %>%
  select(ID_t, sibling_num, sibling_uni_entrance_quali, sibling_employed, sibling_study, sibling_birth_date) %>%
  gather(variable, value, sibling_uni_entrance_quali, sibling_employed, sibling_study, sibling_birth_date) %>%
  unite(temp, sibling_num, variable) %>%
  spread(temp, value) %>%
  rename(
    sibling_uni_entrance_quali_1 = "1_sibling_uni_entrance_quali",
    sibling_uni_entrance_quali_2 = "2_sibling_uni_entrance_quali",
    sibling_employed_1 = "1_sibling_employed",
    sibling_employed_2 = "2_sibling_employed",
    sibling_study_1 = "1_sibling_study",
    sibling_study_2 = "2_sibling_study",
    sibling_birth_date_1 = "1_sibling_birth_date",
    sibling_birth_date_2 = "2_sibling_birth_date"
  ) %>%
  mutate(sibling_birth_date_1 = as.Date(sibling_birth_date_1), 
         sibling_birth_date_2 = as.Date(sibling_birth_date_2))

# add this to other variables
data_sibling_adj_sub_2 <- data_sibling_final %>%
  select(ID_t, sibling_total, sibling_older_total, sibling_twin) %>% distinct()


# create final data frame: cross-sectional data set, i.e., one row for one respondent
data_sibling_final_2 <- left_join(
  data_sibling_adj_sub_1, data_sibling_adj_sub_2, by = "ID_t"
)



#%%%%%%%%%%%%%%%%%%%%%%#
#### Missing Values ####
#%%%%%%%%%%%%%%%%%%%%%%#


# check for NAs in sibling variable
sapply(data_sibling_final_2, function(y) sum(length(which(is.na(y)))))

# for all individuals who do not have a second sibling, set the sibling_*_2 variables
# to 0
data_sibling_final_2 %>% filter(sibling_total == 1) %>% pull(ID_t) %>% unique() %>% head(1)
data_sibling_final_2 %>% filter(sibling_total == 2) %>% pull(ID_t) %>% unique() %>% head(1)
data_sibling_final_2 %>% subset(ID_t == 7001969)
data_sibling_final_2 %>% subset(ID_t == 7001970)
data_sibling_final_2 <- data_sibling_final_2 %>%
  mutate(across(c("sibling_employed_2", "sibling_study_2", "sibling_uni_entrance_quali_2"), 
                ~ case_when(sibling_total == 1 ~ 0, TRUE ~ .)))
data_sibling_final_2 %>% subset(ID_t == 7001969)
data_sibling_final_2 %>% subset(ID_t == 7001970)

# # for all other NAs, create NA dummys and then set NAs in those variables to zero
# cols_NA <- names(colSums(is.na(data_sibling_final_2))[colSums(is.na(data_sibling_final_2)) > 0])
# cols_NA <- cols_NA[!str_detect(cols_NA, "birth_date")]
#   ## create NA dummies
# for (cols_NA_sel in cols_NA) {
#   cols_NA_mut <- paste0(cols_NA_sel, "_NA")
#   data_sibling_final_2 <- data_sibling_final_2 %>%
#     mutate(
#       {{cols_NA_mut}} := ifelse(is.na(!!!syms(cols_NA_sel)), 1, 0),
#       {{cols_NA_sel}} := ifelse(is.na(!!!syms(cols_NA_sel)), 0, !!!syms(cols_NA_sel))
#     )
# }

# check for NAs
colSums(is.na(data_sibling_final_2)) # only NAs for birth columns


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# generate sibling dummy
data_sibling_final_2 <- data_sibling_final_2 %>% mutate(sibling = 1)

# check if all individuals are kept
length(unique(data_sibling_final_2$ID_t)) == id_num_sib
setdiff(unique(data_sibling_final_2$ID_t), id_sib)
setdiff(id_sib, unique(data_sibling_final_2$ID_t))

# reorder columns
data_sibling_final_2 <- data_sibling_final_2 %>%
  select(ID_t, sibling, sibling_total, starts_with("sibling_older_total"), 
         starts_with("sibling_twin"), matches("sibling_.*_1"), matches("sibling_.*_2"))

# check respondents
# df_check_1 <- data_target_cati %>% subset(ID_t %in% c(7001975, 7001971, 7002373, 7002298, 7001994, 7002007))
# df_check_2 <- data_sibling %>% subset(ID_t %in% c(7001975, 7001971, 7002373, 7002298, 7001994, 7002007))
# df_check_3 <- data_sibling_final_2 %>% subset(ID_t %in% c(7001975, 7001971, 7002373, 7002298, 7001994, 7002007)) 
# 
# df_check_1 %>% subset(ID_t == 7002007)
# df_check_2 %>% subset(ID_t == 7002007)
# test <- df_check_3 %>% subset(ID_t == 7002007)


# check for duplicates
sum(duplicated(data_sibling_final_2))

# check for missing values
sum(is.na(data_sibling_final_2))
colSums(is.na(data_sibling_final_2))

# all columns as integer
data_sibling_final_2 <- data_sibling_final_2 %>%
  mutate_if(is.numeric, as.integer)

# ungroup
data_sibling_final_2 <- data_sibling_final_2 %>% ungroup()

# number respondents, rows, and columns
paste("Number of respondents:", length(unique(data_sibling_final_2$ID_t)))
paste("Number of rows:", nrow(data_sibling_final_2))
paste("Number of columns:", ncol(data_sibling_final_2))

# save data frame
saveRDS(data_sibling_final_2, "Data/Prep_3/prep_3_sibling.rds")
