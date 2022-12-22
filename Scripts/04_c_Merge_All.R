#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MERGE REMAINING DATA SETS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this script, the remaining data sets, sibling, partner, child and competencies
# are merged to the espisode+cati+cawi data set.
# -> "Final" panel data frame with all control variables.
#++++
# 1.) Merge sibling: 
# -> For all respondents who are not in the sibling data frame all columns from 
# sibling are set to 0 (as they do not have an sibling).
# -> Age of siblings is generated and a dummy indicating if respondent has a sibling or not.
# -> The activity variables (employment and study) only refer to the CATI interview 
# in 2010/11. Hence, they are set NA for further waves. For school degree the
# same is done if sibling is younger than 18. For the number of sibling variables,
# they are assumed to be constant over time.
#++++
# 2.) Merge child:
#++++
# 3.) Merge partner:
#++++
# 4.) Merge competencies:
#++++


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("tidyr")) install.packages("tidyr")
library(tidyr)  # to manipulate data, e.g. replace_na, spread() etc.

if (!require("sqldf")) install.packages("sqldf")
library(sqldf)  # for sql syntax

if (!require("xlsx")) install.packages("xlsx")
library(xlsx)  # for excel file

# define inputs
  ## selection on cohort preparation
cohort_prep <- "controls_bef_outcome" 
#cohort_prep <- "controls_same_outcome"
  ## treatment replacement
treatment_repl <- "downup" # (only used for saving)


#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# CATI and CAWI
if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi_eps <- readRDS("Data/Prep_4/prep_4_merge_cati_cawi_eps.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi_eps <- readRDS("Data/Prep_4/prep_4_merge_cati_cawi_eps_robustcheck.rds")
}

# Sibling information (time-invariant)
data_sibling <- readRDS("Data/Prep_3/prep_3_sibling.rds")

# child data (time-variant)
data_child <- readRDS("Data/Prep_3/prep_3_child.rds")

# partner information (time-variant)
data_partner <- readRDS("Data/Prep_3/prep_3_partner.rds")

# competencies
data_competencies <- readRDS("Data/Prep_3/prep_3_competencies.rds")

# number of respondents in different data sets
num_id_cati_cawi_eps <- length(unique(data_cati_cawi_eps$ID_t))
num_id_sib <- length(unique(data_sibling$ID_t))

num_id_child <- length(unique(data_child$ID_t))
num_id_partner <- length(unique(data_partner$ID_t))
num_id_comp <- length(unique(data_competencies$ID_t))



#%%%%%%%%%%%%%%%#
#### SIBLING ####
#%%%%%%%%%%%%%%%#

# sibling data: keep only respondents who are in data_cati_cawi_eps
id_cati_cawi_eps <- unique(data_cati_cawi_eps$ID_t)
data_sibling_adj <- data_sibling %>% subset(ID_t %in% id_cati_cawi_eps)
num_id_sib_adj_1 <- length(unique(data_sibling_adj$ID_t))

# sub data frame
data_sibling_adj_1 <- data_sibling_adj %>%
  select(ID_t, sibling_total, starts_with("sibling_older_total"), starts_with("sibling_twin"),
         matches(".*birth_date.*"), starts_with("sibling_uni_entrance_quali"))

data_sibling_adj_2 <- data_sibling_adj %>%
  select(ID_t, starts_with("sibling_employed"), starts_with("sibling_study"))


# merge sibling information
# as this information is cross-sectional, only the ID is needed for the merge
data_merge_sib_1 <- inner_join(
  data_cati_cawi_eps %>% select(ID_t, treatment_period, interview_date_spell) %>% distinct(), 
  data_sibling_adj_1, by = "ID_t"
) 
length(unique(data_merge_sib_1$ID_t))

data_merge_sib_2 <- inner_join(
  data_cati_cawi_eps %>% select(ID_t, treatment_period, interview_date_spell), 
  data_sibling_adj_2, by = "ID_t"
) %>%
  filter(year(interview_date_spell) < 2013) %>%
  select(-interview_date_spell)
length(unique(data_merge_sib_2$ID_t))

data_merge_sib <- left_join(data_merge_sib_1, data_merge_sib_2, by = c("ID_t", "treatment_period"))
length(unique(data_merge_sib$ID_t))


# replace sibling_activity_NA and set sibling activity variable 0
cols_NA <- data_merge_sib %>% select(matches(".*employed.*"), matches(".*study.*")) %>% colnames()
  ## create NA dummy
data_merge_sib <- data_merge_sib %>%
  mutate(
    sibling_employed_1_NA = ifelse(is.na(sibling_employed_1), 1, 0),
    sibling_employed_2_NA = ifelse(is.na(sibling_employed_2), 1, 0),
    sibling_study_1_NA = ifelse(is.na(sibling_study_1), 1, 0),
    sibling_study_2_NA = ifelse(is.na(sibling_study_2), 1, 0)
    )
  ## iterate over columns
for (cols_NA_sel in cols_NA) {
  data_merge_sib <- data_merge_sib %>%
    mutate(
      {{cols_NA_sel}} := ifelse(is.na(!!!syms(cols_NA_sel)), 0, !!!syms(cols_NA_sel))
    )
}


# generate age of sibling
data_merge_sib <- data_merge_sib %>%
  mutate(
    sibling_age_1 = as.numeric(difftime(interview_date_spell, sibling_birth_date_1, units = "weeks")) / 52.5,
    sibling_age_2 = as.numeric(difftime(interview_date_spell, sibling_birth_date_2, units = "weeks")) / 52.5,
  ) %>% select(-matches(".*birth_date.*"))

# adjust school degree
  ## if sibling_age < 18 & interview_date_spell >= 2013, then generate sibling_uni_entrance_quali_NA = 1,
  ## and set sibling_uni_entrance_quali = 0
# data_merge_sib %>% select(ID_t, interview_date_spell, matches(".*age.*"), matches("sibling_uni_entrance")) %>% head(5)
# data_merge_sib %>% select(ID_t, interview_date_spell, matches(".*age.*"), matches("sibling_uni_entrance")) %>% filter(sibling_age_1 < 18) %>% head(5)

data_merge_sib <- data_merge_sib %>%
  mutate(
    sibling_uni_entrance_quali_1_NA = ifelse(
      sibling_age_1 < 18 & !is.na(sibling_age_1) & year(interview_date_spell) >= 2013, 1, sibling_uni_entrance_quali_1_NA),
    sibling_uni_entrance_quali_1 = ifelse(
      sibling_uni_entrance_quali_1_NA == 1 & sibling_age_1 < 18 & !is.na(sibling_age_1) & year(interview_date_spell) >= 2013, 
      0, sibling_uni_entrance_quali_1),
    sibling_uni_entrance_quali_2_NA = ifelse(
      sibling_age_2 < 18 & !is.na(sibling_age_2) & year(interview_date_spell) >= 2013, 1, sibling_uni_entrance_quali_2_NA),
    sibling_uni_entrance_quali_2 = ifelse(
      sibling_uni_entrance_quali_2_NA == 1 & sibling_age_2 < 18 & !is.na(sibling_age_2) & year(interview_date_spell) >= 2013, 
      0, sibling_uni_entrance_quali_2)
    ) 

colSums(is.na(data_merge_sib)) # should be only missings for age
length(unique(data_merge_sib$ID_t))


# merge this prepared data frame to data_cati_cawi_eps
data_merge_1 <- left_join(
  data_cati_cawi_eps, data_merge_sib %>% select(-interview_date_spell), 
  by = c("ID_t", "treatment_period")
)
length(unique(data_merge_1$ID_t))

# generate dummy if respondent has a sibling
data_merge_1 <- data_merge_1 %>%
  mutate(sibling = ifelse(is.na(sibling_total), 0, 1))

# for respondents with no siblings all variables are set to 0
col_sibling <- colnames(data_merge_sib %>% select(starts_with("sibling")))
data_merge_1 <- data_merge_1 %>%
  mutate_if(is.integer, as.numeric) %>% 
  mutate(across(all_of(col_sibling), ~ case_when(sibling == 0 ~ as.numeric(0), TRUE ~ .)))

## check that there are no NAs in sibling variable (except age)
data_merge_1 %>%
  ungroup() %>% select(all_of(col_sibling)) %>% select(-starts_with("sibling_age")) %>%
  summarize(sum(is.na(.))) %>% pull()

# number of respondents, rows and columns
num_id_cati_cawi_eps_sib <- length(unique(data_merge_1$ID_t)) 

# checks
## if sibling == 0, all sibling_ variables need to be 0
data_merge_1 %>% filter(sibling == 0) %>% select(starts_with("sibling")) %>% unique() %>% pull()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
#### CHILD ####
#%%%%%%%%%%%%%#

# information about each respondent's children is appended.
# this information is only collected in the CATI surveys
# Hence, as merge variable the CATI interview date is used additional to the ID
data_merge_4 <- left_join(data_merge_3, data_child %>% select(-wave), 
                          by = c("ID_t", "interview_date_cati" = "interview_date"))


# for respondents with no children all variables are set to zero
col_child <- data_child %>% select(-c(ID_t, wave, interview_date)) %>% colnames()
data_merge_4 <- data_merge_4 %>%
  mutate_at(all_of(col_child), ~replace_na(.,0))

# check that there are no NAs in sibling variable
data_merge_4 %>%
  ungroup() %>% select(all_of(col_child)) %>%
  summarize(sum(is.na(.))) %>% pull()

#length(unique(data_merge_4$ID_t)) # 9,062


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%#
#### Partner ####
#%%%%%%%%%%%%%%%#


# information about each respondent's partner history is appended.
# this information is only collected in the CATI surveys
# Hence, as merge variable the CATI interview date is used additional to the ID
data_merge_5 <- left_join(data_merge_4, data_partner %>% select(-wave), 
                          by = c("ID_t", "interview_date_cati" = "interview_date"))


# for respondents with no partner all variables are set to zero
col_partner <- data_partner %>% select(-c(ID_t, wave, interview_date)) %>% colnames()
data_merge_5 <- data_merge_5 %>%
  mutate_at(all_of(col_partner), ~replace_na(.,0))

# check that there are no NAs in sibling variable
data_merge_5 %>%
  ungroup() %>% select(all_of(col_partner)) %>%
  summarize(sum(is.na(.))) %>% pull()

#length(unique(data_merge_5$ID_t)) # 9,062

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%#
#### Competencies ####
#%%%%%%%%%%%%%%%%%%%%#

# number of individuals with no competence measures
length(setdiff(unique(data_merge_5$ID_t), unique(data_competencies$ID_t)))

# data competencies is merged via the treatment_starts indicator
data_merge_6 <- left_join(data_merge_5, data_competencies, 
                          by = c("ID_t", "treatment_starts"))

# check for missing values in competence measures
data_merge_6 %>%
  ungroup() %>% select(all_of(colnames(data_competencies)[-1])) %>%
  summarize(sum(is.na(.))) %>% pull()


#length(unique(data_merge_5$ID_t)) # 9,062

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#