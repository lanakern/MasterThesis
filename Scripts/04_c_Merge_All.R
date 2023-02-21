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
# 1.) Merge sibling via ID_t: 
# -> For all respondents who are not in the sibling data frame all columns from 
# sibling are set to 0 (as they do not have an sibling).
# -> Age of siblings is generated.
# -> The activity variables (employment and study) only refer to the CATI interview 
# in 2010/11. Hence, they are set NA for further waves. For school degree the
# same is done if sibling is younger than 18. For the number of sibling variables,
# they are assumed to be constant over time.
#++++
# 2.) Merge child via ID_t and interview_date. All child variable values are NA 
# for respondents who do not have children; they are all set to 0.
#++++
# 3.) Merge partner: Same as for child
#++++
# 4.) Merge competencies: Merge same as for child and partner, but missing values
# are kept missing (I deal with them later)
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
# if (!require("lubridate")) install.packages("lubridate")
# library(lubridate)  # for working with dates
# 
# if (!require("xlsx")) install.packages("xlsx")
# library(xlsx)  # for excel file
# 
# # define inputs
#   ## selection on cohort preparation
# #cohort_prep <- "controls_bef_outcome" 
# cohort_prep <- "controls_same_outcome"
#   ## only for saving
# treatment_repl <- "downup" 



#%%%%%%%%%%%%%%%%%#
#### LOAD DATA ####
#%%%%%%%%%%%%%%%%%#

# CATI and CAWI
if (cohort_prep == "controls_same_outcome") {
  data_cati_cawi_eps <- readRDS(paste0("Data/Prep_4/prep_4_merge_cati_cawi_eps_treat", treatment_repl, ".rds")  )
} else if (cohort_prep == "controls_bef_outcome") {
  data_cati_cawi_eps <- readRDS(paste0("Data/Prep_4/prep_4_merge_cati_cawi_eps_treat", treatment_repl, "_robustcheck.rds")  )
}

# Sibling information (time-invariant)
data_sibling <- readRDS("Data/Prep_3/prep_3_sibling.rds")

# child data (time-variant)
if (cohort_prep == "controls_same_outcome") {
  data_child <- readRDS("Data/Prep_3/prep_3_child.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_child <- readRDS("Data/Prep_3/prep_3_child_robustcheck.rds")
}


# partner information (time-variant)
if (cohort_prep == "controls_same_outcome") {
  data_partner <- readRDS("Data/Prep_3/prep_3_partner.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_partner <- readRDS("Data/Prep_3/prep_3_partner_robustcheck.rds")
}


# competencies
if (cohort_prep == "controls_same_outcome") {
  data_competencies <- readRDS("Data/Prep_3/prep_3_competencies.rds")
} else if (cohort_prep == "controls_bef_outcome") {
  data_competencies <- readRDS("Data/Prep_3/prep_3_competencies_robustcheck.rds")
}



# number of respondents in different data sets
num_id_cati_cawi_eps <- length(unique(data_cati_cawi_eps$ID_t))
num_id_sib <- length(unique(data_sibling$ID_t))
num_id_child <- length(unique(data_child$ID_t))
num_id_partner <- length(unique(data_partner$ID_t))
num_id_comp <- length(unique(data_competencies$ID_t))



#%%%%%%%%%%%%%%%#
#### SIBLING ####
#%%%%%%%%%%%%%%%#

# check for missing values and duplicates
sum(duplicated(data_sibling))
sum(is.na(data_sibling))

# sibling data: keep only respondents who are in data_cati_cawi_eps
id_cati_cawi_eps <- unique(data_cati_cawi_eps$ID_t)
data_sibling_adj <- data_sibling %>% subset(ID_t %in% id_cati_cawi_eps)
num_id_sib_adj_1 <- length(unique(data_sibling_adj$ID_t))

# sub data frame
data_sibling_adj_1 <- data_sibling_adj %>%
  select(ID_t, sibling, sibling_total, starts_with("sibling_older_total"), starts_with("sibling_twin"),
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
# cols_NA <- data_merge_sib %>% select(matches(".*employed.*"), matches(".*study.*")) %>% colnames()
#   ## create NA dummy
# data_merge_sib <- data_merge_sib %>%
#   mutate(
#     sibling_employed_1_NA = ifelse(is.na(sibling_employed_1), 1, 0),
#     sibling_employed_2_NA = ifelse(is.na(sibling_employed_2), 1, 0),
#     sibling_study_1_NA = ifelse(is.na(sibling_study_1), 1, 0),
#     sibling_study_2_NA = ifelse(is.na(sibling_study_2), 1, 0)
#     )
#   ## iterate over columns
# for (cols_NA_sel in cols_NA) {
#   data_merge_sib <- data_merge_sib %>%
#     mutate(
#       {{cols_NA_sel}} := ifelse(is.na(!!!syms(cols_NA_sel)), 0, !!!syms(cols_NA_sel))
#     )
# }


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
    # sibling_uni_entrance_quali_1_NA = ifelse(
    #   sibling_age_1 < 18 & !is.na(sibling_age_1) & year(interview_date_spell) >= 2013, 1, sibling_uni_entrance_quali_1_NA),
    sibling_uni_entrance_quali_1 = ifelse(#sibling_uni_entrance_quali_1_NA == 1 & 
      sibling_age_1 < 18 & !is.na(sibling_age_1) & year(interview_date_spell) >= 2013, 
      0, sibling_uni_entrance_quali_1),
    # sibling_uni_entrance_quali_2_NA = ifelse(
    #   sibling_age_2 < 18 & !is.na(sibling_age_2) & year(interview_date_spell) >= 2013, 1, sibling_uni_entrance_quali_2_NA),
    sibling_uni_entrance_quali_2 = ifelse(#sibling_uni_entrance_quali_2_NA == 1 & 
      sibling_age_2 < 18 & !is.na(sibling_age_2) & year(interview_date_spell) >= 2013, 
      0, sibling_uni_entrance_quali_2)
    ) 

colSums(is.na(data_merge_sib)) # should be only missings for age
length(unique(data_merge_sib$ID_t))

# for all students with only one sibling, "_2" variables are set to zero
cols_sib_2 <- data_merge_sib %>% select(ends_with("_2")) %>% colnames()
data_merge_sib <- data_merge_sib %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(across(all_of(cols_sib_2), ~ case_when(sibling_total == 1 ~ 0, TRUE ~ .)))

# merge this prepared data frame to data_cati_cawi_eps
data_merge_1 <- left_join(
  data_cati_cawi_eps, data_merge_sib %>% select(-interview_date_spell), 
  by = c("ID_t", "treatment_period")
)
length(unique(data_merge_1$ID_t))


# for respondents with no siblings all variables are set to 0
  ## replace sibling var with 0
table(data_merge_1$sibling, useNA = "always")
data_merge_1 <- data_merge_1 %>% 
  replace_na(list(sibling = 0, sibling_older_total = 0, sibling_twin = 0))
table(data_merge_1$sibling, useNA = "always")
  ## replace all other vars with ß
col_sibling <- colnames(data_merge_sib %>% select(starts_with("sibling")))
data_merge_1 <- data_merge_1 %>%
  mutate_if(is.integer, as.numeric) %>% 
  mutate(across(all_of(col_sibling), ~ case_when(sibling == 0 ~ as.numeric(0), TRUE ~ .)))

# check NAs in sibling variables
sum(is.na(data_merge_1 %>% select(starts_with("sibling"))))
colSums(is.na(data_merge_1 %>% select(starts_with("sibling"))))

# if sibling == 0, all sibling_ variables need to be 0
data_merge_1 %>% filter(sibling == 0) %>% select(starts_with("sibling")) %>% unique() %>% pull()

# number of respondents, rows and columns
num_id_cati_cawi_eps_sib <- length(unique(data_merge_1$ID_t)) 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%#
#### CHILD ####
#%%%%%%%%%%%%%#

# missing values
sum(is.na(data_child))

# number of respondents with children
id_child <- unique(data_child$ID_t)
length(id_child)

# number of respondents with children who have a match in merged data
if (cohort_prep == "controls_bef_outcome") {
  id_cati_cawi_eps_child <- 
    inner_join(data_merge_1, data_child, 
               by = c("ID_t", "interview_date_CATI" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_child_adj_1 <- length(id_cati_cawi_eps_child)
} else if (cohort_prep == "controls_same_outcome") {
  id_cati_cawi_eps_child <- 
    inner_join(data_merge_1, data_child, 
               by = c("ID_t", "interview_date_start" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_child_adj_1 <- length(id_cati_cawi_eps_child)
}


# show difference
setdiff(id_child, id_cati_cawi_eps_child)

data_child %>% subset(ID_t == 7019629) %>% 
  select(ID_t, starts_with("interview_date"))
data_merge_1 %>% subset(ID_t == 7019629) %>% 
  select(ID_t, starts_with("interview_date"))

data_child %>% subset(ID_t == 7010555) %>% 
  select(ID_t, starts_with("interview_date"))
data_merge_1 %>% subset(ID_t == 7010555) %>% 
  select(ID_t, starts_with("interview_date"))

# keep only respondents in child who also have obsveration in other data set
data_child <- data_child %>% subset(ID_t %in% id_cati_cawi_eps_child)
length(unique(data_child$ID_t))

# information about each respondent's children is appended.
if (cohort_prep == "controls_bef_outcome") {
  data_merge_2 <- left_join(data_merge_1, data_child, 
                            by = c("ID_t", "interview_date_CATI" = "interview_date"))
} else if (cohort_prep == "controls_same_outcome") {
  data_merge_2 <- left_join(data_merge_1, data_child, 
                            by = c("ID_t", "interview_date_start" = "interview_date"))
}

# extract child columns
col_child <- data_child %>% select(-c(ID_t, interview_date)) %>% colnames()

# for respondents with no children all variables are set to zero
data_merge_2 <- data_merge_2 %>%
  mutate_at(all_of(col_child), ~ replace_na(.,0))

# check that there are no NAs in child variable
data_merge_2 %>%
  ungroup() %>% select(all_of(col_child)) %>%
  summarize(sum(is.na(.))) %>% pull()


num_id_cati_cawi_eps_sib_child <- length(unique(data_merge_2$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%#
#### Partner ####
#%%%%%%%%%%%%%%%#

# number of respondents with a partner
id_partner <- unique(data_partner$ID_t)
length(id_partner)

# number of respondents with partner who have a match in merged data
if (cohort_prep == "controls_bef_outcome") {
  id_cati_cawi_eps_child_partner <- 
    inner_join(data_merge_2, data_partner, 
               by = c("ID_t", "interview_date_CATI" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_partner_adj_1 <- length(id_cati_cawi_eps_child_partner)
} else if (cohort_prep == "controls_same_outcome") {
  id_cati_cawi_eps_child_partner <- 
    inner_join(data_merge_2, data_partner, 
               by = c("ID_t", "interview_date_start" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_partner_adj_1 <- length(id_cati_cawi_eps_child_partner)
}


# show difference
setdiff(id_partner, id_cati_cawi_eps_child_partner)

data_partner %>% subset(ID_t == 7002301) %>% 
  select(ID_t, starts_with("interview_date"))

data_merge_2 %>% subset(ID_t == 7002301) %>% 
  select(ID_t, starts_with("interview_date"))

# keep only respondents in partner  who also have observation in other data set
data_partner <- data_partner %>% subset(ID_t %in% id_cati_cawi_eps_child_partner)
length(unique(data_partner$ID_t))


# information about each respondent's relationship is appended.
if (cohort_prep == "controls_bef_outcome") {
  data_merge_3 <- left_join(data_merge_2, data_partner, 
                            by = c("ID_t", "interview_date_CATI" = "interview_date"))
} else if (cohort_prep == "controls_same_outcome") {
  data_merge_3 <- left_join(data_merge_2, data_partner, 
                            by = c("ID_t", "interview_date_start" = "interview_date"))
}

# extract partner columns
col_partner <- data_partner %>% select(-c(ID_t, interview_date)) %>% colnames()

# for respondents with no partner all variables are set to zero
colSums(is.na(data_merge_3))

table(data_merge_3$partner_current, useNA = "always")
data_merge_3 <- data_merge_3 %>% replace_na(list(partner_current = 0, partner_num = 0))
table(data_merge_3$partner_current, useNA = "always")

data_merge_3 <- data_merge_3 %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(across(all_of(col_partner), ~ case_when(partner_current == 0 ~ as.numeric(0), TRUE ~ .))) 


# check that there are no NAs in partner variables
colSums(is.na(data_merge_3 %>% select(all_of(col_partner))))


num_id_cati_cawi_eps_sib_child_partner <- length(unique(data_merge_3$ID_t))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%#
#### Competencies ####
#%%%%%%%%%%%%%%%%%%%%#

# number of respondents with a partner
id_comp <- unique(data_competencies$ID_t)
length(id_comp)

# number of respondents with partner who have a match in merged data
if (cohort_prep == "controls_bef_outcome") {
  id_cati_cawi_eps_child_partner_comp <- 
    inner_join(data_merge_3, data_competencies, 
               by = c("ID_t", "interview_date_CATI" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_comp_adj_1 <- length(id_cati_cawi_eps_child_partner_comp)
} else if (cohort_prep == "controls_same_outcome") {
  id_cati_cawi_eps_child_partner_comp <- 
    inner_join(data_merge_3, data_competencies, 
               by = c("ID_t", "interview_date_start" = "interview_date")) %>%
    pull(ID_t) %>% unique() 
  num_id_comp_adj_1 <- length(id_cati_cawi_eps_child_partner_comp)
}

# analyse differences
setdiff(id_cati_cawi_eps_child_partner, id_cati_cawi_eps_child_partner_comp)

data_merge_3 %>% subset(ID_t == 7009318) %>% select(ID_t, starts_with("interview_date"))
data_competencies %>% subset(ID_t == 7009318)


# keep only respondents who also have observation in other data set
data_competencies <- data_competencies %>% subset(ID_t %in% id_cati_cawi_eps_child_partner_comp)
length(unique(data_competencies$ID_t))
data_competencies %>% subset(ID_t == 7010580)

# information about each respondent's competencies is appended.
# INNER JOIN TO KEEP ONLY RESPONDENTS WITH AT LEAST ONE COMPETENCE MEASURE?
if (cohort_prep == "controls_bef_outcome") {
  data_merge_4 <- inner_join(data_merge_3, data_competencies, 
                            by = c("ID_t", "interview_date_CATI" = "interview_date"))
} else if (cohort_prep == "controls_same_outcome") {
  data_merge_4 <- inner_join(data_merge_3, data_competencies, 
                            by = c("ID_t", "interview_date_start" = "interview_date"))
}


colSums(is.na(data_merge_4 %>% select(starts_with("comp_"))))

num_id_final <- length(unique(data_merge_4$ID_t)) 
num_id_final


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%#
#### FINAL STEPS ####
#%%%%%%%%%%%%%%%%%%%#

# correct data types
data_merge_4 <- data_merge_4 %>% type.convert(as.is = TRUE)

# all child and partner variables should not have any missing value.
sum(is.na(data_merge_4 %>% select(starts_with("child_"), starts_with("partner_"))))

# total missing values
sum(is.na(data_merge_4))

# check for duplicates
sum(duplicated(data_merge_4))
sum(duplicated(data_merge_4 %>% select(ID_t, starts_with("interview_date"))))
sum(duplicated(data_merge_4 %>% select(ID_t, interview_date_start)))

# number of respondents, rows, and columns
print(paste("Number of respondents before data preparation:", num_id_cati_cawi_eps))
print(paste("Number of respondents after merging sibling:", num_id_cati_cawi_eps_sib))
print(paste("Number of respondents after merging child:", num_id_cati_cawi_eps_sib_child))
print(paste("Number of respondents after merging partner:", num_id_cati_cawi_eps_sib_child_partner))
print(paste("Number of respondents after merging competencies:", num_id_final))
print(paste("Number of respondents after data preparation:", num_id_final))
print(paste("Number of rows:", nrow(data_merge_4)))
print(paste("Number of columns:", ncol(data_merge_4)))

# save
if (cohort_prep == "controls_same_outcome") {
  data_merge_all_save <- paste0("Data/Prep_4/prep_4_merge_all_treat", treatment_repl, ".rds")  
} else {
  data_merge_all_save <- paste0("Data/Prep_4/prep_4_merge_all_treat", treatment_repl, "_robustcheck.rds") 
}

saveRDS(data_merge_4, data_merge_all_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "merge_all",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = NA, 
  "num_id" = length(unique(data_merge_4$ID_t)), 
  "num_rows" = nrow(data_merge_4),
  "num_cols" = ncol(data_merge_4),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "grade")
