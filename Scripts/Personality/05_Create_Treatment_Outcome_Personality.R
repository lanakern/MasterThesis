#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### GENERATE TREATMENT AND OUTCOME VARIABLES FOR PERSONALITY ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, the treatment and outcome variables are generated.
# -> Outcome: big five personality traits
# -> Treatment: sport participation as binary variable (0,1) and multiple
# treatment variable considering the frequency.
#++++
# 1.) Treatment
# -> Binary and multivalued treatment indicator: binary considers simply sport
# participation vs. non-participation while multivalued treatment indicator
# considers sport frequency.
# -> Leisure/general and university sport is aggregated: if only one information
# is provided, this info is used. If both are provided and do not coincide
# the higher frequency is considered. 
#++++
# 2.) Outcome
# -> Big five personality traits are recoded so that a lower value indicates
# a higher degree of the respective grade. This is done to ensure the same
# interpretation as for grades, that is "the lower the better". 
#++++
# 3.) Sample Selection
# -> Drop students who have a missing value in binary treatment variable and/or
# outcome
#++++
# -> RESULT: PANEL DATA FRAME
#++++


# load data
if (cohort_prep == "controls_same_outcome") {
  data_raw <- readRDS(paste0("Data/Personality/Prep_4/prep_4_merge_all_treat", treatment_repl, "_personality.rds"))
} else if (cohort_prep == "controls_bef_outcome") {
  data_raw <- readRDS(paste0("Data/Personality/Prep_4/prep_4_merge_all_treat", treatment_repl, "_personality_robustcheck.rds") )
}
num_id <- length(unique(data_raw$ID_t))




#%%%%%%%%%%%%%%%%%#
#### Treatment ####
#%%%%%%%%%%%%%%%%%#

data_raw %>% dplyr::select(ID_t, starts_with("interview_date"), starts_with("sport"))

# count missing values in treatment variables
colSums(is.na(data_raw %>% dplyr::select(starts_with("sport"))))

# there are no non-missing values for sport_uni_freq if sport_uni is missing
data_raw %>% filter(is.na(sport_uni) & !is.na(sport_uni_freq))

# sport_uni_freq is always NA if sport_uni not involved
data_raw %>% filter(sport_uni == "not involved") %>% pull(sport_uni_freq) %>% unique() # should return NA



## CREATE TREATMENT VARIABLE FOR SPORT FREQUENCY ##
#+++++++++++++++++++++++++++++++++++++++++++++++++#

data_1 <- data_raw

# 1.) Recode sport_uni_freq as "never" for sport_uni == "not involved"
data_1 <- data_1 %>%
  mutate(
    sport_uni_freq = ifelse(sport_uni == "not involved" & is.na(sport_uni_freq), "never", sport_uni_freq),
    sport_uni_freq_lag = ifelse(sport_uni_lag == "not involved" & is.na(sport_uni_freq_lag), "never", sport_uni_freq_lag)
    )

table(data_1$sport_uni, useNA = "always")
table(data_1$sport_uni_freq, useNA = "always")
table(data_1$sport_uni_freq_lag, useNA = "always")
data_1 %>% filter(sport_uni == "not involved") %>% pull(sport_uni_freq) %>% unique()
data_1 %>% filter(sport_uni_lag == "not involved") %>% pull(sport_uni_freq_lag) %>% unique()


# 2.) Recode values: 
## General sport (g)
##++ 1 = never
##++ 2 = once a month or less
##++ 3 = several times a month or once a week
##++ 4 = several times a week
##++ 5 = almost daily or daily
## University sport (u)
##++ 1 = daily, 
##++ 2 = several times a week, 
##++ 3 = once a week, 
##++ 4 = several times a month, 
##++ 5 = once a month, 
##++ 6 = less frequently
##++ 7 = never (own generation; see above)
## AGGREGATED:
##++ 1 = never: g1 & u7
##++ 2 = less frequently: g2 & u5 & u6
##++ 3 = monthly: g3 & u4 & u3
##++ 4 = weekly: g4 & u2
##++ 5 = daily: g5 & u1

table(data_1$sport_uni_freq, useNA = "always")
table(data_1$sport_leisure_freq, useNA = "always")


if (any(!is.na(unique(data_1$sport_leisure_freq)))) {
  data_1 <- data_1 %>%
    mutate(
      sport_uni_freq = recode(
        sport_uni_freq,
        "never" = 1, "less frequently" = 2, "once a month" = 2, 
        "several times a month" = 3, "once a week" = 3, 
        "several times a week" = 4, "daily" = 5
      ),
      sport_leisure_freq = recode(
        sport_leisure_freq, 
        "never" = 1, "once a month or less" = 2, 
        "several times a month or once a week" = 3,
        "several times a week" = 4, "almost daily or daily" = 5
      )
    ) 
} else {
  data_1 <- data_1 %>%
    mutate(
      sport_uni_freq = recode(
        sport_uni_freq,
        "never" = 1, "less frequently" = 2, "once a month" = 2, 
        "several times a month" = 3, "once a week" = 3, 
        "several times a week" = 4, "daily" = 5
      )
    )
}


# recode lags (problems with NA. hence if)
if (any(!is.na(unique(data_1$sport_uni_freq_lag)))) {
  data_1 <- data_1 %>%
    # labels
    mutate(
      sport_uni_freq_lag = recode(
        sport_uni_freq_lag,
        "never" = 1, "less frequently" = 2, "once a month" = 2, 
        "several times a month" = 3, "once a week" = 3, 
        "several times a week" = 4, "daily" = 5
      ))
}

if (any(!is.na(unique(data_1$sport_leisure_freq_lag)))) {
  data_1 <- data_1 %>%
    # labels
    mutate(
      sport_leisure_freq_lag = recode(
        sport_leisure_freq_lag, 
        "never" = 1, "once a month or less" = 2, 
        "several times a month or once a week" = 3,
        "several times a week" = 4, "almost daily or daily" = 5
      ))
}

table(data_1$sport_uni_freq, useNA = "always")
table(data_1$sport_leisure_freq, useNA = "always")
table(data_1$sport_leisure_freq_lag, useNA = "always")
table(data_1$sport_uni_freq_lag, useNA = "always")


# generate one treatment frequency variable (for both university and leisure sport)
# same is done for lag
data_1 <- data_1 %>%
  mutate(
    treatment_sport_freq = case_when(
      # if uni sport is missing use info on leisure sport
      is.na(sport_uni_freq) ~ sport_leisure_freq,
      # if leisure sport frequency is higher, however, use info on leisure sport
      sport_leisure_freq > sport_uni_freq & !is.na(sport_leisure_freq) & !is.na(sport_uni_freq) ~ sport_leisure_freq,
      # in all other cases use uni sport
      TRUE ~ sport_uni_freq
    )
  ) %>%
  mutate(
    treatment_sport_freq_lag = case_when(
      # if uni sport is missing use info on leisure sport
      is.na(sport_uni_freq_lag) ~ sport_leisure_freq_lag,
      # if leisure sport frequency is higher, however, use info on leisure sport
      sport_leisure_freq_lag > sport_uni_freq_lag & !is.na(sport_leisure_freq_lag) & !is.na(sport_uni_freq_lag) ~ sport_leisure_freq_lag,
      # in all other cases use uni sport
      TRUE ~ sport_uni_freq_lag
    )
  )

table(data_1$treatment_sport_freq, useNA = "always")
table(data_1$treatment_sport_freq_lag, useNA = "always")

# create dummy to know if I used uni or leisure sport information
data_1 <- data_1 %>%
  mutate(sport_freq_source = 
           case_when(
             # if frequency of university sport equals frequency of sport in treatment variable
             # AND frequency of uni sport does not equal leisure sport or leisure sport is NA
             # then the source is uni
             treatment_sport_freq == sport_uni_freq & 
               (sport_uni_freq != sport_leisure_freq | is.na(sport_leisure_freq)) ~ "uni",
             # same idea for leisure than for uni
             treatment_sport_freq == sport_leisure_freq  & 
               (sport_uni_freq != sport_leisure_freq | is.na(sport_uni_freq)) ~ "leisure",
             # "both" is selected if frequency of leisure and university sport is the same
             sport_uni_freq == sport_leisure_freq & !is.na(sport_uni_freq) & !is.na(sport_leisure_freq) ~ "both",
             # otherwise NA because no info about sport participation is given
             TRUE ~ as.character(NA)
           )
  )

table(data_1$sport_freq_source, useNA = "always")


# recode frequency variable
data_1 <- data_1 %>% mutate(
  treatment_sport_freq = recode(treatment_sport_freq,
    "1" = "never", "2" = "less frequently", "3" = "monthly", "4" = "weekly", "5" = "daily"
  ),
  treatment_sport_freq_lag = recode(
    treatment_sport_freq_lag, 
    "1" = "never", "2" = "less frequently", "3" = "monthly", "4" = "weekly", "5" = "daily"
  )
  )




## CREATE BINARY TREATMENT INDICATOR ##
#+++++++++++++++++++++++++++++++++++++#

data_2 <- data_1

# sport_uni = 1 if respondent participates in university sport
# sport_leisure = 1 if respondent participates in leisure sport
# -> also both variables can take on the value 1
table(data_2$sport_uni, useNA = "always")
table(data_2$sport_uni_freq, useNA = "always")
table(data_2$sport_leisure_freq, useNA = "always")


## CONSIDER ALL FREQUENCY LEVELS ##
if (treatment_def == "all") {
  data_2 <- data_2 %>%
    # rename original sport_uni variable
    rename(sport_uni_orig = sport_uni, sport_uni_lag_orig = sport_uni_lag) %>%
    # create variables
    mutate(
      # respondent participates in university sport if sport_uni_orig equals
      # "involved".
      sport_uni = ifelse(sport_uni_orig == "not involved", 0, 
                         ifelse(is.na(sport_uni_orig), NA, 1)),
      # respondent participates in leisure sport if sport_leisure_freq is NOT NA and not 0
      sport_leisure = ifelse(sport_leisure_freq > 1, 1, 
                             ifelse(is.na(sport_leisure_freq), NA, 0)),
      # do the same for the lags
      sport_uni_lag = ifelse(sport_uni_lag_orig == "not involved", 0, 
                             ifelse(is.na(sport_uni_lag_orig), NA, 1)),
      sport_leisure_lag = ifelse(sport_leisure_freq_lag > 1, 1, 
                                 ifelse(is.na(sport_leisure_freq_lag), NA, 0))
    ) 
  
  table(data_2$sport_uni, useNA = "always")
  table(data_2$sport_leisure, useNA = "always")
  table(data_2$sport_uni_lag, useNA = "always")
  table(data_2$sport_leisure_lag, useNA = "always")
  
  
  # create general dummy for sport-participation (=1) and non-participation (=0)
  # keep NA as later individuals and treatment periods without sport information are dropped
  data_2 <- data_2 %>%
    mutate(
      treatment_sport = case_when(sport_leisure == 1 | sport_uni == 1 ~ 1,
                                  is.na(sport_leisure) & is.na(sport_uni) ~ as.double(NA),
                                  TRUE ~ 0),
      treatment_sport_lag = case_when(sport_leisure_lag == 1 | sport_uni_lag == 1 ~ 1,
                                      is.na(sport_leisure_lag) & is.na(sport_uni_lag) ~ as.double(NA),
                                      TRUE ~ 0)
    ) %>%
    # create indicator for source
    mutate(
      sport_source = 
        case_when(
          is.na(treatment_sport) ~ as.character(NA),
          !is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "both",
          !is.na(sport_uni_orig) & is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "uni",
          is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "leisure",
          TRUE ~ as.character(NA)
        )
    )
  ## ONLY CONSIDER AS SPORT PARTICIPATION WEEKLY PARTICIPATION
} else {
  data_2 <- data_2 %>%
    rename(sport_uni_orig = sport_uni, sport_uni_lag_orig = sport_uni_lag) %>%
    # create variables
    mutate(
      # respondent participates in university sport if sport_uni_freq variable
      # equals at least 4
      sport_uni = ifelse(sport_uni_freq > 3, 1, 
                         ifelse(is.na(sport_uni_freq), NA, 0)),
      # respondent participates in leisure sport if sport_leisure_freq is NOT NA and not 0
      sport_leisure = ifelse(sport_leisure_freq > 3, 1, 
                             ifelse(is.na(sport_leisure_freq), NA, 0)),
      # for lags
      sport_uni_lag = ifelse(sport_uni_freq_lag > 3, 1, 
                             ifelse(is.na(sport_uni_freq_lag), NA, 0)),
      sport_leisure_lag = ifelse(sport_leisure_freq_lag > 3, 1, 
                                 ifelse(is.na(sport_leisure_freq_lag), NA, 0))
    ) 
  
  table(data_2$sport_uni, useNA = "always")
  table(data_2$sport_leisure, useNA = "always")
  
  
  # create general dummy for sport-participation (=1) and non-participation (=0)
  # keep NA as later individuals and treatment periods without sport information are dropped
  data_2 <- data_2 %>%
    mutate(
      treatment_sport = case_when(sport_leisure == 1 | sport_uni == 1 ~ 1,
                                  is.na(sport_leisure) & is.na(sport_uni) ~ as.double(NA),
                                  TRUE ~ 0),
      treatment_sport_lag = case_when(sport_leisure_lag == 1 | sport_uni_lag == 1 ~ 1,
                                      is.na(sport_leisure_lag) & is.na(sport_uni_lag) ~ as.double(NA),
                                      TRUE ~ 0)
    ) %>%
    # create indicator for source
    mutate(
      sport_source = 
        case_when(
          is.na(treatment_sport) ~ as.character(NA),
          !is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "both",
          !is.na(sport_uni_orig) & is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "uni",
          is.na(sport_uni_orig) & !is.na(sport_leisure_freq) & !is.na(treatment_sport) ~ "leisure",
          TRUE ~ as.character(NA)
        )
    )
}



table(data_2$sport_uni, useNA = "always")
table(data_2$sport_leisure, useNA = "always")
table(data_2$sport_uni_freq, useNA = "always")
table(data_2$sport_leisure_freq, useNA = "always")
table(data_2$treatment_sport, useNA = "always")
table(data_2$sport_source, useNA = "always")


# create NA variables
data_2 <- data_2 %>% 
  mutate(
    treatment_sport_NA = ifelse(sport_uni_NA == 1 & sport_leisure_freq_NA == 1, 1, 0),
    treatment_sport_freq_NA = ifelse(sport_uni_freq_NA == 1 & sport_leisure_freq_NA == 1, 1, 0),
    treatment_sport_lag_NA = ifelse(sport_uni_freq_lag_NA == 1 & sport_leisure_freq_lag_NA == 1, 1, 0),
    treatment_sport_freq_lag_NA = ifelse(sport_uni_freq_lag_NA == 1 & sport_leisure_freq_lag_NA == 1, 1, 0)
  )

table(data_2$treatment_sport_NA, useNA = "always")
table(data_2$treatment_sport_freq_NA, useNA = "always")
table(data_2$treatment_sport_lag_NA, useNA = "always")
table(data_2$treatment_sport_freq_lag_NA, useNA = "always")


# drop variables not needed anymore
data_2 <- data_2 %>% rename(
  treatment_sport_source = sport_source, 
  treatment_sport_freq_source = sport_freq_source
)
data_2 <- data_2 %>% dplyr::select(-starts_with("sport"))

# number of respondents
length(unique(data_2$ID_t)) # should be unchanged



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



#%%%%%%%%%%%%%%%#
#### Outcome ####
#%%%%%%%%%%%%%%%#

data_3 <- data_2

# reverse personality traits so that the lower the higher the degree of the
# respective personality trait. Some are already measured in the correct order.
  ## before
data_3 %>% dplyr::select(starts_with("bigfive") & !ends_with("NA")) %>% summary()
data_3 %>% dplyr::select(starts_with("bigfive") & !ends_with("NA")) %>% head(10)
  ## reverse
df_reverse_bigfive <- data.frame(
  "vars_reverse" = data_3 %>% dplyr::select(starts_with("bigfive") & !ends_with("NA")) %>% colnames(),
  "num_scores" = rep(5, 5)
  )
data_3 <- func_reverse_score(data_3, df_reverse_bigfive)
  ## after
data_3 %>% dplyr::select(starts_with("bigfive") & !ends_with("NA")) %>% head(10)
data_3 %>% dplyr::select(starts_with("bigfive") & !ends_with("NA")) %>% summary()


# number of respondents
length(unique(data_3$ID_t)) # should be unchanged



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Sample Selection: Treatment and Outcome ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data_4 <- data_3

# subset: keep only respondents who do not have any missing in treatment and
# grade 
drop_missings <- 
  data_4 %>% dplyr::select(treatment_sport, starts_with("bigfive") & !ends_with("NA")) %>% 
  is.na() %>% rowSums()
data_4 <- data_4[drop_missings == 0,]

num_id_adj_1 <- length(unique(data_4$ID_t)) 
drop_na <- num_id - num_id_adj_1


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%#
#### Final Steps ####
#%%%%%%%%%%%%%%%%%%%#

# ungroup data frame
data_4 <- data_4 %>% ungroup()

# ensure that they are no missing values for treatment and outcome (except freq)
colSums(is.na(data_4 %>% dplyr::select(starts_with("treatment_s"), starts_with("outcome"))))

# check for duplicates
sum(duplicated(data_4))

# print sample reduction
print(paste("Number of respondents before data preparation:", num_id))
print(paste("Number of respondents after dropping respondents with missing value in treatment and/or outcome:", num_id_adj_1))


# save
if (cohort_prep == "controls_same_outcome") {
  data_save <- paste0("Data/Personality/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, "_personality.rds")  
} else {
  data_save <- paste0("Data/Personality/Prep_5/prep_5_treatment_outcome_", treatment_def, 
                      "_", treatment_repl, "_personality_robustcheck.rds")  
}



saveRDS(data_4, data_save)

# save number of rows, columns, and respondents in excel file
df_excel_save <- data.frame(
  "data_prep_step" = "treatment_outcome",
  "data_prep_choice_cohort" = cohort_prep,
  "data_prep_treatment_repl" = treatment_repl,
  "data_prep_treatment_def" = treatment_def,
  "num_id" = length(unique(data_4$ID_t)), 
  "num_rows" = nrow(data_4),
  "num_cols" = ncol(data_4),
  "time_stamp" = Sys.time()
)
## load function
source("Functions/func_save_sample_reduction.R")
func_save_sample_reduction(df_excel_save, "personality")
