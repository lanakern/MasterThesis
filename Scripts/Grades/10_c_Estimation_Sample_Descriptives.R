#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### ESTIMATION SAMPLE DESCRIPTIVES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# In this file, the different estimation samples are analyzed according
# to their number of students, observations, treatment and control group sizes
# and number of variables. To do so, only one MICE data set is loaded as these
# characteristics are identical across those (MICE data sets only differ
# across control variables).
#+++


#### Main Model ####
#++++++++++++++++++#

data_main <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_mice1.rds")
print(paste("Number of respondents:", length(unique(data_main$group))))
print(paste("Number of observations:", nrow(data_main)))
print(paste("Number of observations for sport participations:", nrow(data_main %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_main %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_main)))


#### RC 1: All Sport Participants ####
#++++++++++++++++++++++++++++++++++++#

data_rc_1 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_all_down_extradrop_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_1$group))))
print(paste("Number of observations:", nrow(data_rc_1)))
print(paste("Number of observations for sport participations:", nrow(data_rc_1 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_1 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_1)))


#### RC 2: No LVCF ####
#+++++++++++++++++++++#

data_rc_2 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_no_extradrop_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_2$group))))
print(paste("Number of observations:", nrow(data_rc_2)))
print(paste("Number of observations for sport participations:", nrow(data_rc_2 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_2 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_2)))


#### RC 3: No ExtraActivity drop ####
#+++++++++++++++++++++++++++++++++++#

data_rc_3 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_3$group))))
print(paste("Number of observations:", nrow(data_rc_3)))
print(paste("Number of observations for sport participations:", nrow(data_rc_3 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_3 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_3)))


#### RC 5: Controls-Treatment Before Outcome ####
#+++++++++++++++++++++++++++++++++++++++++++++++#

data_rc_4 <- readRDS("Data/Grades/Prep_10/prep_10_dml_binary_all_weekly_down_extradrop_robustcheck_mice1.rds")
print(paste("Number of respondents:", length(unique(data_rc_4$group))))
print(paste("Number of observations:", nrow(data_rc_4)))
print(paste("Number of observations for sport participations:", nrow(data_rc_4 %>% filter(treatment_sport == 1))))
print(paste("Number of observations for NON sport participations:", nrow(data_rc_4 %>% filter(treatment_sport == 0))))
print(paste("Number of columns:", ncol(data_rc_4)))
