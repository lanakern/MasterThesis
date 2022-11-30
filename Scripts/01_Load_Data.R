#%%%%%%%%%%%%%%%%%%%%%%%%#
#### LOAD DATA FILES ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# In this file, all data files are loaded which are needed for the
# upcoming analysis. Moreover, minor adjustments such as renaming variables,
# replacing missing values, recoding "(not) specified" variables etc., are made.
#++++
# The data sets used in the upcoming analysis are:
# - Biography (SC5_Biography_D_16-0-0.dta)
# - Cohort Profile (SC5_CohortProfile_D_16-0-0.dta)
# - CATI (SC5_pTargetCATI_D_16-0-0.dta)
# - CAWI (SC5_pTargetCAWI_D_16-0-0.dta)
# - School: Includes the schooling history of each respondent (SC5_spSchool_D_16-0-0.dta)
# - Education: Highest degree (CASMIN & ISCED) is taken (SC5_Education_D_16-0-0.dta)
# - Vocational Training (higher education) (SC5_spVocTrain_D_16-0-0.dta)
# - Internship (SC5_spInternship_D_16-0-0.dta)
# - Military (SC5_spMilitary_D_16-0-0.dta)
# - Gap (SC5_spGap_D_16-0-0.dta)
# - Employment (SC5_spEmp_D_16-0-0.dta)
# - Sibling (SC5_spSibling_D_16-0-0.dta)
# - Partner (SC5_spPartner_D_16-0-0.dta)
# - Child (SC5_spChild_D_16-0-0.dta)
# - Competencies (SC5_xTargetCompetencies_D_16-0-0.dta)
#++++
# -> ATTENTION: THIS FILE RUNS AROUND 2 HOURS!
# -> This is because of the variable values labeling procedure!


#%%%%%%%%%#
## SETUP ##
#%%%%%%%%%#


# clear workspace
rm(list = ls())

# install packages if needed, load packages
# if (!require("haven")) install.packages("haven")
# library(haven)  # to import stata (.dta) file into R

if (!require("readstata13")) install.packages("readstata13")
library(readstata13)  # to import stata (.dta) file into R (see data manual why this function is used)

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)  # to manipulate data

if (!require("zoo")) install.packages("zoo")
library(zoo)  # to transform time data

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)  # to create a date variable

if (!require("naniar")) install.packages("naniar")
library(naniar)  # to work with missing values



#%%%%%%%%%%%%%#
## LOAD DATA ##
#%%%%%%%%%%%%%#


#### Biography ####
#+++++++++++++++++#

# -> should be used as a starting point for each life course analysis

# load data 
data_bio <- read.dta13("Data/Raw/SC5_Biography_D_16-0-0.dta", 
                       convert.factors = FALSE)

# add value labels
vars_label_bio <- c("wave", "sptype", "spms", "splast")
for (var_sel in vars_label_bio) {
  data_bio[, var_sel] <- 
    set.label(data_bio, var_sel, lang = "en")
}



#### CohortProfile ####
#+++++++++++++++++++++#

# -> According to the data manual, CohortProfile, is the file which should be
# used as a starting point for the analysis

# load data 
data_cohort_profile <- 
  read.dta13("Data/Raw/SC5_CohortProfile_D_16-0-0.dta",
             convert.factors = FALSE)

# add labels for wave and participation variable variable
data_cohort_profile[, "wave"] <- 
  set.label(data_cohort_profile, "wave", lang = "en")
data_cohort_profile[, "tx80220"] <- 
  set.label(data_cohort_profile, "tx80220", lang = "en")

# sort data according to ID 
data_cohort_profile <- data_cohort_profile %>%
  arrange(ID_t)

# some insights
  ## waves
unique(data_cohort_profile$wave)
  ## check participation status by wave
cbind(addmargins(table(data_cohort_profile$wave, data_cohort_profile$tx80220)))
  # -> not in every wave, every respondent takes part
  # hence, I drop all rows where a respondent does not take part because
  # for this wave no information is available
data_cohort_profile <- data_cohort_profile %>%
  filter(tx80220 == "Participation")

# rename variables and keep only variables of interest
data_cohort_profile <- data_cohort_profile %>%
  rename(interview_day = tx8600d,
         interview_month = tx8600m,
         interview_year = tx8600y,
         competence_month = tx8610m,
         competence_year = tx8610y,
         participation = tx80220,
         data_available_student = tx80521,
         competence_available_student = tx80522,
         competence_available_plausible_student = tx80531, 
         data_available_institution = tx80524,
         first_participation = tx80107
         ) %>%
  select(ID_t, wave, starts_with("interview"), starts_with("competence"),
         participation, data_available_student, data_available_institution, 
         first_participation)


  
#### pTargetCATI ####
#++++++++++++++++++#

# -> less lines than in CohortProfile because there are only lines for persons
# who responded
# includes big five personality traits, frequency of sport in leisure time 
# (NO university sports)

# load data 
data_target_cati <- read.dta13("Data/Raw/SC5_pTargetCATI_D_16-0-0.dta",
                               convert.factors = FALSE)


# add value labels
  ## define variables which should be labelled; some such as ID_t and numeric
  ## variables (especially those which are aggregated later) are dropped. 
vars_label_cati <- data_target_cati %>%
  select(-c(ID_t, t67809a, t67809b, t67809e, t67809f, t67809h, t67810a, t67810b,
            t67810d, t67810e, t67810h, t66003a, t66003b, t66003c, t66003d, t66003e,
            t66003f, t66003g, t66003h, t66003i, t66003j, tg15001,
            tg15003, tg15002, tg15004, t31300a, t31300b, t31300c, t31300d,
            t31300e, t31300f, t31300g, t31300h, t31300i, t31300j, t31300k,
            t31300l, t31300m, t31300n, t31300o, t66406a, t66407a, t66408a,
            t66406b, t66407b, t66408b, t66406c, t66407c, t66408c, t66406d,
            t66408d, t66407d, t66405a, t66405b, t66405c, t66405d, tg2411a, tg2411b,
            tg2411c, tg2411d, tg2411e, tg2411f, tg2411g, tg2411h, tg2411i,
            t514008, t514001, t514002, t514003, t514004, t514005, t514009,
            t66201a, t66201b, t66201c, t66201d, t66208a, t66208b, t66208c,
            t66208d, t34001h, t34001j, t516005, t516009, t516106, 
            t516105, t516300, t34010f, t515052, t515053, t515054, t515051,
            t724403, t70000y, t70000m, t741001, t510010, t731301_g3, t731351_g3,
            t531260, t531261, tg09002, t525015, t521050, t521051, t521052,
            t520002, t520003, t34001h, t34001j, t34009j, t34009h,
            t525008_v1, t525008, t34009i, t34009k
            )
         ) %>% colnames()
for (var_sel in vars_label_cati) {
  data_target_cati[, var_sel] <- 
    set.label(data_target_cati, var_sel, lang = "en")
}


# rename variables
data_target_cati <- data_target_cati %>%
  rename(
    # treatment: sport frequency (independent of university sport)
    sport_leisure_freq = t527102,
    # outcome: final grade
    grade_final = t724403,
    # outcome: big five personality traits
    bigfive_extraversion = t66800a, 
    bigfive_agreeableness = t66800b, 
    bigfive_conscientiousness = t66800c, 
    bigfive_neuroticism = t66800d, 
    bigfive_openness = t66800e, 
    # other personality traits
    personality_sociable = t66800f,
    personality_criticize = t66800g,
    personality_thorough = t66800h,
    personality_nervous = t66800i,
    personality_imaginative = t66800j,
    personality_sensitive = t66800k,
    personality_patience = t515100,
    personality_assertiveness_1 = t67809a, 
    personality_assertiveness_2 = t67809b,
    personality_assertiveness_3 = t67809e, 
    personality_assertiveness_4 = t67809f, 
    personality_assertiveness_5 = t67809h, 
    personality_conflicts_1 = t67810a, 
    personality_conflicts_2 = t67810b, 
    personality_conflicts_3 = t67810d, 
    personality_conflicts_4 = t67810e, 
    personality_conflicts_5 = t67810h, 
    personality_selfesteem_1 = t66003a, 
    personality_selfesteem_2 = t66003b, 
    personality_selfesteem_3 = t66003c, 
    personality_selfesteem_4 = t66003d, 
    personality_selfesteem_5 = t66003e, 
    personality_selfesteem_6 = t66003f, 
    personality_selfesteem_7 = t66003g, 
    personality_selfesteem_8 = t66003h, 
    personality_selfesteem_9 = t66003i, 
    personality_selfesteem_10 = t66003j,
    # sociodemographic variables
    birth_year = t70000y, 
    birth_month = t70000m, 
    gender = t700001,
    birth_country = t405010_g2,
    birth_ger_eastwest = t700101_g1, 
    #birth_state = t700101_g2R, 
    #birth_ger_region = t700101_g3R, 
    #birth_ger_district = t700101_g4R,
    birth_nationality_ger = t406060, 
    #nationality_other = t406100_g2,
    degree_uentrance_ger = tf11105, 
    #mother_tongue_first = t414000_g1R, 
    #mother_tongue_second = t414002_g1R, 
    bilingual = t414050,
    current_residence_country = t751004_g2, 
    current_residence_eastwest = t751001_g1, 
    current_residence_state = t751001_g2R, 
    current_family_status = t733001, 
    kindergarden = t712001,
    childhood_parents = t731101, 
    # education
    educ_school_rep_grade = t725000, 
    educ_uni_type = tg01003_g1, 
    educ_uni_major = tg04001_g7, 
    educ_uni_degree_aspire = t31040a, 
    educ_uni_degree_achieve = t31140a, 
    educ_uni_degree_teaching = tg24201_g1, 
    #educ_uni_loc_eastwest = tg15207_g1R, 
    #educ_uni_loc_state = tg15207_g2R, 
    educ_profession_aspired = t31060b_g9, 
    educ_profession_expected = t31160c_g9, 
    # source of finance and financial situation
    finance_family = t531250, 
    finance_bafoeg_1 = t531251, 
    finance_bafoeg_2 = t531243, 
    finance_loan = t531252, 
    finance_earnings = t531253, 
    finance_payment = t531254, 
    finance_own = t531255, 
    finance_scholarship = t531256, 
    finance_benefits = t531257, 
    finance_other = t531258,
    finance_money_amount = t531260, 
    finance_money_amount_indirect = t531261, 
    finance_sufficient = t531262, 
    finance_tuition_fee = t531210,
    # satisfaction with study (will probably be aggregated in one variable later)
    satisfaction_study_1 = tg2411a, 
    satisfaction_study_2 = tg2411b, 
    satisfaction_study_3 = tg2411c, 
    satisfaction_study_4 = tg2411d, 
    satisfaction_study_5 = tg2411e, 
    satisfaction_study_6 = tg2411f, 
    satisfaction_study_7 = tg2411g, 
    satisfaction_study_8 = tg2411h, 
    satisfaction_study_9 = tg2411i, 
    satisfaction_study_10 = t514008,
    # satisfaction with life (will probably be aggregated in one variable later)
    satisfaction_life_1 = t514001, 
    satisfaction_life_2 = t514002,
    satisfaction_life_3 = t514003,
    satisfaction_life_4 = t514004,
    satisfaction_life_5 = t514005,
    satisfaction_life_6 = t514009,
    # variables measuring the health status of a student
    health_disability = t524200, 
    health_disability_degree = t524205, 
    health_allergic = t524100, 
    health_neuro = t524101,
    health_smoking_v1 = t525008_v1, 
    health_smoking = t525008, 
    health_smoking_number = t525015, 
    health_alcohol_v1 = t525209_v1,
    health_alcohol = t525209,
    health_general = t521000, 
    health_physical = t521050, 
    health_mental = t521051, 
    health_dailyact = t521052, 
    health_height = t520002, 
    health_weight = t520003,
    # parents information
    mother_country_ger = t405060_g1, 
    mother_country_all = t405070_g2, 
    mother_living_ger = t405080_g1, 
    mother_language_first = t414100_g2, 
    mother_educ_school_degree = t731301, 
    mother_educ_school_degree_isced = t731301_g1, 
    mother_educ_school_degree_casmin = t731301_g2,
    mother_educ_years = t731301_g3, 
    mother_educ_degree = t731303, 
    mother_educ_degree_inst = t731308, 
    mother_educ_doctorate = t731310,
    mother_emp_15y = t731401, 
    mother_emp_bef_15y = t731402, 
    mother_emp_prof_isei = t731403_g5,
    mother_emp_prof_egp = t731403_g8,
    mother_emp_prof_blk = t731403_g9,
    mother_emp_prof_pos = t731404, 
    mother_emp_number = t731406_D, 
    mother_emp_manager = t731407, 
    mother_language_target = t412110,
    father_country_ger = t405090_g1, 
    father_country_all = t405100_g2, 
    father_living_ger = t405110_g1, 
    father_language_first = t414120_g2, 
    father_educ_school_degree = t731351, 
    father_educ_school_degree_isced = t731351_g1, 
    father_educ_school_degree_casmin = t731351_g2, 
    father_educ_years = t731351_g3, 
    father_educ_degree = t731353, 
    father_educ_degree_inst = t731358, 
    father_educ_doctorate = t731360, 
    father_emp_15y = t731451, 
    father_emp_bef_15y = t731452, 
    father_emp_prof_isei = t731453_g5, 
    father_emp_prof_egp = t731453_g8, 
    father_emp_prof_blk = t731453_g9, 
    father_emp_prof_pos = t731454, 
    father_emp_number = t731456_D, 
    father_emp_manager = t731457, 
    father_language_target = t412120,
    # parents and friends opinion about studying and degree (will probably be aggregated in one variable later)
    parents_opinion_degree_1 = tg15001, 
    parents_opinion_degree_2 = tg15003, 
    friends_opinion_degree_4 = tg15002, 
    friends_opinion_degree_5 = tg15004,
    # status maintenance
    status_prof_mother = t30560a, 
    status_prof_father = t30560b,
    # motivation for studying degree (will probably be aggregated in one variable later)
    motivation_degree_1 = t66406a, 
    motivation_degree_2 = t66407a, 
    motivation_degree_3 = t66408a, 
    motivation_degree_4 = t66406b, 
    motivation_degree_5 = t66407b, 
    motivation_degree_6 = t66408b, 
    motivation_degree_7 = t66406c, 
    motivation_degree_8 = t66407c, 
    motivation_degree_9 = t66408c, 
    motivation_degree_10 = t66406d, 
    motivation_degree_11 = t66408d, 
    motivation_degree_12 = t66407d, 
    motivation_degree_13 = t66405a,
    motivation_degree_14 = t66405b, 
    motivation_degree_15 = t66405c, 
    motivation_degree_16 = t66405d,
    # interests: mathematics, german (will probably be aggregated in one variable later)
    interest_math_1 = t66201a, 
    interest_math_2 = t66201b, 
    interest_math_3 = t66201c,
    interest_math_4 = t66201d, 
    interest_german_1 = t66208a, 
    interest_german_2 = t66208b,
    interest_german_3 = t66208c,
    interest_german_4 = t66208d,
    # interest reading, art, and music
    interest_reading_study = t34001h, 
    interest_reading_leisure = t34001j, 
    interest_reading_num_books = t34005a, 
    interest_reading_classic = t34006d, 
    interest_reading_poem = t34006e,
    interest_art_works = t34006f, 
    interest_art_musuem = t34009a, 
    interest_music_play = t34009i, 
    interest_music_play_freq = t34009j, 
    interest_music_classic = t34009k, 
    interest_music_classic_freq = t34009h,
    # interest in politics
    interest_politics_signatures = t516005, 
    interest_politics_demo = t516009, 
    interest_politics_understanding = t516106, 
    interest_politics_general = t516105,
    interest_politics_vote = t516300, 
    interest_politics_discussion = t34010f, 
    # university specific questions
    uni_offers_people = tg08001,
    uni_offers_people_partic = tg08002, 
    uni_offers_people_helpful = tg08003, 
    uni_offers_orga = tg08004, 
    uni_offers_orga_partic = tg08005, 
    uni_offers_orga_helpful = tg08006, 
    uni_offers_central_facilities = tg08007, 
    uni_offers_central_facilities_partic = tg08008, 
    uni_offers_central_facilities_helpful = tg08009, 
    uni_offers_course = tg08010, 
    uni_offers_course_partic = tg08011, 
    uni_offers_course_helpful = tg08012, 
    uni_offers_skills = tg08013, 
    uni_offers_skills_partic = tg08014, 
    uni_offers_skills_helpful = tg08015, 
    uni_offers_no = tg08016, 
    uni_nc_waiting = tg09001, 
    uni_nc_waiting_time = tg09002, 
    uni_admission_restr = tg10001, 
    uni_admission_restr_grades = tg11001, 
    uni_admission_restr_test = tg11002, 
    uni_admission_restr_other = tg11003, 
    uni_degree_choice = tg12003, 
    uni_alternative_study = tg13001, 
    uni_institution_choice = tg14001,
    # other university questions
    uni_prob_graduation = t300400, 
    uni_chances_job = t30240a, 
    uni_pay_difficult = t30340a, 
    uni_pay_pressure = t30440a,
    # willingness to take risks
    risk_finance = t515052,
    risk_leisure = t515053, 
    risk_career = t515054, 
    risk_general = t515051, 
    # current living situation
    living_hh_size = t741001,
    living_hh_income = t510010, 
    # generalized opinion education
    opinion_educ_1 = t31300a, 
    opinion_educ_2 = t31300b, 
    opinion_educ_3 = t31300c, 
    opinion_educ_4 = t31300d, 
    opinion_educ_5 = t31300e, 
    opinion_educ_6 = t31300f, 
    opinion_educ_7 = t31300g, 
    opinion_educ_8 = t31300h, 
    opinion_educ_9 = t31300i, 
    opinion_educ_10 = t31300j, 
    opinion_educ_11 = t31300k, 
    opinion_educ_12 = t31300l, 
    opinion_educ_13 = t31300m, 
    opinion_educ_14 = t31300n, 
    opinion_educ_15 = t31300o
  )


# drop all variables not needed for the upcoming analysis
# this is all variables which are not renamed, i.e., starting with t#, ts, tf, th, tx, and tg
# and the ID_i variable
data_target_cati <- data_target_cati %>%
  select(-c(starts_with("tg"), starts_with("ts"), starts_with("tf"), 
            starts_with("th"), starts_with("tx"), starts_with("Version"))) %>%
  select(-(matches("^t[0-9].*"))) %>%
  select(-c(ID_i))





#### pTargetCAWI ####
#++++++++++++++++++#

# -> less lines than in CohortProfile because there are only lines for persons
# who responded
# includes big five personality traits, sport frequency (independent of university sports)

# load data 
data_target_cawi <- read.dta13("Data/Raw/SC5_pTargetCAWI_D_16-0-0.dta",
                               convert.factors = FALSE)

# add factor labels
vars_label_cawi <- data_target_cawi %>%
  # drop variables which will make trouble such as ID_t and grades (tg52020)
  # also variables which will be aggregated should not take on any label
  select(-c(ID_t, tg52020, t67001a, t67001b, t67001c, t67001d, t67001e,
            t67000a, t67000b, t67000c, t67000d, t67000e, t320410, t320406, t320405,
            t320106, t320105, t320110, t241001, t241002, t241003,
            t241004, t241005, t241006, t241007, t241008, t241009, t241000, t242001,
            t242002, t242003, t242004, t242005, t242006, t242007, t242008, t242009,
            t242000, t242011, t242012, t242013, t242014, t242015, t242016, t242017,
            t242018, t242019, t242010, t242020, t242021, t242022, t242023, t242024,
            t242025, t242026, t242027, t242028, t242033, tg54112, tg54113, tg54211, 
            tg54212, tg53231, tg53232, tg53233, tg53234, tg53235, tg53236, tg52042, 
            tg52043, tg53221, tg53223, tg53224, tg53225, t291541, t291542, t291543,
            t291544, t291545, t291546, t291525, t291526, t291527, t291528, t66007a,
            t66007d, t66007b, t66007e, t66010d, t66010b, t66010a, tg53111, tg53112,
            tg53113, tg53114, tg53121, tg53122, tg53123, t527003, t527004, t527010,
            t527017, t527019, t527021, t527022, t527028, t527029, t527032, t527034,
            t30300b, tg52010, tg52011, tg52012, tg52013, tg52014, t261501,
            t261503, t66007a_g1, t66010a_g1, t241011, t241013, t241014, t241015,
            t241016, t241021, t242100, t242101, t242102, t242103, t242104,
            t242105, t242106, t242108, t242109, t261800, t261801, t261802,
            t261803, t261804, t261805, t261806, t261807, t261808, t261809,
            t261810, t261811, t261812, t261813,
            t242110, t242111, t242112, t242113, t242114, t242115,
            t242116, t242118, t242119, t261840, t261841, t261842, t261843,
            t261844, t261845, t261846, t261847, t261848, t261849, t261850, t261851,
            t261852, t261853
  )) %>%
  colnames()

for (var_sel in vars_label_cawi) {
  data_target_cawi[, var_sel] <- 
    set.label(data_target_cawi, var_sel, lang = "en")
}


# rename variables
data_target_cawi <- data_target_cawi %>%
  rename(
    # outcome: current average grade
    grade_current = tg52020, 
    # treatment: university sport
    sport_uni = t242107,
    sport_uni_freq = t242117, 
    # extracurricular activities inside the university (except sport)
    extracurricular_association = t242100, 
    extracurricular_representation = t242101, 
    extracurricular_committee = t242102, 
    extracurricular_political = t242103, 
    extracurricular_action = t242104, 
    extracurricular_fraternal = t242105, 
    extracurricular_religion = t242106, 
    extracurricular_culture = t242108, 
    extracurricular_other = t24210t_O,
    extracurricular_association_freq = t242110, 
    extracurricular_representation_freq = t242111, 
    extracurricular_committee_freq = t242112, 
    extracurricular_political_freq = t242113, 
    extracurricular_action_freq = t242114, 
    extracurricular_fraternal_freq = t242115, 
    extracurricular_religion_freq = t242116, 
    extracurricular_culture_freq = t242118, 
    extracurricular_other_freq = t242119, 
    # extracurricular activities outside the university
    extracurricular_leisure_1 = t261800, 
    extracurricular_leisure_2 = t261801, 
    extracurricular_leisure_3 = t261802, 
    extracurricular_leisure_4 = t261803, 
    extracurricular_leisure_5 = t261804, 
    extracurricular_leisure_6 = t261805, 
    extracurricular_leisure_7 = t261806, 
    extracurricular_leisure_8 = t261807, 
    extracurricular_leisure_9 = t261808, 
    extracurricular_leisure_10 = t261809, 
    extracurricular_leisure_11 = t261810, 
    extracurricular_leisure_12 = t261811, 
    extracurricular_leisure_13 = t261812, 
    extracurricular_leisure_14 = t261813,
    extracurricular_leisure_1_freq = t261840, 
    extracurricular_leisure_2_freq = t261841, 
    extracurricular_leisure_3_freq = t261842, 
    extracurricular_leisure_4_freq = t261843, 
    extracurricular_leisure_5_freq = t261844, 
    extracurricular_leisure_6_freq = t261845, 
    extracurricular_leisure_7_freq = t261846, 
    extracurricular_leisure_8_freq = t261847, 
    extracurricular_leisure_9_freq = t261848, 
    extracurricular_leisure_10_freq = t261849,
    extracurricular_leisure_11_freq = t261850, 
    extracurricular_leisure_12_freq = t261851,
    extracurricular_leisure_13_freq = t261852,
    extracurricular_leisure_14_freq = t261853,
    # religion
    religion_degree = t435000, 
    religion_deno = t435020, 
    # education
    educ_uni_degree_status = tg51004,
    educ_uni_degree_obtained = tg51005, 
    # educ_uni_type_current = tg51003, # too many missing values
    educ_uni_master_current = tg51002, 
    # current activity
    current_emp = tg51101_v1,
    current_volontariat = tg51102_v1,
    current_ref = tg51115_v1,
    current_pracyear = tg51118_v1,
    current_intern = tg51103_v1,
    current_voctrain = tg51104_v1,
    current_study = tg51001,
    current_study_academy = tg51105,
    current_study_college = tg51106,
    current_study_coopuni = tg51107,
    # parental cultural preferences
    parents_number_books = t34005b, 
    parents_classical_literature = t34006k, 
    parents_books_poems = t34006l, 
    parents_art = t34006m,
    # parents opinion studying
    parents_importance_success_1 = t320410, 
    parents_importance_success_2 = t320406, 
    parents_importance_success_3 = t320405, 
    parents_degree_wish = t320407,
    # friends opinion studying
    friends_study_share = t321104, 
    friends_opinion_degree_1 = t320106, 
    friends_opinion_degree_2 = t320105, 
    friends_opinion_degree_3 = t320110, 
    # counseling services at university
    uni_counsel_admission_offer = t241001, 
    uni_counsel_finance_offer = t241002, 
    uni_counsel_orga_offer = t241003, 
    uni_counsel_accommo_offer = t241004, 
    uni_counsel_degree_offer = t241005, 
    uni_counsel_parent_offer = t241006, 
    uni_counsel_law_offer = t241007, 
    uni_counsel_abroad_offer = t241008, 
    uni_counsel_psycho_offer = t241009,
    uni_counsel_other_offer = t241000, 
    uni_counsel_admission_use = t242001, 
    uni_counsel_finance_use = t242002, 
    uni_counsel_orga_use = t242003, 
    uni_counsel_accommo_use = t242004, 
    uni_counsel_degree_use = t242005, 
    uni_counsel_parent_use = t242006, 
    uni_counsel_law_use = t242007, 
    uni_counsel_abroad_use = t242008, 
    uni_counsel_psycho_use = t242009, 
    uni_counsel_other_use = t242000, 
    uni_counsel_admission_quality = t242011, 
    uni_counsel_finance_quality = t242012, 
    uni_counsel_orga_quality = t242013, 
    uni_counsel_accommo_quality = t242014, 
    uni_counsel_degree_quality = t242015, 
    uni_counsel_parent_quality = t242016, 
    uni_counsel_law_quality = t242017, 
    uni_counsel_abroad_quality = t242018, 
    uni_counsel_psycho_quality = t242019, 
    uni_counsel_other_quality = t242010,
    # equipment quality at university
    uni_quality_literature_library = t242020, 
    uni_quality_literature_online = t242021, 
    uni_quality_library_hours = t242022, 
    uni_quality_laboratory = t242023, 
    uni_quality_it = t242024, 
    uni_quality_languages = t242025, 
    uni_quality_center = t242026, 
    uni_quality_examination = t242027, 
    uni_quality_courses = t242028, 
    uni_quality_administration = t242033, 
    # other variables concerning university
    uni_overcrowded_classes = t249400,
    uni_fees = t531007, 
    # study-related questions
    uni_courses_num = tg52030,
    uni_achievement_comp = tg52044, 
    uni_achievement_expect = tg53211, 
    uni_sucessfull_comp = tg52041, 
    uni_perf_fullfilled = tg53212, 
    uni_best_student_1 = tg52042, 
    uni_perf_satisfied = tg53213, 
    uni_best_student_2 = tg52043, 
    uni_learn_group_partic = t261500, 
    uni_learn_group_num = t261501, 
    uni_learn_group_freq = t261502, 
    uni_learn_group_helpful = t264503, 
    uni_learn_group_students_num = t261503, 
    uni_study_talent = t66007a_g1, 
    uni_no_better_grades = t66010a_g1, 
    uni_change_subject = tg51300_v1, 
    uni_change_insitution = tg51500_v1, 
    uni_cost_giving_up = tg54111, 
    uni_termination_1 = tg53221, 
    uni_termination_2 = tg53223, 
    uni_termination_3 = tg53224, 
    uni_termination_4 = tg53225, 
    uni_fear_1 = tg54112, 
    uni_fear_2 = tg54113, 
    uni_anxiety_1 = tg54211, 
    uni_anxiety_2 = tg54212, 
    uni_degree_import = tg54121, 
    uni_degree_complete_prob = t300400, 
    uni_commitment_1 = tg53231, 
    uni_commitment_2 = tg53232, 
    uni_commitment_3 = tg53233, 
    uni_commitment_4 = tg53234, 
    uni_commitment_5 = tg53235, 
    uni_commitment_6 = tg53236, 
    degree_importance_well = tg54131,
    # good preparation for studies
    uni_prep_1 = t291541, 
    uni_prep_2 = t291542, 
    uni_prep_3 = t291543, 
    uni_prep_4 = t291544, 
    uni_prep_5 = t291545, 
    uni_prep_6 = t291546, 
    uni_prep_skills_1 = t291525, 
    uni_prep_skills_2 = t291526, 
    uni_prep_skills_3 = t291527, 
    uni_prep_skills_4 = t291528,
    # time budget during semester
    uni_time_courses = t241011, 
    uni_time_studyact = t241013, 
    uni_time_employment = t241014, 
    uni_time_household = t241015, 
    uni_time_childcare = t241016, 
    uni_time_study = t241021,
    # helplessness study
    helpless_grades_bad = t66010d, 
    helpless_grades_revision = t66010b, 
    helpless_grades_improve = t66010a,
    # self-concept study
    academic_talent = t66007a, 
    academic_skills = t66007d, 
    academic_learning = t66007b, 
    academic_tasks = t66007e,
    # ects / study period
    uni_ects_degree = tg52000,
    uni_ects_total = tg52010, 
    uni_ects_current = tg52011, 
    uni_period_semesters = tg52012, 
    uni_period_trimesters = tg52013, 
    uni_period_years = tg52014, 
    # social integration
    social_integr_1 = tg53111, 
    social_integr_2 = tg53112, 
    social_integr_3 = tg53113, 
    social_integr_4 = tg53114, 
    social_integr_5 = tg53121, 
    social_integr_6 = tg53122, 
    social_integr_7 = tg53123,
    # living conditions
    living_type = t289900_v1, 
    living_alone = t289901, 
    living_roommate = t289902, 
    living_partner = t289903,
    living_rent = t30300b,
    # personality: goals
    personality_goal_pers_1 = t67001a, 
    personality_goal_pers_2 = t67001b, 
    personality_goal_pers_3 = t67001c, 
    personality_goal_pers_4 = t67001d, 
    personality_goal_pers_5 = t67001e, 
    personality_goal_flex_1 = t67000a, 
    personality_goal_flex_2 = t67000b, 
    personality_goal_flex_3 = t67000c, 
    personality_goal_flex_4 = t67000d, 
    personality_goal_flex_5 = t67000e,
    # chronic stress (will probably be aggregated in one variable later)
    stress_1 = t527003, 
    stress_2 = t527004, 
    stress_3 = t527010, 
    stress_4 = t527017, 
    stress_5 = t527019, 
    stress_6 = t527021, 
    stress_7 = t527022, 
    stress_8 = t527028, 
    stress_9 = t527029, 
    stress_10 = t527032, 
    stress_11 = t527034,
    # drugs
    drugs_noprescr = t525402, 
    drugs_prescr = t525401, 
    drugs_cannabinoids = t525403, 
    drugs_ecstasy = t525404, 
    drugs_halluc = t525405, 
    drugs_amphets = t525406, 
    drugs_cocaine = t525407, 
    drugs_other = t525408, 
    drugs_never = t525410,
    drugs_motive_relax = t525421, 
    drugs_motive_perf = t525422
  )


data_target_cawi <- data_target_cawi %>%
  select(-c(starts_with("tg"), starts_with("ts"), starts_with("tf"), 
            starts_with("th"), starts_with("tx"), starts_with("Version"))) %>%
  select(-(matches("^t[0-9].*"))) %>%
  select(-c(ID_i))



#### School ####
#++++++++++++++#

# -> general education history from school entry until the date of completion

# load data
data_school <- read.dta13("Data/Raw/SC5_spSchool_D_16-0-0.dta",
                          convert.factors = FALSE)

# add factor labels for specific variables
vars_label_educ <- 
  c("wave", "ts11205", "ts1120s_g2", "tg2232b_g1", "tg2232b_g2R","ts11209")
for (var_sel in vars_label_educ) {
  data_school[, var_sel] <- 
    set.label(data_school, var_sel, lang = "en")
}


# keep only harmonized episodes (subspell == 0)
data_school <- data_school %>%
  subset(subspell == 0)

# rename variables
data_school <- data_school %>%
  rename(
    # educ_school_start_m = ts1111m, # start and end date variables are taken from biography
    # educ_school_start_y = ts1111y, 
    # educ_school_end_m = ts1112m, 
    # educ_school_end_y = ts1112y, 
    educ_school_type = ts11205, 
    educ_school_country = ts1120s_g2, 
    educ_school_quali_eastwest = tg2232b_g1, 
    educ_school_quali_state = tg2232b_g2R, 
    educ_school_quali = ts11209, 
    educ_school_grade_final = ts11218, 
    educ_school_grade_math = t724712, 
    educ_school_grade_ger = t724714
  )


# keep only variables of interest
data_school <- data_school %>%
  select(ID_t, splink, wave, starts_with("educ_"))



#### Education ####
#+++++++++++++++++#

# -> "information on transitions in respondents’ educational careers"
# splink: link for spell merging

# load data
data_education <- read.dta13("Data/Raw/SC5_Education_D_16-0-0.dta",
                             convert.factors = FALSE)

# add factor labels
vars_label_educ <- c("tx28101", "tx28103", "tx28100")
for (var_sel in vars_label_educ) {
  data_education[, var_sel] <- 
    set.label(data_education, var_sel, lang = "en")
}



# check for duplicates
anyDuplicated(data_education[,c("ID_t","splink")])

# rename variables
data_education <- data_education %>%
  rename(#interview_month = datem,
         #interview_year = datey,
         educ_highest_degree_casmin = tx28101,
         educ_highest_degree_isced = tx28103, 
         #educ_years = tx28102,
         information_source = tx28100
  )

# only keep variables needed
data_education <- data_education %>%
  select(ID_t, splink, educ_highest_degree_casmin, educ_highest_degree_isced)



#### Vocational Preparation ####
#++++++++++++++++++++++++++++++#

# # SO FAR NOT USED
# 
# # -> comprises episodes of vocational preparation after general education, including
# # pre‐training course and  basic vocational training years
# data_voc_prep <- read.dta13("Data/Raw/SC5_spVocPrep_D_16-0-0.dta",
#                             convert.factors = FALSE)
# 
# 
# # add value labels for variables needed
# vars_label_voctrain <- colnames(data_voc_prep)[-1] # drop ID_t
# for (var_sel in vars_label_voctrain) {
#   data_voc_prep[, var_sel] <- 
#     set.label(data_voc_prep, var_sel, lang = "en")
# }


#### Vocational Training ####
#+++++++++++++++++++++++++++#

# -> covers all further trainings, vocational and/or academic, that a respondent ever
# attended, for instance tertiary education at universities, doctoral studies
# subject and degree changes over the course of studies, change of higher education institution
# -> INCLUDES START OF STUDY

# load data
data_voc_train <- read.dta13("Data/Raw/SC5_spVocTrain_D_16-0-0.dta", convert.factors = FALSE)

# add value labels for variables needed
vars_label_voctrain <- 
  c("tg24150_g2", "h_aktstu", "tg01003_ha", "ts15221_g1", "tg24170_g2", "tg24170_g5")
for (var_sel in vars_label_voctrain) {
  data_voc_train[, var_sel] <- 
    set.label(data_voc_train, var_sel, lang = "en")
}


# only keep full or harmonized episodes
data_voc_train <- data_voc_train %>%
  subset(subspell == 0)

# rename variables
data_voc_train <- data_voc_train %>%
  rename(
    # educ_uni_start_m = ts1511m, # taken from biography
    # educ_uni_start_y = ts1511y, 
    # educ_uni_end_m = ts1512m, 
    # educ_uni_end_y = ts1512y, 
    educ_uni_entrance_quali_access = tg24150_g2, 
    educ_uni_first_eps = h_aktstu, 
    educ_uni_type_inst = tg01003_ha, 
    educ_uni_quali = ts15221_g1, 
    educ_uni_degree_1 = tg24170_g2, 
    educ_uni_degree_2 = tg24170_g5
    # educ_uni_degree_blk = tg24169_g9R, 
    # educ_uni_voctrain_type = ts15201 # unispells are identified differently
  )


# keep only variables of interest
data_voc_train <- data_voc_train %>%
  select(ID_t, splink, starts_with("educ_"))



#### Vocational Training Breaks ####
#++++++++++++++++++++++++++++++++++#

# -> breaks from academic and/or vocational education

# load data
data_voc_break <- 
  read.dta13("Data/Raw/SC5_spVocBreaks_D_16-0-0.dta", convert.factors = FALSE)


# add value labels for variables needed
vars_label_vocbreak <- c("tg2419a", "tg2419b", "tg2419c")
for (var_sel in vars_label_vocbreak) {
  data_voc_break[, var_sel] <- 
    set.label(data_voc_break, var_sel, lang = "en")
}

# rename variables
# keep only variables of interest
# drop duplicates: keep only one row in case ID_t and splink are duplicated
data_voc_break <- data_voc_break %>%
  rename(
    educ_uni_break_term_off = tg2419a, 
    educ_uni_break_deregist_temp = tg2419b, 
    educ_uni_break_deregist_nform = tg2419c
  ) %>%
  select(ID_t, splink, starts_with("educ_uni")) %>%
  distinct(ID_t, splink, .keep_all = TRUE)



#### Internship ####
#++++++++++++++++++#

# -> contains information on internships during studies, including duration, context etc.

# load data 
data_internship <- read.dta13("Data/Raw/SC5_spInternship_D_16-0-0.dta",
                              convert.factors = FALSE)

# add factor labels
vars_label_intern <- c("wave", "tg36110", "tg36113")
for (var_sel in vars_label_intern) {
  data_internship[, var_sel] <- 
    set.label(data_internship, var_sel, lang = "en")
}

# only keep full or harmonized episodes
data_internship <- data_internship %>%
  subset(subspell == 0)

# rename variabkes
data_internship <- data_internship %>%
  rename(
    intern_type = tg36110, 
    intern_study_rel = tg36113
  )

# keep only variables of interest
data_internship <- data_internship %>%
  select(ID_t, splink, intern_type, intern_study_rel)



#### Military ####
#++++++++++++++++#

# -> includes episodes of military or civilian service as well as gap years taken 
# to do voluntary work in the social or environmental sector.
data_military <- read.dta13("Data/Raw/SC5_spMilitary_D_16-0-0.dta",
                            convert.factors = FALSE)

# only keep full or harmonized episodes
data_military <- data_military %>%
  subset(subspell == 0)

# i am only interested if person did military service
# hence, only ID_t, and splink are are kept as well as an indicator
data_military <- data_military %>%
  mutate(military = 1) %>%
  select(ID_t, splink, military)



#### Break ####
#+++++++++++++#


# # SO FAR NOT USED
# 
# 
# # -> covers all breaks of further trainings, vocational and/or academic, that a respondent ever attended
# data_break <- read.dta13("Data/Raw/SC5_spVocBreaks_D_16-0-0.dta",
#                          convert.factors = FALSE)
# 
# # add value labels for variables needed
# vars_label_voctrain <- colnames(data_break)[-1] # drop ID_t
# for (var_sel in vars_label_voctrain) {
#   data_break[, var_sel] <- 
#     set.label(data_break, var_sel, lang = "en")
# }



#### Gap ####
#+++++++++++#

# -> gaps in individual life courses

# load data
# rename variables
# keep only variables of interest
data_gap <- 
  read.dta13("Data/Raw/SC5_spGap_D_16-0-0.dta", convert.factors = FALSE) 

# add value labels for variables needed
data_gap[, "ts29101"] <- set.label(data_gap, "ts29101", lang = "en")

data_gap <- data_gap %>%
  rename(gap_type = ts29101) %>%
  select(ID_t, splink, gap_type)




#### Employment ####
#++++++++++++++++++#

# -> covers all spells of regular employment, including traineeships, preparatory
# service (e. g, for the teaching and legal profession), and internships (only in case that the
# target persons are not studying). Information on internships while studying is included in spInternship.

# load data
data_emp <- read.dta13("Data/Raw/SC5_spEmp_D_16-0-0.dta", convert.factors = FALSE)


# add value labels for variables needed
vars_label_emp <- c("ts23901_v1", "ts23203", "ts23256", "tg2608b", "ts23257")
for (var_sel in vars_label_emp) {
  data_emp[, var_sel] <- 
    set.label(data_emp, var_sel, lang = "en")
}

# rename variables
data_emp <- data_emp %>%
  rename(
    current_emp_2 = ts23901_v1, 
    emp_prof_pos = ts23203, 
    emp_student_job = ts23256, 
    emp_student_job_type = tg2608b, 
    emp_student_job_rel = ts23257, 
    emp_net_income = ts23410, 
    emp_act_work_hours = ts23223
  )

# keep only variables of interest
data_emp <- data_emp %>%
  select(ID_t, splink, current_emp_2, emp_prof_pos, emp_student_job, 
         emp_student_job_type, emp_student_job_rel, emp_net_income,
         emp_act_work_hours)


#### Sibling ####
#+++++++++++++++#

# -> includes all siblings of the respondent reported in wave 1.

# load data
data_sibling <- read.dta13("Data/Raw/SC5_spSibling_D_16-0-0.dta",
                           convert.factors = FALSE)

# add factor labels
vars_label_sibling <- 
  c("wave", "tg32708", "tg32709", "tg32711")
for (var_sel in vars_label_sibling) {
  data_sibling[, var_sel] <- 
    set.label(data_sibling, var_sel, lang = "en")
}

# rename variables
data_sibling <- data_sibling %>%
  rename(
    sibling_num = sibling, 
    sibling_birth_m = tg3270m, 
    sibling_birth_y = tg3270y,
    sibling_employed = tg32708, 
    sibling_activity = tg32709, 
    sibling_school_degree = tg32711
  )


# keep only variables of interest
data_sibling <- data_sibling %>% 
  select(ID_t, wave, starts_with("sibling"))


#### Partner ####
#+++++++++++++++#

# -> partnership history of the respondent.
data_partner <- read.dta13("Data/Raw/SC5_spPartner_D_16-0-0.dta",
                           convert.factors = FALSE)

# add factor labels
vars_label_partner <- 
  c("wave", "ts31203", "ts31204_g1", "ts31211", "ts31212", "ts31214",
    "t733004", "t407020_g1", "ts31223_v1", "tg28320", "tg28321", "tg28322",
    "tg28323", "t733005")
for (var_sel in vars_label_partner) {
  data_partner[, var_sel] <- 
    set.label(data_partner, var_sel, lang = "en")
}


# only keep full or harmonized episodes
data_partner <- data_partner %>%
  subset(subspell == 0)

# rename variables
data_partner <- data_partner %>%
  rename(
    partner_gender = ts31203, 
    partner_birth_year = ts3120y, 
    partner_birth_country = ts31204_g1, 
    partner_ger = ts31211, 
    partner_school_degree = ts31212, 
    partner_uni_degree = ts31214, 
    partner_educ_years = ts31212_g3,
    partner_number = partner, 
    partner_living_apart = t733004, 
    partner_living_ger = t407020_g1, 
    partner_emp = ts31223_v1, 
    partner_school = tg28320, 
    partner_voctrain = tg28321, 
    partner_civil = tg28322, 
    partner_uni = tg28323, 
    partner_freq = t733005, 
    partner_start_m = tg2811m, 
    partner_start_y = tg2811y, 
    partner_end_m = tg2804m, 
    partner_end_y = tg2804y
  )


# keep only variables of interest
data_partner <- data_partner %>%
  select(ID_t, wave, starts_with("partner_"))


#### Child ####
#+++++++++++++#

# -> contains information on all biological, foster, and adopted children of the respondent, 
# and any other child that currently lives or has ever lived together with the respondent 
# (e. g., children of former and current partners).
data_child <- read.dta13("Data/Raw/SC5_spChild_D_16-0-0.dta",
                         convert.factors = FALSE)

# add factor labels
vars_label_child <- c("wave", "ts33204_g1", "ts3333c", "ts33203", "ts33301")
for (var_sel in vars_label_child) {
  data_child[, var_sel] <- 
    set.label(data_child, var_sel, lang = "en")
}


# only keep full or harmonized episodes
data_child <- data_child %>%
  subset(subspell == 0) 

# rename variables
data_child <- data_child %>%
  rename(
    child_num = child,
    child_type = ts33204_g1, 
    child_hh = ts3333c, 
    child_birth_m = ts3320m, 
    child_birth_y = ts3320y, 
    child_gender = ts33203,
    child_school = ts33301
  )

# keep only variables of interest
data_child <- data_child %>%
  select(ID_t, wave, starts_with("child"))


#### Competencies ####
#++++++++++++++++++++#

# -> contains data from competence assessments conducted.

# load data, but keep factors unlabeled
data_competencies <- read.dta13("Data/Raw/SC5_xTargetCompetencies_D_16-0-0.dta",
                                convert.factors = FALSE)


# set labels for variables needed
vars_label_comp <- data_competencies %>% select(starts_with("wave")) %>% colnames()
for (var_sel in vars_label_comp) {
  data_competencies[, var_sel] <- 
    set.label(data_competencies, var_sel, lang = "en")
}
#get.lang(data_competencies, print = T)
#varlabel(data_competencies, lang = "en")
#get.label(data_competencies)

# keep only variables of interest and rename them
data_competencies <- data_competencies %>%
  rename(
    math_wle_w1 = mas1_sc1, 
    read_wle_w1 = res1_sc1, 
    readspeed_sum_w1 = rss1_sc3, 
    read_assess_share_w1 = mps1re_sc6, 
    math_assess_share_w1 = mps1ma_sc6, 
    percspeed_paper_sum_w5 = dgs3_sc5a_pb, 
    percspeed_comp_sum_w5 = dgs3_sc5a_cb, 
    percspeed_online_sum_w5 = dgs3_sc5a_wb, 
    reasoning_paper_sum_w5 = dgs3_sc5b_pb, 
    reasoning_comp_sum_w5 = dgs3_sc5b_cb, 
    reasoning_online_sum_w5 = dgs3_sc5b_wb, 
    ict_wle_w5 = ics3_sc1, 
    science_wle_w5 = scs3_sc1u, 
    science_assess_share_w5 = mps3sc_sc6, 
    ict_assess_share_w5 = mps3ic_sc6, 
    math_wle_w12 = mas12_sc1,
    read_wle_w12 = res12_sc1, 
    english_wle_e12 = efs12_sc1, 
    math_assess_share_w12 = mps12ma_sc6, 
    read_assess_share_w12 = mps12re_sc6, 
    english_assess_share_w12 = mps12ef_sc6
  ) %>%
  select(ID_t, starts_with("wave"), starts_with("math"), starts_with("read"),
         starts_with("reasoning"), starts_with("percspeed"), starts_with("ict"), 
         starts_with("science"), starts_with("english")) 



  
#### Recode missing values ####
#+++++++++++++++++++++++++++++#

# load generated function
source("Functions/func_replace_missings.R")

# create vector with missing values
vec_missings_char <- c("not reached", "implausible value", "refused", "don't know", 
                       "don.+t_know", "Question not asked", 
                       "various", "missing by design", "unspecific missing",
                       "survey aborted", "question erroneously not asked",
                       "does not apply", "filtered", "system", 
                       "implausible value removed", "anonymized", 
                       "not determinable", "not participated", 
                       "value from the last sub-episode")
vec_missings_num <- c(-20:-29, -52:-56, -91:-99, -99.0)

# apply function to all loaded data frames in the environment
  ## put all data frames inside a list
dfs_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
  ## apply function to this list of data frame
dfs_list_adj <- lapply(dfs_list, func_replace_missings, vec_missings_num, vec_missings_char)
  ## convert result from list back to data frames
list2env(dfs_list_adj, envir = .GlobalEnv)
  ## remove lists as they are not needed anymore
remove(dfs_list, dfs_list_adj)



#### Recode Variables ####
#++++++++++++++++++++++++#


# recode all character variables as factor
func_recode_character <- function(data) {
  data <- data %>%
    mutate_if(is.factor, as.character)
  return(data)
}
dfs_list <- Filter(function(x) is(x, "data.frame"), mget(ls())) # put all data frames in a list
dfs_list_adj_charac <- lapply(dfs_list, func_recode_character) # apply function to all data frames in list
list2env(dfs_list_adj_charac, envir = .GlobalEnv) # create data frame from list


# recode (not) specified variables
  ## load function
source("Functions/func_recode_specified.R")
  ## apply function to all loaded data frames in the environment
dfs_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs_list_adj_spec <- lapply(dfs_list, func_recode_specified)
list2env(dfs_list_adj_spec, envir = .GlobalEnv)


# recode yes-no-variables
  ## load function
source("Functions/func_recode_yesno.R")
  ## apply function to all loaded data frames in the environment
dfs_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs_list_adj_yesno <- lapply(dfs_list, func_recode_yesno)
list2env(dfs_list_adj_yesno, envir = .GlobalEnv)

# remove lists with data frames
remove(dfs_list, dfs_list_adj_charac, dfs_list_adj_spec, dfs_list_adj_yesno)


#### Save all data frames ####
#+++++++++++++++++++++++++++++#

saveRDS(data_bio, "Data/Prep_1/prep_1_biography.rds")
saveRDS(data_cohort_profile, "Data/Prep_1/prep_1_cohort_profile.rds")
saveRDS(data_target_cati, "Data/Prep_1/prep_1_target_cati.rds")
saveRDS(data_target_cawi, "Data/Prep_1/prep_1_target_cawi.rds")
saveRDS(data_school, "Data/Prep_1/prep_1_school.rds")
saveRDS(data_education, "Data/Prep_1/prep_1_educ.rds")
saveRDS(data_voc_train, "Data/Prep_1/prep_1_voctrain.rds")
saveRDS(data_voc_break, "Data/Prep_1/prep_1_vocbreak.rds")
saveRDS(data_gap, "Data/Prep_1/prep_1_gap.rds")
saveRDS(data_emp, "Data/Prep_1/prep_1_emp.rds")
saveRDS(data_internship, "Data/Prep_1/prep_1_internship.rds")
saveRDS(data_military, "Data/Prep_1/prep_1_military.rds")
saveRDS(data_sibling, "Data/Prep_1/prep_1_sibling.rds")
saveRDS(data_partner, "Data/Prep_1/prep_1_partner.rds")
saveRDS(data_child, "Data/Prep_1/prep_1_child.rds")
saveRDS(data_competencies, "Data/Prep_1/prep_1_competencies.rds")

