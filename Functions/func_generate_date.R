#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION: GENERATE DATE VARIABLES FROM MONTH AND YEAR ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# by Lana Kern

# In this file a function is generated which generates a date variable
# from a given month and year. To do so, some data preparation steps are
# carried out such as replacing month names.


## WRITE FUNCTION ##
#++++++++++++++++++#

# input: 
  ## data: dataset containing day, month and year
  ## day (not necessary, if not provided it is set to 1)
  ## month (as name in German)
  ## year (as number)
  ## varname: name of the new variable

func_generate_date <- function(data, day = NULL, month, year, varname){
  # set day to 1 if it is not given
  if (is.null(day)) {
    data$day <- "1"
  } else {
    data$day <- data %>% select({{day}}) %>% pull()
  }
  
  data <- data %>%
    mutate({{month}} := recode(!!! rlang::syms(month),
                               "Jahresanfang/Winter" = "Januar",
                               "Frühjahr/Ostern" = "April",
                               "Jahresmitte/Sommer" = "Juli", 
                               "Herbst" = "Oktober", 
                               "Jahresende" = "Dezember"
                         ))

  
  # recode month names as numbers (otherwise problems)
  data <- data %>%
    mutate(
      {{month}} := recode(
        !!! rlang::syms(month),
        "Januar" = 1, "Februar" = 2, "März" = 3, "April" = 4, "Mai" = 5,
        "Juni" = 6, "Juli" = 7, "August" = 8, "September" = 9,
        "Oktober" = 10, "November" = 11, "Dezember" = 12
      ))
  
  # if month name is missing replace by March (3) -> temporary solution
  # until problem with recode of März is found; then June (6) (middle of the year)
  data <- data %>%
    mutate({{month}} := if_else(is.na(!!! rlang::syms(month)), 3, !!! rlang::syms(month)))
  
  # generate date
    ## generate column name
  #colname_date <- paste0(prefix, "_date") 
    ## create date 
    ## month and year are determined by function argument
    ## day is adjusted previousöy
  data <- data %>%
    mutate(
      {{varname}} := mdy(paste(paste( !!! rlang::syms(month),  day),
                              !!! rlang::syms(year), sep = ","))
    )
  
  return(data)
  
  
}

#data_partner <- readRDS("Data/Prep_1/prep_1_partner.rds") 
#test <- data_partner %>% select(ID_t, starts_with("partner_end"), starts_with("partner_start"))
#test  <- test %>% arrange(desc(partner_start_m)) %>% head(20)
#test$start_d <- 1

#func_generate_date(data = test, month = "partner_start_m", year = "partner_start_y", varname = "start")
#func_generate_date(data = test, day = "start_d", month = "partner_start_m", year = "partner_start_y", prefix = "start")