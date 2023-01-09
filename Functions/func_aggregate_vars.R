#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION FOR AGGREGATING MULTIPLE VARIABLES INTO ONE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# This function is used to aggregate multiple variables into few or
# even a single variable.
#++++
# INPUTS
# - data: data frame which includes variables used for aggregation. Those variables
# should be named using the same prefix, e.g. stress_1, stress_2, etc., so that 
# they can be identified via a starts_with("prefix_") expression. 
# - varsel_prefix: prefix used to identify variables which should be aggregated, e.g. "stress".
# - cr_alpha: if "yes" cronbach's alpha is calculated and variables used for 
# aggregating are selected based on a cronbach's alpha bigger than 0.7. 
# Please only select this option if the questions have the same direction and 
# same value ranges.
# - method: "pca" vs. "mean" vs. "sum" vs "binary"
#++++
# OUTPUT: data frame with aggregated variables; single variables are dropped.
#+++++

# library(stringr)
# library(dplyr)
# library(psych)


#%%%%%%%%%%%%%%%%%%#
## WRITE FUNCTION ##
#%%%%%%%%%%%%%%%%%%#

func_aggregate_vars <- function(data, varsel_prefix, cr_alpha, method) {
  
  # ungroup data; otherwise problem
  data <- data %>% ungroup()
  
  ## CRONBACH's ALPHA ##
  #++++++++++++++++++++#
  
  # https://bjoernwalther.com/cronbachs-alpha-in-r-berechnen/
  
  # generate empty vector with variables that are not aggregated because
  # according to cronbach's alpha they do not measure the same thing
  vars_not_aggr <- c()
  
  if (cr_alpha == "yes" & method %in% c("mean", "pca")) {
    # calculate cronbach's alpha to ensure that variables really measure the
    # same thing; do so only if 2-20 items are selected, otherwise all items are kept.
    varsel_prefix_length <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames() %>% length()
    
    if (varsel_prefix_length >= 2 & varsel_prefix_length <= 20) {
      
      # calculate alpha
      df_alpha <- psych::alpha(data %>% select(matches(paste0("^", varsel_prefix))), check.keys = TRUE)
      
      # extract total alpha
      alpha_total <- df_alpha$total$raw_alpha
      
      # when variables measure the same thing proceed
      if (alpha_total >= 0.7) {
        # only keep variables for which total alpha would decrease if they would be
        # excluded
        alpha_indiv <- df_alpha$alpha.drop %>% as.data.frame() %>% select(raw_alpha)
        alpha_indiv$vars_keep <- rownames(alpha_indiv)
        vars_keep <- alpha_indiv[alpha_indiv < alpha_total, "vars_keep"]
        vars_keep <- str_remove_all(vars_keep, "-")
      } else {
        # ... otherwise do not aggregate the variables
        vars_keep <- c()
      }
      
    } else {
      # if less than two or more than 20 variables are kept, all variables are kept and
      # no selected based on cronbach's alpha is made
      vars_keep <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames()
    }
  } else {
    vars_keep <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames()
  }
  
  if (length(vars_keep) == 0) {
    # do not aggregate variables
    data_final <- data 
    vars_not_aggr <- varsel_prefix
  } else {
    
    # create new name for variable
    if (str_detect(varsel_prefix, "\\.\\*")) {
      new_column_name <- varsel_prefix %>% str_remove("\\.\\*") %>% str_replace("__", "_")
    } else if (str_detect(varsel_prefix, "_\\[\\^")) {
      new_column_name <- str_split(varsel_prefix, "_\\[", simplify = TRUE)[,1]
    } else {
      new_column_name <- varsel_prefix
    }
    
    
    # create variables to drop
    if (str_detect(varsel_prefix, "\\.\\*")) {
      column_names_drop <- varsel_prefix
    } else if (str_detect(varsel_prefix, "_\\[\\^")) {
      column_names_drop <- varsel_prefix
    } else {
      column_names_drop <- paste0(varsel_prefix, "_.*$")
    }
    
    ## AGRREGATION METHOD ##
    #++++++++++++++++++++++#
    
    ## MEAN ##
    if (method == "mean") {
      data_final <- data %>% 
        mutate(
          {{new_column_name}} := round(
            rowMeans(select(data, all_of(vars_keep)), na.rm = TRUE))
        ) %>%
        select(-matches(column_names_drop))
      
    ## SUM ##
    } else if (method == "sum") {
      data_final <- data %>% 
        mutate(
          {{new_column_name}} := round(
            rowSums(select(data, all_of(vars_keep)), na.rm = TRUE))
        ) %>%
        select(-matches(column_names_drop))
      
    ## PCA ##
    # https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
    } else if (method == "pca") {
      # select variables for PCA
      data_pca <- data %>%
        select(all_of(vars_keep)) 
      # calculate PCA
      pca_result <- prcomp(data_pca, center = TRUE, scale. = TRUE)
      # calculate eigenvalues
      pca_eigenvalues <- pca_result$sdev^2
      # keep components with eigenvalues larger than 1
      pca_keep <- sum(pca_eigenvalues > 1)
      # keep variables
      if (pca_keep == 1) {
        data_final <- data %>%
          mutate({{new_column_name}} := pca_result$x[, 1]) %>% 
          select(-matches(column_names_drop))
      } else {
        # for more than two components, variables are enumerated
        data_final <- data %>% select(-matches(column_names_drop))
        for (comp_sel in 1:pca_keep) {
          new_column_name_sel <- paste0(new_column_name, "_", comp_sel)
          data_final <- data_final %>%
            mutate({{new_column_name_sel}} := pca_result$x[, comp_sel]) 
        }
        
      }
      
    ## BINARY ##
    } else if (method == "binary") {
      data_final <- data %>%
        mutate(
          {{new_column_name}} := case_when(
            # if one variable has a 1, full binary variable also has a 1
            rowSums(select(data, all_of(vars_keep)), na.rm = TRUE) > 0 ~ 1,
            # if row sum is 0 AND not all values are NA, then binary takes on 0
            rowSums(select(data, all_of(vars_keep)), na.rm = TRUE) == 0 &
              rowSums(is.na(select(data, all_of(vars_keep)))) != length(vars_keep) ~ 0,
            # in all other cases binary is NA
            TRUE ~ as.double(NA)
          )
        ) %>%
        select(-matches(column_names_drop))
    }
    
    # replace all NaN with NA
    #data_final <- data_final %>% mutate(across(matches(paste0("^", varsel_prefix)), list(~ifelse(is.nan(.), NA, .))))
    
  }
  

  # return data
  return(list(data_final, vars_not_aggr))
}
  