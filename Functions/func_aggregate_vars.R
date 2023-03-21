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


#%%%%%%%%%%%%%%%%%%#
## WRITE FUNCTION ##
#%%%%%%%%%%%%%%%%%%#

func_aggregate_vars <- function(data, varsel_prefix, cr_alpha, method) {
  
  # ungroup data; otherwise problem
  data <- data %>% ungroup()
  
  
  #++++++++++++++++++++#
  ## CRONBACH's ALPHA ##
  #++++++++++++++++++++#
  
  # https://bjoernwalther.com/cronbachs-alpha-in-r-berechnen/
  
  # generate empty vector with variables that are not aggregated because
  # according to cronbach's alpha they do not measure the same thing
  vars_not_aggr <- c()
  vars_not_aggr_lag <- c()
  
  if (cr_alpha == "yes" & method %in% c("mean", "pca")) {
    # calculate cronbach's alpha to ensure that variables really measure the
    # same thing; do so only if 2-20 items are selected, otherwise all items are kept.
    
    # variable length: decide if lags are selected or not
    colnames_sel <- data %>% 
      dplyr::select(matches(paste0("^", varsel_prefix))) %>% 
      colnames()
    
    if (any(str_ends(colnames_sel, "lag"))) {
      varsel_prefix_length <- data %>% 
        dplyr::select(matches(paste0("^", varsel_prefix, ".*[^lag]$"))) %>% 
        colnames() %>% length()
      
      varsel_prefix_length_lag <- data %>% 
        dplyr::select(matches(paste0("^", varsel_prefix, ".*lag$"))) %>% 
        colnames() %>% length()
    } else {
      varsel_prefix_length <- data %>% 
        dplyr::select(matches(paste0("^", varsel_prefix, ".*[^lag]$"))) %>% 
        colnames() %>% length()
      varsel_prefix_length_lag <- c()
    }
    
    # calculate alpha for non lagged variables
    if (varsel_prefix_length >= 2 & varsel_prefix_length <= 20) {
      
      # calculate alpha
      df_alpha <- psych::alpha(data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*[^lag]$"))), check.keys = TRUE)
      
      # extract total alpha
      alpha_total <- df_alpha$total$raw_alpha
      
      # when variables measure the same thing proceed
      if (alpha_total >= 0.7) {
        # only keep variables for which total alpha would decrease if they would be
        # excluded
        alpha_indiv <- df_alpha$alpha.drop %>% as.data.frame() %>% dplyr::select(raw_alpha)
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
      vars_keep <- data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*[^lag]$"))) %>% colnames()
    }
    
    # for lagged variables
    # SO FAR THEY ARE ALWAYS AGGREGATED IF NON-LAGGED VARIABLES ARE
    vars_keep_lag <- data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*lag$"))) %>% colnames()
    # if (!is.null(varsel_prefix_length_lag)) {
    #   if (varsel_prefix_length_lag >= 2 & varsel_prefix_length_lag <= 20) {
    #     
    #     # calculate alpha
    #     df_alpha <- psych::alpha(data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*lag$"))), check.keys = TRUE)
    #     
    #     # extract total alpha
    #     alpha_total <- df_alpha$total$raw_alpha
    #     
    #     # when variables measure the same thing proceed
    #     if (alpha_total >= 0.7) {
    #       # only keep variables for which total alpha would decrease if they would be
    #       # excluded
    #       alpha_indiv <- df_alpha$alpha.drop %>% as.data.frame() %>% dplyr::select(raw_alpha)
    #       alpha_indiv$vars_keep <- rownames(alpha_indiv)
    #       vars_keep_lag <- alpha_indiv[alpha_indiv < alpha_total, "vars_keep"]
    #       vars_keep_lag <- str_remove_all(vars_keep_lag, "-")
    #     } else {
    #       # ... otherwise do not aggregate the variables
    #       vars_keep_lag <- c()
    #     }
    #     
    #   } else {
    #     # if less than two or more than 20 variables are kept, all variables are kept and
    #     # no selected based on cronbach's alpha is made
    #     vars_keep_lag <- data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*lag$"))) %>% colnames()
    #   }
    # } # close is.null(varsel_prefix_length_lag) (if lags are included)
    
    # if cronbach's alpha should not be calculated
  } else {
    vars_keep <- data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*[^lag]$"))) %>% colnames()
    if (is_empty(vars_keep)) {
      vars_keep <- data %>% dplyr::select(matches(paste0("^", varsel_prefix))) %>% colnames()
    } else {
      vars_keep <- vars_keep
    }
    
    # lagged variables
    vars_keep_lag <- data %>% dplyr::select(matches(paste0("^", varsel_prefix, ".*lag$"))) %>% colnames()
    if (is_empty(vars_keep_lag)) {
      vars_keep_lag <- c()
    } else {
      vars_keep_lag <- vars_keep_lag
    }
  }

  # decide if aggregation is performed or not
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
      column_names_drop <- paste0(varsel_prefix, "_", ".*[^lag]$")
    }
    
    # same for lag
    if (!is_empty(vars_keep_lag)) {
      if (str_detect(varsel_prefix, "\\.\\*")) {
        new_column_name_lag <- varsel_prefix %>% str_remove("\\.\\*") %>% str_replace("__", "_")
      } else if (str_detect(varsel_prefix, "_\\[\\^")) {
        new_column_name_lag <- str_split(varsel_prefix, "_\\[", simplify = TRUE)[,1]
      } else {
        new_column_name_lag <- varsel_prefix
      }
      new_column_name_lag <- paste0(new_column_name_lag, "_lag")

      if (str_detect(varsel_prefix, "\\.\\*")) {
        column_names_drop_lag <- varsel_prefix
      } else if (str_detect(varsel_prefix, "_\\[\\^")) {
        column_names_drop_lag <- varsel_prefix
      } else {
        column_names_drop_lag <- paste0(varsel_prefix, "_.*lag$")
      }
    } else {
      new_column_name_lag <- c()
      column_names_drop_lag <- c()
    }

    
    
    #+++++++++++++++++++++++#
    ## AGRREGATION METHODS ##
    #+++++++++++++++++++++++#
    
    
    ## MEAN ##
    #--------#
    
    if (method == "mean") {
      
      # "normal" variables
      data_final <- data %>% 
        mutate(
          {{new_column_name}} := round(
            rowMeans(dplyr::select(data, all_of(vars_keep)), na.rm = TRUE))
        ) %>%
        dplyr::select(-matches(column_names_drop))
      
      # "lagged" variables (if they exist)
      if (!is.null(new_column_name_lag)) {
        column_names_drop_lag <- data_final %>% 
          dplyr::select(matches(column_names_drop_lag)) %>% colnames()
        
        data_final <- data_final %>% 
          mutate(
            {{new_column_name_lag}} := round(
              rowMeans(dplyr::select(data_final, all_of(vars_keep_lag)), na.rm = TRUE))
          ) %>%
          dplyr::select(-all_of(column_names_drop_lag))
      }

      
      
    ## SUM ##
    #-------#
      
    } else if (method == "sum") {
      
      data_final <- data %>% 
        mutate(
          {{new_column_name}} := round(
            rowSums(dplyr::select(data, all_of(vars_keep)), na.rm = TRUE))
        ) %>%
        dplyr::select(-matches(column_names_drop))
      
      # "lagged" variables (if they exist)
      if (!is.null(new_column_name_lag)) {
        data_final <- data_final %>% 
          mutate(
            {{new_column_name_lag}} := round(
              rowSums(dplyr::select(data_final, all_of(vars_keep_lag)), na.rm = TRUE))
          ) %>%
          dplyr::select(-matches(column_names_drop_lag))
      }
      
      
    ## PCA ##
    #-------#
      
    # https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
    } else if (method == "pca") {
      # PCA can only be performed if at least two variables are kept
      if (length(vars_keep) > 1) {
        # select variables for PCA
        data_pca <- data %>%
          dplyr::select(all_of(vars_keep)) 
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
            dplyr::select(-matches(column_names_drop))
        } else {
          # for more than two components, variables are enumerated
          data_final <- data %>% dplyr::select(-matches(column_names_drop))
          for (comp_sel in 1:pca_keep) {
            new_column_name_sel <- paste0(new_column_name, "_", comp_sel)
            data_final <- data_final %>%
              mutate({{new_column_name_sel}} := pca_result$x[, comp_sel]) 
          }
        }
        } else {
          # if only one variable is kept, no PCA is performed
          # -> variable is left as it is 
          # -> variable is labelled later (hence, stored in vars_not_aggr)
          data_final <- data %>%
            mutate({{new_column_name}} := !!sym(vars_keep)) %>% 
            dplyr::select(-matches(column_names_drop))
          
          vars_not_aggr <- new_column_name
        }
        
        
        # PCA for lagged variable
        if (!is.null(new_column_name_lag)) {
          # identify columns to drop
          column_names_drop_lag <- data_final %>% 
            dplyr::select(matches(column_names_drop_lag)) %>% colnames()
          
          if (length(vars_keep_lag) > 1) {
            # select variables for PCA
            data_pca <- data_final %>%
              dplyr::select(all_of(vars_keep_lag)) 
            # calculate PCA
            pca_result <- prcomp(data_pca, center = TRUE, scale. = TRUE)
            # calculate eigenvalues
            pca_eigenvalues <- pca_result$sdev^2
            # keep components with eigenvalues larger than 1
            pca_keep <- sum(pca_eigenvalues > 1)
            # keep variables
            if (pca_keep == 1) {
              data_final <- data_final %>%
                mutate({{new_column_name_lag}} := pca_result$x[, 1]) %>% 
                dplyr::select(-matches(column_names_drop_lag))
            } else {
              # for more than two components, variables are enumerated
              data_final <- data_final %>% dplyr::select(-all_of(column_names_drop_lag))
              for (comp_sel in 1:pca_keep) {
                new_column_name_sel <- 
                  paste0(str_remove(new_column_name_lag, "_lag"), "_", comp_sel, "_lag")
                data_final <- data_final %>%
                  mutate({{new_column_name_sel}} := pca_result$x[, comp_sel]) 
              }
              
            }
        } else {
        # if only one variable is kept, no PCA is performed
        # -> variable is left as it is 
        # -> variable is labelled later (hence, stored in vars_not_aggr)
          column_names_drop_lag <- data_final %>% 
            dplyr::select(matches(column_names_drop_lag)) %>% colnames()
          
          data_final <- data_final %>%
            mutate({{new_column_name_lag}} := !!sym(vars_keep_lag)) %>% 
            dplyr::select(-matches(column_names_drop_lag))
          
          vars_not_aggr_lag <- new_column_name_lag
      }}
      
      
      
    ## BINARY ##
    #----------#
      
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
        dplyr::select(-matches(column_names_drop))
    }
    
    # replace all NaN with NA
    #data_final <- data_final %>% mutate(across(matches(paste0("^", varsel_prefix)), list(~ifelse(is.nan(.), NA, .))))
    
  }
  
  colnames_exist <- data_final %>% dplyr::select(starts_with(varsel_prefix)) %>% colnames()
  
  if (all(str_detect(colnames_exist, "[0-9]")) == FALSE) {
    colnames_keep <- c(varsel_prefix, paste0(varsel_prefix, "_lag"))
    colnames_drop <- colnames_exist[!colnames_exist %in% colnames_keep]
    data_final <- data_final %>% dplyr::select(-all_of(colnames_drop))
  } else {
    data_final <- data_final
  }


  # return data
  return(list(data_final, vars_not_aggr, vars_not_aggr_lag))
}
  