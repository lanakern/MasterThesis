#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Function: Summary Statistics ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++
# by Lana Kern
#++
# In this file, summary statistics (median, min, and max) are calculated for
# both the binary and multivalued treatment setting.
#++
# INPUT:
# -> "data": data frame containing treatment variable and outcome variable
# -> "treatment:" treatment variable or rather grouping variable (as string)
# -> "variable": variable for which summary statistics are calculated (as string)
#++
# OUTPUT:
# -> Data frame with summary statistics
#++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


func_summary_stats <- function(data, treatment, variable) {
  
    # combine summary statistics
    rbind(
      # summary statistics for "all" group (no discrimination in treatment and outcome)
      data %>% 
        dplyr::select(all_of(variable)) %>%
        summarize(
          median = median(!!rlang::sym(variable)),
          min = min(!!rlang::sym(variable)),
          max = max(!!rlang::sym(variable))
        ) %>%
        mutate({{treatment}} := "all") %>%
        dplyr::select(all_of(treatment), everything()),
      # summary statistics across treatment variable
      data %>% 
        dplyr::select(all_of(treatment), all_of(variable)) %>%
        group_by(!!rlang::sym(treatment)) %>%
        summarize(
          median = median(!!rlang::sym(variable)),
          min = min(!!rlang::sym(variable)),
          max = max(!!rlang::sym(variable))
        ) %>%
        dplyr::select(all_of(treatment), everything())
    ) %>% 
    # as data frame and order columns
    as.data.frame() %>%
    mutate(variable = variable) %>%
    dplyr::select(variable, all_of(treatment), everything())

}

