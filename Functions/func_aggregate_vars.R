#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION FOR AGGREGATING MULTIPLE VARIABLES INTO ONE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#++++
# by Lana Kern
#++++
# INPUTS
# - data: data frame which includes variables used for aggregation. Those variables
# should be named using the same prefix, e.g. stress_1, stress_2, etc., so that 
# they can be identified via a starts_with("prefix_") expression. 
# - varsel_prefix: prefix used to identify variables which should be aggregated
# - cr_alpha: if "yes" cronbach's alpha is calculated and variables used for 
# aggregating are selected based on a cronbach's alpha bigger than 0.7. 
# Please only select this option if the questions have the same direction and 
# same value ranges. If less than three or more than eight variables are 
# used for aggregation, this step is skipped even when cr_alpha = "yes". 
# https://bjoernwalther.com/cronbachs-alpha-in-r-berechnen/
# - method: "pca" vs. "mean" vs. "sum"
#++++
# OUTPUT: data frame with aggregated variables; single variables are dropped.

func_aggregate_vars <- function(data, varsel_prefix, cr_alpha, method) {
  
  # ungroup data; otherwise problem
  data <- data %>% ungroup()
  
  ## CRONBACH's ALPHA ##
  if (cr_alpha == "yes") {
    # calculate cronbach's alpha to ensure that variables really measure the
    # same thing; do so only if 3-8 items are selected
    varsel_prefix_length <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames() %>% length()
    if (varsel_prefix_length >= 3 & varsel_prefix_length <= 8) {
      library(psych)
      df_alpha <- 
        alpha(data %>% select(matches(paste0("^", varsel_prefix))), check.keys = TRUE)
      
      # if total alpha is bigger than 0.7 nothing happens
      df_drop <- df_alpha$alpha.drop
      if (df_alpha$total$raw_alpha >= 0.7) {
        vars_keep <- rownames(df_drop)
      } else {
        vars_keep <- rownames(df_drop[df_drop$raw_alpha > 0.7,])
      }
    } else {
      print("Too many items are selected. Cronbach's alpha is not computed. Please include only 3 to 8 items.")
      vars_keep <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames()
    }
  } else {
    vars_keep <- data %>% select(matches(paste0("^", varsel_prefix))) %>% colnames()
  }
  
  # create new name for variable
  library(stringr)
  new_column_name <- varsel_prefix %>% str_remove("\\.\\*") %>% str_replace("__", "_")
  
  # create variables to drop
  if (str_detect(varsel_prefix, "\\.\\*")) {
    column_names_drop <- varsel_prefix
  } else {
    column_names_drop <- paste0(varsel_prefix, "_.*$")
  }
  
  ## METHOD ##
  if (method == "mean") {
    data_final <- data %>% 
      mutate(
        {{new_column_name}} := round(
          rowMeans(select(data, all_of(vars_keep)), na.rm = TRUE))
        ) %>%
      select(-matches(column_names_drop))
  } else if (method == "sum") {
    data_final <- data %>% 
      mutate(
        {{new_column_name}} := round(
          rowSums(select(data, all_of(vars_keep)), na.rm = TRUE))
      ) %>%
      select(-matches(column_names_drop))
  } else if (method == "pca") {
    # DO THIS
    data_final <- data
  }

  # return data
  return(data_final)
}
  
  
# test <- 
#   data_controls_cawi %>% 
#   ungroup() %>%
#   select(ID_t, wave, starts_with("personality_goal_pers"), starts_with("personality_goal_flex")) %>%
#   mutate_at(data_controls_cawi %>% ungroup() %>% select(starts_with("personality_goal_pers"), starts_with("personality_goal_flex")) %>% colnames(), 
#             funs(recode(., `does not apply at all` = 1, `does rather not apply` = 2, 
#                         `does partly apply` = 3, `does rather apply` = 4,
#                         `does completely apply` = 5, .default = NaN)))
# 
# 
# func_aggregate_vars(test, "personality_goal_pers", "yes", "mean") %>% select(ID_t, wave, starts_with("personality_goal_pers"))
# func_aggregate_vars(test, "personality_goal_flex", "yes", "mean") %>% select(ID_t, wave, starts_with("personality_goal_flex"))
# func_aggregate_vars(test, "parents_importance_success", "yes", "mean") %>% select(ID_t, wave, starts_with("parents_importance_success"))
# func_aggregate_vars(test, "friends_opinion_degree", "yes", "mean") %>% select(ID_t, wave, starts_with("parents_importance_success")) # KOMMEN AUS CATI NOCH DAZU
# func_aggregate_vars(test, "uni_counsel_*_offer", "yes", "mean") %>% select(ID_t, wave, starts_with("uni_counsel_*_offer"))
# func_aggregate_vars(test, "uni_counsel_*_use", "yes", "mean") %>% select(ID_t, wave, starts_with("uni_counsel_*_use"))




# for missing personality variables average over non-missing is inserted
# if all are missing NA is kept
# test <- test %>% 
#   select(ID_t, wave, starts_with("personality_goal_pers")) %>% 
#   mutate(row_avg = round(rowMeans(
#     select(test, starts_with("personality_goal_pers")), na.rm = TRUE
#     ))) %>%
#   mutate(across(starts_with("personality_goal_pers"), ~ ifelse(is.na(.), row_avg, .)))


# Cronbach's alpha: requires at least three items, same direction of questions, i.e.,
# "does completely apply" must have same meaning across questions, same value ranges
# across items, maximum 6-8 items.
# https://bjoernwalther.com/cronbachs-alpha-in-r-berechnen/








  