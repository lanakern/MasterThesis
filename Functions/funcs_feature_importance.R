#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTIONS FOR FEATURE IMPORTANCE ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This file includes all functions needed to assess feature importance for
# the four machine learning algorithms.
# The individual functions are explained in the respective sections.
#+++


#### Feature Importance Score ####
#++++++++++++++++++++++++++++++++#

# This function calculated the feature importance score
# Inputs:
# - "ml_fitted_model": fitted machine learning model
# - "num_features": number of features returned (default = 20)
# Outputs:
# - Data frame consisting of variable name and the respective importance score

func_feature_importance_score <- function(ml_fitted_model, num_features = 20) {
  # calculate feature importance score
  vi_model(ml_fitted_model %>% extract_fit_parsnip(), type = "gain") %>%
    arrange(-Importance) %>%
    head(num_features)
}


#### Feature Importance Plots ####
#++++++++++++++++++++++++++++++++#

# This function creates the feature importance plots for m(D) and g(D, X)
# Inputs:
# - "treatment_setting"; "binary" or "multi"
# - "df_score": data frame containing the feature importance score
# - "ml_algo": ml algorithm name (for plot title)
# - "save_option": saved as one plot ("all in one") or separate plots
# Output:
# - Feature importance plots. If not one plot, they are contained in a list
func_feature_importance_plot <- function(treatment_setting, df_score, ml_algo, save_option) {
  # create plots
  plot_1 <- 
    df_score %>% filter(str_detect(Pred_Type, "Treatment")) %>%
    ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
    geom_point(color = "black", size = 4) +
    scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
    labs(
      title = paste(ml_algo, "-", "Treatment Prediction"),
      y = "", x = "\n Importance Score"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_2 <- 
    df_score %>% filter(str_detect(Pred_Type, "Outcome 0")) %>%
    ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
    geom_point(color = "black", size = 4) +
    scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
    labs(
      title = paste(ml_algo, "-", "Outcome 0 Prediction"),
      y = "", x = "\n Importance Score"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot_3 <- 
    df_score %>% filter(str_detect(Pred_Type, "Outcome 1")) %>%
    ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
    geom_point(color = "black", size = 4) +
    scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
    labs(
      title = paste(ml_algo, "-", "Outcome 1 Prediction"),
      y = "", x = "\n Importance Score"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (save_option == "all_in_one") {
    grid.arrange(plot_1, plot_2, plot_3, ncol = 3)
  } else {
    list("m" = plot_1, "g0" = plot_2, "g1" = plot_3)
  }
  
}