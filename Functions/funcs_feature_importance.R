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
  vi_model(ml_fitted_model %>% extract_fit_parsnip()) %>%
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
  
  ## BINARY TREATMENT SETTING ##
  
  if (treatment_setting == "binary") {
    
    ## Aggregated over Outcome ##
    if (length(unique(df_score$Pred_Type)) == 2) {
      # Treatment
      plot_m <- 
        df_score %>% filter(str_detect(Pred_Type, "Treatment")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5), 
              axis.text.x = element_text(size = 22), axis.title = element_text(size = 22))
      
      # Outcome 0
      plot_g <- 
        df_score %>% filter(str_detect(Pred_Type, "Outcome")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Outcome Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size = 22), axis.title = element_text(size = 22))
      
      
      if (save_option == "all_in_one") {
        grid.arrange(plot_m, plot_g, ncol = 2)
      } else {
        list("m" = plot_m, "g" = plot_g)
      }
    ## All ##
    } else {
      # Treatment
      plot_m <- 
        df_score %>% filter(str_detect(Pred_Type, "Treatment")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size = 22), axis.title = element_text(size = 22))
      
      # Outcome 0
      plot_g0 <- 
        df_score %>% filter(str_detect(Pred_Type, "Outcome 0")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Outcome 0 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size = 22), axis.title = element_text(size = 22))
      
      # Outcome 1
      plot_g1 <- 
        df_score %>% filter(str_detect(Pred_Type, "Outcome 1")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Outcome 1 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size = 22), axis.title = element_text(size = 22))
      
      if (save_option == "all_in_one") {
        grid.arrange(plot_m, plot_g0, plot_g1, ncol = 3)
      } else {
        list("m" = plot_m, "g0" = plot_g0, "g1" = plot_g1)
      }
      
    } # close else() over "Pred_Type"
    
  ## MULTIVALUED TREATMENT SETTING ##
  } else {
    
    # Post-lasso has only one plot
    if (!"Pred_Type" %in% colnames(df_score)) {
      df_score %>% 
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment 1 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else {
      # Treatment 1
      plot_m1 <- 
        df_score %>% filter(str_detect(Pred_Type, "Treatment 1")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment 1 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Treatment 2
      plot_m2 <- 
        df_score %>% filter(str_detect(Pred_Type, "Treatment 2")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment 1 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Treatment 3
      plot_m3 <- 
        df_score %>% filter(str_detect(Pred_Type, "Treatment 3")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Treatment 3 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Outcome 1
      plot_g1 <- 
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
      
      
      # Outcome 2
      plot_g2 <- 
        df_score %>% filter(str_detect(Pred_Type, "Outcome 2")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Outcome 2 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      
      # Outcome 3
      plot_g3 <- 
        df_score %>% filter(str_detect(Pred_Type, "Outcome 3")) %>%
        ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
        geom_point(color = "black", size = 4) +
        scale_x_continuous(limits = c(0, max(df_score$Importance) + 0.1)) +
        labs(
          title = paste(ml_algo, "-", "Outcome 3 Prediction"),
          y = "", x = "\n Importance Score"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (save_option == "all_in_one") {
        grid.arrange(plot_m1, plot_m2, plot_m3, plot_g1, plot_g2, plot_g3, 
                     nrow = 2, ncol = 3)
      } else {
        list("m1" = plot_m1, "m2" = plot_m2, "m3" = plot_m3, 
             "g1" = plot_g1, "g2" = plot_g2, "g3" = plot_g3)
      }
    } # close else() over "Pred_Type"
    
  } # close else() over multivalued treatment setting
  
}