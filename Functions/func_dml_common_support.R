#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION FOR CHECKING COMMON SUPPORT ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#+++
# by Lana Kern
#+++
# This function generates a plot illustrating common support and the
# trimming thresholds used.
#+++
# INPUT: 
# -> "treatment_setting": binary treatment setting ("binary") or multivalued treatment setting ("multi")
# -> "data_pred": data frame containing all nuisance parameter predictions.
# -> "min_trimming": minimal trimming threshold
# -> "max_trimming: maximal trimming threshold
# -> "ml_algo": ML algorithm used to create the plot (only used in plot title)
#+++
# OUTPUT: Plot
#+++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_dml_common_support <- function(treatment_setting, data_pred, min_trimming, max_trimming, ml_algo) {
  
  # extract trimming thresholds
  min_trimming <- min(min_trimming)
  max_trimming <- min(max_trimming)
  
  # nice ml algo name
  if (ml_algo == "xgboost") {
    ml_algo <- "XGBoost"
  } else if (ml_algo == "randomforests") {
    ml_algo <- "Random Forests"
  } else if (ml_algo == "postlasso") {
    ml_algo <- "Post-Lasso"
  } else if (ml_algo == "lasso") {
    ml_algo <- "Lasso"
  } else {
    stop("ML algorithm does not exist.")
  }
  
  ## BINARY TREATMENT SETTING ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
    
    # generate plot
    data_pred %>%
      mutate(treatment_label = ifelse(treatment == 1, "Sport Participatiopn", "No Sport Participation")) %>%
      ggplot(aes(x = m, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), binwidth = 0.01,  alpha = 0.4, color = "black") +
      scale_fill_manual(values = c("grey88", "grey38")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", 
                 color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", 
                 color = "black", size = 0.5) + 
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)), "Propensity Score Overlap")))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = "Treatment Group"))
    
  
  ## MULTIVALUED TREATMENT SETTING ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    # prepare data
    data_pred_m1 <- data_pred %>%
      dplyr::select(treatment, m1) %>%
      mutate(treatment_label = case_when(
        treatment == 1 ~ "Yes", TRUE ~ "No"
      ))
    
    data_pred_m2 <- data_pred %>%
      dplyr::select(treatment, m2) %>%
      mutate(treatment_label = case_when(
        treatment == 2 ~ "Yes", TRUE ~ "No"
      ))
    
    data_pred_m3 <- data_pred %>%
      dplyr::select(treatment, m3) %>%
      mutate(treatment_label = case_when(
        treatment == 3 ~ "Yes", TRUE ~ "No"
      ))
    
    
    # generate plots
    plot_m1 <- 
      data_pred_m1 %>%
      ggplot(aes(x = m1, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), binwidth = 0.01,  alpha = 0.4, color = "black") +
      # bar colors
      scale_fill_manual(values = c("grey88", "grey38")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", color = "black", size = 0.5) + 
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)), "Propensity Score Overlap for Prediction of Weekly Sport Participation")))) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8)) +
      guides(fill = guide_legend(title = "Treatment Group"))
    
    
    plot_m2 <- 
      data_pred_m2 %>%
      ggplot(aes(x = m2, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), binwidth = 0.01,  alpha = 0.4, color = "black") +
      # bar colors
      scale_fill_manual(values = c("grey88", "grey38")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", color = "black", size = 0.5) + 
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle(paste("Propensity Score Overlap for Prediction of Monthly Sport Participation")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8)) +
      guides(fill = guide_legend(title = "Treatment Group"))
    
    
    plot_m3 <-
      data_pred_m3 %>%
      ggplot(aes(x = m3, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), binwidth = 0.01,  alpha = 0.4, color = "black") +
      # bar colors
      scale_fill_manual(values = c("grey88", "grey38")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", color = "black", size = 0.5) + 
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle(paste("Propensity Score Overlap for Prediction of No Sport Participation")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8)) +
      guides(fill = guide_legend(title = "Treatment Group"))
    
    
    # combine plots
    grid.arrange(plot_m1, plot_m2, plot_m3, nrow = 3)
    
    
  }
  

}





