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
#+++
# OUTPUT: Plot
#+++


func_dml_common_support <- function(treatment_setting, data_pred, min_trimming, max_trimming) {
  
  # extract trimming thresholds
  min_trimming <- min(min_trimming)
  max_trimming <- min(max_trimming)
  
  ## BINARY TREATMENT SETTING ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  if (treatment_setting == "binary") {
    
    # generate plot
    data_pred %>%
      mutate(treatment_label = ifelse(treatment == 1, "Sport Participatiopn", "No Sport Participation")) %>%
      ggplot(aes(x = m, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), binwidth = 0.01) +
      scale_fill_manual(values = c("grey88", "grey38")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", 
                 color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", 
                 color = "black", size = 0.5) + 
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle("Propensity Score Overlap") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = "Treatment Group"))
    
  
  ## MULTIVALUED TREATMENT SETTING ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    # generate plot
    data_pred %>%
      mutate(treatment_label = case_when(treatment == 1 ~ "Daily and weekly", 
                                         treatment == 2 ~ "Monthly or less frequently",
                                         TRUE ~ "Never")) %>%
      gather(m1, m2, m3, key = "class", value = "prob") %>% 
      mutate(class = case_when(class == "m1" ~ "Daily + Weekly", 
                               class == "m2" ~ "Monthly or less", 
                               TRUE ~ "Never")) %>%
      ggplot() +
      # histogram
      geom_histogram(aes(y = ..density.., x = prob, fill = treatment_label), alpha = 0.6, binwidth = 0.01) +
      # bar colors
      scale_fill_manual(values = c("darkgreen", "grey38", "darkblue")) +
      # trimming thresholds
      geom_vline(xintercept = min_trimming, linetype = "longdash", color = "black", size = 0.5) +
      geom_vline(xintercept = max_trimming, linetype = "longdash", color = "black", size = 0.5) + 
      # facet
      facet_wrap(~ class, ncol = 1) +
      # aesthetics
      xlab("Propensity Score") + 
      ylab("Density") + 
      ggtitle("Propensity Score Overlap") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = "Treatment Group"))
  }
  

}





