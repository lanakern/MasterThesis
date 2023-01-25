#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### FUNCTION FOR CHECKING COMMON SUPPORT ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


func_dml_common_support <- function(data_pred, min_trimming, max_trimming) {
  
  min_trimming <- min(min_trimming)
  max_trimming <- min(max_trimming)
  
  data_pred %>%
    mutate(treatment_label = ifelse(treatment == 1, "Sport Participatiopn", "No Sport Participation")) %>%
    ggplot(aes(x = m, fill = treatment_label)) +
    geom_histogram(binwidth = 0.01) +
    scale_fill_manual(values = c("grey88", "grey38")) +
    geom_vline(xintercept = min_trimming, linetype = "longdash", 
               color = "black", size = 0.5) +
    geom_vline(xintercept = max_trimming, linetype = "longdash", 
               color = "black", size = 0.5) + 
    xlab("Probability of participating in sports") + 
    ylab("Number of observations") + 
    ggtitle("Propensity Score Overlap") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title = "Treatment Group"))
}





