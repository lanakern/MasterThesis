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
# -> "min_trimming": minimal trimming threshold (data frame in multivalued treatment setting)
# -> "max_trimming: maximal trimming threshold (data frame in multivalued treatment setting)
# -> "text_trimming" indicates if trimming thresholds should be displayed. "yes" or "no".
# In this case line is also always displayed. 
# -> "line_trimming" indicates if trimming line should be displayed: "yes" or "no".
# -> "ml_algo": ML algorithm used to create the plot (only used in plot title)
# -> "dec_places": number of decimal places
#+++
# OUTPUT: Plot
#+++

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

func_dml_common_support <- function(treatment_setting, data_pred, min_trimming, 
                                    max_trimming, text_trimming, line_trimming, 
                                    ml_algo, dec_places, bar_border = "black") {
  
  # extract trimming thresholds for binary treatment setting (only one)
  if (treatment_setting == "binary") {
    min_trimming <- min(min_trimming)
    max_trimming <- min(max_trimming)
  }

  # nice ml algo name
  if (ml_algo == "xgboost" | ml_algo == "xgb") {
    ml_algo <- "XGBoost"
  } else if (ml_algo == "randomforests" | ml_algo == "rf") {
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
    
    # generate histogram
    plot_trimming <- data_pred %>%
      mutate(treatment_label = ifelse(treatment == 1, "Sport", "No Sport")) %>%
      ggplot(aes(x = m, fill = treatment_label)) +
      # histogram
      geom_histogram(aes(y = ..density..), bins = 100,  alpha = 0.8, width = 0, size = 0,
                     #color = bar_border, 
                     #color="#00000000",
                     position = "identity") +
      scale_fill_manual(values = c("grey0", "grey88"))
      
    
    # add trimming lines (with or without text)
    if (text_trimming == "yes") {
      plot_trimming <- plot_trimming + 
        geom_textvline(label = paste("min:", sprintf(paste0("%.", dec_places, "f"), unique(min_trimming))),
                       xintercept = min_trimming, vjust = 1.3, linetype = "longdash", size = 8) + # -0.7 for left hand side
        geom_textvline(label = paste("max:", sprintf(paste0("%.", dec_places, "f"), unique(max_trimming))),  
                       xintercept = max_trimming, vjust = 1.3, linetype = "longdash", size = 8) +
        scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1"))
    } else if (line_trimming == "yes") {
      plot_trimming <- plot_trimming + 
        geom_vline(xintercept = min_trimming, linetype = "longdash", 
                   color = "black", size = 0.5) +
        geom_vline(xintercept = max_trimming, linetype = "longdash", 
                   color = "black", size = 0.5) +
        scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1"))
    } else {
      plot_trimming <- plot_trimming +
        #xlim(0,1) +
        scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), limits = c(0,1), expand = c(0, 0))
    }
    
    # finalize layout
    plot_trimming <- plot_trimming +
      xlab("\nPropensity score\n") + 
      ylab("\nDensity\n") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)), "Propensity Score Overlap")))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 30), 
            axis.text = element_text(size = 28), axis.title = element_text(size = 28),
            legend.text = element_text(size = 28), legend.title = element_text(size = 28)) +
      guides(fill = guide_legend(title = "Group: "))
    
  
    return(plot_trimming)
    
  ## MULTIVALUED TREATMENT SETTING ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (treatment_setting == "multi") {
    
    # prepare data
    data_pred_m1 <- data_pred %>%
      dplyr::select(treatment, m1) %>%
      mutate(treatment_label = case_when(treatment == 1 ~ "Treatment", TRUE ~ "Control")) %>%
      mutate(min_trimming = min_trimming %>% filter(model == "m1") %>% pull(min_trimming),
             max_trimming = max_trimming %>% filter(model == "m1") %>% pull(max_trimming))
    
    data_pred_m2 <- data_pred %>%
      dplyr::select(treatment, m2) %>%
      mutate(treatment_label = case_when(treatment == 2 ~ "Treatment", TRUE ~ "Control")) %>%
      mutate(min_trimming = min_trimming %>% filter(model == "m2") %>% pull(min_trimming),
             max_trimming = max_trimming %>% filter(model == "m2") %>% pull(max_trimming))
    
    data_pred_m3 <- data_pred %>%
      dplyr::select(treatment, m3) %>%
      mutate(treatment_label = case_when(treatment == 3 ~ "Treatment", TRUE ~ "Control")) %>%
      mutate(min_trimming = min_trimming %>% filter(model == "m3") %>% pull(min_trimming),
             max_trimming = max_trimming %>% filter(model == "m3") %>% pull(max_trimming))
    
    
    # generate plots #
    #++++++++++++++++#
    
    ## m1 ##
    plot_m1 <- data_pred_m1 %>%
      ggplot(aes(x = m1, fill = treatment_label)) +
      geom_histogram(aes(y = ..density..), bins = 100,  alpha = 0.9, width = 0, size = 0,
                     #color = bar_border, 
                     position = "identity") +
      scale_fill_manual(values = c("grey0", "grey88"))
    
    if (text_trimming == "yes") {
      plot_m1 <- plot_m1 + 
        geom_textvline(label = paste("min. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m1$min_trimming))), 
                       xintercept = unique(data_pred_m1$min_trimming), 
                       vjust = -0.7, linetype = "longdash") +
        geom_textvline(label = paste("max. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m1$max_trimming))), 
                       xintercept = unique(data_pred_m1$max_trimming), 
                       vjust = -0.7, linetype = "longdash") 
    } else if (line_trimming == "yes") {
      plot_m1 <- plot_m1 + 
        geom_vline(xintercept = unique(data_pred_m1$min_trimming), linetype = "longdash", 
                   color = "black", size = 0.5) +
        geom_vline(xintercept = unique(data_pred_m1$max_trimming), linetype = "longdash", 
                   color = "black", size = 0.5)
    } else {
      plot_m1 <- plot_m1 + scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), limits = c(0,1), expand = c(0, 0))
    }
    
    plot_m1 <- plot_m1 +
      xlab("\nPropensity score\n") + 
      ylab("\nDensity\n") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)))))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 40),
            axis.text = element_text(size = 38), axis.title = element_text(size = 38),
            legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
      guides(fill = guide_legend(title = "Group: "))
    

    ## m2 ##
    plot_m2 <- data_pred_m2 %>%
      ggplot(aes(x = m2, fill = treatment_label)) +
      geom_histogram(aes(y = ..density..), bins = 100,  alpha = 0.9, width = 0, size = 0,
                     #color = bar_border, 
                     position = "identity") +
      scale_fill_manual(values = c("grey0", "grey88"))
    
    if (text_trimming == "yes") {
      plot_m2 <- plot_m2 + 
        geom_textvline(label = paste("min. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m2$min_trimming))), 
                       xintercept = unique(data_pred_m2$min_trimming), 
                       vjust = -0.7, linetype = "longdash") +
        geom_textvline(label = paste("max. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m2$max_trimming))), 
                       xintercept = unique(data_pred_m2$max_trimming), 
                       vjust = -0.7, linetype = "longdash") 
    } else if (line_trimming == "yes") {
      plot_m2 <- plot_m2 + 
        geom_vline(xintercept = unique(data_pred_m2$min_trimming), linetype = "longdash", 
                   color = "black", size = 0.5) +
        geom_vline(xintercept = unique(data_pred_m2$max_trimming), linetype = "longdash", 
                   color = "black", size = 0.5)
    } else {
      plot_m2 <- plot_m2 + scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), limits = c(0,1), expand = c(0, 0))
    }
    
    plot_m2 <- plot_m2 +
      xlab("\nPropensity score\n") + 
      ylab("\nDensity\n") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)))))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 40),
            axis.text = element_text(size = 38), axis.title = element_text(size = 38),
            legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
      guides(fill = guide_legend(title = "Group: "))
    
    
    
    ## m3 ##
    plot_m3 <- data_pred_m3 %>%
      ggplot(aes(x = m3, fill = treatment_label)) +
      geom_histogram(aes(y = ..density..), bins = 100,  alpha = 0.9, width = 0, size = 0,
                     #color = bar_border, 
                     position = "identity") +
      scale_fill_manual(values = c("grey0", "grey88"))
    
    if (text_trimming == "yes") {
      plot_m3 <- plot_m3 + 
        geom_textvline(label = paste("min. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m3$min_trimming))), 
                       xintercept = unique(data_pred_m3$min_trimming), 
                       vjust = -0.7, linetype = "longdash") +
        geom_textvline(label = paste("max. trimming:", sprintf(paste0("%.", dec_places, "f"), unique(data_pred_m3$max_trimming))), 
                       xintercept = unique(data_pred_m3$max_trimming), 
                       vjust = -0.7, linetype = "longdash") 
    } else if (line_trimming == "yes") {
      plot_m3 <- plot_m3 + 
        geom_vline(xintercept = unique(data_pred_m3$min_trimming), linetype = "longdash", 
                   color = "black", size = 0.5) +
        geom_vline(xintercept = unique(data_pred_m3$max_trimming), linetype = "longdash", 
                   color = "black", size = 0.5)
    } else {
      plot_m3 <- plot_m3 + scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), limits = c(0,1), expand = c(0, 0))
    }
    
    plot_m3 <- plot_m3 +
      xlab("\nPropensity score\n") + 
      ylab("\nDensity\n") + 
      ggtitle(bquote(paste(atop(bold(.(ml_algo)))))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 40),
            axis.text = element_text(size = 38), axis.title = element_text(size = 38),
            legend.text = element_text(size = 38), legend.title = element_text(size = 38)) +
      guides(fill = guide_legend(title = "Group: "))
    
    
    # combine plots
    return(list(m1 = plot_m1, m2 = plot_m2, m3 = plot_m3))
    # return(ggarrange(plot_m1 + ggtitle(bquote(bold(.(ml_algo)))),
    #                  plot_m2 + ggtitle(""), plot_m3 + ggtitle(""), nrow = 3))
    
    
  } # close else() over multi
  
} # close function





