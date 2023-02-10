

# load results
if (main_extra_act == "yes") {
  main_extra_act_save <- "extradrop"
} else {
  main_extra_act_save <- ""
}

load_results_main <- paste(
  main_model_type, main_model_outcome, str_replace_all(main_cohort_prep, "_", ""),
  main_treatment_def, main_treatment_repl, main_extra_act_save, sep = "_"
)


df_dml_detail_all <- data.frame()
for (model_algo_sel in c("lasso", #"postlasso", "randomforests", 
                         "xgboost")) {
  df_dml_load <- readRDS(paste0("Output/DML/", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_detail_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_detail_sub <- df_dml_load[[i]]$detail %>% filter(Type %in% c("ATE", "ATTE")) %>% mutate(MICE = i)
    df_dml_detail_sub_all <- rbind(df_dml_detail_sub_all, df_dml_detail_sub)
  }
  
  df_dml_detail_all <- rbind(df_dml_detail_all, df_dml_detail_sub_all)
  
}



#### BOXPLOT FOR MAIN MODEL ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# boxplot is only made for main model and median estimate of ATE and ATTE
# https://docs.doubleml.org/stable/examples/R_double_ml_pension.html
# df_dml_results <- read.xlsx("Output/ESTIMATION_RESULTS.xlsx", sheetName = "Sheet1")
# df_dml_results_boxplot <- df_dml_results %>%
#   filter(cohort_prep == main_cohort_prep, treatment_repl == main_treatment_repl,
#          treatment_def == main_treatment_def, extra_act == main_extra_act,
#          model_type == main_model_type, model_k == main_model_k, model_k_tuning == main_model_k_tuning,
#          model_s_rep == main_model_s_rep, model_trimming == main_model_trimming,
#          model_outcome == main_model_outcome, model_controls == main_model_controls,
#          Type %in% c("ATE", "ATTE")
#          ) %>%
#   select(model_algo, Type, contains("median"))
# 
# df_dml_results_boxplot_ATE <- df_dml_results_boxplot %>% filter(Type == "ATE")
# ggplot(df_dml_results_boxplot_ATE, aes(x = model_algo, y = theta_median, color = model_algo)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = CI_lower_median_95, ymax = CI_upper_median_95, color = model_algo))  +
#   geom_hline(yintercept = 0, color = "grey") +
#   theme_minimal() + 
#   xlab("") +
#   theme(axis.text.x = element_text(angle = 90), legend.position = "none",
#         text = element_text(size = 20))



# generate boxplot with detailed dml results
df_dml_results_boxplot <- df_dml_detail_all 

dml_boxplot <- 
  ggplot(df_dml_results_boxplot, aes(x = ML_algo, y = Treatment_Effect)) +
  geom_boxplot(fill = "grey") +
  xlab("\nMachine Learning Algorithms\n") + ylab("\nTreatment Effects\n") +
  facet_wrap(~ Type) + 
  theme_bw() +
  theme(axis.text = element_text(size = 28), # size of x-axis tick labels
        axis.title = element_text(size = 30, face = "bold"), # size of x-axis labels
        strip.text.x = element_text(size = 30) # size of facet text
        ) 

ggsave("Output/plot_dml_boxplot.png", dml_boxplot)


#%%%%%%%%%%%%%%%%%%%%%%%#
#### Hyperparameters ####
#%%%%%%%%%%%%%%%%%%%%%%%#


df_dml_detail_all <- data.frame()
for (model_algo_sel in c("lasso", #"postlasso", "randomforests", 
                         "xgboost")) {
  df_dml_load <- readRDS(paste0("Output/DML/", model_algo_sel, "_", load_results_main, ".rds"))
  # assign(paste0("dml_result_", model_algo_sel), df_dml_load)
  
  df_dml_detail_sub_all <- data.frame()
  for (i in 1:5) {
    df_dml_detail_sub <- df_dml_load[[i]]$detail %>% filter(Type %in% c("ATE", "ATTE")) %>% mutate(MICE = i)
    df_dml_detail_sub_all <- rbind(df_dml_detail_sub_all, df_dml_detail_sub)
  }
  
  df_dml_detail_all <- rbind(df_dml_detail_all, df_dml_detail_sub_all)
  
}