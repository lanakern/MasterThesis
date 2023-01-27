func_dml_pool_mice <- function(dml_result, N, mice_num) {
  
  # store final estimates of each mice data set in one data frame
  dml_final <- data.frame()
  for (mice_data_sel in 1:mice_num) {
    dml_final <- rbind(
      dml_final,
      dml_result[[mice_data_sel]]$final %>% mutate(MICE = mice_data_sel) %>%
        select(MICE, everything())
    )
  }
  
  # aggregate results across MICE data sets: simply take mean / median of
  # estimates and se
  # For t- and p-value I also take mean / median, but would be identical
  # if I would calculate them again
  # confidence interval is calculated again
  dml_final %>%
    select(Type, starts_with("theta"), starts_with("se"), matches(".*value")) %>%
    group_by(Type) %>%
    summarize(
      theta_mean = mean(theta_mean), theta_median = median(theta_median),
      se_mean = mean(se_mean), se_median = median(se_median),
      tvalue_mean = mean(tvalue_mean), tvalue_median = median(tvalue_median),
      pvalue_mean = mean(pvalue_mean), pvalue_median = median(pvalue_median)
    ) %>%
    mutate(
      CI_lower_mean_95 = theta_mean - qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_mean / sqrt(N),
      CI_upper_mean_95 = theta_mean + qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_mean / sqrt(N),
      CI_lower_median_95 = theta_median - qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_median / sqrt(N),
      CI_upper_median_95 = theta_median + qt(0.95, df = N - 1)^-1 * (1 - 0.95 / 2) * se_median / sqrt(N)
    )
}