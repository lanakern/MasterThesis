
func_dml_common_support <- function(data_pred, type) {
  
  if (type == "side-by-side") {
    # https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
    data_pred %>%
      mutate(treatment_label = ifelse(treatment == 1, "Sport Participatiopn", "No Sport Participation")) %>%
      ggplot(aes(x = m)) +
      geom_histogram(color = "white", binwidth = 0.01) +
      facet_wrap(~treatment_label) +
      xlab("Probability of participating in sports") +
      theme_bw()
  } else if (type == "one-plot") {
    data_pred %>%
      mutate(treatment_label = ifelse(treatment == 1, "Sport Participatiopn", "No Sport Participation")) %>%
      ggplot(aes(x = m, color = treatment_label)) +
      geom_histogram(binwidth = 0.01)
  }
}





