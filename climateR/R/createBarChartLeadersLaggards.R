#' Bar chart: Leaders and laggard countries regarding emission budget
#' @details Creates the bar chart
#' @param data Data frame containing the data
#' @import dplyr, ggplot2
#' @export

createBarChartLeadersLaggards <- function(data, scales, theme) {

  output <- data %>%
    ggplot(aes(x = country, y = budget_left_perc, fill = budget_left_perc)) +
    geom_bar_interactive(aes(data_id = data_id, tooltip = paste0(country, ": ", round(budget_left_perc, 1), "% of budget left")), show.legend = FALSE, stat = "identity") +
    coord_flip() +
    theme_test() +
    theme +
    theme(
      legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", colour = NA), # get rid of legend panel bg
      legend.text = element_text(colour = "white"),
      legend.key = element_blank(), # Removes frame around boxes
      strip.background = element_blank(),
      strip.text = element_text(colour = "white")
    ) +
    facet_grid(~ budget_left > 0, scales = "free", labeller = as_labeller(c("TRUE" = "Leaders", "FALSE" = "Laggards"))) +
    labs(title = "Leaders and laggards", subtitle = "Countries performing worst / best regarding their emission budget", x = "Country", y = "Budget left (%)") +
    scales

  return(output)

}
