#' Line chart: Development of global temperature
#' @details Prepares the line chart displaying the global temperature anomalies
#' @param data Data frame containing the emission data
#' @import dplyr, ggplot2
#' @export

prepareLineChartTemperatures <- function(data, theme) {

  output <- data %>%
    ggplot() +
    geom_ribbon(aes(x = year, ymax = upper, ymin = lower), fill = "grey40", alpha = .5) +
    geom_line_interactive(aes(x = year, y = median, data_id = data_id, tooltip = "Average temperature anomaly", colour = "red")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
    scale_x_continuous(breaks = seq(1850, 2020, 5)) +
    labs(x = "Year", y = "Average temperature anomaly (°C)",
         title = "Average temperature anomaly", subtitle = "In relation to the averaged yearly temperature 1961-1990")

  return(output)

}
