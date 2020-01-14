#' Bar chart: Budget reach depending on justice approach for exemplary states
#' @param data Data frame containing the data
#' @param theme Some ggplot theme adjustments
#' @param cols The color values for scale_color_manual
#' @param start_year The year from which budget reach is measured
#' @param temperature_target The temperature target in °C which should be achieved
#' @import dplyr, ggplot2
#' @export

createBarChartExemplaryStates <- function(data, theme, cols, selected_budget, start_year, temperature_target) {

  output <- data %>%
    ggplot() +
    geom_segment(aes(x = country, xend = country, y = start_year, yend = budget_reach), color = "grey") +
    geom_point_interactive(aes(x = country, y = budget_reach, data_id = data_id,
                               tooltip = paste0(country, ": Budget lasts until year ", budget_reach), size = 10, color = country)) +
    scale_color_manual(values = cols(3)) +
    coord_flip() +
    geom_vline_interactive(aes(xintercept = start_year, tooltip = start_year, data_id = "a"), size = 2, color = "white") +
    theme +
    theme(legend.position = "none",
          aspect.ratio = .4,
          text = element_text(size = 18),
          title = element_text(size = 15),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(x = "", y = "", subtitle = paste0("Budget for ", temperature_target, "°C would last until..."), caption = "(Assumption: Constant emissions)")

  return(output)

}
