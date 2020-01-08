#' Bar chart: Years left per country
#' @param data Data frame with data to plot
#' @param base_year Numeric: Which base year to select?
#' @param theme Some ggplot theme adjustments
#' @param cols The colors for the points
#' @import dplyr, ggplot2
#' @export

createBarChartYearsLeft <- function(data, base_year, theme, cols) {

  output <- ggplot(data) +
    geom_segment(aes(x = country, xend = country, y = base_year, yend = budget_reach), color = "grey") +
    geom_point_interactive(aes(x = country, y = budget_reach, data_id = data_id,
                               tooltip = paste0(country, ": Budget lasts until year ", budget_reach), size = 10, color = country)) +
    coord_flip() +
    scale_color_manual(values = cols) +
    theme +
    theme(legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(title = "Budget reach", subtitle = "Under constant base year emissions", x = "", y = "Budget depletion year")

  return(output)

}
