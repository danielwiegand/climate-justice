#' Scatter plot: Emissions and GDP per capita
#' @details Prepares the scatter plot
#' @param data Data frame containing the emission data
#' @import dplyr, ggplot2
#' @export

prepareScatterPlotGDP <- function(data, theme, limits_x, limits_y, limits_scale, colors) {

  output <- data %>%
    ggplot() +
    geom_point_interactive(aes(x = gdp_per_capita, y = emis_per_capita, data_id = data_id, col = country, alpha = .1, size = emissions,
                               tooltip = paste0(country, " ", year, ":\n", round(emis_per_capita, 1), " t CO2e per capita \n",
                                                formatC(round(gdp_per_capita, 0), format = "f", digits = 0, big.mark = ","), " PPP-$ per capita"))) +
    geom_text_repel(aes(label = label, x = gdp_per_capita, y = emis_per_capita), color = "white", hjust = 0, nudge_y = 0.07, nudge_x = 0.07, point.padding = NA) +
    scale_color_manual(values = colors) +
    scale_x_log10(limits = limits_x, labels = scales::comma) +
    scale_y_log10(limits = limits_y, labels = scales::comma) +
    scale_size_continuous(limits = limits_scale, range = c(1, 20)) +
    theme +
    theme(legend.position = "none") +
    labs(x = "GDP per capita (PPP-$)", y = "Emissions per capita (t CO2e)",
         title = "Emissions and GDP per capita", subtitle = "Relation between economic development and emissions")

  return(output)

}
