#' Rect plot: Emissions per region and capita
#' @details Prepares the rect plot
#' @param data Data frame containing the emission data
#' @import dplyr, ggplot2
#' @export

createRectPlotEmissionsRegion <- function(data, theme, cols, year, pos_label) {

  output <- data %>%
    ggplot() +
    geom_rect_interactive(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = emis_per_capita, fill = continent, data_id = data_id,
                              tooltip = paste0(continent, ": ", emis_per_capita, " t CO2 per capita\n", population_perc,
                                               "% of world population\n", emission_perc, "% of world emissions"))) +
    geom_text(aes(x = pos_label, y = emis_per_capita, label = continent, size = population_perc), vjust = 0, color = "white", angle = 90, nudge_y = 1) +
    theme +
    theme(legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    scale_fill_manual(values = c(cols(8))) +
    scale_x_continuous(breaks = seq(0, 8, 1), limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 18)) +
    labs(x = "Population (billions)", y = "Emissions per capita (t CO2e)",
         title = paste0("Emissions per region and capita ", year), subtitle = "Relation between per capita emissions and population size")

  return(output)

}
