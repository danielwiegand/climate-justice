#' Heatmap: Budget left for all years
#' @details Prepares the heatmap displaying the budget which is left for all countries and years
#' @param data Data frame containing the calculated "just" emissions budget for each state. Needs to have columns "country" and "budget_left_perc"
#' @param scales Values for color scale, with seven vakues
#' @param from Start year
#' @param to Final year
#' @import dplyr, ggplot2
#' @export


createHeatmapEmissionBudgetLeft <- function(data, scales, from, to) {
  data %>%
    na.omit() %>%
    ggplot() +
    geom_tile_interactive(aes(x = country, y = year, fill = budget_left_perc, data_id = data_id,
                              tooltip = paste0(country, " (", year, "): ", round(budget_left_perc, 1), "% of budget left")), color = "white") +
    scale_fill_gradientn(colours = c("slateblue4", "red", "orange", "white", "lightgreen", "chartreuse3", "darkgreen"),
                         values = scales,
                         na.value = "grey", name = "% budget left") +
    coord_flip() +
    scale_y_continuous(breaks = seq(from, to, 1)) +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      panel.border = element_rect(colour = "transparent", fill = NA, size = 0),
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      # plot.title = element_text(size = 8),
      # plot.subtitle = element_text(size = 6),
      title = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.text.x = element_text(angle = 90),
      legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", colour = NA), # get rid of legend panel bg
      legend.text = element_text(colour = "white"),
      legend.key = element_blank() # Removes frame around boxes
    ) +
    labs(x = "", y = "Year", title = "Budget consumption", subtitle = "Percent of budget left per country and year")
}
