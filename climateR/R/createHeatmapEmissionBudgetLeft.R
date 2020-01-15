#' Heatmap: Budget left for all years
#' @details Prepares the heatmap displaying the budget which is left for all countries and years
#' @param data Data frame containing the calculated "just" emissions budget for each state. Needs to have columns "country" and "budget_left_perc"
#' @param scales Values for color scale, with seven vakues
#' @param from Start year
#' @param to Final year
#' @import dplyr, ggplot2
#' @export


createHeatmapEmissionBudgetLeft <- function(data, scales, from, to, theme) {
  data %>%
    na.omit() %>%
    ggplot() +
    geom_tile_interactive(aes(x = country, y = year, fill = budget_left_perc, data_id = data_id,
                              tooltip = paste0(country, " (", year, "): ", round(budget_left_perc, 1), "% of budget left (", round(budget_left, 2), " Gt CO2)")), color = "white") +
    # scale_fill_gradientn(colours = c("slateblue4", "red", "orange", "white", "lightgreen", "chartreuse3", "darkgreen"),
    #                      values = scales,
    #                      na.value = "grey", name = "% budget left") +
    scales +
    coord_flip() +
    scale_y_continuous(breaks = seq(from, to, 1)) +
    theme +
    theme(
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.text.x = element_text(angle = 90),
      legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", colour = NA), # get rid of legend panel bg
      legend.text = element_text(colour = "white"),
      legend.key = element_blank() # Removes frame around boxes
    ) +
    labs(x = "", y = "Year", title = "Budget consumption", subtitle = "Percent of budget left per country and year")
}
