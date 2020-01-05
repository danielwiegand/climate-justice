#' Scatter plot: IPCC Carbon budgets
#' @details Prepares the scatter plot showing the carbon budgets left
#' @param data Data frame containing the data
#' @import dplyr, ggplot2
#' @export

prepareScatterPlotBudgets <- function(data, theme) {

  output <- data %>%
    ggplot() +
    geom_point_interactive(aes(x = warming_degrees, y = probability, size = budget_gt, color = years_left, data_id = data_id,
                               tooltip = paste0(budget_gt, " Gt (", round(years_left, 1), " years left from 2018)"))) +
    scale_size_continuous(range = c(1,20), guide = "none") +
    coord_fixed(ratio = 1.4) +
    theme +
    theme(legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
    scale_color_gradient(low = "red", high = "green") +
    labs(title = "Remaining carbon budget (2018)", subtitle = "Leading to different amounts of warming under different probabilities",
         x = "°C of warming by 2100", y = "Probability (%)")

  return(output)

}
