#' Bar chart: Budget changes depending on justice approach for exemplary states
#' @param data Data frame containing the data
#' @param theme Some ggplot theme adjustments
#' @param cols The color values for scale_color_manual
#' @import dplyr, ggplot2
#' @export

createBarChartExemplaryStates <- function(data, theme, cols, selected_budget) {

  output <- data %>%
    ggplot() +
    geom_segment(aes(x = country, xend = country, y = maximum_year, yend = budget_reach), color = "grey") +
    geom_point_interactive(aes(x = country, y = budget_reach, data_id = data_id,
                               tooltip = paste0(country, ": Budget reaches until year ", budget_reach), size = 10, color = country)) +
    scale_color_manual(values = cols(3)) +
    coord_flip() +
    geom_vline_interactive(aes(xintercept = maximum_year, tooltip = maximum_year, data_id = "a"), size = 2, color = "white") +
    theme +
    theme(legend.position = "none",
          aspect.ratio = .4,
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(x = "", y = "", caption = paste0("This graph shows how many years are left when the selected allocation approach is applied.
               Assumptions: Global budget is ", selected_budget, "Gt CO2, state emissions stay constant."))

  return(output)

}
