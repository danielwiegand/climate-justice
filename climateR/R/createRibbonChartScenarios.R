#' Ribbon chart: Future emission scenarios
#' @param data_ribbon Data frame with data for ribbons
#' @param data_column Data frame with data for the column on the right
#' @param theme Some ggplot theme adjustments
#' @import dplyr, ggplot2
#' @export

createRibbonChartScenarios <- function(data_ribbon, data_column, theme) {

  output <- ggplot(data_ribbon, aes(x = year)) +
    geom_line_interactive(aes(y = historical, tooltip = "Historical emissions", group = 1, hover_css = "stroke:grey; fill:none; fill-opacity:0;"), color = "white",
                          data_id = "line") +
    geom_ribbon_interactive(data = . %>% filter(!is.na(d15_low)), aes(ymin = d15_low, ymax = d15_high, tooltip = "1.5°C consistent: 1.3°C",
                                                                      hover_css = "stroke:none;"), data_id = "ribbon4", alpha = .5, fill = "palegreen") +
    geom_ribbon_interactive(data = . %>% filter(!is.na(d2_low)), aes(ymin = d2_low, ymax = d2_high, tooltip = "2°C consistent: 1.6 - 1.7°C",
                                                                     hover_css = "stroke:none;"), data_id = "ribbon3", alpha = .5, fill = "khaki") +
    geom_ribbon_interactive(data = . %>% filter(!is.na(pledges_low)), aes(ymin = pledges_low, ymax = pledges_high, tooltip = "Pledges & Targets: 2.5 - 2.8°C",
                                                                          hover_css = "stroke:none;"), data_id = "ribbon2", alpha = .5, fill = "powderblue") +
    geom_ribbon_interactive(data = . %>% filter(!is.na(current_policy_low)), aes(ymin = current_policy_low, ymax = current_policy_high),
                            tooltip = "Current policies: 2.8 - 3.2°C", fill = "slategray4", data_id ="ribbon1", hover_css = "stroke:none;", alpha = .5) +
    geom_ribbon_interactive(data = . %>% filter(!is.na(bau_low)), aes(ymin = bau_low, ymax = bau_high), data_id = "ribbon0",
                            tooltip = "Baseline: 4.1 - 4.8°C", fill = "grey", hover_css = "stroke:none;", alpha = .5) +
    geom_rect_interactive(data = data_column, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, data_id = cat, tooltip = desc, fill = desc)) +
    geom_text_repel(data = data_column, aes(x = xmax, y = ymin+(ymax-ymin)/2, label = cat), hjust = 0, color = "white", nudge_x = 1, size = 3) +
    scale_fill_manual(values = c("khaki", "palegreen", "powderblue", "slategray2", "grey")) +
    scale_x_continuous(breaks = seq(1990, 2100, 10), limits = c(1990, 2130)) +
    theme +
    theme(legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    labs(y = "Global greenhouse gas emissions (Gt CO2e / year)", x = "Year", title = "Emission scenarios", subtitle = "Political choices and consequences")

  return(output)

}
