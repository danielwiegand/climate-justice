#' Make label arrows
#' @details Adds labels with arrows at the desired place in the plot
#' @param data ggplot object
#' @param x The place where the arrow start and where the annotation is
#' @param y The place where the arrow start and where the annotation is
#' @param xend End of the arrow, where the arrow is pointing to
#' @param yend End of the arrow, where the arrow is pointing to
#' @param label Label text.
#' @import dplyr, ggplot2
#' @export



makeLabelArrows <- function(data, x, xend, y, yend, label) {
  output <- data +
    annotate(geom = "curve", x = x, xend = xend, y = y, yend = yend, colour = "gray", size = 0.3, alpha = .9,
             curvature = -0.2, arrow = arrow(length = unit(0.02, "npc"))) +
    annotate(geom = "label", x = x, y = y, label = label, family = "Helvetica", alpha = .9,
             colour = "#8f9192", fill = "#1f2226", size = 2.5, label.size = NA)
  # geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
  #            colour = "gray",
  #            size = 0.3,
  #            curvature = -0.2,
  #            arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_label(aes(x = x, y = y, label = label),
  #           alpha = .9,
  #           width = "100px",
  #           size = 3,
  #           colour = "#8f9192",
  #           fill = "#1f2226",
  #           label.size = NA,
  #           family = "Helvetica")
  return(output)
}
