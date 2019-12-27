#' Create Chart scales
#' @details Creates numbers used for a color scale in charts. Seven colors are used, center is zero.
#' @param data Atomic numeric vector
#' @param props Probabilities for quantiles
#' @import dplyr, ggplot2
#' @export

createChartScales <- function(data, props) {
  scales::rescale(c(
    min(data, na.rm = T),
    quantile(data[data < 0], probs = props[1], na.rm = T),
    quantile(data[data < 0], probs = props[2], na.rm = T),
    0,
    quantile(data[data > 0], probs = props[3], na.rm = T),
    quantile(data[data > 0], probs = props[4], na.rm = T),
    max(data, na.rm = T)))
}
