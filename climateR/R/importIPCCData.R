#' Load IPCC budget data used for the climate justice tool.
#' @details Loads and prepares files with data containing IPCC's emission budgets.
#' @import
#' @export

importIPCCData <- function() {

  carbon_budgets <- read.csv("src/ipcc/carbon_budgets.csv") %>%
    mutate(warming_degrees = as.factor(warming_degrees))

  return(carbon_budgets)

}
