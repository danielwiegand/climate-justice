#' Load emission data used for the climate justice tool.
#' @details Loads and prepares files with data containing the continents' emission history.
#' @import dplyr, tidyr
#' @export

importContinentsEmissionData <- function() {

  continents_emissions_per_capita <- read.csv("src/global_carbon_project/emis_per_capita_regions.csv",
                                              stringsAsFactors = F) %>%
    tidyr::gather(-year, key = "continent", value = "emis_per_capita")

  continents_emissions <- read.csv("src/global_carbon_project/emis_regions.csv",
                                   stringsAsFactors = F) %>%
    tidyr::gather(-year, key = "continent", value = "emissions") %>%
    left_join(continents_emissions_per_capita) %>%
    mutate(population = emissions / emis_per_capita * 1000000,
           continent = stringr::str_replace_all(continent, "\\.", " "),
           data_id = as.character(row_number()),
           emissions = emissions * 1000000) # Convert to kg CO2

  return(continents_emissions)

}
