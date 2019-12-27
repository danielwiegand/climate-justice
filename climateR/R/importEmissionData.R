#' Load emission data used for the climate justice tool.
#' @details Loads and prepares files with data containing the countries' emission history.
#' @import dplyr
#' @export

importEmissionData <- function() {

  raw_emissions_per_capita <- read.csv("src/ourworldindata/co-emissions-per-capita.csv",
                                       stringsAsFactors = F)

  raw_emissions <- read.csv("src/ourworldindata/annual-co2-emissions-per-country.csv",
                            stringsAsFactors = F) %>%
    left_join(raw_emissions_per_capita) %>%
    rename(emissions = Annual.CO..emissions..tonnes.,
           emissions_per_cap = Per.capita.CO..emissions..tonnes.per.capita.,
           country = Entity,
           code = Code,
           year = Year) %>%
    mutate(population = emissions / emissions_per_cap,
           country = str_replace_all(country, "'", " ")) %>% # Some functions (eg ggiraph's tooltip) have problems with apostrophes
    filter(year > 1949 & year < 2018)

  return(raw_emissions)

}
