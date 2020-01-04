#' Load emission data used for the climate justice tool.
#' @details Loads and prepares files with data containing the countries' emission history.
#' @import dplyr, tidyr
#' @export

importCountriesEmissionData <- function() {

  countries_emissions_per_capita <- read.csv("src/global_carbon_project/emis_per_capita_countries.csv",
                                             stringsAsFactors = F) %>%
    tidyr::gather(-year, key = "country", value = "emis_per_capita") %>%
    mutate(country = stringr::str_replace_all(country, "\\.", " "))

  countries_emissions_per_gdp <- read.csv("src/global_carbon_project/emis_per_gdp_countries.csv",
                                          stringsAsFactors = F) %>%
    tidyr::gather(-year, key = "country", value = "emis_per_gdp") %>%
    mutate(country = stringr::str_replace_all(country, "\\.", " "))

  countries_emissions <- read.csv("src/global_carbon_project/emis_countries.csv",
                                  stringsAsFactors = F) %>%
    tidyr::gather(-year, key = "country", value = "emissions") %>%
    mutate(country = stringr::str_replace_all(country, "\\.", " "),
           data_id = as.character(row_number()),
           emissions = emissions * 1000000) %>% # Convert to kg CO2
    left_join(countries_emissions_per_capita) %>%
    mutate(population = emissions / emis_per_capita) %>%
    left_join(countries_emissions_per_gdp) %>%
    mutate(gdp = emissions / emis_per_gdp * 1000,
           gdp_per_capita = gdp / population) %>% # Emissions per GDP are in kg CO2
    mutate(country = stringr::str_replace_all(country, c("United States of America" = "United States",
                                                "Democratic Republic of the Congo"= "Democratic Republic of Congo",
                                                "Republic of South Sudan" = "South Sudan",
                                                "Côte d Ivoire" = "Cote d'Ivoire",
                                                "Russian Federation" = "Russia",
                                                "Guinea Bissau" = "Guinea-Bissau")))

  return(countries_emissions)

}
