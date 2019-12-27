#' Load World Map
#' @details Loads and prepares a geoJSON world map. Returns a SpatialPolygonsDataFrame.
#' @param quality Specifies how detailed the map should be. This determines the processing time for creating a chloropleth graph. Available choices: low, medium, high
#' @import
#' @export

loadWorldMap <- function(quality) {

  filename <- paste0("src/geojson/countries_", quality, "res.geojson")

  world_map <- geojson_read(filename,
                            what = "sp")

  # Rename some levels for later merging with data
  world_map@data$admin <- case_when(world_map@data$admin == "The Bahamas" ~ "Bahamas",
                                    world_map@data$admin == "Republic of Congo" ~ "Congo",
                                    world_map@data$admin == "Democratic Republic of the Congo" ~ "Democratic Republic of Congo",
                                    world_map@data$admin == "Ivory Coast" ~ "Cote d'Ivoire",
                                    world_map@data$admin == "Curaçao" ~ "Curacao",
                                    world_map@data$admin == "Faroe Islands" ~ "Faeroe Islands",
                                    world_map@data$admin == "Guinea Bissau" ~ "Guinea-Bissau",
                                    world_map@data$admin == "Hong Kong S.A.R." ~ "Hong Kong",
                                    world_map@data$admin == "Macao S.A.R" ~ "Macao",
                                    world_map@data$admin == "Federated States of Micronesia" ~ "Micronesia (country)",
                                    world_map@data$admin == "Republic of Serbia" ~ "Serbia",
                                    world_map@data$admin == "Sint Maarten" ~ "Sint Maarten (Dutch part)",
                                    world_map@data$admin == "United Republic of Tanzania" ~ "Tanzania",
                                    world_map@data$admin == "East Timor" ~ "Timor",
                                    world_map@data$admin == "United States of America" ~ "United States",
                                    TRUE ~ world_map@data$admin)

  return(world_map)

}
