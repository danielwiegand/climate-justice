#' Chloropleth Chart
#' @details Creates a chloropleth chart by means of the leaflet package
#' @param data SpatialPolygonsDataFrame containing the emission data to be plotted nested within "data"
#' @param data_column Name of the column which contains the value which should be plotted (one value per country)
#' @param palette Color palette, produced e.g. by means of colorNumeric()
#' @param title The title of the plot
#' @param tooltip Character vector containing tooltips for each data row
#' @import dplyr, leaflet
#' @export

createChloroplethChart <- function(data, palette, tooltip, data_column, chart_title) {

  chloropleth <- leaflet(data) %>%
    addTiles() %>%
    addMapPane(name = "maplabels", zIndex = 420) %>% # This is to have labels rendered on top of the fill colors
    addProviderTiles("CartoDB.PositronOnlyLabels", # For labels
                     options = leafletOptions(pane = "maplabels"),
                     group = "map labels") %>%
    setView(lat = 30, lng = 0 , zoom = 2.1) %>%
    addPolygons(stroke = T, fillOpacity = 1, smoothFactor = 0.5, color = ~palette(data@data[[data_column]]), label = tooltip, weight = 0.3,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "11px",
                  direction = "auto")) %>%
    addLegend(pal = palette, values = ~data@data[[data_column]], opacity = 0.8, title = chart_title, position = "bottomleft")

  return(chloropleth)

}
