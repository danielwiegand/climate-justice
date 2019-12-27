
# LIBRARIES ####

library(shiny)
library(shinythemes)
library(climateR)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr) # spread
library(tibble) # rownames_to_column
library(geojsonio) # read .geojson
library(broom) # tidy
library(ggiraph)
library(leaflet)
library(RColorBrewer)

ui <- fluidPage(theme = shinytheme("slate"),
                
                tags$head(
                  tags$link(rel="stylesheet", type="text/css", href="style.css")
                ),
                
                titlePanel("What is Climate Justice?"),
                
                fluidRow(
                  column(1),
                  column(10,
                         
                         tabsetPanel(id = "tabset-panel",
                           tabPanel("Status Quo",
                                    
                                    absolutePanel(draggable = T,
                                                  
                                                  wellPanel(
                                                    
                                                    sliderInput("base_year", "Select base year", min = 1950, max = 2017, value = 1992, step = 1),
                                                    
                                                    selectInput("selected_probability", "Select probability", choices = c("66%" = "66",
                                                                                                                          "50%" = "50",
                                                                                                                          "33%" = "33"),
                                                                selected = "66%"),
                                                    
                                                    selectInput("selected_warming_degrees", "Select warming degrees",
                                                                choices = c("1.5°C" = 1.5, "2°C" = 2),
                                                                selected = "1.5°C"),
                                                    
                                                    selectInput("selected_calculation_approach", "Select a calculation approach",
                                                                choices = c("Per capita approach" = "pca"),
                                                                selected = "pca")
                                                    
                                                  ), style = "z-index: 10; opacity: 0.65;"
                                                  
                                    ),
                                    
                                    girafeOutput("barchart_continents", width = "90%"),
                                    
                                    absolutePanel(
                                      actionButton("forwardPage2", "", icon = icon("chevron-right"), class = "scroll-button"),
                                      top = "35%", right = "10%", fixed = T
                                    )
                                    
                           ),
                           
                           tabPanel("Next page",
                                    
                                    girafeOutput("linechart_continents", width = "90%"),
                                    
                                    absolutePanel(
                                      actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                      top = "35%", right = "10%", fixed = T
                                    )
                                    
                                    ),
                           
                           tabPanel("Leaflet",
                                    
                                    leafletOutput("chloropleth_emissions_per_capita"),
                                    
                                    absolutePanel(
                                      actionButton("forwardPage4", "", icon = icon("chevron-right"), class = "scroll-button"),
                                      top = "35%", right = "10%", fixed = T
                                    )
                                    
                           ),
                           
                           
                           tabPanel("Budget left",
                                    
                                    girafeOutput("heatmap_budget_left_allyears"),
                                    
                                    absolutePanel(
                                      actionButton("forwardPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                      top = "35%", right = "10%", fixed = T
                                    )
                         ),
                         
                         tabPanel("Leaders and laggards",
                                  
                                  girafeOutput("barchart_leaders_laggards"),
                                  
                                  absolutePanel(
                                    actionButton("forwardPage6", "", icon = icon("chevron-right"), class = "scroll-button"),
                                    top = "35%", right = "10%", fixed = T
                                  )
                         )
                         
                         )
                         # girafeOutput("bump_chart_emitters"),
                         
                         
                  )
                )
)