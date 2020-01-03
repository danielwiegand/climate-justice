
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
library(slickR) # Slideshow
library(shinyjs)
library(knitr)
library(kableExtra)
library(ggrepel)


# UI ####

ui <- fluidPage(theme = shinytheme("slate"),
                
                useShinyjs(),
                
                tags$head(
                  tags$link(rel="stylesheet", type="text/css", href="style.css")
                ),
                
                titlePanel("What is Climate Justice?"),
                
                fluidRow(
                  column(1),
                  column(10,
                         
                         tabsetPanel(id = "tabset-panel",
                                     
                                     # Start page ####
                                     
                                     tabPanel("Introduction",
                                              
                                              img(id = "startpage_image_1", src = "climate-justice1.jpg", style = "height: 75vh;"),
                                              hidden(img(id = "startpage_image_2", src = "climate-justice2.jpg", style = "height: 75vh")),
                                              hidden(img(id = "startpage_image_3", src = "climate-justice3.jpg", style = "height: 75vh")),
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated.
                                                                      The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated.
                                                                      The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated."),
                                                            style = "z-index: 100; opacity: 0.85;", top = "20%", left = "12%", fixed = T, width = "15%", align = "justify"
                                                            
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage1", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                              ),
                                     
                                     
                                     # Emission history ####
                                     
                                     tabPanel("Temperatures",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated."),
                                                            style = "z-index: 10; opacity: 0.65;", top = "30%", left = "20%", fixed = T, width = "15%", align = "justify"
                                                            
                                              ),
                                              
                                              girafeOutput("linechart_temperatures", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage2", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Status Quo ####
                                     
                                     tabPanel("Status Quo",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated."),
                                                            style = "z-index: 10; opacity: 0.65;", top = "30%", left = "20%", fixed = T, width = "15%", align = "justify"
                                                            
                                              ),
                                              
                                              girafeOutput("barchart_continents", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage2", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Consequences ####
                                     
                                     tabPanel("Consequences",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("The world's greenhouse gas emissions rise steadily since the beginning of the industrial revolution. 
                                                                       In the 20th century, this rise dramatically accelerated."),
                                                            style = "z-index: 10; opacity: 0.65;", top = "30%", left = "20%", fixed = T, width = "15%", align = "justify"
                                                            
                                              ),
                                              
                                              htmlOutput("iframe_sea_level", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage2", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Next page ####
                                     
                                     tabPanel("Next page",
                                              
                                              girafeOutput("linechart_continents", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Leaflet ####
                                     
                                     tabPanel("Leaflet",
                                              
                                              leafletOutput("chloropleth_emissions_per_capita"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage4", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Rect chart ####
                                     
                                     tabPanel("Rect chart",
                                              
                                              girafeOutput("rect_emissions_per_cap", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # GDP ####
                                     
                                     tabPanel("GDP",
                                              
                                              girafeOutput("scatterplot_emissions_gdp", width = "90%"),

                                              absolutePanel(
                                                wellPanel(
                                                  uiOutput("scatterplot_emissions_gdp_year"),
                                                  style = "z-index: 10; opacity: 0.65; padding-top:5px; padding-bottom:5px;"
                                                ),
                                                bottom = "24%", right = "20%", width = "20%", fixed = T, draggable = T
                                              ),
                                              
                                              absolutePanel(draggable = T,
                                                            wellPanel(style = "width:300px;",
                                                                      selectizeInput("selected_countries_gdp", "Highlight countries", choices = NULL, multiple = TRUE),
                                                                      style = "z-index: 10; opacity: 0.65; padding-top:5px; padding-bottom:5px;"
                                                            ),
                                                            top = "30%", left = "15%"
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Carbon budgets ####
                                     
                                     tabPanel("Carbon budgets",
                                              
                                              girafeOutput("scatterplot_carbon_budgets", width = "90%"),
                                            
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Justice approaches ####
                                     
                                     tabPanel("Justice approaches",
                                              
                                              fluidRow(
                                                column(8,
                                              
                                                  htmlOutput("approaches_table", style = "width:50vw;")
                                                  
                                                ),
                                                
                                                column(4,
                                                       
                                                       girafeOutput("exemplary_years_left")
                                                       
                                                )
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     
                                     # Years left ####
                                     
                                     tabPanel("Years left",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(style = "width:300px;",
                                                                      
                                                                      selectizeInput("selected_countries", "Select countries", choices = NULL, multiple = TRUE),
                                                                      
                                                                      sliderInput("base_year", "Select base year", min = 1960, max = 2018, value = 1992, step = 1, sep = ""),
                                                                      
                                                                      selectInput("selected_probability", "Select probability", choices = c("66%" = "66",
                                                                                                                                            "50%" = "50",
                                                                                                                                            "33%" = "33"),
                                                                                  selected = "66%"),
                                                                      
                                                                      selectInput("selected_warming_degrees", "Select warming degrees",
                                                                                  choices = c("1.27°C" = 1.27, "1.5°C" = 1.5, "1.75°C" = 1.75, "2°C" = 2),
                                                                                  selected = "1.5°C"),
                                                                      
                                                                      selectInput("selected_calculation_approach", "Select a calculation approach",
                                                                                  choices = c("Budget approach" = "budget",
                                                                                              "Grandfathering" = "grandfathering",
                                                                                              "Contraction and Convergence" = "convergence"),
                                                                                  selected = "pca")
                                                                      
                                                            ), style = "z-index: 10; opacity: 0.65;", top = "30%", left = "1%", fixed = T
                                                            
                                              ),
                                              
                                              girafeOutput("years_left"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                     ),
                                     
                                     # Budget left ####
                                     
                                     tabPanel("Budget left",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(style = "width:300px;",
                                                              
                                                              selectizeInput("selected_countries", "Select countries", choices = NULL, multiple = TRUE),
                                                              
                                                              sliderInput("base_year", "Select base year", min = 1960, max = 2018, value = 1992, step = 1, sep = ""),
                                                              
                                                              selectInput("selected_probability", "Select probability", choices = c("66%" = "66",
                                                                                                                                    "50%" = "50",
                                                                                                                                    "33%" = "33"),
                                                                          selected = "66%"),
                                                              
                                                              selectInput("selected_warming_degrees", "Select warming degrees",
                                                                          choices = c("1.27°C" = 1.27, "1.5°C" = 1.5, "1.75°C" = 1.75, "2°C" = 2),
                                                                          selected = "1.5°C"),
                                                              
                                                              selectInput("selected_calculation_approach", "Select a calculation approach",
                                                                          choices = c("Budget approach" = "budget",
                                                                                      "Grandfathering" = "grandfathering",
                                                                                      "Contraction and Convergence" = "convergence"),
                                                                          selected = "pca")
                                                              
                                                            ), style = "z-index: 10; opacity: 0.65;", top = "30%", left = "1%", fixed = T
                                                            
                                              ),
                                              
                                              girafeOutput("heatmap_budget_left_allyears"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                     ),
                                     
                                     # Leaders and laggards ####
                                     
                                     tabPanel("Leaders and laggards",
                                              
                                              girafeOutput("barchart_leaders_laggards"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage6", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                     )
                                     
                         )

                  )
                )
)