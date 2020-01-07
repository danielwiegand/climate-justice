
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
library(shinyWidgets)

useShinyjs()

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
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_temperature_text",
                                                                "Temperature anomalies are based on the HadCRUT4 land-sea dataset as published by the ",
                                                                tags$a(href = "https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html", "Met Office Hadley Centre"),
                                                                ".", tags$br(), "Temperature anomalies are given in degrees celcius relative to the average temperature over the period 1961-1990.
                                                                The median temperature anomaly, as well as the upper and lower bound anomalies (with a 95% confidence interval) 
                                                                are provided. "
                                                            )),
                                                            tags$div(id = "sources_temperature",
                                                                     icon("info-circle"),
                                                              tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
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
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_emissions_timeseries_text",
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
                                                                        "Displayed are only emissions of carbon dioxide (no other greenhouse gases included).", tags$br(), 
                                                                        "Emissions are attributed to the country / continent in which they physically occur."
                                                              )),
                                                            tags$div(id = "sources_emissions_timeseries",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
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
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_continent_emissions_text",
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
                                                                        "Displayed are only emissions of carbon dioxide (no other greenhouse gases included).", tags$br(), 
                                                                        "Emissions are attributed to the country / continent in which they physically occur."
                                                              )),
                                                            tags$div(id = "sources_continent_emissions",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                              
                                     ),
                                     
                                     # Leaflet ####
                                     
                                     tabPanel("Leaflet",
                                              
                                              leafletOutput("chloropleth_emissions_per_capita"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage4", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_chloropleth_text",
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
                                                                        "Displayed are only emissions of carbon dioxide (no other greenhouse gases included).", tags$br(), 
                                                                        "Emissions are attributed to the country / continent in which they physically occur."
                                                              )),
                                                            tags$div(id = "sources_chloropleth", style = "color:#5B5B5B;",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "5%", right = "2%"
                                              )
                                              
                                     ),
                                     
                                     # Rect chart ####
                                     
                                     tabPanel("Rect chart",
                                              
                                              girafeOutput("rect_emissions_per_cap", width = "90%"),
                                              
                                              absolutePanel(draggable = T,
                                                            wellPanel(style = "width:300px;",
                                                                      sliderInput("year_emissions_per_region", label = "Year", min = 1960, max = 2018, step = 1, value = 1960, animate = T, sep = ""),
                                                                      style = "z-index: 10; opacity: 0.65; padding-top:5px; padding-bottom:5px;"
                                                            ),
                                                            top = "30%", right = "15%"
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_rect_text",
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
                                                                        "Displayed are only emissions of carbon dioxide (no other greenhouse gases included).", tags$br(), 
                                                                        "Emissions are attributed to the country / continent in which they physically occur."
                                                              )),
                                                            tags$div(id = "sources_rect",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
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
                                                            top = "25%", left = "15%"
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_gdp_text",
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
                                                                        "Displayed are only emissions of carbon dioxide (no other greenhouse gases included).", tags$br(), 
                                                                        "Emissions are attributed to the country / continent in which they physically occur."
                                                              )),
                                                            tags$div(id = "sources_gdp",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                              
                                     ),
                                     
                                     # Carbon budgets ####
                                     
                                     tabPanel("Carbon budgets",
                                              
                                              girafeOutput("scatterplot_carbon_budgets", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_ipcc_text",
                                                                        "'Years left' are calculated under the assumption of constant global carbon dioxide emissions. Budgets refer
                                                                        to 1.1.2018.", tags$br(),
                                                                        "Emission budget data from IPCC' Special Report on Global Warming of 1.5°C (2018), 
                                                                        available at ", tags$a(href = "https://www.ipcc.ch/sr15", "https://www.ipcc.ch/sr15"), 
                                                                        "(budgets are displayed in chapter 2, table 2.2)."
                                                              )),
                                                            tags$div(id = "sources_ipcc",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                              
                                     ),
                                     
                                     # Justice approaches ####
                                     
                                     tabPanel("Justice approaches",
                                              
                                              fluidRow(
                                                column(4,
                                                       
                                                       absolutePanel(draggable = T,
                                                                     wellPanel(
                                                                       
                                                                       "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.",
                                                                       

                                                                       radioGroupButtons(
                                                                         inputId = "selected_justice_approach",
                                                                         label = "Future emission rights should be distributed based on...",
                                                                         choices = c("the present emission distribution" = "grandfathering", 
                                                                                     "equal shares for everybody" = "budget", 
                                                                                     "both present emissions and equal shares" = "convergence"),
                                                                         direction = "vertical",
                                                                         justified = F,
                                                                         selected = character(0),
                                                                         checkIcon = list(
                                                                           yes = icon("ok", 
                                                                                      lib = "glyphicon"))
                                                                       ),
                                                                       
                                                                       tags$label(class = "control-label", 'for' = "Id011",
                                                                                  "Past emissions should be taken into account"
                                                                       ),
                                                                       
                                                                       switchInput(
                                                                         inputId = "selection_past_emissions",
                                                                         size = "sm"
                                                                       ),
                                                                       style = "z-index: 10; opacity: 0.65; padding-top:5px; padding-bottom:5px;"
                                                                       
                                                                     ), top = "25%", left = "15%"
                                                       )
                                                       
                                                ),
                                                
                                                column(7,
                                                       
                                                       wellPanel(width = "90%", style = "z-index: 10; opacity: 0.65; padding-top:5px; padding-bottom:5px;",
                                                                 
                                                                 uiOutput("justice_approaches_heading"),
                                                                 
                                                                 fluidRow(
                                                                   column(6,
                                                                        uiOutput("justice_approaches_text")  
                                                                   ),
                                                                   column(6,
                                                                          hidden(
                                                                          girafeOutput("exemplary_years_left")
                                                                          )
                                                                   )
                                                                   
                                                                 )
                                                       )
                                                )
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_justice_approaches_text",
                                                                        "Countries displayed here were exemplarily chosen and represent industrialized (United States),
                                                                        newly industrialized (Mexico) and developing (Botswana) countries.", tags$br(),
                                                                        tags$b("Convergence & Contraction:"), "Approach developed by the ", tags$a(href = "http://www.gci.org.uk/",
                                                                                                                                                   "Global Commons Institute"), 
                                                                        ". Main assumption is that countries' per capita emissions converge in a (freely selecable) convergence year.", tags$br(),
                                                                        tags$b("Budget Approach:"), "Developed by ", tags$a(href = "https://www.wbgu.de/fileadmin/user_upload/wbgu/publikationen/archiv/wbgu_factsheet_3.pdf",
                                                                                                                            "WBGU"), 
                                                                        ". The global emissions budget is allocated on a per capita basis for all states.", tags$br(),
                                                                        tags$b("Grandfathering Approach:"), "The budget is allocated to states proportionally to their base year emissions.
                                                                        This approach is more or less implicitly adopted in most climate negotiations.", tags$br(),
                                                                        "Besides the approaches displayed here, several other approaches exist, e.g. the ", 
                                                                        tags$a(href = "https://www.tandfonline.com/doi/abs/10.1080/14693062.2016.1176006", "Regensburg Model"), 
                                                                        " (Sargl et al.) or the ", tags$a(href = "https://www.nature.com/articles/nclimate2384", "Extended Smooth Pathway Model"), "
                                                                        (Raupach et al.). Approaches displayed here were exemplarily chosen", tags$br(),
                                                                        "In contrast to the other approaches, \"Convergence and Contraction\" relies not only on an emission budget,
                                                                        but also on assumptions about future global emission paths. For the sake of example and simplicity, 
                                                                        a constant linear reduction of global emissions to zero is assumed, which is otherwise not realistic.", tags$br(),
                                                                        "Data from the ",
                                                                        tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas.")
                                                              )),
                                                            tags$div(id = "sources_justice_approaches",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                              
                                     ),
                                     
                                     
                                     # Years left ####
                                     
                                     tabPanel("Years left",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(style = "width:300px;",
                                                                      
                                                                      selectizeInput("selected_countries", "Select countries", choices = NULL, multiple = TRUE),
                                                                      
                                                                      sliderInput("base_year", "Select base year", min = 1960, max = 2018, value = 2018, step = 1, sep = ""),
                                                                      
                                                                      selectInput("selected_probability", "Select probability", choices = c("66%" = "66",
                                                                                                                                            "50%" = "50",
                                                                                                                                            "33%" = "33"),
                                                                                  selected = "66%"),
                                                                      
                                                                      selectInput("selected_warming_degrees", "Select warming degrees",
                                                                                  choices = c("1.27°C" = 1.27, "1.5°C" = 1.5, "1.75°C" = 1.75, "2°C" = 2),
                                                                                  selected = 1.5),
                                                                      
                                                                      selectInput("selected_calculation_approach", "Select a calculation approach",
                                                                                  choices = c("Budget approach" = "budget",
                                                                                              "Contraction and Convergence" = "convergence",
                                                                                              "Grandfathering" = "grandfathering"),
                                                                                  selected = "Budget approach")
                                                                      
                                                            ), style = "z-index: 10; opacity: 0.65;", top = "25%", left = "4%", fixed = T
                                                            
                                              ),
                                              
                                              girafeOutput("years_left"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_years_left_text",
                                                                        "Assumption: Country emissions remain constant at level of the selected base year. Note that selection of an 
                                                                        earlier base year leads to cases where the budget is already depleted for some (mainly 
                                                                        industrialized countries)."
                                                              )),
                                                            tags$div(id = "sources_years_left",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                     ),
                                     
                                     # Budget left ####
                                     
                                     tabPanel("Budget left",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(style = "width:300px;",
                                                                      
                                                                      selectizeInput("selected_countries_2", "Select countries", choices = NULL, multiple = TRUE),
                                                                      
                                                                      sliderInput("base_year_2", "Select base year", min = 1960, max = 2018, value = NULL, step = 1, sep = ""),
                                                                      
                                                                      selectInput("selected_probability_2", "Select probability", choices = c("66%" = "66",
                                                                                                                                              "50%" = "50",
                                                                                                                                              "33%" = "33"),
                                                                                  selected = "66%"),
                                                                      
                                                                      selectInput("selected_warming_degrees_2", "Select warming degrees",
                                                                                  choices = c("1.27°C" = 1.27, "1.5°C" = 1.5, "1.75°C" = 1.75, "2°C" = 2),
                                                                                  selected = NULL),
                                                                      
                                                                      selectInput("selected_calculation_approach_2", "Select a calculation approach",
                                                                                  choices = c("Budget approach" = "budget",
                                                                                              "Contraction and Convergence" = "convergence",
                                                                                              "Grandfathering" = "grandfathering"),
                                                                                  selected = "Budget approach")
                                                                      
                                                            ), style = "z-index: 10; opacity: 0.65;", top = "25%", left = "4%", fixed = T
                                                            
                                              ),
                                              
                                              girafeOutput("heatmap_budget_left_allyears"),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_budget_left_text",
                                                                        "Assumption: Country emissions remain constant at level of the selected base year. Note that selection of an 
                                                                        earlier base year leads to cases where the budget is already depleted for some (mainly 
                                                                        industrialized countries)."
                                                              )),
                                                            tags$div(id = "sources_budget_left",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                     ),
                                     
                                     # Leaders and laggards ####
                                     
                                     tabPanel("Leaders and laggards",
                                              
                                              girafeOutput("barchart_leaders_laggards"),
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(style = "width:300px;",
                                                                      
                                                                      sliderInput("base_year_3", "Select base year", min = 1960, max = 2018, value = NULL, step = 1, sep = ""),
                                                                      
                                                                      selectInput("selected_probability_3", "Select probability", choices = c("66%" = "66",
                                                                                                                                              "50%" = "50",
                                                                                                                                              "33%" = "33"),
                                                                                  selected = "66%"),
                                                                      
                                                                      selectInput("selected_warming_degrees_3", "Select warming degrees",
                                                                                  choices = c("1.27°C" = 1.27, "1.5°C" = 1.5, "1.75°C" = 1.75, "2°C" = 2),
                                                                                  selected = NULL),
                                                                      
                                                                      selectInput("selected_calculation_approach_3", "Select a calculation approach",
                                                                                  choices = c("Budget approach" = "budget",
                                                                                              "Contraction and Convergence" = "convergence",
                                                                                              "Grandfathering" = "grandfathering"),
                                                                                  selected = "Budget approach")
                                                                      
                                                            ), style = "z-index: 10; opacity: 0.65;", top = "25%", left = "4%", fixed = T
                                                            
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardPage6", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                     )
                                     
                                     # Conclusio ####
                                     
                                     # tabPanel("Conclusio"
                                     #        
                                     #          
                                     # ),
                                     
                         )
                         
                  )
                )
)