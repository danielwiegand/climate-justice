
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
                                                            
                                                            wellPanel(tags$b("Climate change is threatening the basis for sustained human life on earth.", style = "font-size:20px;"), tags$br(), tags$br(), "While often perceived as a matter
                                                                      of natural science (climate science) or politics (climate summits), its ethical aspects, especially in relation
                                                                      with considerations of justice, are often overlooked.", tags$br(), tags$br(),
                                                                      "Even though the demand for \"Climate Justice\" is emphatically expressed by movements such as \"Fridays for Future\",
                                                                      this notion seems to be used in quite a broad sense, and little seems to be known about what Climate Justice
                                                                      would mean in practice.", tags$br(), tags$br(), "This is the starting point for this introduction to Climate Justice.", tags$br(), tags$br(),
                                                                      tags$b("Go to the following page to learn what Climate Justice is about.", style = "font-size:20px;")),
                                                            style = "z-index: 100; opacity: 0.85; font-size:16px;", top = "20%", right = "16%", fixed = T, width = "22%", align = "justify"
                                                            
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardToPage2", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                     ),
                                     
                                     
                                     # Temperatures ####
                                     
                                     tabPanel("Temperatures",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("While subject to annual fluctuations, global temperature records overall show a marked increase during the
                                                                      last decades."),
                                                            style = "z-index: 10; opacity: 0.75;", top = "30%", left = "20%", fixed = T, width = "15%", align = "justify"
                                                            
                                              ),
                                              
                                              girafeOutput("linechart_temperatures", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardToPage3", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_temperature_text",
                                                                "This diagram shows the development of averaged annual near surface temperatures. Temperature anomalies are based 
                                                                on the HadCRUT4 land-sea dataset as published by the ",
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
                                     
                                     # Emissions ####
                                     
                                     tabPanel("Emissions",
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel("This rise in temperature is caused by human-made greenhouse gas emissions, which rise steadily since the
                                                            beginning of the industrial revolution. In the 20th century, this rise dramatically accelerated."),
                                                            style = "z-index: 10; opacity: 0.75;", top = "29%", left = "20%", fixed = T, width = "17%", align = "justify"
                                                            
                                              ),
                                              
                                              girafeOutput("barchart_continents", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardToPage4", "", icon = icon("chevron-right"), class = "scroll-button"),
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
                                                            
                                                            wellPanel(tags$b("The consequences of climate change for human living conditions are not always but mostly negative."), tags$br(), tags$br(), 
                                                            "Besides an increased occurrence of ", tags$a(href = "https://www.c2es.org/content/extreme-weather-and-climate-change/",
                                                            "extreme weather events"), " such as heat waves or hurricanes, rising sea levels pose a threat to coastal cities.", tags$br(), tags$br(),
                                                            "Find out how much global temperature rise affects a city such as New York!"),
                                                            style = "z-index: 10; opacity: 0.85;", top = "32%", left = "13%", fixed = T, width = "20%", align = "justify"
                                              ),
                                              
                                              htmlOutput("iframe_sea_level", width = "90%"),
                                              
                                              absolutePanel(
                                                actionButton("forwardToPage5", "", icon = icon("chevron-right"), class = "scroll-button"),
                                                top = "35%", right = "10%", fixed = T
                                              )
                                              
                                     ),
                                     
                                     # Emitters ####
                                     
                                     tabPanel("Emitters",
                                              
                                              girafeOutput("linechart_continents", width = "90%"),
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(tags$b("Who is responsible for climate change?"), "Looking at the figures, it becomes clear that greenhouse gas emissions are unequally distributed,
                                                                             and that the continents' shares change over time. Can we say that this distribution is unfair or unjust?"),
                                                            style = "z-index: 10; opacity: 0.75;", top = "30%", left = "21%", fixed = T, width = "20%", align = "justify"
                                              ),
                                              
                                              absolutePanel(
                                                actionButton("forwardToPage6", "", icon = icon("chevron-right"), class = "scroll-button"),
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
                                     
                                     # Per capita ####
                                     
                                     tabPanel("Per capita",
                                              
                                              leafletOutput("chloropleth_emissions_per_capita"),
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(tags$b("We have to look at the per capita emissions to judge the situation."), tags$br(), tags$br(),
                                                            "We can see that, while Asia's (mainly China's) emissions are on the rise, its per capita emissions are still lower than those of 
                                                                      countries such as Saudi Arabia or the United States of America. Thus, an average Asian still emits much 
                                                            less than an average European", tags$br(), tags$br(),
                                                            tags$b("We may have reasons to question the justice of this situation"), " - why should somebody (prima facie) have more rights 
                                                            to emit than somebody else?"),
                                                            style = "z-index: 10; opacity: 0.75;", top = "50%", right = "13%", fixed = T, width = "20%", align = "justify"
                                              ),
                                              
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
                                     
                                     # History ####
                                     
                                     tabPanel("History",
                                              
                                              girafeOutput("rect_emissions_per_cap", width = "90%"),
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(
                                                            tags$b("The situation gets more complicated when we look at past emissions"), ": Industrialized countries showed
                                                            high per capita emissions for many decades, while emissions of Asian or African countries mainly rose due to population
                                                            growth - at least until the 21st century.", tags$br(), tags$br(),
                                                            tags$b("Should we take historical emissions into account when judging the justice of the present situation?"), tags$br(), tags$br(),
                                                            chooseSliderSkin("Flat"),
                                                            sliderInput("year_emissions_per_region", label = "", min = 1960, max = 2018, step = 1, value = 1960, animate = T, sep = "")
                                                            ),
                                                            style = "z-index: 10; opacity: 0.75;", top = "30%", right = "20%", fixed = T, width = "20%", align = "justify"
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
                                     
                                     # Affluence ####
                                     
                                     tabPanel("Affluence",
                                              
                                              girafeOutput("scatterplot_emissions_gdp", width = "90%"),
                                              
                                              absolutePanel(
                                                wellPanel(
                                                  "There seems to be a relation between a country's per capita emissions and its affluence (as measured in 
                                                  terms of Gross Domestic Product).", tags$br(), tags$br(),
                                                  "Even if correlation does not imply causality, and even if this relation between emissions and affluence might change in future,
                                                  restricting a country's emissions might (directly or indirectly) also restrict its chances to prosper in future.", tags$br(), tags$br(),
                                                  tags$b("This makes clear that allocating emission budgets means also allocating affluence and chances."), tags$br(), tags$br(),
                                                  uiOutput("scatterplot_emissions_gdp_year"),
                                                  style = "z-index: 10; opacity: 0.75;", align = "justify"
                                                ),
                                                bottom = "24%", right = "15%", width = "20%", fixed = T, draggable = T
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
                                                                        "Note that both scales are logarithmic.", tags$br(), tags$br(),
                                                                        "Data from the ", tags$a(href = "http://www.globalcarbonatlas.org/en/content/welcome-carbon-atlas", "Global Carbon Atlas."),
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
                                              
                                              absolutePanel(draggable = T,
                                                            
                                                            wellPanel(
                                                             tags$b("What is there to distribute?"), "Climate science tells us that there is a limited budget of greenhouse gas emissions which must
                                                             not be exceeded if the worst consequences of climate change should be avoided.", tags$br(), tags$br(),
                                                             "Example: If we aim at an overall global warming of 1.5°C, and want to achieve this target with a probability of 66%,
                                                             we still could 420 Gt of carbon dioxide at the beginning of 2018. Under constant global 2018 emissions, this budget would
                                                             be used up after about 12 years.", tags$br(), tags$br(),
                                                             "How many degrees of warming we aim at is a political question.", tags$b("And how the remaining budget is to be distributed
                                                             is a question of Climate Justice.")
                                                            ),
                                                            style = "z-index: 10; opacity: 0.9;", top = "30%", right = "20%", fixed = T, width = "20%", align = "justify"
                                              ),
                                              
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
                                     
                                     # Climate Justice ####
                                     
                                     tabPanel("Climate Justice",
                                              
                                              fluidRow(
                                                column(4,
                                                       
                                                       absolutePanel(draggable = T,
                                                                     wellPanel(
                                                                       tags$b("What do we know so far?"), tags$br(), tags$br(),
                                                                       tags$ul(
                                                                         tags$li("Emissions have detrimental effects on the environment..."),
                                                                         tags$li("... but are also related to affluence and chances"),
                                                                         tags$li("To limit climate change, there is a emissions budget left"),
                                                                         tags$li("So far, per capita emissions are distributed unequally")
                                                                         ), tags$br(),
                                                                       "Now, you have to decide:", tags$br(), tags$br(),
                                                                       tags$b("1. Based on what should future emissions be distributed?"),
                                                                       tags$div(style = "margin-left:10px;",
                                                                         radioGroupButtons(
                                                                           inputId = "selected_justice_approach",
                                                                           label = "",
                                                                           choices = c("the present emission distribution" = "grandfathering", 
                                                                                       "equal shares for everybody" = "budget", 
                                                                                       "both present emissions and equal shares" = "convergence"),
                                                                           direction = "vertical",
                                                                           justified = F,
                                                                           selected = character(0),
                                                                           checkIcon = list(
                                                                             yes = icon("ok", 
                                                                                        lib = "glyphicon"))
                                                                       )
                                                                       ),
                                                                       
                                                                       tags$b("2. Should historical emissions be taken into account?"), tags$br(),
                                                                       
                                                                       tags$div(style = "margin-left:10px;",
                                                                         switchInput(
                                                                           inputId = "selection_past_emissions",
                                                                           size = "sm",
                                                                           onLabel = "Yes",
                                                                           offLabel = "No"
                                                                         ),
                                                                         style = "z-index: 10; opacity: 0.75; padding:19px; align:justify;"
                                                                       )
                                                                       
                                                                     ), top = "25%", left = "15%"
                                                       )
                                                       
                                                ),
                                                
                                                column(7,
                                                       
                                                       wellPanel(width = "90%", style = "z-index: 10; opacity: 1; padding-top:5px; padding-bottom:5px;",
                                                                 
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
                                              ),
                                              
                                              absolutePanel(class = "sources",
                                                            hidden(
                                                              wellPanel(class = "sources_panel", id = "sources_laggards_text",
                                                                        "Assumption: Country emissions remain constant at level of the selected base year.", tags$br(),
                                                                        "If all countries have the same percentage of budget left (e.g. when 2018 is selected as base year), the alphabetically
                                                                        first 20 countries are displayed."
                                                              )),
                                                            tags$div(id = "sources_laggards",
                                                                     icon("info-circle"),
                                                                     tags$u("Additional information / sources")
                                                            ),
                                                            bottom = "1%", right = "2%"
                                              )
                                     ),
                                     
                                     # Conclusio ####
                                     
                                     tabPanel("Conclusio",
                                              
                                              fluidRow(
                                                column(10,
                                                       
                                                       girafeOutput("ribbon_chart_projections"),
                                                       
                                                       absolutePanel(class = "sources",
                                                                     hidden(
                                                                       wellPanel(class = "sources_panel", id = "sources_scenarios_text",
                                                                                 "Data source: Climate Action Tracker", tags$br(), "(", 
                                                                                 tags$a(href = "https://climateactiontracker.org/global/temperatures/", "https://climateactiontracker.org/global/temperatures/"),
                                                                                 "). Note that emissions are expressed here in Gt CO2 equivalents instead of Gt CO2 (like in the charts before)."
                                                                       )),
                                                                     tags$div(id = "sources_scenarios",
                                                                              icon("info-circle"),
                                                                              tags$u("Additional information / sources")
                                                                     ),
                                                                     bottom = "1%", right = "2%"
                                                       )
                                                       
                                                ),
                                                
                                                # column(1),
                                                
                                                column(2,
                                                       
                                                       tags$h4("Credits", style = "margin-top:20px;"),
                                                       
                                                       tags$img(src = "daniel.jpg", style = "border:1px solid #FFF; width:70%; padding:5px;"), 
                                                       tags$div(style = "margin-top:20px; color:#b9b9b9; font-size: 13px;",
                                                         "Daniel Wiegand works as a CSR consultant and data scientist. Currently he is doing his doctorate in business ethics at the
                                                         university of philosophy in Munich.", tags$br(), tags$br(),
                                                         "For more information, refer to my ", tags$a(href = "https://danielwiegand.github.io/", "Personal website."), tags$br(), tags$br(),
                                                         "All code to create this website is available on my ", tags$a(href = "https://github.com/danielwiegand/climate-justice", "GitHub page."),
                                                         tags$br(), tags$br(), "For comments and suggestions contact me on ", tags$a(href = "mailto:climate-justice@posteo.de", "climate-justice@posteo.de.")
                                                       )
                                                       )
                                              )
                                     )
                                     
                         )
                         
                  )
                )
)