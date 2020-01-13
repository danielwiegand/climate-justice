server <- function(input, output, session) {
  
  # Contraction stimmt noch nicht ganz
  # - Klar machen, dass es um Budgets geht, nicht um Zielerreichung
  # EVTL WAS MIT FINANCIAL COMPENSATION? Ein Topf, und es wird dargestellt, wer wieviel einzahlt und wieviel rausbekommt?

  # READ DATA ####
  
  carbon_budgets <- importIPCCData()
  
  countries_emissions <- importCountriesEmissionData()
  
  continents_emissions <- importContinentsEmissionData()
  
  historical_temperatures <- read.csv("src/ourworldindata/temperature-anomaly.csv", stringsAsFactors = F) %>%
    filter(entity == "Global") %>%
    select(-entity) %>%
    mutate(data_id = "asd")
  
  global_emissions <- countries_emissions %>%
    group_by(year) %>%
    summarize(emissions = sum(emissions, na.rm = T))
  
  projection_data <- read.csv("src/climateactiontracker/EmissionsGaps_mod.csv", stringsAsFactors = F)
  
  # INPUTS ####
  
  base_year <- reactive({
    input$base_year
  })
  
  selected_probability <- reactive({
    input$selected_probability
  })
  
  selected_warming_degrees <- reactive({
    input$selected_warming_degrees
  })
  
  calculation_approach <- reactive({
    input$selected_calculation_approach
  })
  
  selected_countries <- reactive({
    input$selected_countries
  })
  
  # OPTIONS ####
  
  options(scipen = 999) # Disable scientific notation
  
  # IMPORTANT VARIABLES ####
  
  # Year where the IPCC budgets used were published
  year_ipcc_budgets <- 2018 # For the IPCC SR1.5 report
  
  # Minimum and maximum years of available emissions data
  maximum_year <- max(countries_emissions$year)
  minimum_year <- min(countries_emissions$year)
  
  total_country_emis_since_base_year <- reactive({
    countries_emissions %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      mutate(emissions = ifelse(year == base_year(), 0, emissions)) %>% # Base year is not part of the period the remaining budget applies to
      summarize(total_emis_since_by_gt = sum(emissions)/1000000000) %>%
      ungroup()
  })
  
  cumulated_country_emis_since_base_year <- reactive({
    countries_emissions %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      mutate(emissions = ifelse(year == base_year(), 0, emissions)) %>% # Base year is not part of the period the remaining budget applies to
      mutate(cumulated_emis_since_by_gt = cumsum(emissions)/1000000000) %>%
      ungroup() %>%
      select(country, year, cumulated_emis_since_by_gt)
  })
  
  
  # Total global emissions between base year and year of IPCC budget publication
  total_global_emis_since_base_year <- reactive({
    global_emissions %>%
      filter(year > base_year() & year <= year_ipcc_budgets) %>% # Base year is not part of the period the remaining budget applies to
      summarize(total_global_emis = sum(emissions)) %>%
      as.numeric()
  })
  
  global_emissions_base_year <- reactive({
    global_emissions %>%
      filter(year == base_year()) %>%
      select(emissions) %>%
      as.numeric()
  })
  
  selected_carbon_budget_sr15 <-  reactive({
    carbon_budgets %>%
      filter(probability == selected_probability(),
             warming_degrees == selected_warming_degrees()) %>%
      select(budget_gt) %>%
      as.numeric()
  })
  
  
  # All IPCC carbon budgets related to the base year (incl. additional emissions between base year and year of IPCC budget publication)
  carbon_budgets_at_base_year <- reactive({
    carbon_budgets %>%
      mutate(budget_gt = budget_gt + total_global_emis_since_base_year() / 1000000000)
  })
  
  
  selected_carbon_budget_at_base_year <- reactive({
    carbon_budgets_at_base_year() %>%
      filter(probability == selected_probability(),
             warming_degrees == selected_warming_degrees()) %>%
      select(budget_gt) %>%
      as.numeric()
  })
  
  ggplot_transparent_theme <- theme_test() +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      panel.border = element_rect(colour = "transparent", fill = NA, size = 0),
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      title = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
    )
  
  country_list <- unique(countries_emissions[["country"]])
  
  # VISUALIZATIONS OF STATUS QUO ####
  
    # Line chart: Historical temperatures ####
    
    linechart_temperatures <- prepareLineChartTemperatures(
      data = historical_temperatures, 
      theme = ggplot_transparent_theme
    ) %>%
      makeLabelArrows(x = 1965, xend = 1970, y = .3, yend = .1, label = "Temporary cooling, possibly\n man-made according to studies") %>%
      makeLabelArrows(x = 1985, xend = 1990, y = .6, yend = .4, label = "Rapid warming since the 90s") %>%
      makeLabelArrows(x = 1860, xend = 1865, y = .2, yend = .01, label = "This line marks the average \nbetween 1961 and 1990")
    
    output$linechart_temperatures <- renderGirafe(
      girafe(ggobj = linechart_temperatures, width_svg = 10, height_svg = 5) %>%
        girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0") )
    )
    
    # Iframe: Sea level rise ####
    
    output$iframe_sea_level <- renderUI({
      tags$iframe(src = "https://seeing.climatecentral.org/#12/40.7298/-74.0070?show=lockinAnimated&level=0&unit=feet&pois=hide", style = "height:70vh; width:70vw;")
    })
    
    # Bar chart: Emissions per continent + year ####
    
    barchart_continents <- prepareBarChartContinents(data = continents_emissions, theme = ggplot_transparent_theme) %>%
      makeLabelArrows(x = 2006, xend = 2008.8, y = 36, yend = 31, label = "Financial crisis 2009 lets\nemissions decline temporarily") %>%
      makeLabelArrows(x = 1980, xend = 1982, y = 22, yend = 19, label = "Oil crisis") %>%
      makeLabelArrows(x = 1989, xend = 1991, y = 26, yend = 23, label = "Dissolution of the Soviet Union")
    
    output$barchart_continents <- renderGirafe({
      girafe(ggobj = barchart_continents, width_svg = 10, height_svg = 5)
    })
    
    
    # Line chart: Emissions per continent + year ####
    
    linechart_continents <- prepareLineChartContinents(continents_emissions, theme = ggplot_transparent_theme) %>%
      makeLabelArrows(x = 2002, xend = 2005, y = 12.8, yend = 10.8, label = "China's emissions start \n to rise drastically") %>%
      makeLabelArrows(x = 1989, xend = 1992, y = 9.2, yend = 7.4, label = "Europe's emissions decline after \n the dissolution of Soviet Union") %>%
      makeLabelArrows(x = 2006, xend = 2008.8, y = 9, yend = 6.8, label = "Financial crisis 2009 lets\nemissions decline temporarily")
    
    output$linechart_continents <- renderGirafe(
      girafe(ggobj = linechart_continents, width_svg = 10, height_svg = 5) %>%
        girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none;") )
    )
    
    
    # Chloropleth: Emissions per capita and country in the most recent (maximum) year ####
    
    world_map <- loadWorldMap(quality = "medium")
    
    world_map_emission_per_capita <- countries_emissions %>%
      mutate(emissions = emissions / 1000000) %>% # Convert to Million tons
      filter(year == maximum_year)
    
    world_map_emission_per_capita <- sp::merge(world_map, world_map_emission_per_capita, by.x = "admin", by.y = "country")
    
    palette_world_map_emission_per_capita <- colorNumeric(palette = c("white", "darkred", "slateblue3"),
                                                          domain = world_map_emission_per_capita@data$emissions_per_cap, na.color = "lightgrey")
    
    tooltip_world_map_emission_per_capita <- paste(
      world_map_emission_per_capita@data$admin, ": ",
      sprintf("%.1f", round(world_map_emission_per_capita@data$emis_per_capita, 1), nsmall = 0, big.mark = " ", scientific = F),
      " t CO2 per capita (",
      world_map_emission_per_capita@data$year, ")",
      sep = "")
    
    output$chloropleth_emissions_per_capita <- renderLeaflet(
      createChloroplethChart(
        data = world_map_emission_per_capita,
        data_column = "emis_per_capita",
        palette = palette_world_map_emission_per_capita,
        tooltip = tooltip_world_map_emission_per_capita,
        chart_title = "Emissions per <br>capita (t CO2)"
      )
    )
    
    # Rect chart: Emissions per capita and continent ####
    
    cols_emissions_per_cap_continent <- colorRampPalette(colors = brewer.pal(9, "Paired")[4:9])
    
    chart_emissions_per_cap_continents <- reactive({
      continents_emissions %>%
        filter(year == input$year_emissions_per_region) %>%
        mutate(population = population / 1000000000) %>%
        arrange(-emis_per_capita) %>%
        mutate(emis_per_capita = round(emis_per_capita, 1),
               population_perc = round(population / sum(population, na.rm = T) * 100, 1),
               emission_perc = round(emissions / sum(emissions, na.rm = T) * 100, 1),
               xmax = cumsum(population),
               xmin = lag(xmax, default = 0)) %>%
        mutate(pos_label = rowMeans(select(., c("xmin", "xmax")))) %>%
        createRectPlotEmissionsRegion(
          data = .,
          theme = ggplot_transparent_theme,
          cols = cols_emissions_per_cap_continent,
          year = input$year_emissions_per_region,
          pos_label = pos_label
        )
    })
    
    output$rect_emissions_per_cap <- renderGirafe(
      girafe(ggobj = chart_emissions_per_cap_continents(), width_svg = 10, height_svg = 5)
    )
    
    
    
    # Scatterplot: Emissions per GDP ####
    
    output$scatterplot_emissions_gdp_year <- renderUI({
      sliderInput("animate_emissions_gdp", label = "", min = 1971, max = 2018, step = 1, value = 1971, animate = T, sep = "")
    })
    
    scatterplot_emissions_gdp_palette <- colorRampPalette(brewer.pal(9, "Paired"))
    
    scatterplot_emissions_gdp_colors <- setNames(scatterplot_emissions_gdp_palette(length(country_list)), 
                                                 country_list) # To keep colors constant
    scatterplot_emissions_gdp_xaxis_limits <- summary(countries_emissions$gdp_per_capita[is.finite(countries_emissions$gdp_per_capita)])[c("Min.", "Max.")]
    scatterplot_emissions_gdp_yaxis_limits <- summary(countries_emissions$emis_per_capita[is.finite(countries_emissions$emis_per_capita)])[c("Min.", "Max.")]
    scatterplot_emissions_gdp_scale_size_limits <- summary(countries_emissions$emissions[is.finite(countries_emissions$emissions)])[c("Min.", "Max.")]
    
    scatterplot_emissions_gdp <- reactive({
      req(input$animate_emissions_gdp)
      countries_emissions %>%
        mutate(label = ifelse(country %in% input$selected_countries_gdp, country, "")) %>%
        filter(is.finite(gdp_per_capita),
               year == input$animate_emissions_gdp) %>%
        prepareScatterPlotGDP(data = ., 
                              theme = ggplot_transparent_theme,
                              limits_x = scatterplot_emissions_gdp_xaxis_limits,
                              limits_y = scatterplot_emissions_gdp_yaxis_limits,
                              limits_scale = scatterplot_emissions_gdp_scale_size_limits,
                              colors = scatterplot_emissions_gdp_colors)
    })
    
    output$scatterplot_emissions_gdp <- renderGirafe({
      girafe(ggobj = scatterplot_emissions_gdp(), width_svg = 10, height_svg = 5) %>%
        girafe_options(opts_toolbar(position = "top"))
    })
    
    
    # Point chart: Remaining budgets ####
    
    # Global emission budgets as defined in the IPCC SR1.5 report
    global_emissions_sr15_gt <- reactive({
      global_emissions %>%
        filter(year == 2018) %>%
        select(emissions) %>%
        mutate(emissions = emissions / 1000000000) %>%
        as.numeric()
    })
    
    scatterplot_carbon_budgets <- reactive({
      carbon_budgets %>%
        mutate(probability = as.character(probability),
               data_id = as.character(row_number()),
               years_left = budget_gt / global_emissions_sr15_gt()) %>%
        prepareScatterPlotBudgets(data = ., theme = ggplot_transparent_theme)
    })
    
    output$scatterplot_carbon_budgets <- renderGirafe({
      girafe(ggobj = scatterplot_carbon_budgets(), width_svg = 10, height_svg = 5)
    })
    
    # Justice Approaches compared ####
    
    # Heading of the main box
    output$justice_approaches_heading <- renderUI({
      req(input$selected_justice_approach)
      if(input$selected_justice_approach == "budget") {
        tags$div(tags$h3("Budget Approach"), style = "text-align:center;")
      } else if(input$selected_justice_approach == "convergence") {
        tags$div(tags$h3("Convergence and Contraction"), style = "text-align:center;")
      } else if(input$selected_justice_approach == "grandfathering") {
        tags$div(tags$h3("Grandfathering Approach"), style = "text-align:center;")
      } else if(input$selected_justice_approach == "other") {
        tags$div(tags$h3("Other approaches"), style = "text-align:center;")
      }
    })
    
    # Output: Text in the main box
    output$justice_approaches_text <- renderUI({
      tags$div(id = "justice_approaches_content",
               hidden(tags$div(id = "budget_content", # Gets visible by means of an observer when user selects this justice approach
                               tags$h4("Intuition", style = "font-variant:small-caps;"),
                               "Every human has an equal right to emissions, regardless of nationality.",
                               tags$h4("How it works", style = "font-variant:small-caps; margin-top:30px;"),
                               "The remaining budget is allocated based on equal per capita emissions.",
                               tags$h4("Consequences", style = "font-variant:small-caps; margin-top:30px;"),
                               "Industrialized states currently exceed their budget and accumulate large 'carbon debts'
                                                                                            due to their high per capita emissions. Developing states are allocated large emission
                                                                                            budgets. If historic emissions are taken into account, budgets of industrialized countries
                                                                                            often are already exceeded by now."
               )),
               hidden(tags$div(id = "convergence_content",
                               tags$h4("Intuition", style = "font-variant:small-caps;"),
                               "Every human has an equal right to emissions, but we should implement it gradually.",
                               tags$h4("How it works", style = "font-variant:small-caps; margin-top:30px;"),
                               "Initially, the remaining global budget is distributed based on the existing distribuion ('grandfathering'). Over the years, this is gradually replaced by a distribution based on equal per capita emissions.",
                               tags$h4("Consequences", style = "font-variant:small-caps; margin-top:30px;"),
                               "Industrialized countries have to reduce their emissions, but not as drastically as with the Budget Approach. Developing countries can moderately increase their emissions."
               )),
               hidden(tags$div(id = "grandfathering_content",
                               tags$h4("Intuition", style = "font-variant:small-caps;"),
                               "Budgets should be allocated based on pragmatic considerations. The current distribution of emissions is not totally unjust.",
                               tags$h4("How it works", style = "font-variant:small-caps; margin-top:30px;"),
                               "Each country's share on the global emissions is held constant.",
                               tags$h4("Consequences", style = "font-variant:small-caps; margin-top:30px;"),
                               "Economic development of developing countries could be hindered, as they have to hold their (low) share on global emissions constant. Developed countries are in a comfortable situation."
               )),
               hidden(tags$div(id = "other_content", # Gets visible by means of an observer when user selects this justice approach
                               tags$br(), "We might think of other approaches to allocate the carbon budget:",
                               tags$br(), tags$br(), 
                               tags$ul(tags$li(
                                 "We could distribute emission rights based on the ", tags$u("capabilities"), " of states to reduce emissions, and use a metric such as GDP to approximate this capability"
                               ),
                               tags$li(
                                 "We could have a closer look at the ", tags$u("individual situation"), " states are in: Are they located in geographical latitude where their energy consumption is
                                 necessarily higher? Did they just face a civil war and need more resources to recover?"
                                 ),
                               tags$li(
                                 "Instead of looking at where emissions accrue, we could look at ", tags$u("on whose behalf"), " they accrue - a large portion of China's emissions, for example,
                                 accrues for products which are sold in Europe or the US! In this so called ", tags$a(href = "https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions#consumption-based-trade-adjusted-co2-emissions",
                                 "\"consumption-based view\""), ", even more emissions would be attributed to industrialized countries."
                               
                               )
                               ), tags$br(), tags$br()
               ))
      )
    })
    
    # Observers which trigger appearance of content based on user selection
    observeEvent(input$selected_justice_approach, {
      req(input$selected_justice_approach)
      if(input$selected_justice_approach == "budget") {
        shinyjs::hide("convergence_content")
        shinyjs::hide("grandfathering_content")
        shinyjs::hide("other_content")
        shinyjs::show("budget_content")
        shinyjs::show("exemplary_years_left")
        updateSelectInput(session, "selected_calculation_approach", selected = "budget")
      } else if(input$selected_justice_approach == "convergence") {
        shinyjs::hide("budget_content")
        shinyjs::hide("grandfathering_content")
        shinyjs::hide("other_content")
        shinyjs::show("convergence_content")
        shinyjs::show("exemplary_years_left")
        updateSelectInput(session, "selected_calculation_approach", selected = "convergence")
      } else if(input$selected_justice_approach == "grandfathering") {
        shinyjs::hide("convergence_content")
        shinyjs::hide("budget_content")
        shinyjs::hide("other_content")
        shinyjs::show("grandfathering_content")
        shinyjs::show("exemplary_years_left")
        updateSelectInput(session, "selected_calculation_approach", selected = "grandfathering")
      } else if(input$selected_justice_approach == "other") {
        shinyjs::hide("convergence_content")
        shinyjs::hide("budget_content")
        shinyjs::hide("grandfathering_content")
        shinyjs::hide("exemplary_years_left")
        shinyjs::show("other_content")
        # If "other" is selected, choose the Budget Approach as calculation method and set base year to 1990 (so that the next page "Years left" starts with this combination)
        updateSelectInput(session, "selected_calculation_approach", selected = "budget")
        updateSliderInput(session, "base_year", value = 1990)
      }
    })

    # When the user selects the inclusion of past emissions, set 1990 as base year
    observe({
      if(input$selection_past_emissions == T) {
        updateSliderInput(session, "base_year", value = 1990)
        shinyjs::hide("text_historical_emissions_no")
        shinyjs::show("text_historical_emissions_yes")
      } else {
        updateSliderInput(session, "base_year", value = maximum_year)
        shinyjs::hide("text_historical_emissions_yes")
        shinyjs::show("text_historical_emissions_no")
      }
    })
    
    # Create texts below the slider for historical emissions
    output$text_historical_emissions_yes <- renderUI({
      paste0("Allocation starts in ", base_year(), " ", ifelse(base_year() == 1990, "(this is the year where climate change became a problem widely known)", ""))
    })
    output$text_historical_emissions_no <- renderUI({
      paste0("Allocation starts in ", base_year())
    })
    
    
    # Bar chart: Approaches compared ####
    # Small bar chart with exemplary changes to states' budgets
    
    selected_justice_approach_text <- reactive({
      if(input$selected_justice_approach == "budget") {
        "Budget Approach"
      } else if(input$selected_justice_approach == "convergence") {
        "Convergence and Contraction"
      } else {
        "Grandfathering Approach"
      }
    })
    
    barchart_exemplary_years_left <- reactive({
      just_emission_budgets_countries() %>%
        filter(country %in% c("United States", "Mexico", "Botswana")) %>%
        left_join(countries_emissions) %>%
        mutate(emissions = emissions / 1000000000) %>%
        mutate(budget_reach = base_year() + floor(total_country_budget_gt / emissions)) %>%
        createBarChartExemplaryStates(
          data = .,
          theme = ggplot_transparent_theme,
          cols = cols_countries_years_left,
          selected_budget = selected_carbon_budget_sr15(),
          start_year = maximum_year
        )
    })
    
    output$exemplary_years_left <- renderGirafe({
      girafe(ggobj = barchart_exemplary_years_left())
    })
    
    
    
    # Chart: Future projections
    
    projection_data_2100 <- projection_data %>% filter(year == 2100) %>%
      gather(-year, key = "key", value = "value") %>%
      filter(!key %in% c("optimistic_policy", "d2_median", "d15_median", "historical")) %>%
      mutate(split = str_detect(key, "high")) %>%
      rowwise() %>%
      mutate(cat = str_sub(key, 0, str_locate(key, "_")[1]-1)) %>%
      ungroup() %>%
      select(-key) %>%
      group_by(split) %>%
      spread(split, value) %>%
      rename(ymin = "FALSE", ymax = "TRUE") %>%
      mutate(xmin = year+1, xmax = year+3) %>%
      mutate(cat = c("Baseline: 4.1 - 4.8°C", "Current policies: 2.8 - 3.2°C", "1.5°C consistent: 1.3°C", "2°C consistent: 1.7 - 1.7°C", "Pledges & Targets: 2.5 - 2.8°C"),
             desc = paste0(round(ymin, 0), " to ", round(ymax, 0), " Gt yearly emissions"))
    
    ribbon_chart_projections <- createRibbonChartScenarios(
      data_ribbon = projection_data,
      data_column = projection_data_2100,
      theme = ggplot_transparent_theme
    )
    
    output$ribbon_chart_projections <- renderGirafe({
      girafe(ggobj = ribbon_chart_projections, width_svg = 10, height_svg = 5)
    })
    
  # CALCULATION OF JUST BUDGET ####
  
  population_percentages_allyears <- countries_emissions %>%
    group_by(year) %>%
    mutate(population_percentage = population / sum(population, na.rm = T)) %>%
    ungroup() %>%
    select(country, year, population_percentage)
  
    # Calculate global emissions pathway for Contraction & Convergence ####
    # Assumption: Global pathway is a linear path to zero
    
    calculated_zero_emissions_year <- reactive({
      base_year() + 2 * selected_carbon_budget_at_base_year() / (global_emissions_base_year() / 1000000000)
    })
    
    calculated_reduction_per_year <- reactive({
      (global_emissions_base_year() / 1000000000) ^ 2 / (2 * selected_carbon_budget_at_base_year())
    })
    
    global_future_pathway_linear <- reactive({
      data.frame(
        year = base_year() : calculated_zero_emissions_year(),
        cumulated_reductions_absolute = calculated_reduction_per_year(),
        global_emissions = global_emissions_base_year() / 1000000000
      ) %>%
        mutate(cumulated_reductions_absolute = ifelse(year == base_year(), 0, cumulated_reductions_absolute)) %>% # Base year: No reductions
        mutate(
          cumulated_reductions_absolute = cumsum(cumulated_reductions_absolute),
          global_emissions = global_emissions - cumulated_reductions_absolute,
          Ct = (year - base_year()) / (calculated_zero_emissions_year() - base_year())
        ) %>%
        select(-cumulated_reductions_absolute) %>%
        mutate(global_emissions = global_emissions * 1000000000)
    })
    
    # Per country: Assigned emission budgets (only for base year) ####
    
    # observe(print(
    #   # c(
    #   # just_emission_budgets_countries() %>%
    #   #   summarize(summe = sum(total_country_budget_gt, na.rm = T)),
    #   # selected_carbon_budget_at_base_year()
    #   # )
    #   head(global_future_pathway_linear() %>% arrange(-year))
    #   # global_emissions_base_year()
    # ))
    
    just_emission_budgets_countries <- reactive({
      
      if(input$selected_calculation_approach == "budget") {
        countries_emissions %>%
          left_join(population_percentages_allyears) %>%
          filter(year == base_year()) %>%
          mutate(total_country_budget_gt = population_percentage * selected_carbon_budget_at_base_year()) %>%
          select(country, year, total_country_budget_gt, data_id)
        
      } else if(input$selected_calculation_approach == "grandfathering") {
        countries_emissions %>%
          filter(year == base_year()) %>%
          mutate(emission_share = emissions / global_emissions_base_year()) %>% 
          mutate(total_country_budget_gt = emission_share * selected_carbon_budget_at_base_year()) %>%
          select(country, year, total_country_budget_gt, data_id)
        
      } else if(input$selected_calculation_approach == "convergence") {
        # Very simple approach: Countries' population share is assumed to be constant between base and target year.
        # Linear version of Convergence & Contraction: Linear transition from allocation according to base year emissions to alloc. acc. to population share
        countries_emissions %>%
          left_join(population_percentages_allyears) %>%
          filter(year == base_year()) %>%
          select(country, emissions, population_percentage, data_id) %>%
          rename(emission_forecast = emissions) %>%
          sp::merge(global_future_pathway_linear()) %>%
          group_by(country) %>%
          arrange(year) %>%
          mutate(emission_forecast = calculateContractionConvergence(emission_forecast, Ct, global_emissions, population_percentage)) %>%
          mutate(total_country_budget_gt = sum(emission_forecast[-1]) / 1000000000) %>% # Base year emissions (row 1) do not count to the remaining budget
          ungroup() %>%
          filter(year == base_year()) %>%
          select(country, year, total_country_budget_gt, data_id)
      }
    })
    
    # Per country: Budget still left per year ####
    
    just_emission_budgets_countries_left <- reactive({
      countries_emissions %>%
        filter(year >= base_year()) %>%
        left_join(just_emission_budgets_countries() %>% select(-year, -data_id)) %>% # Join with total budget per country for the base year
        left_join(cumulated_country_emis_since_base_year()) %>%
        mutate(budget_left = total_country_budget_gt - cumulated_emis_since_by_gt,
               budget_left_perc = budget_left / total_country_budget_gt * 100) %>%
        select(country, year, total_country_budget_gt, budget_left, budget_left_perc, data_id)
    })
    
  # VISUALIZATIONS OF CALCULATIONS ####
  
    # Barchart: How many years are left per country? ####
    
    cols_countries_years_left <- colorRampPalette(colors = brewer.pal(9, "Paired"))
    
    barchart_countries_years_left <- reactive({
      req(selected_countries(), just_emission_budgets_countries(), total_country_emis_since_base_year())
      just_emission_budgets_countries() %>%
        filter(country %in% c(selected_countries())) %>%
        left_join(total_country_emis_since_base_year()) %>%
        left_join(countries_emissions) %>%
        mutate(emissions = emissions / 1000000000) %>%
        mutate(budget_reach = base_year() + floor(total_country_budget_gt / emissions)) %>%
        createBarChartYearsLeft(
          data = .,
          base_year = base_year(),
          theme = ggplot_transparent_theme,
          cols = cols_countries_years_left(length(selected_countries()))
        )
    })
    
    output$years_left <- renderGirafe({
      girafe(ggobj = barchart_countries_years_left(), width_svg = 10, height_svg = 4.5)
    })
    
    # Heatmap: How many of the just budget is left? (for all years between base year and maximum year) ####
    
    heatmap_just_budget_scales <- reactive({
      req(selected_countries(), just_emission_budgets_countries_left())
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        select(budget_left_perc) %>%
        createChartScales()
    })
    
    heatmap_budget_left_allyears <- reactive({
      req(just_emission_budgets_countries_left(), selected_countries())
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        createHeatmapEmissionBudgetLeft(data = .,
                                        scales = heatmap_just_budget_scales(),
                                        from = base_year(),
                                        to = maximum_year,
                                        theme = ggplot_transparent_theme)
    })
    
    output$heatmap_budget_left_allyears <- renderGirafe(
      girafe(ggobj = heatmap_budget_left_allyears(), width_svg = 10, height_svg = 4.5)
    )
    
    # Barchart: Budget left (percent), leaders & laggards (facets) ####
    
    barchart_leaders_laggards_scales <- reactive({
      if(sum(just_emission_budgets_countries_left()$budget_left_perc[just_emission_budgets_countries_left()$year == maximum_year] < 0, na.rm = T) > 0) {
        scale_fill_gradientn(colours = c("red", "white", "green"),
                             values = scales::rescale(c(-2000, 0, 100)))
      } else {
        scale_fill_gradientn(colours = c("tan1", "lightgreen", "darkgreen"),
                             values = scales::rescale(c(0, 10, 100)))
      }
    })
    
    barchart_leaders_laggards <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(year == maximum_year) %>%
        group_by(budget_left_perc < 0) %>%
        top_n(10, abs(budget_left_perc)) %>%
        ungroup() %>%
        head(20) %>% # If all countries have the same value, this ensures that not all countries are plotted, only the first 20
        mutate(country = reorder(country, budget_left_perc)) %>%
        createBarChartLeadersLaggards(
          data = .,
          theme = ggplot_transparent_theme,
          scales = barchart_leaders_laggards_scales()
        )
    })
    
    output$barchart_leaders_laggards <- renderGirafe(
      girafe(ggobj = barchart_leaders_laggards(), width_svg = 10, height_svg = 5)
    )
    
    
  # OBSERVERS ####
  
    # Trigger "Sources" ####
    
    onevent("click", "sources_temperature", toggle("sources_temperature_text"))
    onevent("click", "sources_emissions_timeseries", toggle("sources_emissions_timeseries_text"))
    onevent("click", "sources_continent_emissions", toggle("sources_continent_emissions_text"))
    onevent("click", "sources_chloropleth", toggle("sources_chloropleth_text"))
    onevent("click", "sources_rect", toggle("sources_rect_text"))
    onevent("click", "sources_gdp", toggle("sources_gdp_text"))
    onevent("click", "sources_ipcc", toggle("sources_ipcc_text"))
    onevent("click", "sources_justice_approaches", toggle("sources_justice_approaches_text"))
    onevent("click", "sources_years_left", toggle("sources_years_left_text"))
    onevent("click", "sources_budget_left", toggle("sources_budget_left_text"))
    onevent("click", "sources_laggards", toggle("sources_laggards_text"))
    onevent("click", "sources_scenarios", toggle("sources_scenarios_text"))
    
    
    # Sync all panels ####
    # In Shiny, you only can display an inputId once, but we need to have the same inputs (basie data for budget calculation) on several tabs
    # Thus, they have different inputIds, and we need this observer to synchronize them
    
    # Selected countries
    observe({
      updateSelectizeInput(
        session, inputId = "selected_countries", selected = c(input$selected_countries_2)
      )
    })
    observe({
      updateSelectizeInput(
        session, inputId = "selected_countries_2", selected = c(input$selected_countries)
      )
    })
    
    # Base year
    observe({
      updateSliderInput(
        session, inputId = "base_year", value = c(input$base_year_2)
      )
    })
    observe({
      updateSliderInput(
        session, inputId = "base_year", value = c(input$base_year_3)
      )
    })
    observe({
      updateSliderInput(
        session, inputId = "base_year_2", value = c(input$base_year)
      )
    })
    observe({
      updateSliderInput(
        session, inputId = "base_year_3", value = c(input$base_year_2)
      )
    })
    observe({
      updateSliderInput(
        session, inputId = "base_year__3", value = c(input$base_year)
      )
    })
    
    # Selected probability
    observe({
      updateSelectInput(
        session, inputId = "selected_probability", selected = c(input$selected_probability_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_probability", selected = c(input$selected_probability_3)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_probability_2", selected = c(input$selected_probability)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_probability_3", selected = c(input$selected_probability_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_probability__3", selected = c(input$selected_probability)
      )
    })
    
    # Warming degrees
    observe({
      updateSelectInput(
        session, inputId = "selected_warming_degrees", selected = c(input$selected_warming_degrees_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_warming_degrees", selected = c(input$selected_warming_degrees_3)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_warming_degrees_2", selected = c(input$selected_warming_degrees)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_warming_degrees_3", selected = c(input$selected_warming_degrees_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_warming_degrees_3", selected = c(input$selected_warming_degrees)
      )
    })
    
    # Calculation approach
    observe({
      updateSelectInput(
        session, inputId = "selected_calculation_approach", selected = c(input$selected_calculation_approach_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_calculation_approach", selected = c(input$selected_calculation_approach_3)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_calculation_approach_2", selected = c(input$selected_calculation_approach)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_calculation_approach_3", selected = c(input$selected_calculation_approach_2)
      )
    })
    observe({
      updateSelectInput(
        session, inputId = "selected_calculation_approach_3", selected = c(input$selected_calculation_approach)
      )
    })
    
    # Jump to next / previous page ####
    observeEvent(input$forwardToPage2,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Temperatures")
    })
    observeEvent(input$forwardToPage3,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Emissions")
    })
    observeEvent(input$forwardToPage4,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Consequences")
    })
    observeEvent(input$forwardToPage5,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Emitters")
    })
    observeEvent(input$forwardToPage6,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Per capita")
    })
    observeEvent(input$forwardToPage7,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "History")
    })
    observeEvent(input$forwardToPage8,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Affluence")
    })
    observeEvent(input$forwardToPage9,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Carbon budgets")
    })
    observeEvent(input$forwardToPage10,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Climate Justice")
    })
    observeEvent(input$forwardToPage11,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Years left")
    })
    observeEvent(input$forwardToPage12,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Budget left")
    })
    observeEvent(input$forwardToPage13,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Leaders")
    })
    observeEvent(input$forwardToPage14,{
      updateTabsetPanel(session, inputId = "tabset-panel", selected = "Conclusion")
    })
    
    # Create choice values for gdp scatterplot country selection ####
    
    observe({
      updateSelectizeInput(session, "selected_countries_gdp",
                           choices = country_list,
                           selected = c("China", "Mozambique", "Sweden"),
                           server = T
      )
    })
    
    
    # Create choice values for budget left country selection ####
    
    observe({
      updateSelectizeInput(session, "selected_countries",
                           choices = country_list,
                           selected = c("Australia", "Brazil", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
      )
    })
    
    observe({
      updateSelectizeInput(session, "selected_countries_2",
                           choices = country_list,
                           selected = c("Australia", "Brazil", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
      )
    })
    
    observe({
      updateSelectizeInput(session, "selected_countries_3",
                           choices = country_list,
                           selected = c("Australia", "Brazil", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
      )
    })
    
    # Images on start page ####
    
    value_startpage <- reactiveVal(0)
    
    observe({
      invalidateLater(10000, session)
      if(isolate(value_startpage() < 3)) {
        isolate(value_startpage(value_startpage() + 1))
      } else {
        isolate(value_startpage(1))
      }
    })
    
    observe({
      if(value_startpage() == 1) {
        shinyjs::show("startpage_image_1")
        shinyjs::hide("startpage_image_2")
        shinyjs::hide("startpage_image_3")
      } else if(value_startpage() == 2) {
        shinyjs::hide("startpage_image_1")
        shinyjs::show("startpage_image_2")
        shinyjs::hide("startpage_image_3")
      } else {
        shinyjs::hide("startpage_image_1")
        shinyjs::hide("startpage_image_2")
        shinyjs::show("startpage_image_3")
      }
    })
    
    # Images on consequences page ####
    
    value_consequences <- reactiveVal(0)
    
    observe({
      invalidateLater(7000, session)
      if(isolate(value_consequences() < 5)) {
        isolate(value_consequences(value_consequences() + 1))
      } else {
        isolate(value_consequences(1))
      }
    })
    
    observe({
      if(value_consequences() == 1) {
        shinyjs::show("consequences_1")
        shinyjs::hide("consequences_2")
        shinyjs::hide("consequences_3")
        shinyjs::hide("consequences_4")
        shinyjs::hide("consequences_5")
      } else if(value_consequences() == 2) {
        shinyjs::hide("consequences_1")
        shinyjs::show("consequences_2")
        shinyjs::hide("consequences_3")
        shinyjs::hide("consequences_4")
        shinyjs::hide("consequences_5")
      } else if(value_consequences() == 3) {
        shinyjs::hide("consequences_1")
        shinyjs::hide("consequences_2")
        shinyjs::show("consequences_3")
        shinyjs::hide("consequences_4")
        shinyjs::hide("consequences_5")
      } else if(value_consequences() == 4) {
        shinyjs::hide("consequences_1")
        shinyjs::hide("consequences_2")
        shinyjs::hide("consequences_3")
        shinyjs::show("consequences_4")
        shinyjs::hide("consequences_5")
      } else {
        shinyjs::hide("consequences_1")
        shinyjs::hide("consequences_2")
        shinyjs::hide("consequences_3")
        shinyjs::hide("consequences_4")
        shinyjs::show("consequences_5")
      }
    })
    
    
}

