server <- function(input, output, session) {

  # SELECTIZE: https://shiny.rstudio.com/articles/selectize.html
  # BEISPIEL: https://www.business-science.io/assets/2019-11-22-docker/stock_analyzer_aws_deploy.jpg
  # Pfeile in ggplot mit kleinen Erläuterungen?
  # Tabellen: https://www.r-bloggers.com/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/
  
  # THE QUESTION (): 
  # - Foto von Klimaprotesten mit "Climate Justice"-Schild (What do we want?)
  # - Kurze Hinführung zur Frage, was Climate Justice ist, und Problematik von "Gerechtigkeit" anreißen
  # WORLD EMISSIONS
  # UNEQUAL CONTRIBUTIONS
  # - Is this unfair? We cannot say!
  # PER CAPITA
  # - Länder antippbar machen für mehr Informationen?
  # FOLIE: KORRELATIONEN. Wirtschatswachstum, Entwickllungsindex, Demokratieindex
  # FOLIE: Die Budget-Methodik erklären
  # - Evtl. gibts da bei IPCC was, im letzten Bericht? Oder auf der Webseite?
  # FEHLENDE FOLIE: Berechnungsarten und wie sie sich auswirken
  # - per capita: Einfache Form von G
  # - Regensburger Formel?
  # - per capabilities: In GDP gemessen
  # - Problematik: Scope 3. D produziert zB für viele andere Länder, England hat deindustrialisiert (Grafiken Industriequote?) Evtl. gezielter Ländervergleich hier
  # BUDGET LEFT
  # - Ermuntern zum Rumspielen
  # LEADERS AND LAGGARDS
  # EVTL WAS MIT FINANCIAL COMPENSATION? Ein Topf, und es wird dargestellt, wer wieviel einzahlt und wieviel rausbekommt?
  # EVTL WAS, DAS ZIELERREICHUNG ANZEIGT? (OUTLOOK, PFADE, SZENARIEN...)
  # FOLIE: WAS FOLGT DARAUS?
  # - etwas, das Hoffnung macht, Beispiele
  # - Ermunterung zum Handeln
  # - Material, um Druck zu machen
  
  
  # READ DATA ####
  
  raw_emissions <- importEmissionData()
  
  carbon_budgets <- importIPCCData()
  
  
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
  
  selected_countries <- c("Australia", "Brazil", "Burundi", "Canada", "China", "Chile", "Ethiopia", "France", 
                          "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates")


  # IMPORTANT VARIABLES ####

  # Year where the IPCC budgets used were published
  year_ipcc_budgets <- 2018

  countries <- raw_emissions %>%
    filter(!code == "",
           !country == "World") %>%
    mutate(data_id = as.character(row_number()))

  continents <- raw_emissions %>%
      # Leaving out the "Statistical differences" here, as of minor importance and sometimes negative
      filter(country %in% c("EU-28", "China", "India", "United States", "Asia and Pacific (other)", "Americas (other)", "Europe (other)", "International transport")) %>%
      mutate(data_id = as.character(row_number())) %>%
      rename(continent = country)

  # Minimum and maximum years of available emissions data
  maximum_year <- max(raw_emissions$year)
  minimum_year <- min(raw_emissions$year)

  total_country_emis_since_base_year <- reactive({
    countries %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      summarize(total_emis_since_by_gt = sum(emissions)/1000000000) %>%
      ungroup()
  })

  cumulated_country_emis_since_base_year <- reactive({
    countries %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      mutate(cumulated_emis_since_by_gt = cumsum(emissions)/1000000000) %>%
      ungroup() %>%
      select(country, year, cumulated_emis_since_by_gt)
  })

  # Total world emissions between base year and year of IPCC budget publication
  total_world_emis_since_base_year <- reactive({
    raw_emissions %>%
      filter(country == "World",
             year >= base_year() & year <= year_ipcc_budgets) %>%
      summarize(total_world_emis = sum(emissions)) %>%
      as.numeric()
  })

  # All IPCC carbon budgets related to the base year (incl. additional emissions between base year and year of IPCC budget publication)
  carbon_budgets_at_base_year <- reactive({
    carbon_budgets %>%
      mutate(budget_gt = budget_gt + total_world_emis_since_base_year() / 1000000000)
  })


  selected_carbon_budget_at_base_year <- reactive({
    carbon_budgets_at_base_year() %>%
      filter(probability == selected_probability(),
             warming_degrees == selected_warming_degrees()) %>%
      select(budget_gt) %>%
      as.numeric()
  })


  # VISUALIZATIONS OF STATUS QUO ####

    # Bar chart: Emissions per continent + year ####
  
    barchart_continents <- prepareBarChartContinents(data = continents)
  
    output$barchart_continents <- renderGirafe({
      girafe(ggobj = barchart_continents, width_svg = 10, height_svg = 5)
    })
  
  
    # Line chart: Emissions per continent + year ####
  
    linechart_continents <- prepareLineChartContinents(continents)
  
    output$linechart_continents <- renderGirafe(
      girafe(ggobj = linechart_continents, width_svg = 10, height_svg = 5) %>%
        girafe_options(opts_hover(css = "stroke:black; stroke-width:5px; fill:none;") )
    )
  
  
    # Chloropleth: Emissions per capita and country in the most recent (maximum) year ####
  
    world_map <- loadWorldMap(quality = "medium")
  
    world_map_emission_per_capita <- countries %>%
        mutate(emissions = emissions / 1000000) %>% # Convert to Million tons
        filter(year == maximum_year)
  
    world_map_emission_per_capita <- sp::merge(world_map, world_map_emission_per_capita, by.x = "admin", by.y = "country")
  
    palette_world_map_emission_per_capita <- colorNumeric(palette = c("white", "darkred", "slateblue3"),
                                                          domain = world_map_emission_per_capita@data$emissions_per_cap, na.color = "lightgrey")
  
    tooltip_world_map_emission_per_capita <- paste(
        world_map_emission_per_capita@data$admin, ": ",
        sprintf("%.1f", round(world_map_emission_per_capita@data$emissions_per_cap, 1), nsmall = 0, big.mark = " ", scientific = F),
        " t CO2 per capita (",
        world_map_emission_per_capita@data$year, ")",
        sep = "")
  
    output$chloropleth_emissions_per_capita <- renderLeaflet(
      createChloroplethChart(
        data = world_map_emission_per_capita,
        data_column = "emissions_per_cap",
        palette = palette_world_map_emission_per_capita,
        tooltip = tooltip_world_map_emission_per_capita,
        chart_title = "Emissions per capita (t CO2)"
      )
    )
  
  # CALCULATION OF JUST BUDGET ####

  population_percentages_allyears <- countries %>%
    group_by(year) %>%
    mutate(population_percentage = population / sum(population, na.rm = T)) %>%
    ungroup() %>%
    select(country, year, population_percentage)

  # Per country: Assigned emission budgets (only for base year)
  just_emission_budgets_countries <- reactive({
    if(input$selected_calculation_approach == "pca") {
      countries %>%
        left_join(population_percentages_allyears) %>%
        filter(year == base_year()) %>%
        mutate(total_country_budget_gt = population_percentage * selected_carbon_budget_at_base_year()) %>%
        select(country, year, total_country_budget_gt, data_id)
    }
  })

  # Per country: Budget still left per year
  just_emission_budgets_countries_left <- reactive({
    countries %>%
      filter(year >= base_year()) %>%
      left_join(just_emission_budgets_countries() %>% select(-year, -data_id)) %>% # Join with total budget per country for the base year
      left_join(cumulated_country_emis_since_base_year()) %>%
      mutate(budget_left = total_country_budget_gt - cumulated_emis_since_by_gt,
             budget_left_perc = budget_left / total_country_budget_gt * 100) %>%
      select(country, year, total_country_budget_gt, budget_left, budget_left_perc, data_id)
  })

  # VISUALIZATIONS OF CALCULATIONS ####

    # Barchart: Budget left (percent), leaders & laggards (facets) ####
    barchart_leaders_laggards <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(year == maximum_year) %>%
        group_by(budget_left_perc < 0) %>%
        top_n(10, abs(budget_left_perc)) %>%
        ungroup() %>%
        mutate(country = reorder(country, budget_left_perc)) %>%
        ggplot(aes(x = country, y = budget_left_perc, fill = budget_left_perc)) +
        geom_bar_interactive(aes(data_id = data_id, tooltip = paste0(country, ": ", round(budget_left_perc, 1), "% of budget left")), show.legend = FALSE, stat = "identity") +
        coord_flip() +
        theme_test() +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          panel.border = element_rect(colour = "transparent", fill = NA, size = 0),
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
          title = element_text(colour = "white"),
          axis.text = element_text(colour = "white"),
          axis.text.x = element_text(angle = 90),
          legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", colour = NA), # get rid of legend panel bg
          legend.text = element_text(colour = "white"),
          legend.key = element_blank() # Removes frame around boxes
        ) +
        facet_grid(~ budget_left > 0, scales="free", labeller = as_labeller(c("TRUE" = "Leaders", "FALSE" = "Laggards"))) +
        labs(title = "Leaders and laggards", subtitle = "Countries performing worst / best regarding their emission budget", x = "Country", y = "Budget left (%)") +
        scale_fill_gradientn(colours = c("red", "white", "green"),
                           values = scales::rescale(c(-2000, 0, 100)))
    })
    
    output$barchart_leaders_laggards <- renderGirafe(
      girafe(ggobj = barchart_leaders_laggards(), width_svg = 10, height_svg = 5)
    )
  
  
    # Heatmap: How many of the just budget is left? (for all years between base year and maximum year) ####
    
    heatmap_budget_left_allyears <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries) %>%
        heatmapEmissionBudgetLeft(data = .,
                                  scales = barchart_just_budget_scales(),
                                  from = base_year(),
                                  to = maximum_year)
    })
    
    output$heatmap_budget_left_allyears <- renderGirafe(
      girafe(ggobj = heatmap_budget_left_allyears(), width_svg = 10, height_svg = 4.5)
    )
  
    barchart_just_budget_scales <- reactive({
      createChartScales(data = just_emission_budgets_countries_left()$budget_left_perc, props = c(.4, .9, .1, .2))
    })
  
  # OBSERVERS ####
  
    # Jump to next / previous page ####
  observeEvent(input$forwardPage2,{
    updateTabsetPanel(session, inputId = "tabset-panel", selected = "Next page")
  })
  
}