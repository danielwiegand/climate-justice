server <- function(input, output, session) {

  # BEISPIEL: https://www.business-science.io/assets/2019-11-22-docker/stock_analyzer_aws_deploy.jpg
  # Pfeile in ggplot mit kleinen Erläuterungen?
  # Tabellen: https://www.r-bloggers.com/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/
  # Aktuellere Daten: http://www.globalcarbonatlas.org/en/CO2-emissions !!!!
  ## !!!!! CO2 data has been converted from tonnes of carbon to tonnes of carbon dioxide (CO₂) using a conversion factor of 3.664 !!!!!
  # Werte von globalcarbonatlas sind sicher CO2 - müssen für Budgetvergleich in C umgerechnet werden?
  
  # THE QUESTION (): 
  # - Foto von Klimaprotesten mit "Climate Justice"-Schild (What do we want?)
  # - Kurze Hinführung zur Frage, was Climate Justice ist, und Problematik von "Gerechtigkeit" anreißen
  # - Es geht auch um intergenerational justice (Brundtland)
  # - Vllt haben Staaten auch Rechte auf Ressourcen und Entwicklung
  # - A Perfect Moral Storm
  # KONTEXT KLIMAWANDEL: TEMPERATUREN
  # KONTEXT KLIMAWANDEL: FOLGEN (MEERESSPIEGEL?)
  # WORLD EMISSIONS
  # UNEQUAL CONTRIBUTIONS
  # - Is this unfair? We cannot say!
  # PER CAPITA
  # - Länder antippbar machen für mehr Informationen?
  # FOLIE: KORRELATIONEN. Wirtschatswachstum, Entwickllungsindex, Demokratieindex
  # - https://ourworldindata.org/uploads/2018/10/CO2-emissions-by-income-and-region.png
  # - Im  Allge-meinen  besteht  eine  enge  Korrelation  zwischen  Pro-Kopf-Emissionen  und  dem  Ein-kommen. 
  # - In den einigenLändern hat das BIP den stärksten Einfluss auf die Treibhausgasemissio-nen verglichen mit anderen Faktoren wie der Bevölkerungszahl, dem Einkommenoder dem Energiemix. Diese starke Korrelation wird in verschiedenen Ländern wie den Ver-einigten  Staaten,  Indonesien,  Indien,  Australien  und  dem  Iran  deutlich.  Doch  es  gibt auch  Beispiele  für  Länder,deren  Veränderungen  im  Treibhausgasausstoß  sich  nicht  in erster  Linie  mit  dem  BIP  erklären  läßt.  Hierzu  zählen  Russland  und  die  Ukraine. Ihr Rückgang  in  den  Emissionen in  den  vergangenen  Jahren wurde  zwar  auch  durch die Stilllegung  vieler Betriebe forciert. Laut  Baumert  et  al.dominieren  jedochUmstruktu-rierungen, die zu einer  Verminderung der Energieintensität geführt haben. Noch  gerin-ger  ist  der  Zusammenhang  zwischen  den  Veränderungen  des  BIP  und  dem  Treibhaus-gasausstoßim  Fall  von  Ländern  wie  Argentinien,  die  vorrangig  landwirtschaftlich  ge-prägt sind(Baumert / Herzog / Pershing 2005: 26).
  # FOLIE: Die Budget-Methodik erklären
  # - Evtl. gibts da bei IPCC was, im letzten Bericht? Oder auf der Webseite?
  # - Klar machen, dass es um Budgets geht, nicht um Zielerreichung
  # - Das Kohlenstoffbudget wird als endliches, weltweites Allmendegut angesehen**, dass es international aufzuteilen gilt.
  # FEHLENDE FOLIE: Berechnungsarten und wie sie sich auswirken
  # - per capita: Einfache Form von G
  # - Regensburger Formel?
  # - per capabilities: In GDP gemessen
  # - Problematik: Scope 3. D produziert zB für viele andere Länder, England hat deindustrialisiert (Grafiken Industriequote?) Evtl. gezielter Ländervergleich hier
  # BUDGET LEFT
  # - Ermuntern zum Rumspielen
  # BUDGET: VORAUSSCHAUEND
  # LEADERS AND LAGGARDS
  # EVTL WAS MIT FINANCIAL COMPENSATION? Ein Topf, und es wird dargestellt, wer wieviel einzahlt und wieviel rausbekommt?
  # EVTL WAS, DAS ZIELERREICHUNG ANZEIGT? (OUTLOOK, PFADE, SZENARIEN...)
  # - evtl ourworld in data, Future emissions
  # - Kann man vllt verschiedene Pfade anzeigen für 1.5°C, 2°C etc? Siehe zB IPCC Summary S. 14
  # FOLIE: WAS FOLGT DARAUS?
  # - etwas, das Hoffnung macht, Beispiele
  # - Ermunterung zum Handeln
  # - Material, um Druck zu machen
  
  
  # READ DATA ####
  
  carbon_budgets <- importIPCCData()
  
  historical_temperatures <- read.csv("src/ourworldindata/temperature-anomaly.csv", stringsAsFactors = F) %>%
    filter(entity == "Global") %>%
    select(-entity) %>%
    mutate(data_id = "asd")
  
  
  continents_emissions_per_capita <- read.csv("src/global_carbon_project/emis_per_capita_regions.csv",
                                              stringsAsFactors = F) %>%
    gather(-year, key = "continent", value = "emis_per_capita")
  
  continents_emissions <- read.csv("src/global_carbon_project/emis_regions.csv",
                                   stringsAsFactors = F) %>%
    gather(-year, key = "continent", value = "emissions") %>%
    left_join(continents_emissions_per_capita) %>%
    mutate(population = emissions / emis_per_capita * 1000000,
           continent = str_replace_all(continent, "\\.", " "),
           data_id = as.character(row_number()),
           emissions = emissions * 1000000) # Convert to kg CO2
  
  
  world_emissions = continents_emissions %>%
    select(year, emissions) %>%
    group_by(year) %>%
    summarize(emissions = sum(emissions, na.rm = T))
  
  
  countries_emissions_per_capita <- read.csv("src/global_carbon_project/emis_per_capita_countries.csv",
                                            stringsAsFactors = F) %>%
    gather(-year, key = "country", value = "emis_per_capita") %>%
    mutate(country = str_replace_all(country, "\\.", " "))
  
  countries_emissions_per_gdp <- read.csv("src/global_carbon_project/emis_per_gdp_countries.csv",
                                          stringsAsFactors = F) %>%
    gather(-year, key = "country", value = "emis_per_gdp") %>%
    mutate(country = str_replace_all(country, "\\.", " "))
  
  countries_emissions <- read.csv("src/global_carbon_project/emis_countries.csv",
                                  stringsAsFactors = F) %>%
    gather(-year, key = "country", value = "emissions") %>%
    mutate(country = str_replace_all(country, "\\.", " "),
           data_id = as.character(row_number()),
           emissions = emissions * 1000000) %>% # Convert to kg CO2
    left_join(countries_emissions_per_capita) %>%
    mutate(population = emissions / emis_per_capita) %>%
    left_join(countries_emissions_per_gdp) %>%
    mutate(gdp = emissions / emis_per_gdp * 1000,
           gdp_per_capita = gdp / population) %>% # Emissions per GDP are in kg CO2
    mutate(country = str_replace_all(country, c("United States of America" = "United States",
                                                "Democratic Republic of the Congo"= "Democratic Republic of Congo",
                                                "Republic of South Sudan" = "South Sudan",
                                                "Côte d Ivoire" = "Cote d'Ivoire",
                                                "Russian Federation" = "Russia",
                                                "Guinea Bissau" = "Guinea-Bissau")))
  
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

  # IMPORTANT VARIABLES ####

  # Year where the IPCC budgets used were published
  year_ipcc_budgets <- 2018

  # Minimum and maximum years of available emissions data
  maximum_year <- max(countries_emissions$year)
  minimum_year <- min(countries_emissions$year)

  total_country_emis_since_base_year <- reactive({
    countries_emissions %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      summarize(total_emis_since_by_gt = sum(emissions)/1000000000) %>%
      ungroup()
  })

  cumulated_country_emis_since_base_year <- reactive({
    countries_emissions %>%
      filter(year >= base_year()) %>%
      group_by(country) %>%
      mutate(cumulated_emis_since_by_gt = cumsum(emissions)/1000000000) %>%
      ungroup() %>%
      select(country, year, cumulated_emis_since_by_gt)
  })

  # Total world emissions between base year and year of IPCC budget publication
  total_world_emis_since_base_year <- reactive({
    world_emissions %>%
      filter(year >= base_year() & year <= year_ipcc_budgets) %>%
      summarize(total_world_emis = sum(emissions)) %>%
      as.numeric()
  })
  
  global_emissions_base_year <- reactive({
    world_emissions %>%
      filter(year == base_year()) %>%
      select(emissions) %>%
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
  
  ggplot_transparent_theme <- theme_test() +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      panel.border = element_rect(colour = "transparent", fill = NA, size = 0),
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      title = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.text.x = element_text(angle = 90)
      )
  
  # VISUALIZATIONS OF STATUS QUO ####

    # Line chart: Historical temperatures ####
  
  linechart_temperatures <- historical_temperatures %>%
    ggplot() +
    geom_ribbon(aes(x = year, ymax = upper, ymin = lower), fill = "grey40", alpha = .5) +
    geom_line_interactive(aes(x = year, y = median, data_id = data_id, tooltip = "Average temperature anomaly", colour = "red")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +#
    ggplot_transparent_theme +
    scale_x_continuous(breaks = seq(1850, 2018, 5)) +
    labs(x = "Year", y = "Average temperature anomaly (°C)",
         title = "Average temperature anomaly", subtitle = "In relation to the averaged yearly temperature 1961-1990")
  
  output$linechart_temperatures <- renderGirafe(
    girafe(ggobj = linechart_temperatures, width_svg = 10, height_svg = 5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0") )
  )
    
    # Iframe: Sea level rise ####
  
  output$iframe_sea_level <- renderUI({
    tags$iframe(src = "https://seeing.climatecentral.org/#12/40.7298/-74.0070?show=lockinAnimated&level=0&unit=feet&pois=hide", style = "height:70vh; width:70vw;")
  })
  
    # Bar chart: Emissions per continent + year ####

  
    barchart_continents <- prepareBarChartContinents(data = continents_emissions) %>%
      makeLabelArrows(x = 2006, xend = 2009, y = 33, yend = 30, label = "Financial crisis")
    
    output$barchart_continents <- renderGirafe({
      girafe(ggobj = barchart_continents, width_svg = 10, height_svg = 5)
    })
  
  
    # Line chart: Emissions per continent + year ####
  
    linechart_continents <- prepareLineChartContinents(continents_emissions) %>%
      makeLabelArrows(x = 1998, xend = 2001, y = 4.5, yend = 3.6, label = "China's emissions start \n to rise drastically")
  
    output$linechart_continents <- renderGirafe(
      girafe(ggobj = linechart_continents, width_svg = 10, height_svg = 5) %>%
        girafe_options(opts_hover(css = "stroke:black; stroke-width:5px; fill:none;") )
    )
  
  
    # Chloropleth: Emissions per capita and country in the most recent (maximum) year ####
  
    world_map <- loadWorldMap(quality = "medium")
  
    # STIMMEN HIER DIE EINHEIT EMISSIONEN? IN DER FUNKTION WIRD MIT MRD MALGENOMMEN
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
        chart_title = "Emissions per capita (t CO2)"
      )
    )
  
    # Rect chart: Emissions per capita and continent ####
    
    chart_emissions_per_cap_continents <- reactive({
      continents_emissions %>%
        filter(year == base_year()) %>%
        mutate(population = population / 1000000000) %>%
        arrange(-emis_per_capita) %>%
        mutate(emis_per_capita = round(emis_per_capita, 1),
               population_perc = round(population / sum(population, na.rm = T) * 100, 1),
               emission_perc = round(emissions / sum(emissions, na.rm = T) * 100, 1),
               xmax = cumsum(population),
               xmin = lag(xmax, default = 0)) %>%
        ggplot() +
        geom_rect_interactive(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = emis_per_capita, fill = continent, data_id = data_id,
                                  tooltip = paste0(continent, ": ", emis_per_capita, " t CO2 per capita\n", population_perc, "% of world population\n", emission_perc, "% of world emissions"))) +
        ggplot_transparent_theme +
        scale_fill_brewer(palette = "Reds", name = "Accent", direction = -1) +
        scale_x_continuous(breaks = seq(0, 8, 1)) +
        labs(x = "Population (billions)", y = "Emissions per capita (t CO2e)",
             title = "Emissions per region and capita", subtitle = "Relation between per capita emissions and population size")
    })
    
    output$rect_emissions_per_cap <- renderGirafe(
      girafe(ggobj = chart_emissions_per_cap_continents(), width_svg = 10, height_svg = 5)
    )
    
    
    
    # Scatterplot: Emissions per GDP ####
  
    palette <- colorRampPalette(brewer.pal(9, "Paired"))

    output$scatterplot_emissions_gdp_year <- renderUI({
      sliderInput("animate_emissions_gdp", label = "Year", min = 1971, max = 2018, step = 1, value = 1971, animate = T, sep = "")
      })

    # Farbe nach Kontinent!
    scatterplot_emissions_gdp <- reactive({
      countries_emissions %>%
        mutate(label = ifelse(country %in% input$selected_countries_gdp, country, "")) %>%
        filter(is.finite(gdp_per_capita),
               year == input$animate_emissions_gdp) %>% # Noch interaktiv machen
        ggplot() +
        geom_point_interactive(aes(x = gdp_per_capita, y = emis_per_capita, data_id = data_id, col = country, alpha = .1,
                                   tooltip = paste0(country, ":"))) +
        geom_text_repel(aes(label = label, x = gdp_per_capita, y = emis_per_capita), color = "white", hjust = 0, nudge_y = 0.07, nudge_x = 0.07, point.padding = NA) +
        scale_color_manual(values = palette(200)) +
        scale_x_log10() +
        scale_y_log10() +
        theme(legend.position = "none") +
        ggplot_transparent_theme +
        labs(x = "GDP per capita (PPP-$)", y = "Emissions per capita (t CO2e)",
             title = "Emissions and GDP per capita", subtitle = "Relation between economic development and emissions")
    })

    output$scatterplot_emissions_gdp <- renderGirafe({
      girafe(ggobj = scatterplot_emissions_gdp(), width_svg = 10, height_svg = 5)
    })

# scatterplot_emissions_gdp <- countries_emissions %>%
#   filter(is.finite(gdp_per_capita)) %>% # Noch interaktiv machen
#   mutate(label = ifelse(country %in% c("China", "Mozambique", "Sweden"), country, "")) %>%
#   ggplot() +
#   geom_point(aes(x = gdp_per_capita, y = emis_per_capita, col = country, alpha = .6, size = 60)) +
#   geom_text(aes(label = label, x = gdp_per_capita, y = emis_per_capita, alpha = .5, size = 16)) +
#   shadow_wake(alpha = 0.2, colour = "grey", wake_length = .3, exclude_layer = 2) +
#   # scale_color_manual(values = palette(200)) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme(legend.position = "none") +
#   ggplot_transparent_theme +
#   labs(title = 'Emissions and GDP per capita: {frame_time}', subtitle = "Relation between economic development and emissions",
#        x = "GDP per capita (PPP-$)", y = "Emissions per capita (t CO2e)") +
#   transition_time(year) +
#   ease_aes('linear')
# 
# anim_save("www/animation_countries_gdp.gif", animate(scatterplot_emissions_gdp, height = 800, width = 1600, fps = 3))


    
    # Point chart: Remaining budgets ####
    
    global_emissions_sr15_gt <- reactive({
      world_emissions %>%
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
        ggplot() + 
        geom_point_interactive(aes(x = warming_degrees, y = probability, size = budget_gt, color = years_left, data_id = data_id,
                                   tooltip = paste0(budget_gt, " Gt (", round(years_left, 1), " years left from 2018)"))) +
        scale_size_continuous(range = c(1,20), guide = "none") +
        coord_fixed(ratio = 1.4) +
        ggplot_transparent_theme +
        scale_color_gradient(low = "red", high = "green") +
        labs(title = "Remaining carbon budget (2018)", subtitle = "Leading to different amounts of warming under different probabilities",
             x = "°C of warming by 2100", y = "Probability (%)")
    })
    
    output$scatterplot_carbon_budgets <- renderGirafe({
      girafe(ggobj = scatterplot_carbon_budgets(), width_svg = 10, height_svg = 5)
    })
    
    # Table: Approaches compared ####
    
    table_justice_approaches <- data.frame(
      
      "Principle" = c("The remaining budget per country is allocated based on equal per capita emissions",
                            "An initial allocation based on base year emissions is gradually replaced by an allocation based on equal per capita emissions",
                            "The remaining budget per country is allocated based on base year emissions"),
      "Rationale" = c("Every human should have the same right to emit",
                        "Every human should have the same right to emit, but we have to take into account the present situation first",
                        "The present emission inequality is not (too) unjust"),
      "Consequences" = c("Industrial states live beyond their means and accumulate large carbon debts", 
                           "", 
                           "Present differences in terms of emissions are justified and persist")
    )
    
    rownames(table_justice_approaches) <- c("Budget Approach", "Convergence & Contraction", "Grandfathering")
    
    output$approaches_table <- renderText({
      table_justice_approaches %>%
        knitr::kable("html", col.names = c("Principle", "Rationale", "Consequences")) %>%
        kable_styling(c("hover", "responsive"), full_width = F, position = "center") %>%
        column_spec(1, bold = T) %>%
        row_spec(0, bold = T) %>%
        row_spec(0:3, extra_css = "border-bottom: 1px dashed #b7b7b7; border-top: 0px; padding:20px;") %>%
        footnote(general = "Here is a general comments of the table. ",
                 number = c("Footnote 1; ", "Footnote 2; "),
                 alphabet = c("Footnote A; ", "Footnote B; "),
                 symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2"),
                 footnote_as_chunk = T, title_format = c("italic", "underline")
        )
    })
    
    # Barchart: Approaches compared ####
    observe(print(
      just_emission_budgets_countries() %>%
        filter(country %in% c("United States", "Botswana")) %>%
        left_join(countries_emissions) %>%
        mutate(emissions = emissions / 1000000000) %>%
        mutate(years_left = total_country_budget_gt / emissions)
    ))
    
    barchart_exemplary_years_left <- reactive({
      just_emission_budgets_countries() %>%
        filter(country %in% c("United States", "Botswana")) %>%
        left_join(countries_emissions) %>%
        mutate(emissions = emissions / 1000000000) %>%
        mutate(years_left = total_country_budget_gt / emissions) %>%
        ggplot() +
        geom_segment(aes(x = country, xend = country, y = 0, yend = years_left), color = "grey") +
        geom_point_interactive(aes(x = country, y = years_left, data_id = data_id, 
                                   tooltip = paste0(country, ": ", round(years_left, 1), " years left"), size = 10, color = country)) +
        coord_flip() +
        ggplot_transparent_theme +
        labs(title = "Budget reach", subtitle = "Under constant base year emissions", x = "Year", y = "Country")
    })
    
    output$exemplary_years_left <- renderGirafe({
      girafe(ggobj = barchart_exemplary_years_left())
    })
    
    
    
  # CALCULATION OF JUST BUDGET ####

  population_percentages_allyears <- countries_emissions %>%
    group_by(year) %>%
    mutate(population_percentage = population / sum(population, na.rm = T)) %>%
    ungroup() %>%
    select(country, year, population_percentage)

  # Per country: Assigned emission budgets (only for base year)
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
        # The emission_share does not add up to 100% because international transport is missing, but this is correct here
        mutate(emission_share = emissions / global_emissions_base_year()) %>% 
        mutate(total_country_budget_gt = emission_share * selected_carbon_budget_at_base_year()) %>%
        select(country, year, total_country_budget_gt, data_id)
    } else if(input$selected_calculation_approach == "convergence") {
      countries_emissions %>%
        left_join(population_percentages_allyears) %>%
        filter(year == base_year()) %>%
        select(country, emissions, population_percentage, data_id) %>%
        rename(emission_forecast = emissions) %>%
        sp::merge(global_future_pathway_linear()) %>%
        group_by(country) %>%
        arrange(year) %>%
        mutate(emission_forecast = calculateContractionConvergence(emission_forecast, Ct, global_emissions, population_percentage)) %>%
        mutate(total_country_budget_gt = sum(emission_forecast, na.rm = T) / 1000000000) %>%
        ungroup() %>%
        filter(year == base_year()) %>%
        select(country, year, total_country_budget_gt, data_id)
      }
  })

  # Per country: Budget still left per year
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
  
  test <- reactive({
    req(selected_countries())
    just_emission_budgets_countries() %>%
      filter(country %in% c(selected_countries())) %>%
      left_join(total_country_emis_since_base_year()) %>%
      left_join(countries_emissions) %>%
      mutate(emissions = emissions / 1000000000) %>%
      mutate(years_left_at_by = base_year() + total_country_budget_gt / emissions) %>%
      ggplot() +
      geom_segment(aes(x = country, xend = country, y = base_year(), yend = years_left_at_by), color = "grey") +
      geom_point_interactive(aes(x = country, y = years_left_at_by, data_id = data_id, 
                                 tooltip = paste0(country, ": Budget reaches until ", round(years_left_at_by, 1)), size = 10, color = country)) +
      coord_flip() +
      ggplot_transparent_theme +
      labs(title = "Budget reach", subtitle = "Under constant base year emissions", x = "Year", y = "Country")
    })
    
    output$years_left <- renderGirafe({
      girafe(ggobj = test(), width_svg = 10, height_svg = 4.5)
    })
  
  
  observe(print(
    selected_countries()
  ))
  
  
    # Heatmap: How many of the just budget is left? (for all years between base year and maximum year) ####
    
    heatmap_budget_left_allyears <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        heatmapEmissionBudgetLeft(data = .,
                                  scales = barchart_just_budget_scales(),
                                  from = base_year(),
                                  to = maximum_year)
    })
    
    output$heatmap_budget_left_allyears <- renderGirafe(
      girafe(ggobj = heatmap_budget_left_allyears(), width_svg = 10, height_svg = 4.5)
    )
  
    barchart_just_budget_scales <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        select(budget_left_perc) %>%
        createChartScales(props = c(.4, .9, .1, .2))
    })

  

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
    
    
  # OBSERVERS ####
  
    # Jump to next / previous page ####
  observeEvent(input$forwardPage2,{
    updateTabsetPanel(session, inputId = "tabset-panel", selected = "Next page")
  })
  
    # Create choice values for gdp scatterplot country selection ####
    
    observe({
      updateSelectizeInput(session, "selected_countries_gdp",
                           choices = c(countries_emissions$country),
                           selected = c("China", "Mozambique", "Sweden")
      )
    })
    
    
    # Create choice values for budget left country selection ####
    
    observe({
      updateSelectizeInput(session, "selected_countries",
                           choices = c(countries_emissions$country),
                           selected = c("Australia", "Brazil", "Burundi", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates")
      )
    })
    
    
    # Images on start page ####
    
    value <- reactiveVal(0)
    
    observe({
      invalidateLater(10000, session)
      if(isolate(value() < 3)) {
        isolate(value(value() + 1))
      } else {
        isolate(value(1))
      }
    })
    
    observe({
      if(value() == 1) {
        shinyjs::show("startpage_image_1")
        shinyjs::hide("startpage_image_2")
        shinyjs::hide("startpage_image_3")
      } else if(value() == 2) {
        shinyjs::hide("startpage_image_1")
        shinyjs::show("startpage_image_2")
        shinyjs::hide("startpage_image_3")
      } else {
        shinyjs::hide("startpage_image_1")
        shinyjs::hide("startpage_image_2")
        shinyjs::show("startpage_image_3")
      }
    })
    
    ##################### TEST C&C ############################

    # BUDGETETABLIERUNG
    # Wir müssen ein Konvergenzjahr wählen
    # Was passiert nach der Konvergenz? Wie ist die Kontraktion?
    # Wirken sich untersch. Pfade auf das Budget aus?
    
    calculated_zero_emissions_year <- reactive({
      round(base_year() + 2 * selected_carbon_budget_at_base_year() / (global_emissions_base_year() / 1000000000), 0)
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
        mutate(
          cumulated_reductions_absolute = cumsum(cumulated_reductions_absolute),
          global_emissions = global_emissions - cumulated_reductions_absolute,
          Ct = (year - base_year()) / (calculated_zero_emissions_year() - base_year())
        )
    })
    
    # observe(print(
    #   # just_emission_budgets_countries_left() %>%
    #   #   filter(country %in% selected_countries())
    #   selected_countries()
    # ))
 

}
