server <- function(input, output, session) {

  # Tabellen: https://www.r-bloggers.com/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/
  # Gerechtigkeitsargu verfeinern
  # Die Rechtspfeile machen
  # Die Texte machen
  # Pfeile in die Plots machen
  # Contraction stimmt noch nicht ganz
  # Grenzwerte wenn man 2018 eingibt
  # Quellen und Autor
  # AWS anmelden
  # Farbskala bei Heatmap bei späteren basisjahren (keine neg. Werte)  ist falsch
  
  # GLIEDERUNG ####
  # THE QUESTION (): 
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
  # - These probabilities reflect the sizable uncertainty in the sensitivity of the climate to CO2 emissions. Coloured bars correspond to the carbon budget from different studies listed on the left, while values below zero mean that current cumulative emissions already exceeded the “well below” 1.5C carbon budget.
  # - Grafik unter https://www.carbonbrief.org/analysis-how-much-carbon-budget-is-left-to-limit-global-warming-to-1-5c
  # - Unsicherheiten: https://www.wri.org/blog/2018/10/according-new-ipcc-report-world-track-exceed-its-carbon-budget-12-years
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
  
  countries_emissions <- importCountriesEmissionData()

  continents_emissions <- importContinentsEmissionData()
  
  historical_temperatures <- read.csv("src/ourworldindata/temperature-anomaly.csv", stringsAsFactors = F) %>%
    filter(entity == "Global") %>%
    select(-entity) %>%
    mutate(data_id = "asd")
  
  global_emissions <- countries_emissions %>%
    group_by(year) %>%
    summarize(emissions = sum(emissions, na.rm = T))
  
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
    makeLabelArrows(x = 1965, xend = 1970, y = .3, yend = .1, label = "Temporary cooling, possibly <a href = 'https://insideclimatenews.org/news/20100923/global-warming-may-have-slowed-1970s-due-suddenly-cooler-oceans'>man-made</a>") %>%
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
      makeLabelArrows(x = 1998, xend = 2001, y = 10, yend = 8, label = "China's emissions start \n to rise drastically") %>%
      makeLabelArrows(x = 1987, xend = 1990, y = 10.2, yend = 8.2, label = "Europe's emissions decline after \n the dissolution of Soviet Union")

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
      sliderInput("animate_emissions_gdp", label = "Year", min = 1971, max = 2018, step = 1, value = 1971, animate = T, sep = "")
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
      # Linear version of Convergenve & Contraction: Linear transition from allocation according to base year emissions to alloc. acc. to population share
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
        mutate(years_left_at_by = base_year() + floor(total_country_budget_gt / emissions)) %>%
        ggplot() +
        geom_segment(aes(x = country, xend = country, y = base_year(), yend = years_left_at_by), color = "grey") +
        geom_point_interactive(aes(x = country, y = years_left_at_by, data_id = data_id, 
                                   tooltip = paste0(country, ": Budget reaches until year ", years_left_at_by), size = 10, color = country)) +
        coord_flip() +
        scale_color_manual(values = cols_countries_years_left(length(selected_countries()))) +
        ggplot_transparent_theme +
        theme(legend.position = "none",
              axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
        labs(title = "Budget reach", subtitle = "Under constant base year emissions", x = "Country", y = "Budget reach (year)")
      })
      
      output$years_left <- renderGirafe({
        girafe(ggobj = barchart_countries_years_left(), width_svg = 10, height_svg = 4.5)
      })
  
    # Heatmap: How many of the just budget is left? (for all years between base year and maximum year) ####
    
    heatmap_budget_left_allyears <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        createHeatmapEmissionBudgetLeft(data = .,
                                  scales = heatmap_just_budget_scales(),
                                  from = base_year(),
                                  to = maximum_year,
                                  theme = ggplot_transparent_theme)
    })
    
    heatmap_just_budget_scales <- reactive({
      req(selected_countries(), just_emission_budgets_countries_left())
      just_emission_budgets_countries_left() %>%
        filter(country %in% selected_countries()) %>%
        select(budget_left_perc) %>%
        createChartScales(props = c(.4, .9, .1, .2))
    })
    
    output$heatmap_budget_left_allyears <- renderGirafe(
      girafe(ggobj = heatmap_budget_left_allyears(), width_svg = 10, height_svg = 4.5)
    )

    # Barchart: Budget left (percent), leaders & laggards (facets) ####
    
    barchart_leaders_laggards <- reactive({
      just_emission_budgets_countries_left() %>%
        filter(year == maximum_year) %>%
        group_by(budget_left_perc < 0) %>%
        top_n(10, abs(budget_left_perc)) %>%
        ungroup() %>%
        mutate(country = reorder(country, budget_left_perc)) %>%
        createBarChartLeadersLaggards(
          data = .,
          theme = ggplot_transparent_theme
        )
    })
    
    output$barchart_leaders_laggards <- renderGirafe(
      girafe(ggobj = barchart_leaders_laggards(), width_svg = 10, height_svg = 5)
    )
    
    
  # OBSERVERS ####
  
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
  observeEvent(input$forwardPage2,{
    updateTabsetPanel(session, inputId = "tabset-panel", selected = "Next page")
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
                           selected = c("Australia", "Brazil", "Burundi", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
      )
    })
    
    observe({
      updateSelectizeInput(session, "selected_countries_2",
                           choices = country_list,
                           selected = c("Australia", "Brazil", "Burundi", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
      )
    })
    
    observe({
      updateSelectizeInput(session, "selected_countries_3",
                           choices = country_list,
                           selected = c("Australia", "Brazil", "Burundi", "Canada", "China", "Chile", "Ethiopia", "France",
                                        "Germany", "India", "Japan", "Qatar", "Russia", "Spain", "United Kingdom", "United States", "United Arab Emirates"),
                           server = T
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
    

}
