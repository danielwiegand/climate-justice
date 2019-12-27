server <- function(input, output, session) {

  # SELECTIZE: https://shiny.rstudio.com/articles/selectize.html
  # BEISPIEL: https://www.business-science.io/assets/2019-11-22-docker/stock_analyzer_aws_deploy.jpg
  
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


  # IMPORTANT VARIABLES

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
  
    # Bump Chart: Country Emissions ####
  
    # Find countries where data from all years are available
    countries_all_years_available <- countries %>%
        group_by(country) %>%
        mutate(data_from_years = n()) %>%
        ungroup() %>%
        filter(data_from_years == max(data_from_years))
  
    countries_emissions_ranking <- countries_all_years_available %>%
        arrange(year, -emissions) %>%
        group_by(year) %>%
        mutate(ranking = row_number()) %>%
        ungroup() %>%
        filter(ranking < 11) %>% # Only show the top 10 of each year
        mutate(color = case_when(country == "United States" ~ "a",
                                 country == "China" ~ "b",
                                 country == "India" ~ "c",
                                 country == "Japan" ~ "d",
                                 country == "Germany" ~ "e",
                                 country == "Iran" ~ "f",
                                 country == "South Korea" ~ "g",
                                 country == "Canada" ~ "h",
                                 country == "Mexico" ~ "i",
                                 country == "Indonesia" ~ "j",
                                 TRUE ~ "k"))
  
    cols_ranking <- colorRampPalette(colors = brewer.pal(9, "OrRd")[4:9])
    labels_ranking <- c("United States", "China", "India", "Japan", "Germany", "Iran", "South Korea", "Canada", "Mexico", "Indonesia", "Other states")
  
    bump_chart_emitters <- ggplot(countries_emissions_ranking, aes(x = year, y = ranking, group = country, color = color)) +
      geom_line_interactive(aes(data_id = data_id, tooltip = country), size = 1.5) +
      scale_colour_manual(name = "Country", values = c(cols_ranking(10), "grey"), labels = labels_ranking) +
      scale_y_reverse() +
      theme_test() +
      labs(x = "Year", y = "Rank", title = "World's largest emitters", subtitle = "The top ten CO2 emitting countries over the years")
  
    output$bump_chart_emitters <- renderGirafe(
      girafe(ggobj = bump_chart_emitters) %>%
        girafe_options(opts_hover(css = "stroke:black; fill:none;") )
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
      na.omit() %>%
      ggplot() +
      geom_tile_interactive(aes(x = country, y = year, fill = budget_left_perc, data_id = data_id,
                                tooltip = paste0(country, " (", year, "): ", round(budget_left_perc, 1), "% of budget left")), color = "white") +
      scale_fill_gradientn(colours = c("slateblue4", "red", "orange", "white", "lightgreen", "chartreuse3", "darkgreen"),
                           values = barchart_just_budget_scales(),
                           na.value = "grey", name = "% budget left") +
      coord_flip() +
      scale_y_continuous(breaks = seq(base_year(), maximum_year, 1)) +
      theme(
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        panel.border = element_rect(colour = "transparent", fill = NA, size = 0),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        # plot.title = element_text(size = 8),
        # plot.subtitle = element_text(size = 6),
        title = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_text(angle = 90),
        legend.background = element_rect(fill = "transparent", colour = NA), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent", colour = NA), # get rid of legend panel bg
        legend.text = element_text(colour = "white"),
        legend.key = element_blank() # Removes frame around boxes
      ) +
      labs(x = "Country", y = "Year", title = "Budget consumption", subtitle = "Percent of budget left per country and year")
  })
  
  output$heatmap_budget_left_allyears <- renderGirafe(
    girafe(ggobj = heatmap_budget_left_allyears(), width_svg = 10, height_svg = 5)
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

# # LIBRARIES
# 
# library(climateR)
# library(ggplot2)
# library(dplyr)
# library(magrittr)
# library(stringr)
# library(tidyr) # spread
# library(tibble) # rownames_to_column
# library(geojsonio) # read .geojson
# library(broom) # tidy
# library(ggiraph)
# library(leaflet)
# library(RColorBrewer)
# 
# 
# # READ DATA
# 
# raw_emissions <- importEmissionData()
# 
# carbon_budgets <- importIPCCData()
# 
# 
# # INPUTS
# 
# base_year <- 1992
# 
# selected_probability <- 66
# 
# selected_warming_degrees <- 1.5
# 
# calculation_approach <- "pca"
# 
# 
# # IMPORTANT VARIABLES
# 
# # Year where the IPCC budgets used were published
# year_ipcc_budgets <- 2018
# 
# countries <- raw_emissions %>%
#   filter(!code == "",
#          !country == "World") %>%
#   mutate(data_id = as.character(row_number()))
# 
# continents <- raw_emissions %>%
#   # Leaving out the "Statistical differences" here, as of minor importance and sometimes negative
#   filter(country %in% c("EU-28", "China", "India", "United States", "Asia and Pacific (other)", "Americas (other)", "Europe (other)", "International transport")) %>%
#   mutate(data_id = as.character(row_number())) %>%
#   rename(continent = country)
# 
# # Minimum and maximum years of available emissions data
# maximum_year <- max(raw_emissions$year)
# minimum_year <- min(raw_emissions$year)
# 
# total_country_emis_since_base_year <- countries %>%
#   filter(year >= base_year) %>%
#   group_by(country) %>%
#   summarize(total_emis_since_by_gt = sum(emissions)/1000000000) %>%
#   ungroup()
# 
# cumulated_country_emis_since_base_year <- countries %>%
#   filter(year >= base_year) %>%
#   group_by(country) %>%
#   mutate(cumulated_emis_since_by_gt = cumsum(emissions)/1000000000) %>%
#   ungroup() %>%
#   select(country, year, cumulated_emis_since_by_gt)
# 
# # Total world emissions between base year and year of IPCC budget publication
# total_world_emis_since_base_year <- raw_emissions %>%
#   filter(country == "World",
#          year >= base_year & year <= year_ipcc_budgets) %>%
#   summarize(total_world_emis = sum(emissions)) %>%
#   as.numeric()
# 
# # All IPCC carbon budgets related to the base year (incl. additional emissions between base year and year of IPCC budget publication)
# carbon_budgets_at_base_year <- carbon_budgets %>%
#   mutate(budget_gt = budget_gt + total_world_emis_since_base_year / 1000000000)
# 
# selected_carbon_budget_at_base_year <- carbon_budgets_at_base_year %>%
#   filter(probability == selected_probability,
#          warming_degrees == selected_warming_degrees) %>%
#   select(budget_gt) %>%
#   as.numeric()

# # VISUALIZATIONS STATUS QUO ####
# 
# # Bar chart: Emissions per continent + year ####
# 
# barchart_continents <- prepareBarChartContinents(data = continents)
# 
# girafe(ggobj = barchart_continents)
# 
# 
# # Line chart: Emissions per continent + year ####
# 
# linechart_continents <- prepareLineChartContinents(continents)
# 
# girafe(ggobj = linechart_continents) %>%
#   girafe_options(opts_hover(css = "stroke:black; fill:none;") )
# 
# 
# # Chloropleth: Emissions per capita and country in the most recent (maximum) year ####
# 
# world_map <- loadWorldMap(quality = "medium")
# 
# world_map_emission_per_capita <- countries %>%
#   mutate(emissions = emissions / 1000000) %>% # Convert to Million tons
#   filter(year == maximum_year)
# 
# world_map_emission_per_capita <- sp::merge(world_map, world_map_emission_per_capita, by.x = "admin", by.y = "country")
# 
# palette_world_map_emission_per_capita <- colorNumeric(palette = c("white", "darkred"),
#                                                       domain = world_map_emission_per_capita@data$emissions_per_cap, na.color = "lightgrey") 
# 
# tooltip <- paste(
#   world_map_emission_per_capita@data$admin, ": ",
#   sprintf("%.1f", round(world_map_emission_per_capita@data$emissions_per_cap, 1), nsmall = 0, big.mark = " ", scientific = F),
#   " t CO2 per capita (",
#   world_map_emission_per_capita@data$year, ")",
#   sep = "")
# 
# createChloroplethChart(
#   data = world_map_emission_per_capita,
#   data_column = "emissions_per_cap",
#   palette = palette_world_map_emission_per_capita,
#   tooltip = tooltip,
#   chart_title = "Emissions per capita (t CO2)"
# )
# 
# # Bump Chart: Country Emissions ####
# 
# # Find countries where data from all years are available
# countries_all_years_available <- countries %>%
#   group_by(country) %>%
#   mutate(data_from_years = n()) %>%
#   ungroup() %>%
#   filter(data_from_years == max(data_from_years))
# 
# countries_emissions_ranking <- countries_all_years_available %>%
#   arrange(year, -emissions) %>%
#   group_by(year) %>%
#   mutate(ranking = row_number()) %>%
#   ungroup() %>%
#   filter(ranking < 11) %>% # Only show the top 10 of each year
#   mutate(color = case_when(country == "United States" ~ "a",
#                            country == "China" ~ "b",
#                            country == "India" ~ "c",
#                            country == "Japan" ~ "d",
#                            country == "Germany" ~ "e",
#                            country == "Iran" ~ "f",
#                            country == "South Korea" ~ "g",
#                            country == "Canada" ~ "h",
#                            country == "Mexico" ~ "i",
#                            country == "Indonesia" ~ "j",
#                            TRUE ~ "k"))
# 
# cols_ranking <- colorRampPalette(colors = brewer.pal(9, "OrRd")[4:9])
# labels_ranking <- c("United States", "China", "India", "Japan", "Germany", "Iran", "South Korea", "Canada", "Mexico", "Indonesia", "Other states")
# 
# bump_chart <- ggplot(countries_emissions_ranking, aes(x = year, y = ranking, group = country, color = color)) +
#   geom_line_interactive(aes(data_id = data_id, tooltip = country), size = 1.5) +
#   scale_colour_manual(name = "Country", values = c(cols_ranking(10), "grey"), labels = labels_ranking) +
#   scale_y_reverse() +
#   theme_test() +
#   labs(x = "Year", y = "Rank", title = "World's largest emitters", subtitle = "The top ten CO2 emitting countries over the years")
# girafe(ggobj = bump_chart) %>%
#   girafe_options(opts_hover(css = "stroke:black; fill:none;") )







# # CALCULATION OF JUST BUDGET ####
# 
# population_percentages_allyears <- countries %>%
#   group_by(year) %>%
#   mutate(population_percentage = population / sum(population, na.rm = T)) %>%
#   ungroup() %>%
#   select(country, year, population_percentage)
# 
# # Per country: Assigned emission budgets (only for base year)
# just_emission_budgets_countries <- if(input$selected_calculation_approach == "pca") {
#   countries %>%
#     left_join(population_percentages_allyears) %>%
#     filter(year == base_year) %>%
#     mutate(total_country_budget_gt = population_percentage * selected_carbon_budget_at_base_year) %>%
#     select(country, year, total_country_budget_gt, data_id)
# }
# 
# # Per country: Budget still left per year
# just_emission_budgets_countries_left <- countries %>%
#   filter(year >= base_year) %>%
#   left_join(just_emission_budgets_countries %>% select(-year, -data_id)) %>% # Join with total budget per country for the base year
#   left_join(cumulated_country_emis_since_base_year) %>%
#   mutate(budget_left = total_country_budget_gt - cumulated_emis_since_by_gt,
#          budget_left_perc = budget_left / total_country_budget_gt * 100) %>%
#   select(country, year, total_country_budget_gt, budget_left, budget_left_perc, data_id)
# 
# # VISUALIZATIONS OF CALCULATIONS ####
# 
# # Barchart Per capita approach: Budget left (absolute) ####
# # barchart_budget_left <- just_emission_budgets_countries_left %>%
# #   filter(year == maximum_year) %>%
# #   na.omit(budget_left) %>%
# #   mutate(country = reorder(country, budget_left)) %>%
# #   ggplot(aes(x = country, y = budget_left, fill = budget_left)) +
# #   geom_bar_interactive(aes(data_id = data_id, tooltip = paste0(country, ": ", round(budget_left, 1), " Gt left")), show.legend = FALSE, stat = "identity") +
# #   coord_flip() +
# #   theme_test() +
# #   labs(title = "Leaders and laggards", subtitle = "Countries performing worst / best regarding their emission budget", x = "Country", y = "Budget left (Gt)") +
# #   scale_fill_gradient(low = "red", high = "green")
# # girafe(ggobj = barchart_budget_left)
# 
# 
# # Barchart: Budget left (percent), leaders & laggards (facets) ####
# barchart_leaders_laggards <- just_emission_budgets_countries_left %>%
#   filter(year == maximum_year) %>%
#   group_by(budget_left_perc < 0) %>%
#   top_n(10, abs(budget_left_perc)) %>%
#   ungroup() %>%
#   mutate(country = reorder(country, budget_left_perc)) %>%
#   ggplot(aes(x = country, y = budget_left_perc, fill = budget_left_perc)) +
#   geom_bar_interactive(aes(data_id = data_id, tooltip = paste0(country, ": ", round(budget_left_perc, 1), "% of budget left")), show.legend = FALSE, stat = "identity") +
#   coord_flip() +
#   theme_test() +
#   facet_grid(~ budget_left > 0, scales="free", labeller = as_labeller(c("TRUE" = "Leaders", "FALSE" = "Laggards"))) +
#   labs(title = "Leaders and laggards", subtitle = "Countries performing worst / best regarding their emission budget", x = "Country", y = "Budget left (%)") +
#   scale_fill_gradientn(colours = c("red", "white", "green"),
#                      values = scales::rescale(c(-2000, 0, 100)))
# girafe(ggobj = barchart_leaders_laggards)
# 
# 
# # Heatmap: How many of the just budget is left? (for all years between base year and maximum year) ####
# heatmap_budget_left_allyears <- just_emission_budgets_countries_left %>%
#   na.omit() %>%
#   ggplot() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   geom_tile_interactive(aes(x = country, y = year, fill = budget_left_perc, data_id = data_id, 
#                             tooltip = paste0(country, " (", year, "): ", round(budget_left_perc, 1), "% of budget left")), color = "white") +
#   scale_fill_gradientn(colours = c("slateblue4", "red", "orange", "white", "lightgreen", "chartreuse3", "darkgreen"),
#                        values = barchart_just_budget_scales, 
#                        na.value = "grey", name = "% budget left") +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(base_year, maximum_year, 1)) +
#   labs(x = "Country", y = "Year", title = "Budget consumption", subtitle = "Percent of budget left per country and year")
# girafe(ggobj = heatmap_budget_left_allyears)
# 
# barchart_just_budget_scales <- createChartScales(data = just_emission_budgets_countries_left$budget_left_perc, props = c(.4, .9, .1, .2))
# 
# 
# # Heatmap: Alle Budgets vs. % aufgebraucht für das Base year ####
# # 
# # heatmap_budget_left_maximumyear <- total_country_emis_since_base_year %>% # EVTL IST DAS EINE VERALLGEMEINERUNGSFUNKTION MIT DER DIE OBIGE ERSETZT WERDEN KANN
# #   left_join(population_percentages_allyears) %>%
# #   filter(year == base_year) %>%
# #   merge(carbon_budgets_at_base_year) %>%
# #   mutate(country_budget_gt = population_percentage * budget_gt,
# #          budget_left = country_budget_gt - total_emis_since_by_gt,
# #          budget_left_perc = budget_left / country_budget_gt * 100,
# #          description = paste(warming_degrees, probability, sep = "-"),
# #          data_id = as.character(row_number()))
# 
# # heatmap_budget_left_maximumyear_plot <- heatmap_budget_left_maximumyear %>% ggplot() +
# #   theme(axis.text.x = element_text(angle = 90)) +
# #   geom_tile_interactive(aes(x = country, y = probability, fill = budget_left_perc, data_id = data_id, tooltip = paste0(country, ": ", round(budget_left_perc, 1), "% of budget left")), color = "white") +
# #   scale_fill_gradientn(colours = c("slateblue4", "red", "orange", "white", "lightgreen", "darkgreen"),
# #                        values = scales::rescale(c(
# #                          min(test$budget_left_perc, na.rm = T),
# #                          quantile(test$budget_left_perc[test$budget_left_perc < 0], probs = .1, na.rm = T),
# #                          quantile(test$budget_left_perc[test$budget_left_perc < 0], probs = .9, na.rm = T),
# #                          0,
# #                          quantile(test$budget_left_perc[test$budget_left_perc > 0], probs = .1, na.rm = T),
# #                          max(test$budget_left_perc, na.rm = T))),
# #                        na.value = "grey", name = "Gt left") +
# #   facet_grid(~warming_degrees) +
# #   coord_flip() +
# #   # scale_y_continuous(breaks = seq(base_year, maximum_year, 1)) +
# #   labs(x = "Country", y = "Year", title = "Budget consumption", subtitle = "Percent of budget left per country")
# # girafe(ggobj = heatmap_budget_left_maximumyear_plot)
