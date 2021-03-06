# Load packages
library(tidyverse)
library(shiny)
# library(shinyWidgets)
library(rsconnect)
library(htmlwidgets)
library(tigris)
library(leaflet)
library(plotly)
library(beeswarm)


# Load data
# setwd("/Users/beelee/Desktop/Columbia/Fall_2021/P8105-Data_Science/p8105-final_project/ap/apuv_map/")
apuv_df = readRDS("apuv.RDS")
ext_val = readRDS("ext_val.RDS")
dis_df = readRDS("dis_rates.RDS")
us_counties = tigris::counties(c("NY", "PA", "OH"))

# Filter apuv df for a year desired and merge it with county sf objects
filter_merge = function(y){
  apuv_df %>% 
    filter(year == y) %>%
    pivot_wider(
      names_from = season,
      names_glue = "{season}_{.value}",
      values_from = c(pm25_max_pred:i380)
    ) %>% 
    geo_join(us_counties, ., "GEOID",  "countyfips") %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
}

# Filter apuv df for a season of the selected year
select_year_season = function(y, s){
  
  if(y == 2005){res = merged_2005_df}
  if(y == 2006){res = merged_2006_df}
  if(y == 2007){res = merged_2007_df}
  if(y == 2008){res = merged_2008_df}
  if(y == 2009){res = merged_2009_df}
  if(y == 2010){res = merged_2010_df}
  if(y == 2011){res = merged_2011_df}
  if(y == 2012){res = merged_2012_df}
  if(y == 2013){res = merged_2013_df}
  if(y == 2014){res = merged_2014_df}
  if(y == 2015){res = merged_2015_df}
  
  if(s == "Spring"){
    res = 
      res %>% 
      select(-contains(c("Summer", "Fall", "Winter"))) %>% 
      rename_all(funs(str_replace_all(., "Spring_", "")))
  }
  if(s == "Summer"){
    res =
      res %>% 
      select(-contains(c("Fall", "Winter", "Spring"))) %>% 
      rename_all(funs(str_replace_all(., "Summer_", "")))
  }
  if(s == "Fall"){
    res =
      res %>% 
      select(-contains(c("Winter", "Spring", "Summer"))) %>% 
      rename_all(funs(str_replace_all(., "Fall_", "")))
  }
  if(s == "Winter"){
    res =
      res %>% 
      select(-contains(c("Spring", "Summer", "Fall"))) %>% 
      rename_all(funs(str_replace_all(., "Winter_", "")))
  }
  
  return(res)
}

# Preprocess some dfs to prepare for the Shiny.app
merged_2005_df = filter_merge(2005)
merged_2006_df = filter_merge(2006)
merged_2007_df = filter_merge(2007)
merged_2008_df = filter_merge(2008)
merged_2009_df = filter_merge(2009)
merged_2010_df = filter_merge(2010)
merged_2011_df = filter_merge(2011)
merged_2012_df = filter_merge(2012)
merged_2013_df = filter_merge(2013)
merged_2014_df = filter_merge(2014)
merged_2015_df = filter_merge(2015)

merged_asth_df = 
  dis_df  %>% 
  filter(outcome == "asthma") %>% 
  mutate(
    fips = factor(fips)
  ) %>% 
  geo_join(us_counties, ., "GEOID",  "fips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(
    logRate = log(age_adjusted_incidence_rate),
    logRate = replace(logRate, (is.na(logRate) | logRate == -Inf), 0)
  )

#change
merged_lc_df = 
  dis_df  %>% 
  filter(outcome == "lung cancer") %>% 
  mutate(
    fips = factor(fips)
  ) %>% 
  geo_join(us_counties, ., "GEOID",  "fips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(
    logRate = log(age_adjusted_incidence_rate),
    logRate = replace(logRate, (is.na(logRate) | logRate == -Inf), 0)
    # age_adjusted_incidence_rate = replace(age_adjusted_incidence_rate, is.na(age_adjusted_incidence_rate), 0)
  )

#change
merged_mel_df = 
  dis_df  %>% 
  filter(outcome == "melanoma") %>% 
  mutate(
    fips = factor(fips)
  ) %>% 
  geo_join(us_counties, ., "GEOID",  "fips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(
    logRate = log(age_adjusted_incidence_rate),
    logRate = replace(logRate, (is.na(logRate) | logRate == -Inf), 0)
    # age_adjusted_incidence_rate = replace(age_adjusted_incidence_rate, is.na(age_adjusted_incidence_rate), 0)
  )

apuv_plot =
  apuv_df %>%
  filter(state!="ME") %>%
  unite(season_year, c("season", "year"), sep = "_") %>%
  mutate(
    season_year = factor(season_year),
    season_year = fct_inorder(season_year),
    county = factor(county)
  ) %>%
  arrange(state, county) %>%
  unite(county_state, c("county", "state"), sep = ", ") %>%
  mutate(
    county_state = factor(county_state),
    county_state = fct_inorder(county_state)
  ) %>%
  select(season_year, county_state, pm25_pop_pred, o3_pop_pred, edd) %>%
  pivot_longer(
    pm25_pop_pred:edd,
    names_to = "climate",
    values_to = "level"
  ) %>%
  mutate(
    climate = factor(climate, levels = c("pm25_pop_pred", "o3_pop_pred", "edd"),
                     labels = c("PM2.5", "O3", "UV"))
  ) %>%
  ggplot(aes(x=season_year, y=level, group=1, color=county_state)) +
  geom_line() +
  scale_x_discrete(breaks=c("Spring_2005", "Winter_2015"),
                   labels=c("2005", "2015")) +
  labs(
    title = "Air Pollutant Concentrations and UV Intensities over the Years",
    color = "County",
    y = "Councentration/Intensity"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))
  ) +
  facet_grid(climate~., scales = "free_y")

#change
beeswarm_dis_df =
  dis_df %>%
  arrange(state, county) %>%
  unite(county_state, c("county", "state"), sep = ", ") %>%
  mutate(
    county_state = factor(county_state),
    county_state = fct_inorder(county_state),
    logRate = log(age_adjusted_incidence_rate),
    logRate = replace(logRate, (is.na(logRate) | logRate == -Inf), 0),
    outcome = factor(outcome)
  ) %>%
  select(-fips)

#change
beeswarm_dis_plot =
  beeswarm(logRate ~ outcome,
           data = beeswarm_dis_df,
           method = 'swarm',
           pwcol = county_state,
           corral = "wrap")

#change
dis_plot =
  ggplot(beeswarm_dis_plot, aes(x, y)) +
  geom_violin(aes(group = x.orig), color = "grey", fill = "grey") +
  geom_point(aes(color = col)) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(1:3),
                     labels=c("Asthma", "Lung Cancer", "Melanoma")) +
  labs(
    title = "Health Outcome Rates in the Following Years",
    color = "County",
    y = "log(Count per 100K)"
  ) +
  theme(
    axis.title.x=element_blank()
  )
  

# Shiny.app
ui = fluidPage(
  
  # setBackgroundColor(
  #   color = "#363434"
  # ),
  titlePanel("Mapping Climate Exposures and Health Outcomes"),
             
  p("Air pollution and UV radiation and risks of chronic diseases such as asthma, lung cancer and melanoma are known to be correlated.", br(), "Let's explore the population weighted particulate matter (PM2.5) and ozone(O3) levels, and erythermally weighted UV dosage over the years in counties in New York, Pennsylvania, and Ohio.", strong("Select a desired year and season"), "to view these climate exposures on a county level!", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
             
  fluidRow(
    column(width = 1, offset = 5, selectInput("yr", "Year", choices = unique(apuv_df$year))),
    column(width = 1, selectInput("ss", "Season", choices = unique(apuv_df$season))),
    column(width = 3, offset = 2, tags$a(href = "https://ephtracking.cdc.gov/download", "Go to Data Source", target = "_blank"), br(), tags$a(href = "https://19january2017snapshot.epa.gov/air-research/downscaler-model-predicting-daily-air-pollution_.html", "Learn about theDownscaler Model", target = "_blank"), style="text-align:right; color:black")
  ),
             
  fluidRow(
    tabsetPanel(
      tabPanel("Particulate Matter (2.5) Level", 
               column(leafletOutput("pm25"), width = 9), 
               column(plotOutput("box_pm"), width = 3)
      ),
      tabPanel("Ozone Level", 
               column(leafletOutput("o3"), width = 9),
               column(plotOutput("box_oz"), width=3)
      ),
      tabPanel("UV Radiation Level", 
               column(leafletOutput("edd"), width = 9),
               column(plotOutput("box_uv"), width=3)
      )
    )
  ),
  
  br(),
  br(),
  hr(),
  
  p("Maybe the incidence of these health conditions would differ from county to county as a result of the climate? We plotted natural log of asthma, lung cancer and melanoma age-adjusted incidence rates in the years that followed.", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
             
  fluidRow(
    tabsetPanel(
      tabPanel("2016 Asthma Incidence",
               column(leafletOutput("asth"), width = 9),
               column(plotOutput("box_asth"), width = 3)
      ),
      tabPanel("2014-2018 Lung Cancer Incidence",
               column(leafletOutput("lc"), width = 9),
               column(plotOutput("box_lc"), width = 3)
      ),
      tabPanel("2014-2018 Melanoma Incidence",
               column(leafletOutput("mel"), width = 9),
               column(plotOutput("box_mel"), width = 3)
      )
    )
  ),
  
  br(),
  br(),
  hr(),
  
  p("By now, you must be interested in looking at the correlation of a particlar county. Play around these plots to see if you may dig out some trends between climate and health outcomes.", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
             
  column(width = 6, plotlyOutput('cli_plotly')),
  column(width = 6, plotlyOutput('out_plotly'))
           
)

server = function(input, output) {
  
  pm_oz_uv = reactive({
    
    pou = select_year_season(input$yr, input$ss)
    return(pou)
    
  })
  
  output$pm25 = renderLeaflet({
    pal = colorBin(palette = "viridis", bins = 9, domain = c(as.numeric(ext_val[4]), as.numeric(ext_val[1])), reverse = T)
    labels = sprintf("<strong>%s, %s</strong><br/>%g \u00b5g/m\u00b3", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$pm25_pop_pred) %>% lapply(htmltools::HTML)
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(pm_oz_uv()$pm25_pop_pred),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~pm25_pop_pred,
                title = "Concentration (\u00b5g/m\u00b3)",
                opacity = 0.7)
  })
  
  output$o3 = renderLeaflet({
    pal = colorBin(palette = "inferno", bins = 9, domain = c(as.numeric(ext_val[5]),as.numeric(ext_val[2])), reverse = T)
    labels = sprintf("<strong>%s, %s</strong><br/> %g ppm", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$o3_pop_pred) %>% lapply(htmltools::HTML)
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(pm_oz_uv()$o3_pop_pred),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~o3_pop_pred,
                title = "Concentration (ppm)",
                opacity = 0.7)
  })
  
  output$edd = renderLeaflet({
    pal = colorBin(palette = "BuPu", bins = 9, domain = c(as.numeric(ext_val[6]),as.numeric(ext_val[3])))
    labels = sprintf("<strong>%s, %s</strong><br/> %g J/m\u00b2", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$edd) %>% lapply(htmltools::HTML)
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(pm_oz_uv()$edd),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~edd,
                title = "Intensity (J/m\u00b2)",
                opacity = 0.7)
  })
  
  output$box_pm = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = pm25_pop_pred, fill = state)) +
      geom_violin(color = "#5240f1", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_viridis_d(direction = -1) +
      labs(
        title = "PM2.5 Concentrations by State This Season",
        y = bquote(PM[.(2.5)] * " (" * mu * "g/" * m^3 * ")")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box_oz = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = o3_pop_pred, fill = state)) +
      geom_violin(color = "#f17b12", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_viridis_d(direction = -1, option = "magma") +
      labs(
        title = "Ozone Concentrations by State This Season",
        y = bquote(O[.(3)] * " (ppm)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box_uv = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = edd, fill = state))+
      geom_violin(color = "#3d0071", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_brewer(palette="BuPu") +
      labs(
        title = "UV Intensities by State This Season",
        y = bquote("UV (J/" * m^2 *")")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$asth = renderLeaflet({
    pal = colorBin(palette = "YlGn", bins = 6, domain = c(min(merged_asth_df$logRate, na.rm = T), max(merged_asth_df$logRate, na.rm = T)))
    labels = sprintf("<strong>%s, %s</strong><br/> %g", merged_asth_df$county, merged_asth_df$state, merged_asth_df$logRate) %>% lapply(htmltools::HTML)
    leaflet(merged_asth_df) %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(merged_asth_df$logRate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~merged_asth_df$logRate,
                title = "log(Count per 100K)",
                opacity = 0.7)
  })
  #change
  output$lc = renderLeaflet({
    pal = colorBin(palette = "PuRd", bins = 6, domain = c(min(merged_lc_df$logRate, na.rm = T), max(merged_lc_df$logRate, na.rm = T)))
    labels = sprintf("<strong>%s, %s</strong><br/> %g per 100K", merged_lc_df$county, merged_lc_df$state, merged_lc_df$logRate) %>% lapply(htmltools::HTML)
    leaflet(merged_lc_df) %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(merged_lc_df$logRate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~logRate,
                title = "log(Count per 100K)",
                opacity = 0.7)
  })
  #change
  output$mel = renderLeaflet({
    pal = colorBin(palette = "YlOrBr", bins = 6, domain = c(min(merged_mel_df$logRate, na.rm = T), max(merged_mel_df$logRate, na.rm = T)))
    labels = sprintf("<strong>%s, %s</strong><br/> %g per 100K", merged_asth_df$county, merged_asth_df$state, merged_mel_df$logRate) %>% lapply(htmltools::HTML)
    leaflet(merged_mel_df) %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(merged_mel_df$logRate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~logRate,
                title = "log(Count per 100K)",
                opacity = 0.7)
  })
  
  output$box_asth = renderPlot({
    merged_asth_df %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = logRate, fill = state))+
      geom_violin(color = "#1f4e0a", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_brewer(palette="YlGn") +
      labs(
        title = "Asthma Incidence Rate in 2016",
        y = bquote("log(Count per 100K)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box_lc = renderPlot({
    merged_lc_df %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = logRate, fill = state))+
      geom_violin(color = "#3d0071", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_brewer(palette="PuRd") +
      labs(
        title = "Lung Cancer Incidence Rate in 2014-2018",
        y = bquote("log(Count per 100K)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box_mel = renderPlot({
    merged_mel_df %>% 
      filter(!is.na(state)) %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = logRate, fill = state))+
      geom_violin(color = "#660000", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_brewer(palette="YlOrBr") +
      labs(
        title = "Melanoma Incidence Rate in 2014-2018",
        y = bquote("log(Count per 100K)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$cli_plotly = renderPlotly({
    ggplotly(apuv_plot)
  })

  output$out_plotly = renderPlotly({
    ggplotly(dis_plot)
  })
}

shinyApp(ui = ui, server = server)
