# Load packages
library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(tigris)
library(rsconnect)
library(ggplot2)

# Load data
# setwd("/Users/beelee/Desktop/Columbia/Fall_2021/P8105-Data_Science/p8105-final_project/ap/apuv_map/")
apuv_df = readRDS("apuv.RDS")
ext_val = readRDS("ext_val.RDS")
us_counties = tigris::counties(c("NY", "PA", "OH"))

# Filter dataframes for the year desired and merge with the county sf objects
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

# Return the filtered and merged object
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

# Preprocess some dataframes before loading Shiny.app
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

# Shiny.app
ui = fluidPage(
  
  titlePanel("Air Pollution and UV Radiation Exposure Map"),
  
  fluidRow(
    column(p("Some climate conditions and particular chronic disease risks are known to be correlated. Let's explore the Particulate Matter, Ozone, and UV radiation levels over the years in counties in New York, Pennsylvania, and Ohio.", strong("ON THE RIGHT"), ", select year and season to these climate exposures in every county!",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"), tags$a(href = "https://ephtracking.cdc.gov/download", "Go to Data Source", target = "_blank"), tags$a(href = "https://19january2017snapshot.epa.gov/air-research/downscaler-model-predicting-daily-air-pollution_.html", "Learn about the Downscaler Model", target = "_blank"), style="text-align:center;color:black", width=8),
    column(selectInput("yr", "Select a year", choices = unique(apuv_df$year)), selectInput("ss", "Select a season", choices = unique(apuv_df$season)), width=4),
    
    fluidRow(
      
      tabsetPanel(
        tabPanel("Particulate Matter (2.5) Level", 
                 column(leafletOutput("pm25"), width = 9),
                 column(plotOutput("box1"), br(), width=3)
        ),
        tabPanel("Ozone Level", 
                 column(leafletOutput("o3"), width = 9),
                 column(plotOutput("box2"), br(), width=3)
        ),
        tabPanel("UV Radiation Level", 
                 column(leafletOutput("edd"), width = 9),
                 column(plotOutput("box3"), br(), width=3))
      )
    )
  
    # fluidRow(
    #   column(
    #     tabsetPanel(
    #       tabPanel("Particulate Matter (2.5) Level", leafletOutput("pm25")),
    #       tabPanel("Ozone Level", leafletOutput("o3")),
    #       tabPanel("UV Radiation Level", leafletOutput("edd"))
    #     ), width = 9
    #   ),
    #   column(plotlyOutput("box"), br(),width=3,style="border:1px solid black")
    # )
  )
    
  # sidebarLayout(
  #   sidebarPanel(
  #     tags$a(href = "https://ephtracking.cdc.gov/download", "Go to Data Source", target = "_blank"),
  #     tags$a(href = "https://19january2017snapshot.epa.gov/air-research/downscaler-model-predicting-daily-air-pollution_.html", "Learn about the Downscaler Model", target = "_blank"),
  #     # tags$script(type = "text/x-mathjax-config", 'MathJax.Hub.Config({"HTML-CSS": { linebreaks: {automatic: true}},SVG: {linebreaks: {automatic: true}}});'),
  #     h5("Some climate conditions and particular chronic disease risks are known to be correlated. Let's explore the Particulate Matter, Ozone, and UV radiation levels over the years in counties in New York, Pennsylvania, and Ohio."),
  #     selectInput("yr", "Select a year", choices = unique(apuv_df$year)),
  #     selectInput("ss", "Select a season", choices = unique(apuv_df$season))
  #     # selectInput("out", "Select a outcome", choices = c("lung", "melanoma", "asthma"))
  #   ),
  # 
  #   mainPanel(
  #     tabsetPanel(
  #       tabPanel("Particulate Matter (2.5) Level", leafletOutput("pm25")),
  #       tabPanel("Ozone Level", leafletOutput("o3")),
  #       tabPanel("UV Radiation Level", leafletOutput("edd"))
  #       # tabPanel("Outcome", leafletOutput("edd"))
  #     )
  #   )
  # )
)

server = function(input, output) {
  
  pm_oz_uv = reactive({

    pou = select_year_season(input$yr, input$ss)
    return(pou)
    
  })
  
  output$pm25 = renderLeaflet({
    pal = colorBin(palette = "viridis", bins = 9, domain = c(as.numeric(ext_val[4]), as.numeric(ext_val[1])), reverse = T)
    # labels = sprintf("<strong>%s, %s</strong><br/>Mean: %g ug/m^3<br/>Max: %g ug/m^3", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$pm25_pop_pred, pm_oz_uv()$pm25_max_pred) %>% lapply(htmltools::HTML)
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
    # labels = sprintf("<strong>%s, %s</strong><br/>Mean: %g ppm<br/>Max: %g ppm", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$o3_pop_pred, pm_oz_uv()$o3_max_pred) %>% lapply(htmltools::HTML)
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
    # labels = sprintf("<strong>%s, %s</strong><br/>edd: %g J/m^2 <br/>edr: %g J/m^2", pm_oz_uv()$county, pm_oz_uv()$state, pm_oz_uv()$edd, pm_oz_uv()$edr) %>% lapply(htmltools::HTML)
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
  
  output$box1 = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = pm25_med_pred, fill = state)) +
      geom_violin(color = "#5240f1", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_viridis_d(direction = -1) +
      labs(
        title = "County PM Level Distribution by State",
        y = bquote(PM[.(2.5)] * " (" * mu * "g/" * m^3 * ")")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box2 = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = o3_med_pred, fill = state)) +
      geom_violin(color = "#f17b12", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_viridis_d(direction = -1, option = "magma") +
      labs(
        title = "County Ozone Level Distribution by State",
        y = bquote(O[.(3)] * " (ppm)")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  output$box3 = renderPlot({
    pm_oz_uv() %>% 
      ggplot(aes(x = factor(state, rev(levels(factor(state)))), y = edd, fill = state))+
      geom_violin(color = "#3d0071", draw_quantiles = 0.5) +
      theme_minimal() +
      scale_fill_brewer(palette="BuPu") +
      labs(
        title = "County UV Level Distribution by State",
        y = bquote("UV (J/" * m^2 *")")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
}

shinyApp(ui = ui, server = server)
