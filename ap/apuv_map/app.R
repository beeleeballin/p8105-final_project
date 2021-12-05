#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(tigris)

setwd("/Users/beelee/Desktop/Columbia/Fall_2021/P8105-Data_Science/p8105-final_project/ap/apuv_map/")
apuv_df = readRDS("apuv.RDS")
ext_val = readRDS("ext_val.RDS")

us_counties = 
  tigris::counties(c("NY", "PA", "OH"))

# county_url = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
# counties = geojsonio::geojson_read(county_url, what = "sp")
# 
# install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")
# 
# ggplot(ny_counties) + 
#   geom_sf() + 
#   theme_void()

merged_2001_df = 
  apuv_df %>% 
  filter(year == 2001) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2002_df = 
  apuv_df %>% 
  filter(year == 2002) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2003_df = 
  apuv_df %>% 
  filter(year == 2003) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2004_df = 
  apuv_df %>% 
  filter(year == 2004) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2005_df = 
  apuv_df %>% 
  filter(year == 2005) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2006_df = 
  apuv_df %>% 
  filter(year == 2006) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2007_df = 
  apuv_df %>% 
  filter(year == 2007) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2008_df = 
  apuv_df %>% 
  filter(year == 2008) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2009_df = 
  apuv_df %>% 
  filter(year == 2009) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2010_df = 
  apuv_df %>% 
  filter(year == 2010) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2011_df = 
  apuv_df %>% 
  filter(year == 2011) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2012_df = 
  apuv_df %>% 
  filter(year == 2012) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2013_df = 
  apuv_df %>% 
  filter(year == 2013) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2014_df = 
  apuv_df %>% 
  filter(year == 2014) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2015_df = 
  apuv_df %>% 
  filter(year == 2015) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_2016_df = 
  apuv_df %>% 
  filter(year == 2016) %>%
  geo_join(us_counties, ., "GEOID",  "countyfips") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

merged_df = list(merged_2001_df, merged_2002_df)


ui = fluidPage(
  
  titlePanel("Annual Air Pollution and UV Radiation Map"),
  
  sidebarLayout(
    sidebarPanel(
      tags$a(href = "https://ephtracking.cdc.gov/download", "Data Source", target = "_blank"),
      h5("Explaine your data"),
      selectInput("yr", "Select a year", choices = unique(apuv_df$year))
      # selectInput("st", "Select a state", choices = unique(apuv_df$state))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Particulate Matter 2.5 Level", leafletOutput("pm25")),
        tabPanel("Ozone Level", leafletOutput("o3")),
        tabPanel("UV Radiation Level", leafletOutput("edd"))
      )
    )
  )
)


server = function(input, output) {
  
  pm_oz_uv = reactive({
    if(input$yr == 2001){pou = merged_2001_df}
    if(input$yr == 2002){pou = merged_2002_df}
    if(input$yr == 2003){pou = merged_2003_df}
    if(input$yr == 2004){pou = merged_2004_df}
    if(input$yr == 2005){pou = merged_2005_df}
    if(input$yr == 2006){pou = merged_2006_df}
    if(input$yr == 2007){pou = merged_2007_df}
    if(input$yr == 2008){pou = merged_2008_df}
    if(input$yr == 2009){pou = merged_2009_df}
    if(input$yr == 2010){pou = merged_2010_df}
    if(input$yr == 2011){pou = merged_2011_df}
    if(input$yr == 2012){pou = merged_2012_df}
    if(input$yr == 2013){pou = merged_2013_df}
    if(input$yr == 2014){pou = merged_2014_df}
    if(input$yr == 2015){pou = merged_2015_df}
    if(input$yr == 2016){pou = merged_2016_df}
    return(pou)
  })
  
  output$pm25 = renderLeaflet({
    pal = colorBin(palette = "YlGn", 9, domain = c(as.numeric(ext_val[4]), as.numeric(ext_val[1])))
    # labels = sprintf("some label")
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      # addProviderTiles(provider = "Stamen.TonerBackground") %>% 
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(pm_oz_uv()$pm25_pop_pred),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~pm25_pop_pred,
                title = "some legend title",
                opacity = 0.7)
  })
  
  output$o3 = renderLeaflet({
    pal = colorBin(palette = "OrRd", 9, domain = c(as.numeric(ext_val[5]),as.numeric(ext_val[2])))
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(pm_oz_uv()$o3_pop_pred),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~o3_pop_pred,
                title = "some legend title",
                opacity = 0.7)
  })
  
  output$edd = renderLeaflet({
    pal = colorBin(palette = "PuBu", 9, domain = c(as.numeric(ext_val[6]),as.numeric(ext_val[3])))
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(pm_oz_uv()$edd),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~edd,
                title = "some legend title",
                opacity = 0.7)
  })
  
}

shinyApp(ui = ui, server = server)
