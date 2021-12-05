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
us_counties = tigris::counties(c("NY", "PA", "OH"))

# Filter df for the year desired and merge with the sf object
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
  
  if(y == 2001){res = merged_2001_df}
  if(y == 2002){res = merged_2002_df}
  if(y == 2003){res = merged_2003_df}
  if(y == 2004){res = merged_2004_df}
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
  if(y == 2016){res = merged_2016_df}
  
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

merged_2001_df = filter_merge(2001)
merged_2002_df = filter_merge(2002)
merged_2003_df = filter_merge(2003)
merged_2004_df = filter_merge(2004)
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
merged_2016_df = filter_merge(2016)

ui = fluidPage(
  
  titlePanel("Annual Air Pollution and UV Radiation Map"),
  
  sidebarLayout(
    sidebarPanel(
      tags$a(href = "https://ephtracking.cdc.gov/download", "Data Source", target = "_blank"),
      h5("Some climate conditions and a few particular chronic disease risks are known to be highly correlated. Let's explore the Particulate Matter //expression(PM_{2.5}), Ozone (O_{3}) and UV radiation (edd) levels over the years in New York, Pennsylvania, and Ohio."),
      selectInput("yr", "Select a year", choices = unique(apuv_df$year)),
      selectInput("ss", "Select a season", choices = unique(apuv_df$season))
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
    
    pou = select_year_season(input$yr, input$ss)
    return(pou)
    
  })
  
  output$pm25 = renderLeaflet({
    pal = colorBin(palette = "YlGn", 9, domain = c(as.numeric(ext_val[4]), as.numeric(ext_val[1])))
    # labels = sprintf("some label")
    pm_oz_uv() %>% 
      leaflet() %>% 
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
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
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
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
      addProviderTiles(provider = "Stamen.Toner") %>% 
      setView(-78, 41.8, zoom = 5.5) %>% 
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
