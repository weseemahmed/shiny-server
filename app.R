library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(shiny)
library(shinythemes)
library(rmapshaper)
library(tmap)


provinces.sf <- st_read("Shapefiles/Provinces", stringsAsFactors = FALSE, quiet = TRUE) %>% st_transform(4326)

er.sf <- st_read("Shapefiles/Economic Regions", stringsAsFactors = FALSE, quiet = TRUE) %>% st_transform(4326)

cd.sf <- st_read("Shapefiles/Census Divisions", stringsAsFactors = FALSE, quiet = TRUE) %>% st_transform(4326)


master.pr <- read.csv("data.pr.csv", 
                      header = T, stringsAsFactors = F)

master.er <- read.csv("data.er.csv", 
                      header = T, stringsAsFactors = F)

master.cd <- read.csv("data.cd.csv", header = T, stringsAsFactors = F)
master.cd <- master.cd[,-1]


pr.geo <- ms_simplify(provinces.sf[,"PRNAME", "geometry"], keep = 0.1)
er.geo <- ms_simplify(er.sf[, "ERNAME", "geometry"], keep = 0.1)
cd.geo <- ms_simplify(cd.sf[, c("CDNAME", "CDUID", "geometry")], keep = 0.1)


Provinces <- merge(master.pr, pr.geo, by = "PRNAME") %>% st_as_sf()
Economic_Regions <- merge(master.er, er.geo, by = "ERNAME") %>% st_as_sf()
Census_Divisions <- merge(cd.geo, master.cd,by = "CDUID") %>% st_as_sf()
Census_Divisions <- Census_Divisions[, -1]


varlist <- setdiff(names(Provinces[,-1]), "geometry")

mypal <- c("white", "#005f88")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = "100%"),
  absolutePanel(top = 15, left = 85, draggable = T,
                  selectInput("var", label = "Select Cluster", choices = varlist, selected = "Aerospace Vehicles and Defense", 
                              width = "400px")  
                  , width = "400px")
      
  )
  
server <- (function(input, output) {
    output$map = renderLeaflet({

        tm <- tm_basemap(leaflet::providers$OpenStreetMap) +
          tm_shape(Provinces) +
          tm_fill(col = input$var, legend.show = FALSE, palette = mypal, breaks = c(0,1,2,3,4,5,6), 
                  popup.vars = c("Location Quotient: " =input$var)) +
          
          tm_polygons(input$var, border.col = "black") +
          tm_shape(Economic_Regions) +
          tm_fill(col = input$var, legend.show = FALSE, palette = mypal, breaks = c(0,1,2,3,4,5,6), 
                  popup.vars = c("Location Quotient: " = input$var)) +
          
          tm_polygons(input$var, palette = "Blues", border.col = "black") +
          tm_shape(Census_Divisions) +
          tm_fill(col = input$var, legend.show = T, title = "Location Quotient", palette = mypal, breaks = c(0,1,2,3,4,5,6), 
                  labels = c("0 to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5", "5+"), popup.vars = c("Location Quotient: " = input$var)) +
          
          tm_polygons(input$var, border.col = "black") 
        
      
      tmap_leaflet(tm) %>%
        setMaxBounds(-174, 30, -35, 77) %>%
        setView(lng = -91, lat = 61, zoom = 4) %>%
        leaflet::hideGroup(list("Economic_Regions", "Census_Divisions")) 
    })
  })
shinyApp(ui, server)


