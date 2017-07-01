
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plyr)

endpoint <- "http://envdatapoc.co.nz/sparql"
query <- "SELECT *
WHERE {
?site <http://envdatapoc.co.nz/def/councilSiteID> ?siteID ;
<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat ;
<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
}"

qd <- SPARQL(endpoint,query)

monsites <- qd$results


server <- (function(input, output, session) {
  
  #Filter the data according to the values entered into the filter text boxes
  #***Need to change this so that instead of it always filtering gap, it filters using the field selected in the filter dropdown
  
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- -40.542788
  lng <- 176.144708
  zoom <- 5
  
  # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("Esri.WorldImagery") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, popup = ~siteID)
    
  })
  
  })

