
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

#query2 gets the measurements from all sites
#Need to replace the line that restricts the site with a variable to allow changing site

query2 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>


SELECT *
WHERE {
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> <http://envdatapoc.co.nz/id/measurement-site/HRC-00003> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
?site rdfs:label ?name .
?site <http://envdatapoc.co.nz/def/catchment> ?catchment .
?site <http://envdatapoc.co.nz/def/managementZone> ?mgmnt .
?site <http://envdatapoc.co.nz/def/reach> ?reach .
?site <http://envdatapoc.co.nz/def/elevation> ?elevation .

}"

qd2 <- SPARQL(endpoint,query2)
monsitesmeasure <- qd2$results


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
      addProviderTiles("Esri.WorldStreetMap") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, popup = ~siteID, color = "#444444", fillColor = "#Ae51b4", fillOpacity=0.9, stroke=1)
    
  })
  
  #Draw the line chart
  measurementchart <- ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line()
  output$plot1 <- renderPlot({
    ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line()
    })
  
  })

