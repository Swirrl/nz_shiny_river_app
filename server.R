
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plyr)

#endpoint <- "http://envdatapoc.co.nz/sparql"

#This endpoint is th 
endpoint <- "https://production-drafter-nz.publishmydata.com/v1/sparql/live"

#Query to get all the monitoring sites. Features a subquery to pull out only the most recent measurement for each site
newquery <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT  DISTINCT ?obs ?value ?latest ?sitesub ?lat ?long ?siteID ?name ?reach ?elevation ?mgmnt ?catchmentname ?image ?mtype
WHERE {
  { SELECT (max(?datetime) AS ?latest) ?sitesub ?mtype
WHERE {
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/observedProperty> ?mtype .
?sitesub <http://envdatapoc.co.nz/def/councilSiteID> ?siteID .
?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?valuesub .
OPTIONAL {?sitesub <http://schema.org/image> ?image .}
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat .
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
?sitesub rdfs:label ?name .
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/catchment> ?catchment .
?catchment rdfs:label ?catchmentname .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/managementZone> ?mgmnt .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/reach> ?reach .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/elevation> ?elevation .}
}
GROUP BY ?sitesub ?mtype
}
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/observedProperty> ?mtype .
?sitesub <http://envdatapoc.co.nz/def/councilSiteID> ?siteID .
?obs <http://www.w3.org/ns/sosa/resultTime> ?latest .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
OPTIONAL {?sitesub <http://schema.org/image> ?image .}
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat .
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
?sitesub rdfs:label ?name .
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/catchment> ?catchment .
?catchment rdfs:label ?catchmentname .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/managementZone> ?mgmnt .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/reach> ?reach .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/elevation> ?elevation .}
#filter (?latest = ?datetime  ?sitesub = ?site)
}
ORDER BY asc(?siteID)"


qd <- SPARQL(endpoint,newquery)


monsites <- qd$results
monsites <- monsites[which (monsites$mtype == "<https://registry.scinfo.org.nz/lab/nems/def/property/FLow+%5BWater+Level%5D>"), ]
dtmonsites <- data.frame("Name" = monsites$name,
                         "Latest" = monsites$value,
                         "Elevation" = monsites$elevation,
                         "Catchment" = monsites$catchmentname,
                         "Lat" = monsites$lat,
                         "Long" = monsites$long)

query3_1 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?resultset ?datetime ?value ?name ?type ?reach
  WHERE {
    ?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
    ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
    ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> " 

query3_2 <- " .
    
    ?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
    ?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
    ?obs <http://www.w3.org/ns/sosa/observedProperty> ?type .
    ?site rdfs:label ?name .
    ?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
    OPTIONAL {?site <http://envdatapoc.co.nz/def/reach> ?reach .}
    
    
  }"

provtiles = 'Esri.WorldImagery'

server <- (function(input, output, session) {
  
  #draw the table in the 'data' tab
  output$table <- DT::renderDataTable(datatable(dtmonsites, escape = TRUE))
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- -40.542788
  lng <- 176.144708
  zoom <- 5
  
  #set the palettes for the circle markers
  pal <- colorNumeric(
    palette = "Blues",
    domain = monsites$elevation)
  
  pal2 <-colorFactor(
    palette = "Set1",
    domain = monsites$reach)
  
  palFlow <- colorNumeric(
    palette = "PiYG",
    domain = monsites$value
  )
  
 
 # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(provtiles) %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, popup = ~name, color = "#444444", fillColor = ~palFlow(value), fillOpacity=0.9, stroke=1,layerId=monsites$sitesub) %>%
      addLegend("bottomleft", pal = palFlow, values = monsites$value, opacity = 1)
                
    
  })
  
  # Introduce the click event so that when a marker is clicked on, it changes the content in the sidebar
observe({
  click<-input$map_marker_click
  if(is.null(click))
    return()
  print(click$id)
  query3 <- paste0(query3_1,click$id,query3_2)
  #print(query3)
  qd3 <- SPARQL(endpoint,query3)
  monsitesmeasure <- qd3$results
  monsitesmeasure$datetimeformatted <- as.POSIXct(monsitesmeasure$datetime, origin = "1970-01-01")
  monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/FLow+%5BWater+Level%5D>'),]
  filtmonsites <- monsites[ which(monsites$sitesub==click$id), ]
  chartdata <- data.frame("site" = monsitesflow$name,
                          "date" = monsitesflow$datetimeformatted,
                          "value" = monsitesflow$value)
  output$plot1 <- renderPlot({
    ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line()
  })
  output$stationname <- renderText(sprintf(filtmonsites$name))
  output$maxflow <- renderText(paste0(max(chartdata$value, na.rm = TRUE),' L / sec'))
  output$climate <-renderText('Warm and wet')
  output$elevation <-renderText(paste0(filtmonsites$elevation,'m'))
  output$geology <-renderText('Volcanic basic')
  chartdatasorted <- chartdata[order(chartdata$date), ]
  latestmeasurement <- last(chartdatasorted$value)
  imgsource <- filtmonsites$image
  output$photo <- renderText({
       c('<img src="',imgsource,'", height=300, style = "border: solid 1px silver; box-shadow: 5px 5px 2px grey", alt="No Image">')
    })
  output$latestreading <- renderText(paste0(latestmeasurement,' L / sec'))
})

observe({
  if(input$mapbackground == 'terr') {
    provtiles <- 'Esri.WorldStreetMap'
  }
  else
  {
    provtiles <- 'Esri.WorldImagery'
  }
  print(provtiles)
  leafletProxy("map") %>%
    addProviderTiles(provtiles)
})

})

