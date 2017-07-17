
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(plyr) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plotly)

#To add rivers back in need to uncomment this line and the line adding the polygon to the map
#rivers <- readOGR(dsn = 'nz_riverssimp.geojson', layer = 'OGRGeoJSON')

#endpoint <- "http://envdatapoc.co.nz/sparql"
endpoint <- "http://guest:eidipoc@envdatapoc.co.nz/sparql"

#This endpoint is the drafter one, with no authentication or 
#endpoint <- "https://production-drafter-nz.publishmydata.com/v1/sparql/live"

#Query to get all the monitoring sites. Features a subquery to pull out only the most recent measurement for each site
newquery <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT  DISTINCT ?obs ?value ?latest ?sitesub ?lat ?long ?siteID ?name ?reach ?elevation ?mgmnt ?catchmentname ?image ?geologylabel ?landcoverlabel ?climatelabel ?malfval ?maxflowval ?minflowval ?meanannflowval ?meanfloodflowval
WHERE {
{ SELECT (max(?datetime) AS ?latest) ?sitesub
WHERE {
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
?sitesub <http://envdatapoc.co.nz/def/councilSiteID> ?siteID .
?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?valuesub .
OPTIONAL {?sitesub <http://schema.org/image> ?image .}
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat .
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
?sitesub rdfs:label ?name .
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/MALF> ?malf .
?malf <http://qudt.org/1.1/schema/qudt#numericValue> ?malfval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/maxRecordedFlow> ?maxflow .
?maxflow <http://qudt.org/1.1/schema/qudt#numericValue> ?maxflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/meanAnnualFlow> ?meanannflow .
?meanannflow <http://qudt.org/1.1/schema/qudt#numericValue> ?meanannflowval . }
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/meanAnnualFloodFlow> ?meanfloodflow .
?meanfloodflow <http://qudt.org/1.1/schema/qudt#numericValue> ?meanfloodflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/minRecordedFlow> ?minflow .
?minflow <http://qudt.org/1.1/schema/qudt#numericValue> ?minflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/catchment> ?catchment .
?catchment rdfs:label ?catchmentname .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/managementZone> ?mgmnt .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/reach> ?reach .
?reach <http://envdatapoc.co.nz/def/climate> ?climate .
?reach <http://envdatapoc.co.nz/def/geology> ?geology .
?reach <http://envdatapoc.co.nz/def/landcover> ?landcover .
?climate rdfs:label ?climatelabel.
?geology rdf:label ?geologylabel.
?landcover rdfs:label ?landcoverlabel .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/elevation> ?elevation .}
}
GROUP BY ?sitesub
}
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
?sitesub <http://envdatapoc.co.nz/def/councilSiteID> ?siteID .
?obs <http://www.w3.org/ns/sosa/resultTime> ?latest .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
OPTIONAL {?sitesub <http://schema.org/image> ?image .}
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat .
?sitesub	<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
?sitesub rdfs:label ?name .
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/MALF> ?malf .
?malf <http://qudt.org/1.1/schema/qudt#numericValue> ?malfval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/maxRecordedFlow> ?maxflow .
?maxflow <http://qudt.org/1.1/schema/qudt#numericValue> ?maxflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/meanAnnualFlow> ?meanannflow .
?meanannflow <http://qudt.org/1.1/schema/qudt#numericValue> ?meanannflowval . }
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/meanAnnualFloodFlow> ?meanfloodflow .
?meanfloodflow <http://qudt.org/1.1/schema/qudt#numericValue> ?meanfloodflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/minRecordedFlow> ?minflow .
?minflow <http://qudt.org/1.1/schema/qudt#numericValue> ?minflowval .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/catchment> ?catchment .
?catchment rdfs:label ?catchmentname .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/managementZone> ?mgmnt .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/reach> ?reach .
?reach <http://envdatapoc.co.nz/def/climate> ?climate .
?reach <http://envdatapoc.co.nz/def/geology> ?geology .
?reach <http://envdatapoc.co.nz/def/landcover> ?landcover .
?climate rdfs:label ?climatelabel.
?geology rdf:label ?geologylabel.
?landcover rdfs:label ?landcoverlabel .}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/elevation> ?elevation .}
#filter (?latest = ?datetime  ?sitesub = ?site)
}
ORDER BY asc(?siteID)"

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

qd <- SPARQL(endpoint,newquery)


monsites <- qd$results
monsites$percdiffmean <- ((monsites$value - monsites$meanannflowval)/monsites$meanannflowval)*100
monsites$percdiffmax <- (monsites$value - monsites$maxflowval)*100
monsites$obs <- gsub('^.|.$', '', monsites$obs)
#monsites <- monsites[which (monsites$mtype == "<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>"), ]
dtmonsites <- data.frame("Name" = monsites$name,
                         "Latest" = monsites$value,
                         "Perc diff from mean" = monsites$percdiffmean,
                         "Elevation" = monsites$elevation,
                         "Catchment" = monsites$catchmentname,
                         "Lat" = monsites$lat,
                         "Long" = monsites$long,
                         "URI" = monsites$obs)

query3_1 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?resultset ?datetime ?value ?name ?type ?reach
  WHERE {
    ?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
    ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
    ?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
    ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> " 

query3_2 <- " .
    
    ?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
    ?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
    ?obs <http://www.w3.org/ns/sosa/observedProperty> ?type .
    ?site rdfs:label ?name .
    ?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
    OPTIONAL {?site <http://envdatapoc.co.nz/def/reach> ?reach .}
    
    
  }"

query4_1 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?resultset ?datetime ?value ?name ?type ?reach
WHERE {
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
?obs <http://www.w3.org/ns/sosa/observedProperty> ?type .
?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
?site rdfs:label ?name .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
OPTIONAL {?site <http://envdatapoc.co.nz/def/reach> ?reach .}
FILTER ( ?name IN('"

query4_2 <- "'))

}"

provtiles = 'Esri.WorldStreetMap'

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
  palFlowDiffMean <- colorNumeric(
    palette = "PiYG",
    domain = monsites$percdiffmean
  )
  palFlowDiffMax <- colorNumeric(
    palette = "PiYG",
    domain = monsites$percdiffmax
  )
 
 # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(provtiles) %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, popup = ~name, color = "#444444", fillColor = ~palFlowDiffMax(percdiffmax), fillOpacity=0.9, stroke=1,layerId=monsites$sitesub) %>%
      addLegend("bottomleft", pal = palFlowDiffMax, values = monsites$percdiffmax, opacity = 1)
      #addPolygons(data = rivers)
                
    
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
  monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>'),]
  filtmonsites <- monsites[ which(monsites$sitesub==click$id), ]
  chartdata <- data.frame("site" = monsitesflow$name,
                          "date" = monsitesflow$datetimeformatted,
                          "value" = monsitesflow$value)
  output$plot1 <- renderPlot({
    ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line()
  })
  output$plot2_big_line <- renderPlot({
    ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line() 
  })
  output$stationname <- renderText(sprintf(filtmonsites$name))
  output$maxflow <- renderText(paste0(filtmonsites$maxflowval,' m3 / sec'))
  output$meanflow <- renderText(paste0("<a href='",filtmonsites$obs,"'>",filtmonsites$meanannflowval," m3 / sec</a>"))
  output$climate <-renderText(paste0(filtmonsites$climatelabel))
  output$elevation <-renderText(paste0(filtmonsites$elevation,'m'))
  output$geology <-renderText(paste0(filtmonsites$geologylabel))
  output$landcover <-renderText(paste0(filtmonsites$landcoverlabel))
  chartdatasorted <- chartdata[order(chartdata$date), ]
  latestmeasurement <- last(chartdatasorted$value)
  imgsource <- filtmonsites$image
  output$photo <- renderText({
       c('<img src="',imgsource,'", height=300, style = "border: solid 1px silver; box-shadow: 5px 5px 2px grey", alt="No Image">')
    })
  output$latestreading <- renderText(paste0(latestmeasurement,' m3 / sec'))
  output$downloadData <- downloadHandler(
    filename = function() { paste(Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(chartdata, file)
    }
  )
})



observe({
  if(input$mapbackground == 'terr') {
    provtiles <- 'Esri.WorldStreetMap'
  }
  else
  {
    provtiles <- 'Esri.WorldImagery'
  }
  leafletProxy("map") %>%
    addProviderTiles(provtiles)
})

observeEvent(input$refreshchart, {
  
  click <- input$sitesel
  click <- paste(click,sep="", collapse = "','")
  print(click)
  # click <- "63101"
  if(is.null(click))
    return()
  print(click)
  query4 <- paste0(query4_1,click,query4_2)
  print(query4)
  qd4 <- SPARQL(endpoint,query4)
  monsitesmeasuremulti <- qd4$results
  monsitesmeasuremulti$datetimeformatted <- as.POSIXct(monsitesmeasuremulti$datetime, origin = "1970-01-01")
  # monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>'),]
  # filtmonsites <- monsites[ which(monsites$sitesub==click$id), ]
  chartdatamulti <- data.frame("site" = monsitesmeasuremulti$name,
                          "date" = monsitesmeasuremulti$datetimeformatted,
                          "value" = monsitesmeasuremulti$value)
  
  output$plot2_big_line <- renderPlotly({ggplotly(
    ggplot(data=chartdatamulti, aes(x=date, y=value, Group=site)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line(aes(colour=site), size=1.5) + labs(x="Date/Time", y="Flow Volume m3/second")) 
  })
  
})

})

