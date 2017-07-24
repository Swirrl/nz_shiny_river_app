
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

startquery <-  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT DISTINCT *
WHERE {
{SELECT (max(?datetime) AS ?latest) ?sitesub
WHERE {
?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .
?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
}
GROUP BY ?sitesub
      }
?obs <http://www.w3.org/ns/sosa/resultTime> ?latest .
?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?sitesub .  
?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
?obs <http://www.w3.org/ns/sosa/observedProperty> <https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level> .
?sitesub <http://envdatapoc.co.nz/def/lawaSiteID> ?siteID .
?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
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
?geology rdfs:label ?geologylabel.
?landcover rdfs:label ?landcoverlabel .
}
OPTIONAL {?sitesub <http://envdatapoc.co.nz/def/elevation> ?elevation .}
OPTIONAL {?sitesub <http://schema.org/image> ?image .}
}"



#function that formats numbers
formatNos <- function (x) {
  formatC(x, digits = 2, format = "f", big.mark = ",")
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

qd <- SPARQL(endpoint,startquery)



monsites <- qd$results
monsites$percdiffmean <- ((monsites$value - monsites$meanannflowval)/monsites$meanannflowval)*100

#highlow <- function (x,y) {ifelse ((x > y),'HIGHER','LOWER')}

monsites$percdiffmax <- (monsites$value - monsites$maxflowval)*100
monsites$obsnoangle <- gsub('^.|.$', '', monsites$obs)
monsites$sitesubnoangle <- gsub('^.|.$', '', monsites$sitesub)
monsites$maxflownoangle <- gsub('^.|.$', '', monsites$maxflow)
monsites$climatenoangle <- gsub('^.|.$', '', monsites$climate)
monsites$geologynoangle <- gsub('^.|.$', '', monsites$geology)
monsites$landcovernoangle <- gsub('^.|.$', '', monsites$landcover)
monsites$malfnoangle <- gsub('^.|.$', '', monsites$malf)
monsites$minflownoangle <- gsub('^.|.$', '', monsites$minflow)
monsites$meanannflownoangle <- gsub('^.|.$', '', monsites$meanannflow)
monsites$meanfloodflownoangle <- gsub('^.|.$', '', monsites$meanfloodflow)
monsites$resultsetnoangle <- gsub('^.|.$', '', monsites$resultset)
#monsites <- monsites[which (monsites$mtype == "<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>"), ]
dtmonsites <- data.frame("Name" = monsites$name,
                         "Latest" = monsites$value,
                         "Date/Time" = as.POSIXct(monsites$latest, origin = "1970-01-01"),
                         "Annual Mean Flow" = monsites$meanannflowval,
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

#This is the query that powers the multi-line chart
query4_1 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
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
FILTER (?datetime > '"

query4_2 <- "'^^xsd:dateTime && ?datetime < '"

query4_3 <- "'^^xsd:dateTime && ?name IN('"

query4_4 <- "'))

}"

provtiles = 'Esri.WorldStreetMap'

server <- (function(input, output, session) {
  
  
  
  #draw the table in the 'data' tab
  output$table <- DT::renderDataTable(datatable(dtmonsites, escape = TRUE))
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- -40.542788
  lng <- 176.144708
  zoom <- 6
  
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
 
  palFlowDiffMeanDecile <- colorQuantile("Blues", monsites$percdiffmean, n=10)
  
  palFlowDiffMeanBin <- colorBin("PiYG", monsites$percdiffmean, 10, pretty = TRUE)
  
 # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(provtiles) %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, color = "#ffffff",weight = 2, fillColor = ~palFlowDiffMeanDecile(percdiffmean), radius=15, fillOpacity=0.9,layerId=monsites$sitesub) %>%
      addLegend("bottomleft", pal = palFlowDiffMeanDecile, values = monsites$percdiffmean, opacity = 1)
      #addPolygons(data = rivers)
      #altpopup for circle markers - in case we revert to popup on click, this popup has the hyperlinked items: paste0('<div class="popuptitle">Site: <a href="http://envdatapoc.co.nz/doc/measurement-site/',hoversite$siteID,'?tab=api">',hoversite$name,'</a></div><div class="popupbody">Latest measurement: <a href="',hoversite$resultsetnoangle,'?tab=api">',formatNos(hoversite$value),'m<sup>3</sup> / sec</a></div><div class="popupbody">The annual mean flow at this site is <a href="',hoversite$meanannflownoangle,'?tab=api">', formatNos(hoversite$meanannflowval),' m<sup>3</sup> / sec</a></div>')        
    
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
  output$stationname <- renderText(paste0('<a href="http://envdatapoc.co.nz/doc/measurement-site/',filtmonsites$siteID,'?tab=api">',filtmonsites$name,'</a>'))
  output$latestreading <- renderText(paste0('<a href="',filtmonsites$resultsetnoangle,'?tab=api">',formatNos(filtmonsites$value),' m<sup>3</sup> / sec</a>'))
  output$latestdatetime <- renderText(paste0('<a href="',filtmonsites$resultsetnoangle,'?tab=api">',as.POSIXct(filtmonsites$latest, origin='1970-01-01'),'</a>'))
  output$maxflow <- renderText(paste0('<a href="',filtmonsites$maxflownoangle,'?tab=api">',formatNos(filtmonsites$maxflowval),' m<sup>3</sup> / sec</a>'))
  output$meanflow <- renderText(paste0('<a href="',filtmonsites$meanannflownoangle,'?tab=api">',formatNos(filtmonsites$meanannflowval),' m<sup>3</sup> / sec</a>'))
  output$malf <- renderText(paste0('<a href="',filtmonsites$malfnoangle,'?tab=api">',formatNos(filtmonsites$malfval),' m<sup>3</sup> / sec</a>'))
  output$minflow <- renderText(paste0('<a href="',filtmonsites$minflownoangle,'?tab=api">',formatNos(filtmonsites$minflowval),' m<sup>3</sup> / sec</a>'))
  output$meanfloodflow <- renderText(paste0('<a href="',filtmonsites$meanfloodflownoangle,'?tab=api">',formatNos(filtmonsites$meanfloodflowval),' m<sup>3</sup> / sec</a>'))
  output$climate <-renderText(paste0('<a href="',filtmonsites$climatenoangle,'?tab=api">',filtmonsites$climatelabel,'</a>'))
  output$elevation <-renderText(paste0('<a href="http://envdatapoc.co.nz/doc/measurement-site/',filtmonsites$siteID,'?tab=api">',filtmonsites$elevation,'m</a>'))
  output$geology <-renderText(paste0('<a href="',filtmonsites$geologynoangle,'?tab=api">',filtmonsites$geologylabel,'</a>'))
  output$landcover <-renderText(paste0('<a href="',filtmonsites$landcovernoangle,'?tab=api">',filtmonsites$landcoverlabel,'</a>'))
  chartdatasorted <- chartdata[order(chartdata$date), ]
  latestmeasurement <- last(chartdatasorted$value)
  imgsource <- filtmonsites$image
  output$photo <- renderText({
       c('<img src="',imgsource,'", height=300, style = "border: solid 1px silver; box-shadow: 5px 5px 2px grey", alt="No Image">')
    })
  #output$latestreading <- renderText(paste0(latestmeasurement,' m3 / sec')1
  output$downloadData <- downloadHandler(
    filename = function() { paste(Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(chartdata, file)
    }
  )
})

observeEvent(input$map_marker_mouseout$id, {
  leafletProxy("map") %>% clearPopups()
})

# When circle is hovered over...show a popup
observeEvent(input$map_marker_mouseover$id, {
  radius = 2
  pointId <- input$map_marker_mouseover$id
  hoversite = monsites[monsites$sitesub == pointId, lat]
  latp <- hoversite$lat
  lngp <- hoversite$long
  offset = isolate((input$map_bounds$north - input$map_bounds$south) / (23 + radius + (18 - input$map_zoom)^2 ))
  latoffset <- as.numeric(latp) + offset
  leafletProxy("map") %>% addPopups(lat = latoffset, lng = lngp, paste0('<div class="popuptitle">Site:',hoversite$name,'</div><div class="popupbody">Latest measurement: ',formatNos(hoversite$value),' m<sup>3</sup> / sec</div><div class="popupbody">Annual mean flow: ', formatNos(hoversite$meanannflowval),' m<sup>3</sup> / sec</div><div>Time of last measurement: ',as.POSIXct(hoversite$latest, origin = "1970-01-01"),'</div>'))
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
  
  rivers <- input$sitesel
  rivers <- paste(rivers,sep="", collapse = "','")
  fromdate <- input$datepicker[1]
  todate <- input$datepicker[2]
  print(as.POSIXct(fromdate))
  print(as.POSIXct(todate))
  print(click)
  #click <- "Hautapu at Alabasters"
  if(is.null(click))
    return()
  print(click)
  query4 <- paste0(query4_1,fromdate,query4_2,todate,query4_3,rivers,query4_4)
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
    ggplot(data=chartdatamulti, aes(x=date, y=value, Group=site)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line(aes(colour=site), size=1.1) + labs(x="Date/Time", y="Flow Volume m3/second")) 
  })
  
})

})

