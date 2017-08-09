
server <- (function(input, output, session) {
  
  
  source('global.R', local=TRUE)
  
  #draw the table in the 'data' tab
  output$table <- DT::renderDataTable(datatable(dtmonsites,colnames = c("Site Name", "Latest flow reading","Time of latest reading (yyyy-mm-dd hh:mm:ss)","Annual Mean Flow", "% Between latest and mean", "Elevation","Catchment","Climate","Geology", "Landcover","Reach","Lat","Long"), escape = 1))
  
  # Put the default map co-ordinates and zoom level for the map into variables
  lat <- -40.542788
  lng <- 176.144708
  zoom <- 6
  
  
  #Hard coded breaks for the binning for markers. Uses library classIntervals
  breakspercdiffmean <- classIntervals(monsites$percdiffmean,n=6, style='fixed', fixedBreaks=c(0,25,50,100,150,200,5000), intervalClosure = 'right')
  breakspercdiffmax <- classIntervals(monsites$percdiffmax,n=6, style='fixed', fixedBreaks=c(0,25,50,100,150,200,10000), intervalClosure = 'right')
  
  #Functions for getting marker colours
  #Many of these are commented out because they are not active in the report, but preserved in case someon wants to re-activate in the future
  #percentage difference from the mean
  palFlowDiffMeanBin <- colorBin("Spectral", domain = breakspercdiffmean$brks, bins = breakspercdiffmean$brks, pretty = FALSE, na.color = '#dddddd')
  ##percentage difference from the max
  #palFlowDiffMaxBin <- colorBin("Spectral", domain = breakspercdiffmax$brks, bins = breakspercdiffmax$brks, pretty = FALSE, na.color = '#dddddd')
  ##elevation
  #palElevation <- colorNumeric(palette = "Spectral",domain = monsites$elevation)
  ##geology
  #palGeology <-colorFactor(palette = "Set2",domain = monsites$geologylabel)
  ##climate
  #palClimate <-colorFactor(palette = "Set1",domain = monsites$climatelabel)
  ##landcover
  #palLandcover <-colorFactor(palette = "Set1",domain = monsites$landcoverlabel)
  ##Just the flow
  #palFlow <- colorNumeric(palette = "Spectral",domain = monsites$value)
  ##Just the meanflow
  #palMeanFlow <- colorNumeric(palette = "Spectral",domain = monsites$meanannflowval)
  ##Just the maxflow
  #palMaxFlow <- colorNumeric(palette = "Spectral",domain = monsites$maxflowval)
  
  #functions to set the size of the markers
  #Based on the most recent flow value
  sizeFlow <- function(value) {
    return(value/10)
  }
  ##Based on the max flow value
  #sizeMaxFlow <- function(value) {
  #  return(value/100)
  #}
  ##based on the percentage differences
  #sizePercFlow <- function(value) {
  #  return((value/20)+10)
  #}
  ##based on the elevation
  #sizeElevation <- function(value) {
  #  return(value/30)
  #}
  
 #Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(provtiles) %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlowDiffMeanBin(percdiffmean), radius=10, fillOpacity=0.9,layerId=monsites$sitesub) %>%
      addLegend("bottomleft",title="Current flow as % of annual mean flow", pal = palFlowDiffMeanBin, values = monsites$percdiffmean, opacity = 1)
      #addPolygons(data = rivers)
      #altpopup for circle markers - in case we revert to popup on click, this popup has the hyperlinked items: paste0('<div class="popuptitle">Site: <a href="http://envdatapoc.co.nz/doc/measurement-site/',hoversite$siteID,'?tab=api">',hoversite$name,'</a></div><div class="popupbody">Latest measurement: <a href="',hoversite$resultsetnoangle,'?tab=api">',formatNos(hoversite$value),'m<sup>3</sup> / sec</a></div><div class="popupbody">The annual mean flow at this site is <a href="',hoversite$meanannflownoangle,'?tab=api">', formatNos(hoversite$meanannflowval),' m<sup>3</sup> / sec</a></div>')        
    
  })
  #Download the data when the button is clicked
  output$downloadData1 <- downloadHandler(
    
    filename = function() { paste(Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(dlmonsites, file)
    }
  )
  
# Introduce the click event so that when a marker is clicked on, it changes the content in the sidebar
observe({
  withProgress(message = "Please wait - getting data.",detail = 'This may take several seconds...', min=0, max = 10, value = 8, { #This adds the progress bar
    click<-input$map_marker_click
  if(is.null(click))
    return()
  #print(click$id)
  query3 <- paste0(query3_1,click$id,query3_2)
  #print(query3)
  qd3 <- SPARQL(endpoint,query3)
  monsitesmeasure <- qd3$results
  monsitesmeasure$datetimeformatted <- as.POSIXct(monsitesmeasure$datetime, origin = "1970-01-01")
  #monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>'),]
  filtmonsites <- monsites[ which(monsites$sitesub==click$id), ]
  chartdata <- data.frame("site" = monsitesmeasure$name,
                          "date" = monsitesmeasure$datetimeformatted,
                          "value" = monsitesmeasure$value)
  dlchartdata <- data.frame("site" = monsitesmeasure$name,
                          "date" = monsitesmeasure$datetimeformatted,
                          "Flow m3 per s)" = monsitesmeasure$value)
  output$plot1 <- renderPlot({
    ggplot(data=chartdata, aes(x=date, y=value, colour="Current")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position="bottom",legend.title=element_blank()) + geom_line() + labs(x="Date/Time", y=expression(River~Flow~m^3/sec)) + geom_hline(aes(yintercept = filtmonsites$meanannflowval, linetype="Mean Annual Flow"), color = "green") + scale_linetype_manual(name="",values=c(2) ,guide = guide_legend(override.aes = list(color = c("green"))))
  })
  # output$plot2_big_line <- renderPlotly({ggplotly(
  #   ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line() + labs(x="Date/Time", y="Flow Volume m3/second")
  # )})
  output$stationname <- renderText(paste0('<a href="http://envdatapoc.co.nz/doc/measurement-site/',filtmonsites$siteID,'?tab=api">',filtmonsites$name,'</a>'))
  output$latestreading <- renderText(paste0('<a href="',filtmonsites$resultsetnoangle,'?tab=api">',formatNos(filtmonsites$value),' m<sup>3</sup> / sec</a>'))
  output$latestdatetime <- renderText(paste0('<a href="',filtmonsites$resultsetnoangle,'?tab=api">',format(as.POSIXct(filtmonsites$latest, origin='1970-01-01'),"%Y-%m-%d %X"),'</a>'))
  output$maxflow <- renderText(paste0(ifelse(is.na(filtmonsites$maxflowval),'Not available',paste0('<a href="',filtmonsites$maxflownoangle,'?tab=api">',formatNos(filtmonsites$maxflowval),' m<sup>3</sup> / sec</a>'))))
  output$meanflow <- renderText(paste0(ifelse(is.na(filtmonsites$meanannflowval),'Not available',paste0('<a href="',filtmonsites$meanannflownoangle,'?tab=api">',formatNos(filtmonsites$meanannflowval),' m<sup>3</sup> / sec</a>'))))
  output$malf <- renderText(paste0(ifelse(is.na(filtmonsites$malfval),'Not available',paste0('<a href="',filtmonsites$malfnoangle,'?tab=api">',formatNos(filtmonsites$malfval),' m<sup>3</sup> / sec</a>'))))
  output$minflow <- renderText(paste0(ifelse(is.na(filtmonsites$minflowval),'Not available',paste0('<a href="',filtmonsites$minflownoangle,'?tab=api">',formatNos(filtmonsites$minflowval),' m<sup>3</sup> / sec</a>'))))
  output$meanfloodflow <- renderText(paste0(ifelse(is.na(filtmonsites$meanfloodflowval),'Not available',paste0('<a href="',filtmonsites$meanfloodflownoangle,'?tab=api">',formatNos(filtmonsites$meanfloodflowval),' m<sup>3</sup> / sec</a>'))))
  output$climate <-renderText(paste0(ifelse(is.na(filtmonsites$climatelabel),'Not available',paste0('<a href="',filtmonsites$climatenoangle,'?tab=api">',filtmonsites$climatelabel,'</a>'))))
  output$elevation <-renderText(paste0(ifelse(is.na(filtmonsites$elevation),'Not available',paste0('<a href="http://envdatapoc.co.nz/doc/measurement-site/',filtmonsites$siteID,'?tab=api">',filtmonsites$elevation,'m</a>'))))
  output$geology <-renderText(paste0(ifelse(is.na(filtmonsites$geologylabel),'Not available',paste0('<a href="',filtmonsites$geologynoangle,'?tab=api">',filtmonsites$geologylabel,'</a>'))))
  output$landcover <-renderText(paste0(ifelse(is.na(filtmonsites$landcover),'Not available',paste0('<a href="',filtmonsites$landcovernoangle,'?tab=api">',filtmonsites$landcoverlabel,'</a>'))))
  output$reachId <-renderText(paste0(ifelse(is.na(filtmonsites$reach),'Not available',paste0('<a href="',filtmonsites$reachnoangle,'?tab=api">',filtmonsites$reachlabel,'</a>'))))
  #chartdatasorted <- chartdata[order(chartdata$date), ]
  #latestmeasurement <- last(chartdatasorted$value)
  
  imgurl <- filtmonsites$image
  image <- ifelse((is.na(imgurl)),paste0('<p></p>'),paste0('<img src="',imgurl,'", height=200, style = "border: solid 1px silver; box-shadow: 5px 5px 2px grey", alt="No Image">')) 
  
  output$photo <- renderText(image)

  
  output$downloadData <- downloadHandler(
    filename = function() { paste(Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(dlchartdata, file)
    }
  )
  
  })
})

# When circle marker is hovered over show a popup
observeEvent(input$map_marker_mouseover$id, {
  radius = 2
  pointId <- input$map_marker_mouseover$id
  hoversite = monsites[monsites$sitesub == pointId, lat]
  latp <- hoversite$lat
  lngp <- hoversite$long
  offset = isolate((input$map_bounds$north - input$map_bounds$south) / (23 + radius + (18 - input$map_zoom)^2 ))
  latoffset <- as.numeric(latp) + offset
  leafletProxy("map") %>% addPopups(lat = latoffset, lng = lngp, paste0('<div class="popuptitle">Site:',hoversite$name,'</div><div class="popupbody">Latest measurement: ',formatNos(hoversite$value),' m<sup>3</sup> / sec</div><div class="popupbody">Annual mean flow: ', ifelse(is.na(hoversite$meanannflowval),'Not available</div>',paste0(formatNos(hoversite$meanannflowval),' m<sup>3</sup> / sec</div>')),'<div>Time of last measurement: ',format(as.POSIXct(hoversite$latest, origin = "1970-01-01"),"%Y-%m-%d %X"),'</div>'))
})

#Clear the popup when mouse leaves the marker
observeEvent(input$map_marker_mouseout$id, {
  leafletProxy("map") %>% clearPopups()
})

#Radio button to change the map background between terrain and satellite
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


#Radio buttons to change the colour and size of the map markers

observe({
    radvar = 10
    if(input$markersize== 'sizena') {
      radvar = 10
      leafletProxy("map") %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlowDiffMeanBin(percdiffmean), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
            addLegend("bottomleft",title="Current flow as % of annual mean flow", pal = palFlowDiffMeanBin, values = monsites$percdiffmean, opacity = 1)

      }
    if(input$markersize== 'latflow') {
      radvar = ~sizeFlow(value)
      leafletProxy("map") %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlowDiffMeanBin(percdiffmean), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
            addLegend("bottomleft",title="Current flow as % of annual mean flow", pal = palFlowDiffMeanBin, values = monsites$percdiffmean, opacity = 1)
    }
    ##This stuff controls the colouring and sizing of markers according to the radio boxes over in the UI. Removed, as they weren't useful, but kept h
    ## in case someone wants to reimplement them in the future.
    # if(input$markersize== 'meanflow') {
    #   radvar = ~sizeFlow(meanannflowval)
    # }
    # if(input$markersize== 'maxflow') {
    #   radvar = ~sizeMaxFlow(maxflowval)
    # }
    # if(input$markersize== 'percmean') {
    #   radvar = ~sizePercFlow(percdiffmean)
    # }
    # if(input$markersize== 'percmax') {
    #   radvar = ~sizePercFlow(percdiffmax)
    # }
    # if(input$markersize== 'elevation') {
    #   radvar = ~sizeElevation(elevation)
    # }
    # if(input$markercolour == 'percmean') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlowDiffMeanBin(percdiffmean), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Current flow as % of annual mean flow", pal = palFlowDiffMeanBin, values = monsites$percdiffmean, opacity = 1)
    #   }
    # if(input$markercolour == 'percmax') {
    #   print("percmax worked")
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlowDiffMaxBin(percdiffmax), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Current flow as % of max flow", pal = palFlowDiffMaxBin, values = monsites$percdiffmax, opacity = 1)
    # }
    # if(input$markercolour == 'latflow') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palFlow(value), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Current flow (m3/s)", pal = palFlow, values = monsites$value, opacity = 1)
    # } 
    # if(input$markercolour == 'meanflow') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palMeanFlow(meanannflowval), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Mean annual flow (m3/s)", pal = palMeanFlow, values = monsites$meanannflowval, opacity = 1)
    # }
    # if(input$markercolour == 'maxflow') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palMaxFlow(maxflowval), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Max flow (m3/s)", pal = palMaxFlow, values = monsites$maxflowval, opacity = 1)
    # }
    # if(input$markercolour == 'elevation') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palElevation(elevation), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Elevation(m)", pal = palElevation, values = monsites$elevation, opacity = 1)
    # }
    # if(input$markercolour == 'geology') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palGeology(geologylabel), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Geology", pal = palGeology, values = monsites$geologylabel, opacity = 1)
    # }
    # if(input$markercolour == 'climate') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palClimate(climatelabel), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Climate", pal = palClimate, values = monsites$climatelabel, opacity = 1)
    # }
    # if(input$markercolour == 'landcover') {
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     addCircleMarkers(data = monsites, color = "#666666",weight = 2, fillColor = ~palLandcover(landcoverlabel), radius=radvar, fillOpacity=0.9,layerId=monsites$sitesub) %>%
    #     addLegend("bottomleft",title="Landcover", pal = palLandcover, values = monsites$landcoverlabel, opacity = 1)
    # }
    ################################
})


##The observer function for the multi-line chart

observeEvent(input$refreshchart, {
  
  withProgress(message = "Please wait - getting data.",detail = 'This may take several seconds...',
  {
  output$nodata <- renderText("<p></p>")
  rivers <- input$sitesel
  rivers <- paste(rivers,sep="", collapse = "','")
  
  fromdate <- input$datepicker[1]
  todate <- input$datepicker[2]
  fromdatesparql <- paste0(input$datepicker[1],'T00:00:00+12:00')
  todatesparql <- paste0(input$datepicker[2],'T00:00:00+12:00')
  
  timediff <- (todate - fromdate)
  if (timediff <= 3) {
    query4filt <- ""
  }
  if (timediff > 3 && timediff <= 21) {
    query4filt <-'FILTER(CONTAINS(str(?datetime),":00:00")) . '
  }
  if (timediff >21) {
    query4filt <- 'FILTER(CONTAINS(str(?datetime),"00:00:00")) . '
  }
 
  query4 <- paste0(query4_1,fromdatesparql,query4_2,todatesparql,query4_3,rivers,query4_4,query4filt,query4_5)
  
  qd4 <- SPARQL(endpoint,query4)
  monsitesmeasuremulti <- qd4$results
  if(nrow(monsitesmeasuremulti)==0) {
    output$nodata <- renderText('<p style = "font-style:italic"><strong>There is no data matching the search criteria that you specified</strong></p>')
    df <- data.frame()
    output$plot2_big_line <- renderPlotly({ggplotly(
       ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
     )})
    return() } 
  monsitesmeasuremulti$datetimeformatted <- as.POSIXct(monsitesmeasuremulti$datetime, origin = "1970-01-01")
  # monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/flow-water-level>'),]
  # filtmonsites <- monsites[ which(monsites$sitesub==click$id), ]
  chartdatamulti <- data.frame("site" = monsitesmeasuremulti$name,
                          "date" = monsitesmeasuremulti$datetimeformatted,
                          "value" = monsitesmeasuremulti$value)
  
  output$plot2_big_line <- renderPlotly({ggplotly(
    ggplot(data=chartdatamulti, aes(x=date, y=value, Group=site)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line(aes(colour=site), size=1.1) + labs(x="Date/Time", y=("River Flow m<sup>3</sup>/second"))) 
  })
  
  output$downloadData2<- downloadHandler(
    filename = function() { paste(Sys.time(), '.csv', sep='') },
    content = function(file) {
      write.csv(chartdatamulti, file)
    }
  )
  
  
  })
})

})

