
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plyr)

endpoint <- "http://ons.publishmydata.com/sparql"
scotpayquery <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?s ?value ?areaname ?sexname ?areacode ?statname WHERE {
?s qb:dataSet <http://statistics.data.gov.uk/data/annual-survey-of-hours-and-earnings-2016-earnings> ;
<http://statistics.data.gov.uk/def/measure-properties/value> ?value ;
<http://statistics.data.gov.uk/def/dimension/earnings> <http://statistics.data.gov.uk/def/concept/earnings/annual-pay-gross>;
<http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?area;
<http://statistics.data.gov.uk/def/dimension/earningsStatistics> ?statcode;
<http://purl.org/linked-data/sdmx/2009/dimension#sex> ?sex;
<http://statistics.data.gov.uk/def/dimension/workingPattern> <http://statistics.data.gov.uk/def/concept/working-pattern/full-time> .
?area <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/S12> ;
<http://statistics.data.gov.uk/def/statistical-geography#officialname> ?areaname .
?sex rdfs:label ?sexname .
?area rdfs:label ?areacode .
?statcode rdfs:label ?statname .
}"

#This is the megaquery that pulls a range of indicators from Scottish Datastore
scotendpoint <- "http://statistics.gov.scot/sparql"
scotstatquery <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?s ?areaname ?indicatorlabel ?yearname ?value ?areacode
WHERE {
{?s qb:dataSet data:job-seekers-allowance;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/quarter/2012-Q4> ;
<http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/16-64> ;
<http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all> .
data:job-seekers-allowance rdfs:label ?indicatorlabel}
UNION
{?s qb:dataSet data:dwellings-hectare;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2012> .
data:dwellings-hectare rdfs:label ?indicatorlabel}
UNION
{?s qb:dataSet data:smoking-at-booking;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2013-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/current-smoker> .
BIND ('Antenatal smoking' as ?indicatorlabel) .}
UNION
{?s qb:dataSet data:low-birthweight;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2013-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/birthWeight> <http://statistics.gov.scot/def/concept/birth-weight/low-weight-births> .
data:low-birthweight rdfs:label ?indicatorlabel}
UNION
{?s qb:dataSet data:fire;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2012> ;
<http://statistics.gov.scot/def/dimension/indicator(fire)> <http://statistics.gov.scot/def/concept/indicator-fire/deliberate-fires-excluding-chimney-fires-per-100-000-population> .
BIND ('Deliberate fires' as ?indicatorlabel) .}
UNION
{?s qb:dataSet data:breastfeeding;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2013> ;
<http://statistics.gov.scot/def/dimension/breastfeedingDataCollectionTime> <http://statistics.gov.scot/def/concept/breastfeeding-data-collection-time/first-visit> ;
<http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/breastfed> .
data:breastfeeding rdfs:label ?indicatorlabel}
UNION
{?s qb:dataSet data:fuel-poverty-shcs;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2010-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/all> ;
<http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all> ;
<http://statistics.gov.scot/def/dimension/disabilityStatus> <http://statistics.gov.scot/def/concept/disability-status/all> .
BIND ('Perc of households in fuel poverty' as ?indicatorlabel) .}
UNION
{?s qb:dataSet data:alcohol-related-discharge ;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/government-year/2012-2013> .
data:alcohol-related-discharge rdfs:label ?indicatorlabel} .
?s sdmxd:refArea ?area ;
sdmxd:refPeriod ?year .
?year rdfs:label ?yearname .
?area stat:code <http://statistics.gov.scot/id/statistical-entity/S12> ;
rdfs:label ?areaname ;
skos:notation ?areacode
}"

# SPARQL results from ONS are too big to run succesfully
#*** need to break it up into smaller chunks so it'll process it
#qd <- SPARQL(endpoint,query)


# Load the constituency geography file
scotcouncil <- readOGR("ScottishCouncilAreas2_simplified.geojson", "OGRGeoJSON")

# Load the csv file containing the pay data
pgdata <- read.csv2("ons_ashe_scot_paygap.csv",header = TRUE, sep=",")
#Swap the URI prefixes for ones that work
pgdata <- lapply(pgdata, function(x) {gsub("http://statistics.data.gov.uk", "http://ons.publishmydata.com", x)})

#Load the csv file containing the scottish government data (remove csv to make it run)
sgdata <- read.csv2("scot_stat_data.csv",header = TRUE, sep=",")

#Alternative way of getting data - SPARQL. Getting an error "Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
#arguments imply differing number of rows: 160, 0" Think its something to do with the shape of the response
#sgdata <- SPARQL(scotendpoint,scotstatquery)

#Turn the value column into from scientific notation to number
pgdata <- transform(pgdata, value = as.numeric(as.character(value)))
sgdata <- transform(sgdata, value = as.numeric(as.character(value)))

#Turn into dataframes
pgdata <- as.data.frame(pgdata)
sgdata <- as.data.frame(sgdata)

#Pick out only the median statistic from the paygap data (ie discarding distribution)
pgdata2 <- pgdata[ which(pgdata$statname == "Median"), ]

#Pivot the ONS Data to get the URI
pgdata3 <- dcast(pgdata2,areaname + areacode ~ sexname, value.var = 's')

#Pivot the data row headers to the left of the tilde, column headers to the right, then value
pgdata2 <- dcast(pgdata2,areaname + areacode ~ sexname, value.var = 'value')

#Add in the calculated field for the gap
pgdata2$gap <- with(pgdata2,Male - Female)
#merge the two pivoted 
pgdata2 <- merge(pgdata2,pgdata3,by="areacode")


sgdata2 <- dcast(sgdata,areaname + areacode ~ indicatorlabel, value.var = 'value')
sgdata3 <- dcast(sgdata,areaname + areacode ~ indicatorlabel, value.var = 's')

sgdata2 <- merge(sgdata2,sgdata3,by="areacode")

#pgdist <- pgdata[ which(pgdata$year == "2016"), ]

#Pivot the distribution of pay across the deciles. Think this will not be needed for this particular report
pgdist2 <- dcast(pgdata,areaname + areacode + statname ~ sexname, value.var = 'value')

#Again, add the calculated field to this dataframe
pgdist2$gap <- with(pgdist2, Male - Female)

#Pluck the national figure out of the data
pgdist2Nat <- pgdist2[ which(pgdist2$areacode == "K03000001"),]

#Merge the two dataframes into one, horizontally
combdata <- merge(pgdata2,sgdata2,by="areacode")

#put in the ranks of each in
combdata$MedianPay.Rank[order(combdata$All.x, decreasing = TRUE)] <- 1:nrow(combdata)
combdata$MedianPayF.Rank[order(combdata$Female.x, decreasing = TRUE)] <- 1:nrow(combdata)
combdata$MedianPayM.Rank[order(combdata$Male.x, decreasing = TRUE)] <- 1:nrow(combdata)
combdata$PayGap.Rank[order(combdata$gap, decreasing = FALSE)] <- 1:nrow(combdata)
combdata$AlcoholDisch.Rank[order(combdata$`Alcohol Related Hospital Discharge.x`, decreasing = FALSE)] <- 1:nrow(combdata)
combdata$Breastfeeding.Rank[order(combdata$Breastfeeding.x, decreasing = TRUE)] <- 1:nrow(combdata)
combdata$DelibFires.Rank[order(combdata$`Deliberate fires.x`, decreasing = FALSE)] <- 1:nrow(combdata)
combdata$Dwellings.Rank[order(combdata$`Dwellings per Hectare.x`, decreasing = FALSE)] <- 1:nrow(combdata)
combdata$JSA.Rank[order(combdata$`Job Seeker's Allowance Claimants.x`, decreasing = FALSE)] <- 1:nrow(combdata)


server <- (function(input, output, session) {
  
  #Filter the data according to the values entered into the filter text boxes
  #***Need to change this so that instead of it always filtering gap, it filters using the field selected in the filter dropdown
  
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- 57.542788
  lng <- 0.144708
  zoom <- 6
  
  # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addPolygons(data = scotcouncil, opacity=1, color = "black", weight = 1, fillOpacity=0.8, layerId = scotcouncil$CODE)
    
  })
  
  observe({
    
    #selected <- reactive({
    #  subset(combdata,
    #         (dynamicFilterValue < input$upper & dynamicFilterValue >= input$lower) | is.na(dynamicFilterValue))
    #})
    
    #merge the data from the csv / sparql with the geojson data for mapping
    scotcouncil@data <- left_join(scotcouncil@data, combdata, by=c("CODE"="areacode"))
    #test changing the map based on the dropdown
    if(input$map == 'mapmpay') {
      legendtitle <- 'Median Male Pay';
      dynamicValue <- scotcouncil$Male.x;
      dynamicURL1 <- paste0(scotcouncil$Male.y,"?tab=api");
      dv2 <- 'Male.x';
      prefix <- '£';
      suffix <- ''
    } else if(input$map == 'mapfpay'){
      legendtitle <- 'Median Female Pay';
      dynamicValue <- scotcouncil$Female.x;
      dynamicURL1 <- paste0(scotcouncil$Female.y,"?tab=api");
      dv2 <- 'Female.x';
      prefix <- '£';
      suffix <- ''
    } else if(input$map == 'mapapay'){
      legendtitle <- 'Median Pay';
      dynamicValue <- scotcouncil$All.x;
      dynVal <- 'All.x';
      dv2 <- 'All.x';
      dynamicURL1 <- paste0(scotcouncil$All.y,"?tab=api");
      prefix <- '£';
      suffix <- ''
    } else if(input$map == 'mapgap'){
      legendtitle <- 'Pay Gap';
      dynamicValue <- scotcouncil$gap;
      dv2 <- 'gap';
      dynamicURL1 <- '';
      prefix <- '£';
      suffix <- ''
    } else if(input$map == 'mapbf'){
      legendtitle <- 'Breastfeeding Rate';
      dynamicValue <- scotcouncil$Breastfeeding.x;
      dynamicURL1 <- scotcouncil$Breastfeeding.y;
      dv2 <- 'Breastfeeding.x';
      prefix <- '';
      suffix <- '%'
    } else if(input$map == 'mapfire'){
      legendtitle <- 'Deliberate Fires';
      dynamicValue <- scotcouncil$`Deliberate fires.x`;
      dynamicURL1 <- scotcouncil$`Deliberate fires.y`;
      dv2 <- '`Deliberate fires.x`';
      prefix <- '';
      suffix <- ''
    } else if(input$map == 'mapjsa'){
      legendtitle <- 'JSA Claimants';
      dynamicValue <- scotcouncil$`Job Seeker's Allowance Claimants.x`;
      dynamicURL1 <- scotcouncil$`Job Seeker's Allowance Claimants.y`;
      dv2 <- "Job Seeker's Allowance Claimants.x";
      prefix <- '';
      suffix <- '%'
    } else if(input$map == 'mapdwell'){
      legendtitle <- 'Dwellings per Hectare';
      dynamicValue <- scotcouncil$`Dwellings per Hectare.x`;
      dynamicURL1 <- scotcouncil$`Dwellings per Hectare.y`;
      dv2 <- 'Dwellings per Hectare.x';
      prefix <- '';
      suffix <- ' dwellings per hectare'
    } else if(input$map == 'mapalc'){
      legendtitle <- 'Alcohol-related Discharge';
      dynamicValue <- scotcouncil$`Alcohol Related Hospital Discharge.x`;
      dynamicURL1 <- scotcouncil$`Alcohol Related Hospital Discharge.y`;
      prefix <- '';
      dv2 <- 'Alcohol Related Hospital Discharge.x';
      suffix <- ''
    }
    
    #Change the filter and y-axis based on second dropdown
    
    if(input$filter == 'filtermpay') {
      axistitle <- 'Median Male Pay';
      dynamicFilterValue <- scotcouncil$Male.x;
      dynamicURL2 <- paste0(scotcouncil$Male.y,"?tab=api");
      dfv2 <- 'Male.x';
      prefix2 <- '£';
      suffix2 <- ''
    } else if(input$filter == 'filterfpay'){
      axistitle <- 'Median Female Pay';
      dynamicFilterValue <- scotcouncil$Female.x;
      dynamicURL2 <- paste0(scotcouncil$Female.y,"?tab=api");
      dfv2 <- 'Female.x';
      prefix2 <- '£';
      suffix2 <- ''
    } else if(input$filter == 'filterapay'){
      axistitle <- 'Median Pay';
      dynamicFilterValue <- scotcouncil$All.x;
      dynamicURL2 <- paste0(scotcouncil$All.y,"?tab=api");
      dfv2 <- 'All.x';
      prefix2 <- '£';
      suffix2 <- ''
    } else if(input$filter == 'filtergap'){
      axistitle <- 'Pay Gap';
      dynamicFilterValue <- scotcouncil$gap;
      dfv2 <- 'gap';
      dynamicURL2 <- '';
      prefix2 <- '£';
      suffix2 <- ''
    } else if(input$filter == 'filterbf'){
      axistitle <- 'Breastfeeding Rate';
      dynamicFilterValue <- scotcouncil$Breastfeeding.x;
      dynFiltVal <- 'Breastfeeding.x';
      dfv2 <- 'Breastfeeding.x';
      dynamicURL2 <- scotcouncil$Breastfeeding.y;
      prefix2 <- '';
      suffix2 <- '%'
    } else if(input$filter == 'filterfire'){
      axistitle <- 'Deliberate Fires';
      dynamicFilterValue <- scotcouncil$`Deliberate fires.x`;
      dynamicURL2 <- scotcouncil$`Deliberate fires.y`;
      dfv2 <- 'Deliberate fires.x';
      prefix2 <- '';
      suffix2 <- ''
    } else if(input$filter == 'filterjsa'){
      axistitle <- 'JSA Claimants';
      dynamicFilterValue <- scotcouncil$`Job Seeker's Allowance Claimants.x`;
      dfv2 <- "Job Seeker's Allowance Claimants.x";
      dynamicURL2 <- scotcouncil$`Job Seeker's Allowance Claimants.y`;
      prefix2 <- '';
      suffix2 <- '%'
    } else if(input$filter == 'filterdwell'){
      axistitle <- 'Dwellings per Hectare';
      dynamicFilterValue <- scotcouncil$`Dwellings per Hectare.x`;
      dynamicURL2 <- scotcouncil$`Dwellings per Hectare.y`;
      dfv2 <- 'Dwellings per Hectare.x';
      prefix2 <- '';
      suffix2 <- ' dwellings per hectare'
    } else if(input$filter == 'filteralc'){
      axistitle <- 'Alcohol-related Discharge';
      dynamicFilterValue <- scotcouncil$`Alcohol Related Hospital Discharge.x`;
      dynamicURL2 <- scotcouncil$`Alcohol Related Hospital Discharge.y`;
      dfv2 <- 'Alcohol Related Hospital Discharge.x';
      prefix2 <- '';
      suffix2 <- ''
    }
    
    # Draw the table (the columns need to match with all those in selected())
    
    urlddata <- data.frame(paste0(combdata$areacode),
                           paste0(combdata$areaname.x.x),
                           paste0("<a href='",combdata$All.y,"?tab=api'>", combdata$All.x,"</a>"),
                           paste0("<a href='",combdata$Male.y,"?tab=api'>", combdata$Male.x,"</a>"),
                           paste0("<a href='",combdata$Female.y,"?tab=api'>", combdata$Female.x,"</a>"),
                           paste0("<a href='",combdata$`Alcohol Related Hospital Discharge.y`,"'>", combdata$`Alcohol Related Hospital Discharge.x`,"</a>"),
                           paste0("<a href='",combdata$`Job Seeker's Allowance Claimants.y`,"'>", combdata$`Job Seeker's Allowance Claimants.x`,"</a>"),
                           paste0("<a href='",combdata$`Deliberate fires.y`,"'>", combdata$`Deliberate fires.x`,"</a>"),
                           paste0("<a href='",combdata$`Dwellings per Hectare.y`,"'>", combdata$`Dwellings per Hectare.x`,"</a>"),
                           paste0("<a href='",combdata$Breastfeeding.y,"'>", combdata$Breastfeeding.x,"</a>")
    )
    
    colnames(urlddata) <- c("Council Code",
                            "Council Name",
                            "Median Pay (All)",
                            "Median Pay (Male)",
                            "Median Pay (Female)",
                            "Alcohol Related Hospital Discharge",
                            "Jobseekers Allowance Claimants",
                            "Deliberate Fires",
                            "Dwellings per Hectare",
                            "Breastfeeding")
    
    output$table <- DT::renderDataTable(datatable(urlddata, escape = FALSE))
    
    #create temporary dataframe for use in map and scatter
    
    dynamicdf <- scotcouncil@data[, c('NAME','CODE',dynVal,dynFiltVal)]
    print(dynamicdf)
    
    #sets the colour range to be used on the choropleth
    qpal <- colorNumeric("Spectral", dynamicValue, na.color = "#bdbdbd")
    
    #the popup on the map
    
    popup <- paste0("<h5>",scotcouncil$NAME,"</h5><br /><h5>",legendtitle,": <a href='",dynamicURL1,"'>",prefix,dynamicValue,suffix,"</a></h5><br /><h5>",axistitle,": <a href='",dynamicURL2,"'>",prefix2,dynamicFilterValue,suffix2,"</a></h5>")
    
    #draw the map with stuff on
    #***Need to make this dynamic based on what's selected in the map dropdown
    leafletProxy("map", data = scotcouncil) %>%
      addProviderTiles("Esri.WorldStreetMap") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = scotcouncil, fillColor = ~qpal(dynamicValue), fillOpacity = 0.8, 
                  color = "#bdbdbd", weight = 1, popup = popup, layerId = scotcouncil$CODE) %>%
      addLegend(pal = qpal, values = ~dynamicValue, opacity = 0.7,
                position = 'bottomleft',
                title = paste0(legendtitle))
    
    #scatterplot
    output$plot1 <- renderPlot({
      ggplot(dynamicdf, aes(x=dynamicdf[3], y=dynamicdf[4],label=dynamicdf[1])) + geom_point(shape=21, size=6, color='blue', fill='red', alpha=0.3) + stat_smooth(method='lm', col='red') + labs(x = legendtitle, y=axistitle) + theme_bw()
    })
    
    output$click_scatter <- renderPrint({
      #nearPoints(dynamicdf, input$plot_click, addDist = TRUE)
      input$plot_click
    })  
    
    
    # output$plot1 <- renderPlot({
    #   ggplot(scotcouncil@data, aes(x=dynamicValue, y=dynamicFilterValue,label=scotcouncil$areaname.x.x)) + geom_point(shape=21, size=6, color="blue",fill="red", alpha=0.3) + stat_smooth(method = "lm", col = "red") + labs(x=legendtitle, y=axistitle) + theme_bw() 
    # })
    
    observe({
      click<-input$map_shape_click
      if(is.null(click))
        return()
      
      
      
      available <- combdata[ which(combdata$areacode == click$id), ]
      
      #**** Trying to make the area stat box table - needs more work to get it into the right shape****
      #Don't think I need this to be a datatable. Just a text box with variables???
      
      areastatboxraw <- urlddata[ which(urlddata[1] == click$id), ]
      areastatbox <- (areastatboxraw)
      output$areastats <- DT::renderDataTable(datatable(areastatbox, escape = FALSE))
      
      #Build the area stat box
      statboxtext <- paste0("<table id='statbox'><tr><td>Council area: </td><td class='boldtabletext'>", 
                            available[1,2]," (", available[1,1],")</td></tr>
                            <tr><td>Median Pay (All):</td><td class='boldtabletext'>", sprintf('£%.0f',available[1,3]),"</td></tr>
                            <tr><td>Median Pay (Female):</td><td class='boldtabletext'>", sprintf('£%.0f',available[1,4]),"</td></tr>
                            <tr><td>Median Pay (Male):</td><td class='boldtabletext'>", sprintf('£%.0f',available[1,5]),"</td></tr>
                            <tr><td>Pay Gap:</td><td class='boldtabletext'>", sprintf('£%.0f',available[1,6]),"</td></tr>
                            <tr><td>Alcohol Related Hospital Discharge:</td><td class='boldtabletext'>", available[1,12],"</td></tr>
                            <tr><td>Breastfeeding Rate:</td><td class='boldtabletext'>", sprintf('%.1f%%',available[1,13]),"</td></tr>
                            <tr><td>Deliberate Fires:</td><td class='boldtabletext'>", available[1,14],"</td></tr>
                            <tr><td>Dwellings per Hectare:</td><td class='boldtabletext'>", available[1,15],"</td></tr>
                            <tr><td>Jobseekers Allowance Claimants:</td><td class='boldtabletext'>", sprintf('%.1f%%',available[1,16]),"</td></tr>
                            </table>")
      output$areastatbox<-renderText({
        statboxtext
      })
      
      
      scatterpoint <- dynamicdf[ which(dynamicdf$CODE == click$id), ]
      
      #Colour the scatterplot according the area of the map clicked
      output$plot1 <- renderPlot({
        ggplot() + geom_point(data=dynamicdf, aes(x=dynamicdf[3], y=dynamicdf[4],label=dynamicdf[1]), shape=21, size=6, color="blue", fill="red", alpha=0.3) + geom_point(data=scatterpoint, aes(x=scatterpoint[3], y=scatterpoint[4]), shape=21, size=6, color="blue",fill="purple", alpha=0.8) + stat_smooth(data=dynamicdf, aes(x=dynamicdf[3], y=dynamicdf[4]),method = "lm", col = "red") + labs(x=legendtitle, y=axistitle) + theme_bw() 
      })
      
      
    })
    
    
    
})
  
  })

