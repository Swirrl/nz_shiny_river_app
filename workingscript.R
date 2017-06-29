#libraries

library(SPARQL) ; library(leaflet) ; library(rgdal) 

#load the sparql data

endpoint <- "http://envdatapoc.co.nz/sparql"
query <- "SELECT *
WHERE {
?site <http://envdatapoc.co.nz/def/councilSiteID> ?siteID ;
<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat ;
<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
}"

qd <- SPARQL(endpoint,query)

monsites <- qd$results

ui <- shinyUI(fluidPage(
  fluidRow(
    column(7, offset = 1,
           br(),
           div(h3("Smoking in Pregnancy")),
           div(h4("The percentage of women who were current smokers at the time of first booking with maternity services, by Scottish Ward")),
           br())),
  fluidRow(
    column(7, offset = 1,
           tabsetPanel(
             tabPanel("Map",leafletOutput("map", height="600"),
                      br(),
                      actionButton("reset_button", "Reset view")),
             tabPanel("Table",DT::dataTableOutput("table"))
           )
    ),
    column(3,
           sliderInput("smokerange", "Choose the range of values you would like to display", min = 0, max = 35, value = c(0,35)),
           div("This is an example of an interactive map, created using RStudio (with various helpful libraries), which visualises data from the Scottish Datastore (a PublishMyData linked data platform). In this example, the data shows the proportion of expectant mothers who are recorded as smoking at the time of booking maternity services."),
           br(),
           div("The slider below allows further interaction with the map by suppressing (in this case) Wards who fall outside the selected range. This could be useful for (for example) selecting areas to target certain grants - such as those to reduce smoking in pregnancy."),
           br(),
           div("Click the 'table' tab at the top to see a table of the filtered data."),
           br()))
  
))

