
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plyr)

navbarPage("New Zealand River Monitoring", id="nav",
           
           tabPanel("MAP",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                          
                          #includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls",style = " height: 100vh; overflow-y: auto; ", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                      width = 450, height = "auto",
                                      
                                      h2("River Flow Monitoring Stations"),
                                      #plotOutput("plot1"),
                                    
                                      
                                      
                                      
                                      #sliderInput("paygaprange", "Choose the range of values you would like to display", min = -5000, max = 20000, value = c(-5000,20000)),
                                      #numericInput("lower", "Filter values between", value= -5000),
                                      #numericInput("upper", "and", value=20000),
                                      plotOutput("plot1", height=300, width=400,
                                                 click = clickOpts(
                                                   id = "plot_click"
                                                 )),
                                      textOutput("click_scatter"),
                                      #DT::dataTableOutput("areastats"),
                                      htmlOutput("areastatbox")
                                      #parking this for now - arranging two chart elements side-by-side
                                      #fluidRow(
                                      #  column(6,plotOutput("plot1"))#,
                                      #  column(6,plotOutput("plot2"))
                                      #)
                                      
                                      
                                      
                                      
                                      #selectInput("color", "Color", vars),
                                      #selectInput("size", "Size", vars, selected = "adultpop"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # ),
                                      
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for: ', tags$em('New Zealand Ministry of Environment'), ' Swirrl (2017).'
                        )
                    )
           ),
           
           tabPanel("DATA",
                    fluidRow(
                      column(3,
                             div(h3("Datatable")),
                             DT::dataTableOutput("table")
                      )
                    )
           ),
           
           tabPanel("ABOUT",
                    fluidRow(
                      column(10,
                             div(h5("About this tool")),
                             p("This tool is a demonstration to show that data from different linked datastores can be brought together and visualised in one place, to allow quick comparison. The data here is drawn from two places - the ", a("Office for National Statistics linked data store", href="http://ons.publishmydata.com"), " and",
                               a("statistics.gov.scot", href="http://statistics.gov.scot")," the home of Scotland's official statistics."),
                             p("The data is currently pulled in as a couple of csv files, both extracted from the respective datastores using their SPARQL endpoints. To see these queries in action follow these links:"),
                             tags$ul(tags$li(a("Office for National Statistics SPARQL query", href="http://ons.publishmydata.com/sparql?query=PREFIX%20qb%3A%20%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23%3E%0APREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%0ASELECT%20%3Fs%20%3Fvalue%20%3Fareaname%20%3Fsexname%20%3Fareacode%20%3Fstatname%20WHERE%20%7B%0A%3Fs%20qb%3AdataSet%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdata%2Fannual-survey-of-hours-and-earnings-2016-earnings%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fmeasure-properties%2Fvalue%3E%20%3Fvalue%20%3B%0A%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fdimension%2Fearnings%3E%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fconcept%2Fearnings%2Fannual-pay-gross%3E%3B%0A%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refArea%3E%20%3Farea%3B%0A%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fdimension%2FearningsStatistics%3E%20%3Fstatcode%3B%0A%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23sex%3E%20%3Fsex%3B%0A%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fdimension%2FworkingPattern%3E%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fconcept%2Fworking-pattern%2Ffull-time%3E%20.%0A%3Farea%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fstatistical-entity%23code%3E%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fid%2Fstatistical-entity%2FS12%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fstatistical-geography%23officialname%3E%20%3Fareaname%20.%0A%3Fsex%20rdfs%3Alabel%20%3Fsexname%20.%0A%3Farea%20rdfs%3Alabel%20%3Fareacode%20.%0A%3Fstatcode%20rdfs%3Alabel%20%3Fstatname%20.%0A%7D")),
                                     tags$li(a("Scottish Government Statistics SPARQL query", href="http://statistics.gov.scot/sparql?query=PREFIX%20qb%3A%20%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23%3E%0APREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%0APREFIX%20sdmx%3A%20%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fconcept%23%3E%0APREFIX%20data%3A%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdata%2F%3E%0APREFIX%20sdmxd%3A%20%3Chttp%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23%3E%0APREFIX%20mp%3A%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F%3E%0APREFIX%20stat%3A%20%3Chttp%3A%2F%2Fstatistics.data.gov.uk%2Fdef%2Fstatistical-entity%23%3E%0APREFIX%20skos%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2004%2F02%2Fskos%2Fcore%23%3E%0ASELECT%20%3Fs%20%3Fareaname%20%3Findicatorlabel%20%3Fyearname%20%3Fvalue%20%3Fareacode%0AWHERE%20%7B%0A%7B%3Fs%20qb%3AdataSet%20data%3Ajob-seekers-allowance%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fquarter%2F2012-Q4%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2F16-64%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fgender%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fgender%2Fall%3E%20.%0Adata%3Ajob-seekers-allowance%20rdfs%3Alabel%20%3Findicatorlabel%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Adwellings-hectare%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2012%3E%20.%0Adata%3Adwellings-hectare%20rdfs%3Alabel%20%3Findicatorlabel%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Asmoking-at-booking%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2013-01-01T00%3A00%3A00%2FP2Y%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FpopulationGroup%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fpopulation-group%2Fcurrent-smoker%3E%20.%0ABIND%20(%22Antenatal%20smoking%22%20as%20%3Findicatorlabel)%20.%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Alow-birthweight%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2013-01-01T00%3A00%3A00%2FP2Y%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FbirthWeight%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fbirth-weight%2Flow-weight-births%3E%20.%0Adata%3Alow-birthweight%20rdfs%3Alabel%20%3Findicatorlabel%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Afire%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2012%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator(fire)%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-fire%2Fdeliberate-fires-excluding-chimney-fires-per-100-000-population%3E%20.%0ABIND%20(%22Deliberate%20fires%22%20as%20%3Findicatorlabel)%20.%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Abreastfeeding%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2013%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FbreastfeedingDataCollectionTime%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fbreastfeeding-data-collection-time%2Ffirst-visit%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FpopulationGroup%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fpopulation-group%2Fbreastfed%3E%20.%0Adata%3Abreastfeeding%20rdfs%3Alabel%20%3Findicatorlabel%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Afuel-poverty-shcs%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2010-01-01T00%3A00%3A00%2FP2Y%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2Fall%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fgender%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fgender%2Fall%3E%20%3B%0A%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FdisabilityStatus%3E%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fdisability-status%2Fall%3E%20.%0ABIND%20(%22Perc%20of%20households%20in%20fuel%20poverty%22%20as%20%3Findicatorlabel)%20.%7D%0AUNION%0A%7B%3Fs%20qb%3AdataSet%20data%3Aalcohol-related-discharge%20%3B%0Amp%3Aratio%20%3Fvalue%20%3B%0Asdmxd%3ArefPeriod%20%3Chttp%3A%2F%2Freference.data.gov.uk%2Fid%2Fgovernment-year%2F2012-2013%3E%20.%0Adata%3Aalcohol-related-discharge%20rdfs%3Alabel%20%3Findicatorlabel%7D%20.%0A%3Fs%20sdmxd%3ArefArea%20%3Farea%20%3B%0Asdmxd%3ArefPeriod%20%3Fyear%20.%0A%3Fyear%20rdfs%3Alabel%20%3Fyearname%20.%0A%3Farea%20stat%3Acode%20%3Chttp%3A%2F%2Fstatistics.gov.scot%2Fid%2Fstatistical-entity%2FS12%3E%20%3B%0Ardfs%3Alabel%20%3Fareaname%20%3B%0Askos%3Anotation%20%3Fareacode%0A%7D"))),
                             p("The data is joined using the area code - standard across both datastores."),
                             p("Where possible, observations have been hyperlinked - eg clicking the map will reveal a popup which displays the two relevant values for an area. These two values are clickable, and will inject the user straight into the observation's landing page on whichever triple store the data came from. This is a key benefit of using linked data in this way."),
                             p("Planned iterations of this tool to include:"),
                             tags$ul(tags$li("Changing the source from csv download to live connection to SPARQL endpoint"),
                                     tags$li("Add an area statbox"),
                                     tags$li("Format the table"),
                                     tags$li("Better links between map and scatterplot")),
                             p("Because this demonstrator is built in R, it is open source, and therefore free to use. Once the data is in the R environment, then all of R's many packages can be used to work with the data, from text mining, to statistical analyses."),
                             p("The code for this tool is ", a("all on GitHub", href="https://github.com/northernjamie/scot_ons_demo"), " - feel free to fork it and make it better!")
                      )
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
)