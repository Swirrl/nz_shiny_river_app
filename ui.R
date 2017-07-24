
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(plyr); library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plotly) ; library(shinycssloaders) ; library(shinysky)

navbarPage("New Zealand River Monitoring", id="nav",
           
           
           tabPanel("ABOUT THIS TOOL",
                    fluidRow(
                      column(10,
                             div(h3("About this tool")),
                             p("Welcome to the interactive river sensor explorer. For more information about this project, see the 'About this Project' tab."),
                             h3("How to use this tool"),
                             p("This tool has been designed to be fairly intuitive, showing data for river monitoring sites on a map. Clicking a site will show some key statistics for that site - latest reading, mean average flow, etc."),
                             p("The 'Data' tab contains a filterable, sortable, searchable list of all sites, along with key facts and context about each site."),
                             p("The 'Chart' tab allows you to view historical data for a site. This chart also has the ability to show multiple sites on the same axes, to support comparisons between sites."),
                             p("Any text on this site that ",a("appears blue", href = "http://envdatapoc.co.nz/"),(" is a hyperlink. Clicking these links will take you to that thing's page on the PublishMyData platform. This is a more technical view of the data, but does provide more information abut the thing, and can also provide a platform for more advanced queries in the SPARQL endpoint."))
                             
                      )
                    )
           ),
           tabPanel("MAP",
                    div(class="outer",
                        tags$style(type="text/css", "
                                         #loadmessage {
                                   position: fixed;
                                   top: 0px;
                                   left: 0px;
                                   width: 100%;
                                   padding: 5px 0px 5px 0px;
                                   text-align: center;
                                   font-weight: bold;
                                   font-size: 100%;
                                   color: #000000;
                                   background-color: #c0d6f9;
                                   z-index: 105;
                                   }
                                   "),
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                          
                          #includeScript("gomap.js")
                        ),
                        busyIndicator(wait=1000),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls",style = " height: 120vh; overflow-y: auto; ", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                      width = 450, height = 900,
                                      radioButtons('mapbackground','Map Background', choices = c('Terrain' = 'terr','Satellite' = 'sat'), selected = 'terr'),
                                      h2("River Flow Monitoring Stations"),
                                      htmlOutput("stationname"),
                                      tags$head(tags$style(".popupbody{
                                            font-weight: bold;
                                            font-size: 16px;
                                            }"
                                              )
                                      ),
                                      tags$head(tags$style(".popuptitle{
                                            font-weight: bold;
                                                           font-size: 24px;
                                                           }"
                                              )
                                      ),
                                      tags$head(tags$style("#stationname{color: #222222;
                                                                        font-size: 24px;
                                                                        font-weight: bold;
                                                                        }"
                                                          )
                                                ),
                                      tags$head(tags$style("#latestdatetime,#malf,#minflow,#meanfloodflow,#landcover,#meanflow,#maxflow,#latestreading,#climate,#elevation,#geology{color: #222222;
                                                                        font-size: 26px;
                                                                        font-weight: bold;
                                                                        display:inline;
                                                                       }"
                                                          )
                                                ),
                    
                                    
                                      
                                      
                                      
                                      #sliderInput("paygaprange", "Choose the range of values you would like to display", min = -5000, max = 20000, value = c(-5000,20000)),
                                      #numericInput("lower", "Filter values between", value= -5000),
                                      #numericInput("upper", "and", value=20000),
                                      plotOutput("plot1", height=200, width=400,
                                                 click = clickOpts(
                                                   id = "plot_click"
                                                 )),
                                      div(downloadButton('downloadData', 'Download the data')),
                                      p(h3("Latest reading: ",style="display:inline"), htmlOutput('latestreading')),
                                      p(h3("Date/time of reading: ",style="display:inline"), htmlOutput('latestdatetime')),
                                      p(h3("Mean annual flow: ",style="display:inline"), htmlOutput('meanflow')),
                                      p(h3("Min flow: ",style="display:inline"), htmlOutput('minflow')),
                                      p(h3("Max flow: ",style="display:inline"), htmlOutput('maxflow')),
                                      p(h3("MALF: ",style="display:inline"), htmlOutput('malf')),
                                      p(h3("Mean annual flood flow: ",style="display:inline"), htmlOutput('meanfloodflow')),
                                      p(h3("Landcover: ",style="display:inline"), htmlOutput('landcover')),
                                      p(h3("Climate: ",style="display:inline"), htmlOutput('climate')),
                                      p(h3("Elevation: ",style="display:inline"), htmlOutput('elevation')),
                                      p(h3("Geology: ",style="display:inline"), htmlOutput('geology')),
                                      htmlOutput(("photo"), align = 'center'),
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
                      column(12,
                             div(h3("Datatable")),
                             DT::dataTableOutput("table")
                             
                      )
                    )
           ),
           tabPanel("CHART",
                    fluidRow(
                      column(4,
                             div(selectInput("sitesel","Select sites:",monsites$name,multiple=TRUE))),
                      column(4,
                             div(dateRangeInput("datepicker", "Choose date range:", format = "dd M yyyy", startview = "month",start = "2017-07-01", weekstart = 0,language = "en", separator = " to "))),
                      column(4,
                             div(selectInput("periodsel", "Choose time interval",c("Minutes"),selected = "Minutes"))),
                      column(12,
                             div(actionButton("refreshchart","Refresh"))),
                      column(12,
                             div(h3("")),
                             plotlyOutput("plot2_big_line", height=600)
                      )
                    )
           ),
           tabPanel("DASHBOARD",
                    fluidRow(
                      column(12,
                             div(h3("Dashboard..!")),
                             div(h4("Possibly coming soon"))
                      )
                    )
           ),
           tabPanel("ABOUT THIS PROJECT",
                    fluidRow(
                      column(10,
                             div(h3("About this project")),
                             h4(p("Plain English Summary")),
                             p("The Ministry, NRS and Regional Sector are developing a data tool to enable people to query real time, linked river level and flow data provided by participating regional councils. The tool will enable people to search and compare data on river levels and flows in the same way the Trivago website enables people to search and compare the price of hotel rooms. 
                               The main reason for developing the tool is so people can get river data within a few minutes. At the moment people have to email their data requests to data providers and wait around a month for an answer. The data tool is possible because regional councils and the Ministry for the Environment have standardised the way they measure, collect and exchange regional environmental data in New Zealand. 
                               The Ministry is currently testing the idea to see if it works and aims to show the concept from this website from July. If the regional government sector and the natural resource sector think the concept is useful, the next step will be to develop a larger project to provide access to more data through the tool."),
                               h4(p("Technical Summary")),
                               p("The Environmental Integrated Data Infrastructure (architectural proof of concept scoping) project received funding from the Better Public Services Seed Fund, administered by the Treasury. The purpose of this project is to demonstrate the exchange of data between MfE and the other Natural Resource Sector agencies (DIA, DOC, LINZ, MBIE, MPI, STATS) and data supplier organisations such as regional councils. 
                               The proof of concept will use established technologies, including web technologies, location intelligence, and linked data, to connect different data holdings into a single, seamless, virtual data service.  The data layer will be able to be accessed from current systems, and the proof of concept is complying to data and web standards as much as possible and will also provide an online tool to enable browsing, querying, and accessing the data. The data will continue to be stored and managed by the contributing agencies.
                               In the first instance, the POC will be demonstrating the brokering of real-time water-related data services (flow) from regional councils, using common vocabularies, and building on the NEMS (National Environmental Monitoring Standards) data exchange protocol (Environmental Observation Data Protocol) developed between NIWA, Landcare, GNS, Horizons and Genesis Energy."),
                               h4(p("Disclaimer")),
                               p("Read our disclaimer statement about information on this website. The information on this website is, according to the Ministry for the Environment’s best efforts, accurate at the time of publication and the Ministry makes every reasonable effort to keep it current and accurate. However, visitors to this website are advised that:"),
                               tags$ul(tags$li("this website is established for the Environmental Integrated Data Infrastructure  Proof of Concept (e-IDI POC) project and is for use during the e-IDI POC project only"),
                                       tags$li("the information and data provided on this website are for the purposes of the e-IDI POC project only and are not recommended for operational use"),
                                       tags$li("we recommend that users exercise their own skill and care with respect to their use of this website, and carefully evaluate the accuracy, currency, completeness and relevance of the data on this website for their purposes."),
                                       tags$li("the information provided does not alter the laws of New Zealand and other official guidelines or requirements"),
                                       tags$li("visitors to the Ministry for the Environment website should take specific advice from qualified professional people before undertaking any action following information received from this website"),
                                       tags$li("the Ministry for the Environment does not accept any responsibility or liability whatsoever whether in contract, tort, equity or otherwise for any action taken as a result of reading, or reliance placed on the Ministry for the Environment because of having read, any part, or all, of the information in this website or for any error, or inadequacy, deficiency, flaw in or omission from the information provided in this website"),
                                       tags$li("all links and references to other websites, organisations or people not within the Ministry for the Environment are provided for convenience only and should not be taken as endorsement of those websites or information contained in those websites nor of organisations or people referred to"),
                                       tags$li("The Ministry for the Environment also does not impliedly endorse any website, organisation or people who have off-site links to the Ministry for the Environment’s website")),
                              p("If you find any information on this website that you believe may be inaccurate, please send an email to ", a("webmaster@mfe.govt.nz", href="mailto:webmaster@mfe.govt.nz"),".")

                             
                             
                      )
                    )
           ),
           conditionalPanel("false", icon("crosshair"))
)

