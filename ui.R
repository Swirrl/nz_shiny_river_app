
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
                        absolutePanel(id = "controls",style = " height: 120vh; overflow-y: auto; ", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                      width = 450, height = 900,
                                      radioButtons('mapbackground','Map Background', choices = c('Terrain' = 'terr','Satellite' = 'sat'), selected = 'terr'),
                                      h2("River Flow Monitoring Stations"),
                                      textOutput("stationname"),
                                      tags$head(tags$style("#stationname{color: #222222;
                                                                        font-size: 24px;
                                                                        font-weight: bold;
                                                                        }"
                                                          )
                                                ),
                                      tags$head(tags$style("#maxflow,#latestreading,#climate,#elevation,#geology{color: #222222;
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
                                      p(h3("Max flow: ",style="display:inline"), textOutput('maxflow')),
                                      p(h3("Latest reading: ",style="display:inline"), textOutput('latestreading')),
                                      p(h3("Climate: ",style="display:inline"), textOutput('climate')),
                                      p(h3("Elevation: ",style="display:inline"), textOutput('elevation')),
                                      p(h3("Geology: ",style="display:inline"), textOutput('geology')),
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
                      column(10,
                             div(h3("Datatable")),
                             DT::dataTableOutput("table")
                      )
                    )
           ),
           
           tabPanel("ABOUT",
                    fluidRow(
                      column(10,
                             div(h5("About this tool")),
                             p("This tool is a demonstration to show that data from different river monitoring sites can be shown in one R-Shiny visualisation.")
                               
                      )
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
)