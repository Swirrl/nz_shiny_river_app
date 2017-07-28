library(shiny) ; library(plyr) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2) ; library(plotly) ; library(classInt) ; library(grDevices)

endpoint <- "http://guest:eidipoc@envdatapoc.co.nz/sparql"
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

monsites$percdiffmean2 <- (monsites$value/monsites$meanannflowval)*100

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

dtmonsites <- data.frame("Name" = paste0('<a href="http://envdatapoc.co.nz/doc/measurement-site/',monsites$siteID,'?tab=api" target="_blank">',monsites$name,'</a>'),
                         "Latest" = paste0('<a href="',monsites$resultsetnoangle,'?tab=api" target="_blank">',monsites$value,'</a>'),
                         "Date/Time" = format(as.POSIXct(monsites$latest, origin = "1970-01-01"),"%Y-%m-%d %X"),
                         "Annual Mean Flow" = round(monsites$meanannflowval, digits = 2),
                         "Perc diff from mean" = round(monsites$percdiffmean, digits = 2),
                         "Elevation" = monsites$elevation,
                         "Catchment" = monsites$catchmentname,
                         "Climate" = paste0('<a href="',monsites$climatenoangle,'?tab=api" target="_blank">',monsites$climatelabel,'</a>'),
                         "Geology" = paste0('<a href="',monsites$geologynoangle,'?tab=api" target="_blank">',monsites$geologylabel,'</a>'),
                         "Landcover" = paste0('<a href="',monsites$landcovernoangle,'?tab=api" target="_blank">',monsites$landcoverlabel,'</a>'),
                         "Lat" = round(as.numeric(monsites$lat),digits = 7),
                         "Long" = round(as.numeric(monsites$long),digits = 7))

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