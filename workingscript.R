#libraries

library(SPARQL) ; library(leaflet) ; library(rgdal); library(ggplot2)

#load the sparql data


endpoint <- "http://envdatapoc.co.nz/sparql"

#Get the locations of the monitoring sites
query1 <- "SELECT *
WHERE {
?site <http://envdatapoc.co.nz/def/councilSiteID> ?siteID ;
<http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat ;
<http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long .
}"

qd <- SPARQL(endpoint,query)

monsites <- qd$results

query2 <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT *
WHERE {
  ?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
  ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> <http://envdatapoc.co.nz/id/measurement-site/HRC-00003> .
  ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
  ?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
  ?obs <http://www.w3.org/ns/sosa/resultTime> ?datetime .
  ?obs <http://www.w3.org/ns/sosa/observedProperty> ?type .
  ?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
  
  ?site rdfs:label ?name ;
  	    <http://envdatapoc.co.nz/def/catchment> ?catchment ;
        <http://envdatapoc.co.nz/def/managementZone> ?mgmnt ;
        <http://envdatapoc.co.nz/def/reach> ?reach ;
        <http://envdatapoc.co.nz/def/elevation> ?elevation .
        
}"

qd2 <- SPARQL(endpoint,query2)
monsitesmeasure <- qd2$results
monsitesmeasure$datetimeformatted <- as.POSIXct(monsitesmeasure$datetime, origin = "1970-01-01")
monsitesflow <- monsitesmeasure[ which(monsitesmeasure$type == '<https://registry.scinfo.org.nz/lab/nems/def/property/FLow+%5BWater+Level%5D>'),]


chartdata <- data.frame("site" = monsitesflow$name,
                        "date" = monsitesflow$datetimeformatted,
                        "value" = monsitesflow$value)

#Draw the chart


#Chart with zero-based x axis
measurementchart <- ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line() + scale_y_continuous(limits=c(0,6000))

#Chart with non-zero-based x axis
measurementchart <- ggplot(data=chartdata, aes(x=date, y=value)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) + geom_line()


measurementchart


