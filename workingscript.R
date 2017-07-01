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

