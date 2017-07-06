#SPARQL to get monitoring data:
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX void: <http://rdfs.org/ns/void#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT *
WHERE {
  ?obs rdf:type <http://www.w3.org/ns/sosa/Observation> .
  ?obs <http://www.w3.org/ns/sosa/hasFeatureOfInterest> ?site .
  ?obs <http://www.w3.org/ns/sosa/hasResult> ?resultset .
  ?resultset <http://qudt.org/1.1/schema/qudt#numericValue> ?value .
  ?site rdfs:label ?name .
}

LIMIT 100