

library(keyring)
library(novaRush)
library(jsonlite)
library(httr)

# This file is used for testing

config <- list(
  host = 'datadudes2.xyz',
  ledger = 'test2',
  create = TRUE,
  signMessages = TRUE,
  privateKey = key_get("privateKey", keyring = "Fluree"))

setConfiguration(config = config)

Sys.sleep(1)

connect()

Sys.sleep(1)

context_list <- list(
  "xsd" = "http://www.w3.org/2001/XMLSchema#",
  "sur" = "https://w3id.org/survey-ontology#",
  "prov" = "http://www.w3.org/ns/prov#",
  "dc" = "http://purl.org/dc/elements/1.1/",
  "dcterms" = "http://purl.org/dc/terms/",
  "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
  "qb" = "http://purl.org/linked-data/cube#",
  "ro" = "http://purl.org/wf4ever/ro#",
  "ore" = "http://www.openarchives.org/ore/terms/",
  "wfprov" = "http://purl.org/wf4ever/wfprov#",
  "wfdesc" = "http://purl.org/wf4ever/wfdesc#"
)

setContext(context_list)

path <- "/Users/jacohuman/Desktop/Nova_werk/Des_Nov_2025/GraphDB/survey_output.jsonld"
lines <- readLines(path)
json_string <- paste(lines, collapse = "\n")

data_as_list <- fromJSON(json_string, simplifyDataFrame = F, simplifyMatrix = F, simplifyVector = F)
d <- transact(data_as_list)
sendTransaction(d)
