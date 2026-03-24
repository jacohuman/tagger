library(keyring)
library(novaRush)
library(GraphDB)
library(jsonlite)
library(httr)

# Configure the Fluree instance

conf <- setConfig(host = "datadudes2.xyz", port = NULL, ledger = "test/mcp", signMessages = FALSE)

# Set the context
c <- list(
  "rdfs"     = "http://www.w3.org/2000/01/rdf-schema#",
  "rdf"      = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "xsd"      = "http://www.w3.org/2001/XMLSchema#",
  "sur"      = "https://w3id.org/survey-ontology#",
  "dc"       = "http://purl.org/dc/terms/",
  "skos"     = "http://www.w3.org/2004/02/skos/core#",
  "ex"       = "http://example.org/data/"
)

conf <- setContext(conf, c)

# privateKey = key_get("privateKey", keyring = "Fluree")


# Read question entities form file
questions <- fromJSON("mcp_test2questions.jsonld", simplifyVector = FALSE)

# Transact to Fluree
transaction <- list("insert" = questions)
txn_result <- Transact(config = conf, transaction = transaction)
print(txn_result)


# Read existing tags from file
tags <- readRDS("Test2_tags.rds")

# Construct the tag entities
tag_entities <- lapply(tags, function(tag) {
  id <- paste0("http://example.org/data/tags/", gsub("\\s+", "_", tolower(tag)))
  list(
    "@id" = id,
    "@type" = "skos:Concept",
    "rdfs:label" = tag
  )
})

# Transact to Fluree
tag_transaction <- list("insert" = tag_entities)
txn <- novaRush::Transact(config = conf, transaction = tag_transaction)
print(txn)
