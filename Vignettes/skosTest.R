

# Test the transitivity of the skos:exactMatch property

conf <- setConfig(host = "datadudes2.xyz", ledger = "integrationDemo", signMessages = FALSE)

c <- list(
  "f" = "https://ns.flur.ee/ledger#",
  "ex" = "http://example.org/",
  "schema" = "http://schema.org/",
  "sur" = "https://w3id.org/survey-ontology#",
  "rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
  "skos" = "http://www.w3.org/2004/02/skos/core#"
)

conf <- setContext(currentConfig = conf, context = c)
createLedger(config = conf, ledgerName = "transitiveTest")
createLedger(config = conf, ledgerName = "equivalenceClass")

questionData <-
'{
  "insert": [
    {
      "@id": "http://example.org/data/q/q1",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please capture the GPS location of the household.",
      "skos:exactMatch": [
        {
          "@id": "http://example.org/data/q/q2"
        },
        {
          "@id": "http://example.org/data/q/q5"
        }
      ]
    },
    {
      "@id": "http://example.org/data/q/q2",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please record the GPS location of the household.",
      "skos:exactMatch": [
        {
          "@id": "http://example.org/data/q/q1"
        },
        {
          "@id": "http://example.org/data/q/q5"
        }
      ]
    },
    {
      "@id": "http://example.org/data/q/q3",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "stand_number",
      "sur:hasText": "What is the stand number of the household?"
    },
    {
      "@id": "http://example.org/data/q/q4",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "age_head",
      "sur:hasText": "How old is the household head?"
    },
    {
      "@id": "http://example.org/data/q/q5",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please capture the GPS location of the household."
    },
    {
      "@id": "http://example.org/data/q/q6",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "favorite_fruit",
      "sur:hasText": "What is your favorite fruit?"
    }
  ]
}'


Transact(config = conf, ledger = "transitiveTest", transaction = questionData)

qry <- '{
  "select": {
    "?s": [
      "@id",
      {
        "skos:exactMatch": ["*"]
      }
    ]
  },
  "where": {
    "@id": "?s",
    "skos:exactMatch": "?match"
  }
}'

qry <- '{
  "select": {
    "?q": [
      "@id",
      {
        "skos:exactMatch": ["@id"]
      }
    ]
  },
  "where": {
    "@id": "?q",
    "skos:exactMatch": "?_"
  }
}'

qry <- '{
  "select": ["?from", "?to"],
  "where": [
    {
      "@id": "?from",
      "skos:exactMatch": {
        "*": {
          "@id": "?to"
        }
      }
    },
    {
      "@filter": {
        "!=": ["?from", "?to"]
      }
    }
  ]
}'

Query(config = conf, ledger = "transitiveTest", query = qry)




# Test alternative approach with shared "commonQuestion" property

commonQuestion <-
'{
  "insert": [
    {
      "@id": "http://example.org/data/eq/eq1",
      "@type": "skos:Concept"
    }
  ]
}'

Transact(config = conf, ledger = "equivalenceClass", transaction = commonQuestion)

questionData <-
'{
  "insert": [
    {
      "@id": "http://example.org/data/q/q1",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please capture the GPS location of the household.",
      "skos:exactMatch": { "@id": "http://example.org/data/eq/eq1" }
    },
    {
      "@id": "http://example.org/data/q/q2",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please record the GPS location of the household.",
      "skos:exactMatch": { "@id": "http://example.org/data/eq/eq1" }
    },
    {
      "@id": "http://example.org/data/q/q3",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "stand_number",
      "sur:hasText": "What is the stand number of the household?"
    },
    {
      "@id": "http://example.org/data/q/q4",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "age_head",
      "sur:hasText": "How old is the household head?"
    },
    {
      "@id": "http://example.org/data/q/q5",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "gps_location",
      "sur:hasText": "Please capture the GPS location of the household.",
      "skos:exactMatch": { "@id": "http://example.org/data/eq/eq1" }
    },
    {
      "@id": "http://example.org/data/q/q6",
      "@type": "sur:OpenQuestion",
      "rdfs:label": "favorite_fruit",
      "sur:hasText": "What is your favorite fruit?"
    }
  ]
}'

Transact(config = conf, ledger = "equivalenceClass", transaction = questionData)


qry <- '{
  "select": ["?cluster"],
  "where": {
    "@id": "http://example.org/data/q/q2",
    "skos:exactMatch": { "@id": "?cluster" }
  }
}'

qry <- '{
  "select": {
    "?q": [
      "@id",
      "rdfs:label",
      "sur:hasText"
    ]
  },
  "where": [
    {
      "@id": "http://example.org/data/q/q2",
      "skos:exactMatch": { "@id": "?cluster" }
    },
    {
      "skos:exactMatch": { "@id": "?cluster" },
      "@id": "?q"
    }
  ]
}'

Query(config = conf, ledger = "equivalenceClass", query = qry)
