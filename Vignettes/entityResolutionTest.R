
generate_test_questions <- function(path = "q_test/") {
  dir.create(path, showWarnings = FALSE)

  questions <- list(
    list(id = "q1", label = "gps_location", text = "Please capture the GPS location of the household."),
    list(id = "q2", label = "gps_location", text = "Please record the GPS location of the household."),
    list(id = "q3", label = "stand_number", text = "What is the stand number of the household?"),
    list(id = "q4", label = "age_head", text = "How old is the household head?"),
    list(id = "q5", label = "gps_location", text = "Please capture the GPS location of the household."),  # duplicate of q1
    list(id = "q6", label = "favorite_fruit", text = "What is your favorite fruit?")
  )

  for (q in questions) {
    file <- file.path(path, paste0(q$id, ".jsonld"))
    obj <- list(
      `@context` = "http://schema.org",
      `@graph` = list(list(
        `@id` = paste0("http://example.org/data/q/", q$id),
        `@type` = list("sur:OpenQuestion"),
        `rdfs:label` = q$label,
        `sur:hasText` = q$text
      ))
    )
    jsonlite::write_json(obj, path = file, auto_unbox = TRUE, pretty = TRUE)
  }

  message("Sample JSON-LD data written to ", path)
}

generate_test_questions()

qDF <- read_question_files("q_test")

pairs <- compare_questions_all(qDF)

auto_matches <- review_matches(pairs, "matches_review_test.csv")

finalize_jsonld(
  df_all = list(qDF),
  auto_matches = auto_matches,
  review_file = "matches_review_test.csv",
  output_file = "linked_questions_test.jsonld"
)
