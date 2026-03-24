
# READ JSON FILES INTO DATA FRAME
read_question_files <- function(path = "q/") {
  files <- list.files(path, pattern = "\\.jsonld$", full.names = TRUE)

  purrr::map_dfr(files, function(f) {
    json <- jsonlite::fromJSON(f, flatten = TRUE)

    graph <- json$`@graph`

    if (!is.list(graph[[1]])) {
      graph <- list(graph)
    }
    question <- graph[[1]]

    tibble(
      id = tools::file_path_sans_ext(basename(f)),
      qid = question$`@id`,
      label = question$`rdfs:label`,
      text = question$`sur:hasText`
    )
  })
}

# COMPARE ALL QUESTIONS
compare_questions_all <- function(df) {
  df$block <- 1

  pairs <- pair_blocking(df, df, on = "block") %>%
    filter(.x != .y)

  pairs <- compare_pairs(
    pairs,
    on = c("label", "text"),
    comparators = list(
      label = cmp_jarowinkler(0.9),
      text  = cmp_jarowinkler(0.9)
    ),
    inplace = FALSE
  )

  pairs <- pairs %>%
    mutate(
      score = (label + text) / 2,
      id_x = df$qid[.x],
      id_y = df$qid[.y],
      label_x = df$label[.x],
      label_y = df$label[.y],
      text_x = df$text[.x],
      text_y = df$text[.y],
      match_type = case_when(
        score >= 0.95 ~ "skos:exactMatch",
        score >= 0.85 ~ "REVIEW",
        TRUE ~ NA_character_
      )
    )
  return(pairs)
}


# REVIEW THE MATCHES
review_matches <- function(pairs, output_file,
                           exact_threshold = 0.9,
                           close_threshold = 0.75) {

  reviewed <- pairs %>%
    # Filter out any weak pairs
    filter(score >= close_threshold) %>%
    mutate(
      match_type = case_when(
        score >= exact_threshold ~ "skos:exactMatch",
        TRUE ~ "skos:closeMatch"
      ),
      use = TRUE
    ) %>%
    select(.x, .y, label, text, score, id_x, id_y,
           label_x, label_y, text_x, text_y,
           match_type, use)

  write_csv(reviewed, output_file)
  message("Review file written to: ", output_file)
  return(reviewed)
}

# FINALIZE MATCHES AND WRITE UPDATE JSON FILES
finalize_jsonld <- function(df_all, auto_matches, review_file, output_file) {

  # Combine all question data frames
  df_all_combined <- bind_rows(df_all)

  # Read in any reviewed matches
  review_df <- read.csv(review_file, stringsAsFactors = FALSE)
  confirmed <- review_df %>%
    filter(use == TRUE & !is.na(match_type)) %>%
    select(id_x, id_y, match_type)

  print(confirmed)

  # Combine the reviewed matches with the auto matches
  all_matches <- bind_rows(auto_matches, confirmed) %>%
    distinct(id_x, id_y, match_type)

  match_map <- all_matches %>%
    group_by(id_x) %>%
    summarise(
      skos_exactMatch = list(id_y[match_type == "skos:exactMatch"]),
      skos_closeMatch = list(id_y[match_type == "skos:closeMatch"]),
      .groups = "drop"
    )

  entities <- df_all_combined %>%
    mutate(
      `@id` = qid,
      `@type` = "sur:OpenQuestion",
      `rdfs:label` = label,
      `sur:hasText` = text
    ) %>%
    select(`@id`, `@type`, `rdfs:label`, `sur:hasText`) %>%
    pmap(function(`@id`, `@type`, `rdfs:label`, `sur:hasText`) {
      entity <- list(
        `@id` = `@id`,
        `@type` = `@type`,
        `rdfs:label` = `rdfs:label`,
        `sur:hasText` = `sur:hasText`
      )

      # Add matches if any
      matched <- match_map %>% filter(id_x == `@id`)
      if (nrow(matched) > 0) {
        if (length(matched$skos_exactMatch[[1]]) > 0) {
          entity[["skos:exactMatch"]] <- map(matched$skos_exactMatch[[1]], ~ list(`@id` = .x))
        }
        if (length(matched$skos_closeMatch[[1]]) > 0) {
          entity[["skos:closeMatch"]] <- map(matched$skos_closeMatch[[1]], ~ list(`@id` = .x))
        }
      }

      return(entity)
    })

  # Final JSON-LD structure
  jsonld <- list(
    `@context` = list(
      sur = "https://w3id.org/survey-ontology#",
      rdfs = "http://www.w3.org/2000/01/rdf-schema#",
      skos = "http://www.w3.org/2004/02/skos/core#"
    ),
    `@graph` = entities
  )

  # Write to file
  write_json(jsonld, output_file, auto_unbox = TRUE, pretty = TRUE)
  message("JSON-LD written to ", output_file)
}
