
#library(jsonlite)
#library(dplyr)
#library(purrr)
#library(tidyr)
#library(DT)

load_ontology_questions <- function(file_paths, survey_ids = NULL) {
  if (is.null(survey_ids)) {
    survey_ids <- basename(file_paths) %>% tools::file_path_sans_ext()
  }

  all_questions <- purrr::map2_dfr(file_paths, survey_ids, function(path, survey_id) {
    json <- fromJSON(path, flatten = TRUE)
    if (!"@graph" %in% names(json)) return(NULL)
    df <- json$`@graph`
    df <- df[grepl("^http://example.org/data/q/", df$`@id`), ]
    df$survey <- survey_id
    return(df)
  })

  return(all_questions)
}

group_questions_by_tag <- function(questions_df) {
  questions_df %>%
    select(`@id`, `sur:hasText`, `dc:subject`, `ex:section`, survey) %>%
    unnest_longer(`dc:subject`) %>%
    rename(
      tag = `dc:subject`,
      text = `sur:hasText`,
      section = `ex:section`,
      qid = `@id`
    )
}

view_questions_table <- function(grouped_df) {
  datatable(
    grouped_df,
    options = list(pageLength = 15),
    rownames = FALSE,
    colnames = c("Question ID", "Question Text", "Tag", "Section", "Survey")
  )
}

files <- c("Test1_ontology_output.jsonld", "Test2_ontology_output.jsonld", "Test3_ontology_output.jsonld")
survey_ids <- c("Sur:1", "Sur:2", "Sur:3")

questions_df <- load_ontology_questions(files, survey_ids)
questions_by_tag <- group_questions_by_tag(questions_df)

view_questions_table(questions_by_tag)
