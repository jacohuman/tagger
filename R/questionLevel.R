#' Extract Closed Answer entities from the Codebook.
#'
#' @description
#' This function extracts predefined answer options for closed questions
#' and formats them as sur:ClosedAnswer entities.
#'
#' @param codebook The survey codebook as a dataframe.
#' @param base_uri The base URI string to prepend to generated IDs.
#' @param procedure_uri The full IRI of the sur:SurveyProcedure these answers belong to.
#'
#' @return A list of R lists, where each inner list represents a sur:ClosedAnswer entity.
#'
#' @import dplyr
#' @import stringr
#' @importFrom purrr map
#' @export
extract_closed_answer_entities <- function(codebook, base_uri, procedure_uri) {
  # Ensure base_uri ends with a slash
  if (!stringr::str_ends(base_uri, "/")) {
    base_uri <- paste0(base_uri, "/")
  }

  options_data <- codebook %>%
    dplyr::filter(!is.na(option) & !is.na(response_value))

  if (nrow(options_data) == 0) {
    # If no options are found, an empty list is returned
    return(list())
  }

  closed_answer_list <- options_data %>%
    # Ensure response_value is a character (for creating the id's)
    dplyr::mutate(response_value_char = as.character(response_value)) %>%
    dplyr::mutate(
      # Create unique ID for the answer option: ans/{question_id}_{response_value}
      `@id` = paste0(base_uri, "ans/", question_id, "_", response_value_char),
      `@type` = "sur:ClosedAnswer",
      # Link to the procedure
      `sur:inSurveyProcedure` = list(list("@id" = procedure_uri)),
      `sur:hasText` = as.character(option),
      # Store the coded value and type
      # TODO:  handle different data types. (eg. xsd:integer & xsd:float)
      `sur:hasValue` = purrr::map(response_value_char, ~list("@value" = .x, "@type" = "xsd:string"))
    ) %>%
    dplyr::select(`@id`, `@type`, `sur:inSurveyProcedure`, `sur:hasText`, `sur:hasValue`) %>%

    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    lapply(function(x) {
      # Ensure list columns are correctly formatted
      x$`sur:inSurveyProcedure` <- x$`sur:inSurveyProcedure`[[1]]
      as.list(x)
    })

  return(closed_answer_list)
}


#' Extract Question and Talk entities from the questions dataframe.
#'
#' @description
#' This function formats survey questions according to the Survey Ontology,
#' mapping question types to appropriate classes. These questions are then
#' grouped by section for tagging.
#'
#' @param questions The survey questions dataframe.
#' @param base_uri The base URI string to prepend to any generated IDs.
#' @param procedure_uri The full IRI of the sur:SurveyProcedure these questions belong to.
#'
#' @return A list of R lists, where each inner list represents a sur:Question or sur:Talk entity.
#'
#' @import dplyr
#' @import stringr
#' @importFrom purrr map
#'
#' @export
extract_question_entities <- function(questions, base_uri, procedure_uri, question_classifier, tag_set = new.env()) {
  if (!stringr::str_ends(base_uri, "/")) {
    base_uri <- paste0(base_uri, "/")
  }

  #TODO: Ensure correct mapping of elements
  question_type_mapping <- list(
    # Open Questions
    "Xform Text" = c("sur:SingleInputQuestion"),
    "Xform Integer" = c("sur:SingleInputQuestion"),
    "Xform Decimal" = c("sur:SingleInputQuestion"),
    "Xform GPS" = c("sur:OpenQuestion"),
    "Xform Image" = c("sur:OpenQuestion"),
    # Closed Questions
    "Xform Select One" = c("sur:MultipleChoiceQuestion"),
    "Xform Select Many" = c("sur:CheckboxQuestion"),
    # Informational Elements
    "Xform Information" = c("sur:Talk")
  )
  default_type <- c("sur:Question")

  map_question_type <- function(q_type) {
    result <- question_type_mapping[[q_type]]
    if (is.null(result)) {
      return(default_type)
    } else {
      return(result)
    }
  }

  question_list <- questions %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `@id` = paste0(base_uri, "q/", id),
      `@type` = purrr::map(question_type, map_question_type),
      `sur:inSurveyProcedure` = list(list("@id" = procedure_uri)),
      `rdfs:label` = question_name,
      `sur:hasText` = question_text,
      `ex:section` = section_name,
      `dc:subject` = list({
        tags <- question_classifier(question_text, section_name)

        #TODO: Fix tag set update logic
        for (tag in tags) {
          tag_clean <- tolower(trimws(tag))
          if (!(tag_clean %in% ls(tag_set))) {
            tag_set[[tag_clean]] <- TRUE
          }
        }
        # Convert each tag to some URI
        lapply(tags, function(tag) {
          tag_id <- paste0(base_uri, "tags/", stringr::str_replace_all(tolower(trimws(tag)), " ", "_"))
          list("@id" = tag_id)
        })
      })
      # TODO: Add sur:leadsTo logic here
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(`@id`, `@type`, `sur:inSurveyProcedure`, `rdfs:label`, `sur:hasText`, `dc:subject`, `ex:section`) %>%
    purrr::pmap(function(...) as.list(list(...)))

  return(question_list)
}

#' Classify Questions with Tags
#'
#' @param question_text The text of the question to classify.
#' @param section_name Optional; the name of the section the question belongs to.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr content_type_json
#' @importFrom httr content
#' @importFrom httr http_error
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_split
#'
#' @export
question_classifier <- function(question_text, section_name = NULL) {
  api_key <- Sys.getenv("OPENAI_API_KEY")

  system_msg <- "You are a survey tagging assistant. Given a question (and optionally the section it's from), respond with 2–3 concise semantic tags (1–2 words each) that describe what the question concerns. Return only the tags, as a comma-separated list. Do not explain."

  user_msg <- paste0(
    if (!is.null(section_name)) {
      paste0("Section: ", section_name, "\n")
    } else {
      ""
    },
    "Question: ", question_text, "\n\n",
    "Respond with tags only, like:\nlocation, household, gps"
  )
  response <- tryCatch({
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      httr::content_type_json(),
      body = jsonlite::toJSON(list(
        model = "gpt-4",
        messages = list(
          list(role = "system", content = system_msg),
          list(role = "user", content = user_msg)
        ),
        temperature = 0.1
      ), auto_unbox = TRUE)
    )
  }, error = function(e) {
    warning("LLM API call failed: ", e$message)
    return(NULL)
  })

  if (is.null(response) || httr::http_error(response)) {
    warning("HTTP error or null response")
    return(c("uncategorized"))
  }

  result <- tryCatch({
    content_text <- httr::content(response, as = "parsed")
    answer <- content_text$choices[[1]]$message$content
    cat("Question:\n", question_text, "\nSection:\n", section_name, "\nTags:\n", answer, "\n\n")
    tags <- stringr::str_split(answer, ",\\s*")[[1]]
    tags <- tolower(trimws(tags))
    tags[tags != ""]  # remove blanks
  }, error = function(e) {
    warning("Failed to parse LLM response: ", e$message)
    return(c("uncategorized"))
  })

  return(result)
}
