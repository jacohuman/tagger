#' Extract top-level entities.
#'
#' @description
#' This function creates the sur:Survey (Research Object), sur:SurveyProcedure,
#' and sur:SurveyDataSet entities for a given survey.
#'
#' @param questions The survey questions data frame.
#' @param main The main survey data frame.
#' @param base_uri The base URI as a string.
#' @param study_id A unique identifier string for the overall study.
#' @param dataset_id A unique identifier string for the data set.
#'
#' @return A list containing three elements: 'survey_ro', 'procedure', 'dataset'.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_ends
#'
#'
#' @export
extract_survey_level_entities <- function(main, questions, base_uri, study_id, dataset_id) {
  # Ensure base_uri ends with a slash
  if (!stringr::str_ends(base_uri, "/")) {
    base_uri <- paste0(base_uri, "/")
  }

  if (nrow(questions) == 0) stop("The provided questions dataframe is empty.")
  if (nrow(main) == 0) stop("The provided main dataframe is empty.")

  survey_proc_id_num <- unique(questions$survey_id)[1]
  survey_title <- unique(main$survey_external_id)[1]

  # Determine first question (minimum id)
  first_question_row <- questions %>% dplyr::filter(id == min(id, na.rm = TRUE))
  first_question_id_num <- first_question_row$id[1]
  first_question_uri <- paste0(base_uri, "q/", first_question_id_num)

  # Create URI's
  procedure_uri <- paste0(base_uri, "proc/", survey_proc_id_num)
  dataset_uri <- paste0(base_uri, "ds/", dataset_id)
  survey_ro_uri <- paste0(base_uri, "survey/", study_id)

  # Create Procedure Entity
  procedure_entity <- list(
    `@id` = procedure_uri,
    `@type` = "sur:SurveyProcedure",
    `sur:hasTitle` = survey_title,
    `sur:startsWith` = list("@id" = first_question_uri)
  )

  # Create DataSet Entity
  dataset_entity <- list(
    `@id` = dataset_uri,
    `@type` = "sur:SurveyDataSet",
    `dc:title` = paste(survey_title, "Dataset"),

    # TODO: add other fields if necessary
    `dcterms:issued` = list("@value" = format(Sys.Date(), "%Y-%m-%d"), "@type" = "xsd:date")
  )

  # Create Survey Research Object Entity
  survey_ro_entity <- list(
    `@id` = survey_ro_uri,
    `@type` = "sur:Survey",
    `dc:title` = paste(survey_title, "Study"),
    `rdfs:comment` = paste("Research Object containing resources for the", survey_title),
    `ore:aggregates` = list(
      list("@id" = procedure_uri),
      list("@id" = dataset_uri)
    )
  )

  return(list(
    survey_ro = survey_ro_entity,
    procedure = procedure_entity,
    dataset = dataset_entity
  ))
}
