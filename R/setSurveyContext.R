#' Set the survey context.
#'
#' @description
#' Set the context to use when capturing the data for a given survey.
#' This context set can be updated accordingly, but as a start the primary
#' ontologies referenced are given.
#'
#' @param additional_context  Any additional context fields to add to the base
#'                            context; given as a `list`.
#'
#' @return A list representing the context entity.
#'
#' @export
set_survey_context <- function(additional_context = NULL) {
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

  if (!is.null(additional_context)) {
    context_list = c(context_list, additional_context)
  }
  return(context_list)
}
