
#' Extract Fieldworker Entities
#'
#' @description
#' This function extracts fieldworkers entities.
#'
#' @param main The main data frame of a survey where all the submissions are stored.
#' @param base_uri The base URI to be prepended to the generated IDs.
#'
#' @return A list of R lists, where each inner list represents a sur:Participant entity.
#'
#' @importFrom dplyr
#' @import tidyr
#' @import stringr
#'
#' @export
extract_participant_entities <- function(main, base_uri) {
  # Ensure base_uri ends with a slash
  if (!stringr::str_ends(base_uri, "/")) {
    base_uri <- paste0(base_uri, "/")
  }

  participant_list <- main %>%
    # Get unique fieldworkers
    dplyr::distinct(fieldworker_id, uploaded_by) %>%
    # If a name is missing,  add "Unknown" to the missing fields
    dplyr::mutate(uploaded_by = ifelse(is.na(uploaded_by), "Unknown Unknown", uploaded_by)) %>%
    # Create the structure for each participant
    dplyr::mutate(
      `@id` = paste0(base_uri, "part/", fieldworker_id),
      `@type` = "sur:Participant",
      `sur:participantId` = fieldworker_id,
      `rdfs:label` = uploaded_by,
    ) %>%
    dplyr::select(`@id`, `@type`, `sur:participantId`, `rdfs:label`) %>%

    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    lapply(function(x) as.list(x))

  return(participant_list)
}

#' Extract Submission-related entities.
#'
#' @description
#' This function processes each row of the main dataframe to create
#' sur:SurveyCompletionTask and associated sur:CompletedQuestion entities.
#'
#' @param main The main survey dataframe.
#' @param questions_df The questions dataframe.
#' @param codebook_df The codebook dataframe.
#' @param base_uri The base URI string.
#' @param procedure_uri The full IRI of the sur:SurveyProcedure used.
#' @param dataset_uri The full IRI of the sur:SurveyDataSet associated with the observations.
#'
#' @return A list of R lists, containing all generated sur:SurveyCompletionTask
#'   and sur:CompletedQuestion entities for all submissions.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import jsonlite
#'
#' @export
extract_submission_entities <- function(main, questions_df, codebook_df, base_uri, procedure_uri, dataset_uri) {
  # Ensure base_uri ends with a slash
  if (!stringr::str_ends(base_uri, "/")) {
    base_uri <- paste0(base_uri, "/")
  }

  # Question info lookup (ID, type, name)
  question_lookup <- questions_df %>%
    dplyr::select(question_id = id, question_name, question_type) %>%
    dplyr::distinct() # Ensure unique entries if questions file has duplicates

  # Closed answer option lookup (question_id, response_value -> answer_option_uri)
  closed_answer_lookup <- codebook_df %>%
    dplyr::filter(!is.na(option) & !is.na(response_value)) %>%
    dplyr::mutate(
      answer_option_uri = paste0(base_uri, "ans/", question_id, "_", response_value),
      response_value_char = as.character(response_value) # Ensure value is character for joining
    ) %>%
    dplyr::select(question_id, response_value_char, answer_option_uri) %>%
    dplyr::distinct()

  # Identify Answer Columns
  # TODO: These columns are METADATA, might need to be adapted
  metadata_cols <- c("submission_id", "upload_date", "start_date", "end_date",
                     "fieldworker_id", "uploaded_by", "location_available",
                     "response_channel_id", "device_id", "survey_external_id",
                     "longitude", "latitude", "instance", "comment", "gps_location")

  # The rest of the columns are answers
  answer_cols <- setdiff(names(main), metadata_cols)

  # Process Each Submission (row)
  all_entities <- purrr::map(1:nrow(main), function(i) {
    submission_row <- main[i, ]
    task_entities <- list()

    # Create Survey Completion Task
    submission_id <- submission_row$submission_id
    task_id <- paste0(base_uri, "task/", submission_id)
    participant_id <- paste0(base_uri, "part/", submission_row$fieldworker_id)

    task_entity <- list(
      `@id` = task_id,
      `@type` = "sur:SurveyCompletionTask",
      `sur:sessionId` = submission_id,
      # Format dates - ensure they are POSIXct or convertible
      `prov:startedAtTime` = list("@value" = format(as.POSIXct(submission_row$start_date), "%Y-%m-%dT%H:%M:%S"), "@type" = "xsd:dateTime"),
      `prov:endedAtTime` = list("@value" = format(as.POSIXct(submission_row$end_date), "%Y-%m-%dT%H:%M:%S"), "@type" = "xsd:dateTime"),
      `prov:wasAssociatedWith` = list("@id" = participant_id),
      `sur:describedByProcess` = list("@id" = procedure_uri)

      # TODO add location, using longitude/latitude/gps_location
    )
    task_entities[[length(task_entities) + 1]] <- task_entity

    # Create Completed Question for each answer (column) - iterate over questions
    for (col_name in answer_cols) {
      answer_value <- submission_row[[col_name]]

      # TODO: might need to be handled differently (should be left empty?)
      # Skip if the answer is NA or an empty string
      if (is.na(answer_value) || answer_value == "") {
        next
      }

      # Find corresponding question info using the column name
      question_info <- question_lookup %>% dplyr::filter(question_name == col_name)

      # Handle case if multiple questions have the same name (shouldn't happen ideally)
      question_info <- question_info[1, ]
      question_id_num <- question_info$question_id
      question_uri <- paste0(base_uri, "q/", question_id_num)
      question_type <- question_info$question_type

      # Create Completed Question entity
      cq_id <- paste0(base_uri, "cq/", submission_id, "/", question_id_num)
      cq_entity <- list(
        `@id` = cq_id,
        `@type` = "sur:CompletedQuestion",
        `qb:dataSet` = list("@id" = dataset_uri),
        #`sur:answeredIn` = list("@id" = task_id),
        #`prov:wasAttributedTo` = list("@id" = participant_id),
        `sur:completesQuestion` = list("@id" = question_uri),

        `sur:hasCompletionTimestamp` = list("@value" = format(as.POSIXct(submission_row$end_date), "%Y-%m-%dT%H:%M:%S"), "@type" = "xsd:dateTime")
      )

      # Add the answer based on the question type
      # add other types when needed
      is_closed_question <- question_type %in% c("Xform Select One", "Xform Select Many")

      if (is_closed_question) {
        # If it is a closed question (get the corresponding options from the closed_answer lookup table)
        answer_value_char <- as.character(answer_value)
        option_match <- closed_answer_lookup %>%
          dplyr::filter(question_id == question_id_num, response_value_char == answer_value_char)

        if (nrow(option_match) > 0) {
          cq_entity$`sur:hasAnswer` <- list("@id" = option_match$answer_option_uri[1])
        } else {
          # TODO: Handle case where the answer value doesn't match any of the defined options.
          # Or default to storing it as text?
          cq_entity$`sur:hasAnswerText` <- list("@value" = as.character(answer_value), "@type" = "xsd:string")
        }
      } else {
        # For open questions, store the literal value
        # TODO: fix logic to determine the correct type
        value_type <- "xsd:string"
        if(is.numeric(answer_value)) value_type <- "xsd:decimal"
        # Basic check for URL format
        if(is.character(answer_value) && grepl("^http[s]?://", answer_value)) value_type <- "xsd:anyURI"

        cq_entity$`sur:hasAnswerText` <- list("@value" = as.character(answer_value), "@type" = value_type)
      }

      task_entities[[length(task_entities) + 1]] <- cq_entity
    }

    return(task_entities)
  })

  # Flatten
  final_list <- unlist(all_entities, recursive = FALSE)
  return(final_list)
}

