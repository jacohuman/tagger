#' normalize_survey_name
#' @description
#' This function normalizes a survey name by removing leading "X" before a number,
#' then replacing underscores and punctuation with spaces, and finally
#' capitalizing each word to create a PascalCase format.
#' @param name A character string representing the survey name to be normalized.
#'
#' @returns A character string with the normalized survey name in PascalCase.
#' @export
#'
#' @examples
#' normalize_survey_name("X1st_survey_data")
#'
normalize_survey_name <- function(name) {
  # Remove leading "X" before a number
  name <- gsub("^X(?=[0-9])", "", name, perl = TRUE)

  # Replace underscores and punctuation with spaces
  name <- gsub("[._]", " ", name)

  # Fix common ordinals
  name <- gsub("\\b1st\\b", "First", name, ignore.case = TRUE)
  name <- gsub("\\b2nd\\b", "Second", name, ignore.case = TRUE)
  name <- gsub("\\b3rd\\b", "Third", name, ignore.case = TRUE)
  name <- gsub("\\b4th\\b", "Fourth", name, ignore.case = TRUE)

  # Capitalize each word
  words <- strsplit(name, " +")[[1]]
  words <- tools::toTitleCase(tolower(words))

  # Join back into PascalCase
  result <- paste0(words, collapse = "")
  return(result)
}
