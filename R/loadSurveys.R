#' loadSurveys
#' @param path Character string specifying the path to the survey directories.
#'
#' @returns List of survey data frames, where each element corresponds to a survey directory.
#' @export
#'
#' @examples
#' loadSurveys("path/to/surveys")

loadSurveys <- function(path) {
  survey_dirs <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)

  if (length(survey_dirs) == 0) {
    warning("No survey directories found in the specified path: ", path)
    return(list())
  }

  survey_data_list <- list()

  for (dir in survey_dirs) {
    survey_name <- basename(dir)
    survey_name <- make.names(survey_name)

    survey_data_list[[survey_name]] <- import_csv_files(dir)
  }

  return(survey_data_list)
}
