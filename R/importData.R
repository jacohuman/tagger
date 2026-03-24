
#' Import a list of CSV files into R.
#'
#' @description
#' Generates a file list of all files of type .csv within a specified path.
#' These files are then imported using `read.csv()`.
#'
#' @param path The absolute path to the .csv files to import.
#'
#' @export
import_csv_files <- function(path) {
  file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

  if (length(file_list) == 0) {
    warning("No .csv files could be found in the specified path: ", path)
    return(list())
  }

  df_list <- list()

  for (file in file_list) {
    df_name <- gsub("\\.csv$", "", basename(file))
    df_name <- make.names(df_name)

    tryCatch({
      df_list[[df_name]] <- read.csv(file) |> tibble::as_tibble()
    }, error = function(e) {
      warning("Failed to read file: ", file, "\nError: ", conditionMessage(e))
      df_list[[df_name]] <- NULL
    })
  }
  return(df_list)
}
