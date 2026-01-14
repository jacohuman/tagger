#' Load a forms tibble from (.Rda) file
#'
#' @param path Path to the .Rda forms file to read
#'
#' @export
load_forms <- function(path) {
  stopifnot(file.exists(path))
  object_names <- load(path)
  stopifnot(length(object_names) >= 1)
  object_name <- object_names[[1]]
  stopifnot(exists(object_name))
  forms <- get(object_name)

  forms
}
