#' Load a forms tibble from (.Rda) file
#'
#' @param path Path to the .Rda froms file to read
#'
#' @export
load_forms <- function(path) {
  stopifnot(file.exists(path))
  forms <- load(path)
  stopifnot(exists(forms))

  forms_flat <- forms %>%
    unnest(form) %>%
    filter(!is.na(caption), nzchar(caption))

  return(forms_flat)
}
