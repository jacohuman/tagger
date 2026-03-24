
#' get_subdir_from_id
#'
#' @param id a string representing an ID, typically a path or URL
#' @param idx an integer representing the index of the subdirectory to extract. default is 5.
#'
#' @returns a string representing the subdirectory at the specified index, or "misc" if the index is out of bounds.
#' @export
#'
#' @examples
#' get_subdir_from_id("a/b/c/d/e/f/g")
get_subdir_from_id <- function(id, idx = 5) {
  parts <- strsplit(id, "/")[[1]]
  if (length(parts) >= idx) return(parts[idx])
  return("misc")
}

#' sanitize_id
#' @description
#' This function sanitizes an ID by removing any leading path components, leaving only the last segment.
#' @param id Character string representing an ID, typically a path or URL
#'
#' @returns a sanitized string containing only the last segment of the ID.
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' sanitize_id("a/b/c/d/e/f/g")
#'
sanitize_id <- function(id = NULL) {
  stringr::str_replace(id, "^.*/", "")

}
