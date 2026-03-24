# Save .jsonld and .html files to correct subdirectory in GitHub Pages repo
#' write_entity_files
#' @description
#' Function to write jsonld and html files for an entity to the appropriate subdirectory
#' in the GitHub Pages repository idenfied by gh_pages_path and subdir.
#'
#' @param entity A list representing the entity to write, typically in JSON-LD format.
#'
#' @returns Writes JSON-LD and HTML files for the entity to the specified GitHub Pages path.
#' @export
#'
#' @examples
#' walk(all_graph_entities, write_entity_files)

write_entity_files <- function(entity = NULL, gh_pages_path = NULL, subdir = NULL) {

  if (is.null(entity) || is.null(gh_pages_path) || is.null(subdir)) {
    stop("Both 'entity' and 'gh_pages_path' and subdir must be provided.")
  }
  id <- entity[["@id"]]
  if (is.null(id)) return()

  file_base <- sanitize_id(id)
  subdir    <- get_subdir_from_id(id)
  out_dir   <- file.path(gh_pages_path, subdir)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # JSON-LD wrapped in @context and @graph
  jsonld_obj <- list(
    "@context" = "http://schema.org",  # Update if using a different context
    "@graph" = list(entity)
  )

  # HTML from entity
  html_str <- generate_html(entity)

  # File paths
  jsonld_path <- file.path(out_dir, paste0(file_base, ".jsonld"))
  html_path   <- file.path(out_dir, paste0(file_base, ".html"))

  # Write both files
  write(toJSON(jsonld_obj, auto_unbox = TRUE, pretty = TRUE), jsonld_path)
  write(html_str, html_path)
}
