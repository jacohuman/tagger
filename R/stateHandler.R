#' A list to keep track of tagging progress.
#' The tag_state is saved as progress is made so the pipeline can be resumed in
#' case it is paused or interrupted.
#'
new_tag_state <- function(questions) {
  structure(
    list(
      questions  = questions,        # tibble: id, caption
      embeddings = NULL,             # matrix
      hclust     = NULL,             # hclust object
      clusters   = NULL,             # tibble: id, level, cluster_id
      tags       = NULL,             # tibble: level, cluster_id, tag
      tag_matrix = NULL,             # tibble: id, tag_level_1...tag_level_n
      cleaned    = NULL,             # vocab cleanup info
      audit      = NULL,             # audit results
    ),
    class = "tag_state"
  )
}

#' Save tagging state to disk
#'
#' @param state tag_state to write to disk
#' @param path Path where the tag_state should be written
#'
#' @export
save_tag_state <- function(state, path) {
  saveRDS(state, path)
}

#' Load tagging state from disk
#'
#' @param path Path from where the tag_state should be read
#'
#' @export
load_tag_state <- function(path)
  readRDS(path)
