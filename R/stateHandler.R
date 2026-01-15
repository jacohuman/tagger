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
      assignments = NULL,            # tibble: question + cluster_level_* columns
      clusters   = NULL,             # tibble: level, cluster_id, parent_cluster, tag
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
#' @examples
#' state <- list(questions = tibble::tibble(id = "q_001", caption = "Age?"))
#' class(state) <- "tag_state"
#' tmp <- tempfile(fileext = ".rds")
#' save_tag_state(state, tmp)
#'
#' @export
save_tag_state <- function(state, path) {
  saveRDS(state, path)
}

#' Load tagging state from disk
#'
#' @param path Path from where the tag_state should be read
#'
#' @examples
#' \dontrun{
#' state <- load_tag_state("question_tagger_state.rds")
#' }
#'
#' @export
load_tag_state <- function(path) {
  readRDS(path)
}
