#' BERTopic environment readiness check
#'
#' @description
#' Utility to validate that the Python dependencies required by BERTopic are
#' visible from `reticulate` before running [run_bertopic_tagger_pipeline()].
#'
#' @return Named logical vector describing module availability.
#' @export
check_bertopic_environment <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  c(
    bertopic = reticulate::py_module_available("bertopic"),
    sentence_transformers = reticulate::py_module_available("sentence_transformers"),
    umap = reticulate::py_module_available("umap"),
    hdbscan = reticulate::py_module_available("hdbscan"),
    sklearn = reticulate::py_module_available("sklearn")
  )
}
