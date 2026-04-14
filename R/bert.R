#' BERTopic environment readiness check
#'
#' @description
#' Utility to validate that the Python dependencies required by BERTopic are
#' visible from `reticulate` before running [run_bertopic_tagger_pipeline()].
#'
#' @param verbose Logical. If `TRUE`, prints setup guidance when dependencies
#'   are missing.
#'
#' @return Named logical vector describing module availability.
#' @export
check_bertopic_environment <- function(verbose = interactive()) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  status <- c(
    bertopic = reticulate::py_module_available("bertopic"),
    sentence_transformers = reticulate::py_module_available("sentence_transformers"),
    umap = reticulate::py_module_available("umap"),
    hdbscan = reticulate::py_module_available("hdbscan"),
    sklearn = reticulate::py_module_available("sklearn")
  )

  missing <- names(status)[!status]
  if (isTRUE(verbose) && length(missing) > 0) {
    message(
      "Missing Python modules: ", paste(missing, collapse = ", "), "\n",
      "Run install_bertopic_environment() once, then re-run check_bertopic_environment()."
    )
  }

  status
}

#' Install Python dependencies required for BERTopic mode
#'
#' @description
#' Creates/updates a dedicated Python environment used by `reticulate`, then
#' installs the BERTopic dependency set so [run_bertopic_tagger_pipeline()] can run.
#'
#' @param envname Name of the Python environment to create/use.
#' @param method Python environment backend. One of `"auto"`, `"virtualenv"`,
#'   or `"conda"`.
#' @param python_version Optional Python version passed through to
#'   [reticulate::install_miniconda()].
#' @param pip Logical. Forwarded to [reticulate::py_install()].
#'
#' @return Invisibly returns [check_bertopic_environment()] output after install.
#' @export
install_bertopic_environment <- function(
    envname = "r-tagger-bertopic",
    method = c("auto", "virtualenv", "conda"),
    python_version = NULL,
    pip = TRUE
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for BERTopic integration.", call. = FALSE)
  }

  method <- match.arg(method)
  py_pkgs <- c(
    "bertopic",
    "sentence-transformers",
    "umap-learn",
    "hdbscan",
    "scikit-learn"
  )

  if (method %in% c("auto", "conda")) {
    if (!reticulate::miniconda_exists()) {
      reticulate::install_miniconda(python_version = python_version)
    }
  }

  install_method <- if (method == "auto") "auto" else method
  reticulate::py_install(
    packages = py_pkgs,
    envname = envname,
    method = install_method,
    pip = pip
  )

  if (install_method == "conda") {
    reticulate::use_condaenv(envname, required = TRUE)
  } else if (install_method == "virtualenv") {
    reticulate::use_virtualenv(envname, required = TRUE)
  } else {
    # "auto" chooses the best backend available; try conda first, then virtualenv.
    try(reticulate::use_condaenv(envname, required = FALSE), silent = TRUE)
    try(reticulate::use_virtualenv(envname, required = FALSE), silent = TRUE)
  }

  invisible(check_bertopic_environment(verbose = TRUE))
}
