# R/config.R

# Package-wide defaults for Ollama endpoints and model names.
# These defaults can be updated or alternatively overridden by setting
# environment variables with the same names or passing them as parameters.

# Defaults
DEFAULT_OLLAMA_BASE  <- "http://localhost:11434"
DEFAULT_EMBED_MODEL  <- "nomic-embed-text"
DEFAULT_TAGGER_MODEL <- "llama3.1:8b"

#' Internal config getter
#'
#' @description
#' Invoke this function to return the list of configuration parameters to use
#' in subsequent embedding or tagging logic.
#'
#' @returns A list of Ollama configuration parameters.
#'
#' @export
ollama_config <- function(base_url = NULL, embed_model = NULL, tagger_model = NULL) {
  list(
    if (is.null(base_url)) {
      base_url     = Sys.getenv("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE)
    }
    if (is.null(embed_model)) {
      embed_model    = Sys.getenv("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE)
    }
    if (is.null(tagger_model)) {
      tagger_model     = Sys.getenv("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE)
    }
  )
}

