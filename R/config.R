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
#' @return A list of Ollama configuration parameters.
#'
#' @examples
#' ollama_config()
#' ollama_config(base_url = "http://localhost:11434")
#'
#' @export
ollama_config <- function(
    base_url = Sys.getenv("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE),
    embed_model = Sys.getenv("EMBED_MODEL", DEFAULT_EMBED_MODEL),
    tagger_model = Sys.getenv("TAGGER_MODEL", DEFAULT_TAGGER_MODEL)
) {
  list(
    base_url = base_url,
    embed_model = embed_model,
    tagger_model = tagger_model
  )
}
