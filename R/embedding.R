# R/embedding.R

# This file defines functions used in the embedding step of the tagging pipeline.
# The configured embedding model (as defined in `config.R`) will be used to generate
# the embedding vectors.

# TODO: Retain/construct question ID's from original forms tibble.


#' Extract unique question captions from nested forms
#'
#' @description
#' Flattens a nested `form` column, pulls the `caption` field of the questions,
#' normalizes white space, removes empty or `NA` captions and returns
#' the first \code{limit_n} rows with a generated ID of the form `"q_001"`,
#' `"q_002"`, ...
#'
#' @param forms A data frame (or tibble) with a list-column named `form`
#'   containing data frames/tibbles that include a `caption` column for each question.
#' @param limit_n Limit the number of question captions to select for tagging.  This is
#'   used during development to reduce the waiting time between pipeline runs.
#'
#' @return A tibble with two columns:
#' \itemize{
#'   \item \code{caption} — the cleaned, unique caption text
#'   \item \code{id} — a zero-padded identifier in the form \code{"q_###"}
#' }
#'
#' @examples
#' forms <- tibble::tibble(
#'   form = list(tibble::tibble(caption = c("Age", "Household size", "Age")))
#' )
#' unique_q <- unnest_questions(forms, limit_n = 100)
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr %>%
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom dplyr slice_head
#' @importFrom dplyr row_number
#' @importFrom stringr str_squish
#'
#' @export
unnest_questions = function(forms, limit_n = Inf) {
  unique_q <- forms %>%
    tidyr::unnest(form) %>%
    dplyr::transmute(caption = caption) %>%
    dplyr::filter(!is.na(caption), nzchar(caption)) %>%
    dplyr::mutate(caption = stringr::str_squish(caption)) %>%
    dplyr::distinct(caption) %>%
    dplyr::slice_head(n = limit_n) %>%
    dplyr::mutate(id = sprintf("q_%03d", dplyr::row_number()))

  # return the unique set of question captions
  return(unique_q)
}


#' Embed a single question using an Ollama embedding model
#'
#' @description
#' Calls the local Ollama embeddings API (\code{/api/embeddings}) and returns
#' the numeric embedding vector for the supplied text.
#'
#' @param text The text to embed.
#' @param model Embedding model name/tag available in the running Ollama instance.
#' @param base_url Base URL for the Ollama server,
#'   e.g. \code{"http://localhost:11434"}.
#' @param timeout_sec Request timeout in seconds.
#'
#' @return A numeric vector containing the embedding for \code{text}.
#'
#' @examples
#' \dontrun{
#' v <- embed_single_question(
#'   "How many people live in your household?",
#'   model = "nomic-embed-text",
#'   base_url = "http://localhost:11434"
#' )
#' length(v)  # embedding dimension
#' }
#'
#' @export
embed_single_question = function(text, model, base_url, timeout_sec = 60) {
  ollama_embed(text, model = model, base_url = base_url, timeout_sec = timeout_sec)
}


#' Embed multiple question texts
#'
#' @description
#' Wrapper that iterates over a character vector of texts and
#' calls [embed_single_question()] for each element. As the texts are sent to the
#' embedding model a simple textual progress bar is displayed.
#'
#' @param texts Character vector. Each element is a question to embed.
#' @param model Embedding model name or tag available on the running Ollama instance.
#' @param base_url Base URL for the Ollama server.
#' @param timeout_sec Request timeout in seconds.
#'
#' @return A list of numeric vectors. Each list element is the embedding for
#'   the corresponding element of \code{texts}.
#'
#' @examples
#' \dontrun{
#' qs <- c("What is your age?", "What is your highest level of education?")
#' embs <- embed_multiple_questions(
#'   qs,
#'   model = "nomic-embed-text",
#'   base_url = "http://localhost:11434"
#' )
#' str(embs)
#' }
#'
#' @seealso [embed_single_question()]
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @export
embed_multiple_questions <- function(texts, model, base_url, timeout_sec = 60) {
  # Add a simple progress bar to keep track of the embedding progress
  pb <- utils::txtProgressBar(min = 0, max = length(texts), style = 3)
  out <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    out[[i]] <- embed_single_question(
      texts[[i]],
      model = model,
      base_url = base_url,
      timeout_sec = timeout_sec
    )
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  return(out)
}
