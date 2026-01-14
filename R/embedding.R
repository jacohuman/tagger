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
#'
#' @return A tibble with two columns:
#' \itemize{
#'   \item \code{caption} — the cleaned, unique caption text
#'   \item \code{id} — a zero-padded identifier in the form \code{"q_###"}
#' }
#'
#' @examples
#' unique_q <- unnest_questions(forms, limit_n = 100)
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom slice_head
#' @importFrom row_number
#' @importFrom stringr str_squish
#'
#' @export
unnest_questions = function(forms, limit_n) {
  unique_q <- forms %>%
    unnest(form) %>%
    transmute(caption = caption) %>%
    filter(!is.na(caption), nzchar(caption)) %>%
    mutate(caption = stringr::str_squish(caption)) %>%
    distinct(caption) %>%
    slice_head(n = limit_n) %>%
    mutate(id = sprintf("q_%03d", dplyr::row_number()))

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
#' @param config Embedding model name/tag available in the running Ollama instance.
#' @param base Base URL for the Ollama server,
#'   e.g. \code{"http://localhost:11434"}.
#'
#' @return A numeric vector containing the embedding for \code{text}.
#'
#' @examples
#' v <- embed_single_question("How many people live in your household?")
#' length(v)  # embedding dimension
#'
#' @importFromr httr2 req_timeout
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 req_body_json
#'
#' @export
embed_single_question = function(text, model = EMBED_MODEL, base = OLLAMA_BASE) {
  req  <- request(paste0(base, "/api/embeddings")) |>
    req_body_json(list(model = model, prompt = text)) |>
    # TODO: Update timeout to some realistic parameter.
    req_timeout(60)
  resp <- req_perform(req)
  j    <- resp_body_json(resp, simplifyVector = TRUE)
  return(as.numeric(j$embedding))
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
#' @param base Base URL for the Ollama server.
#'
#' @return A list of numeric vectors. Each list element is the embedding for
#'   the corresponding element of \code{texts}.
#'
#' @examples
#' qs <- c("What is your age?", "What is your highest level of education?")
#' embs <- embed_multiple_questions(qs)
#' str(embs)
#'
#' @seealso [embed_single_question()]
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @export
embed_multiple_questions <- function(texts, model = EMBED_MODEL, base = OLLAMA_BASE) {
  # Add a simple progress bar to keep track of the embedding progress
  pb <- txtProgressBar(min = 0, max = length(texts), style = 3)
  out <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    out[[i]] <- embed_single_question(texts[[i]], model = model, base = base)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(out)
}
