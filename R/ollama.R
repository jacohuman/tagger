# R/ollama.R

# This file contains functions for interacting with a running Ollama instance.
# The functions defined below each does the following respectively:
# tests the connection to the Ollama server, builds and sends prompts as http
# requests to running ollama models and parses responses or errors accordingly.


#' Send a request to an Ollama server.
#'
#' @description
#' This generic helper wraps a request body as JSON and sends it to the
#' specified Ollama endpoint.
#'
#' @param base_url Base URL for the Ollama server.
#' @param endpoint Endpoint path, e.g. \code{"/api/generate"}.
#' @param body Request body that will be encoded as JSON.
#' @param timeout_sec Request timeout in seconds.
#'
#' @return An httr2 response object.
#'
#' @examples
#' \dontrun{
#' resp <- ollama_request(
#'   base_url = "http://localhost:11434",
#'   endpoint = "/api/tags",
#'   body = list()
#' )
#' resp$status_code
#' }
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_body_json
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_timeout
#'
#' @export
ollama_request <- function(base_url, endpoint, body, timeout_sec = 60) {
  httr2::request(paste0(base_url, endpoint)) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_perform()
}

#' Send a prompt to an Ollama model.
#'
#' @description
#' Generic prompt helper that accepts runtime parameters and returns the text
#' response for tagging or other generation tasks.
#'
#' @param prompt The user prompt to send to the model.
#' @param model Name of the Ollama model to use.
#' @param base_url Base URL for the Ollama server.
#' @param num_predict Maximum number of tokens to generate.
#' @param temperature Controls the randomness/creativity of the output.
#' @param format Optional response format passed to Ollama.
#' @param timeout_sec Request timeout in seconds.
#'
#' @return Character response content.
#'
#' @examples
#' \dontrun{
#' ollama_generate(
#'   prompt = "Return the word ok.",
#'   model = "llama3.1:8b",
#'   base_url = "http://localhost:11434"
#' )
#' }
#'
#' @importFrom httr2 resp_body_json
#'
#' @export
ollama_generate <- function(
    prompt,
    model,
    base_url,
    num_predict = 64,
    temperature = 0.2,
    format = NULL,
    timeout_sec = 250
) {
  body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(
      num_predict = num_predict,
      temperature = temperature
    )
  )
  if (!is.null(format)) {
    body$format <- format
  }

  resp <- ollama_request(base_url, "/api/generate", body, timeout_sec = timeout_sec)
  parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  response <- rlang::`%||%`(parsed$response, parsed$message$content)
  rlang::`%||%`(response, "")
}

#' Embed a string using an Ollama embedding model.
#'
#' @param text Text to embed.
#' @param model Embedding model name.
#' @param base_url Base URL for the Ollama server.
#' @param timeout_sec Request timeout in seconds.
#'
#' @return Numeric embedding vector.
#'
#' @examples
#' \dontrun{
#' ollama_embed(
#'   text = "How many people live in your household?",
#'   model = "nomic-embed-text",
#'   base_url = "http://localhost:11434"
#' )
#' }
#'
#' @export
ollama_embed <- function(text, model, base_url, timeout_sec = 60) {
  resp <- ollama_request(
    base_url,
    "/api/embeddings",
    list(model = model, prompt = text),
    timeout_sec = timeout_sec
  )
  parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  as.numeric(parsed$embedding)
}

#' Test Ollama connection
#'
#' @description
#' Before running the tagging pipeline configure and test the Ollama connection
#' to ensure the necessary models are running and reachable.
#'
#' @param base_url Base url where the Ollama instance is running.
#' @param model_name Desired model to use for embedding/tagging.
#'
#' @return TRUE on connection success.
#'
#' @examples
#' \dontrun{
#' test_connection("http://localhost:11434", "llama3.1:8b")
#' }
#'
#' @export
test_connection <- function(base_url, model_name) {
  prompt_text <- "Return the word ok."
  response <- ollama_generate(
    prompt = prompt_text,
    model = model_name,
    base_url = base_url,
    num_predict = 3,
    temperature = 0
  )
  nzchar(response)
}
