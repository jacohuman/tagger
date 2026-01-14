# R/ollama.R

# This file contains functions for interacting with a running Ollama instance.
# The functions defined below each does the following respectively:
# tests the connection to the Ollama server, builds and sends prompts as http
# requests to running ollama models and parses responses or errors accordingly.


#' Test Ollama connection
#'
#' @description
#' Before running the tagging pipeline configure and test the Ollama connection
#' to ensure the necessary models are running and reachable. A test prompt is sent
#' to each of the running models to ensure a connection can be established.
#'
#' @param ollama_url Base url where the ollama instance is running.
#' @param model_name Desired model to use for embedding/tagging.
#'
#' @returns TRUE on connection success.
#'
#' @importFrom httr http_status
#' @importFrom jsonlite fromJSON
#'
#' @export
test_connection <- function(ollama_url, model_name) {

  prompt_text <- "Explain the concept of quantum entanglement in simple terms."

  request_body <- list(
    model = model_name,
    prompt = prompt_text
  )

  if (http_status(response)$category == "Success") {

    response_content <- content(response, "text", encoding = "UTF-8")
    parsed_response <- fromJSON(response_content)

    cat("Ollama Response:\n")
    if (!is.null(parsed_response$response)) {
      cat(parsed_response$response, "\n")
    } else {
      print(parsed_response) # Print the whole structure if 'response' isn't found
    }
  } else {
    cat("Error sending request to Ollama:\n")
    print(http_status(response))
    cat(content(response, "text", encoding = "UTF-8"), "\n")
  }
}


#' Send a request to an Ollama server.
#'
#' @description
#' This generic function wraps a user prompt as a http request before sending it
#' to an active Ollama server. The LLM response is parsed, interpreted and returned
#' if the request was successful; an appropriate error message is returned upon
#' failure.
#'
#' @param prompt The user prompt to send to the model.
#' @param num_predict Maximum number of tokens the language model will generate for its response.
#' @param temperature Controls the randomness/creativity of the output (low temperatures make responses more predictable)
#'
#' @importFrom httr request
#' @importFrom httr req_error
#' @importFrom httr req_perform
#' @importFrom jsonlite resp_body_json
#'
#' @export
ollama_generate <- function(prompt, num_predict = 3, temperature = 0.2) {

  req <- request(paste0(base, "/api/generate")) |>
    req_body_json(list(
      model  = model,
      prompt = prompt,
      stream = FALSE,
      options = list(num_predict = num_predict, temperature = temperature)
    )) |>
    req_error(is_error = function(resp) FALSE)


  # TODO:  Handle error responses gracefully
  resp <- req_perform(req)
  if (resp_status(resp) == 200) {
    raw <- resp_body_json(resp)$response
    cand <- str_extract(tolower(str_squish(raw)), "^[a-z]+(?: [a-z]+)?")
    if (!is.na(cand) && nzchar(cand)) return(cand)
  }
  "untagged"
}
