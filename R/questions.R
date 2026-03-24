#' Prepare survey questions for tagging
#'
#' @description
#' This function prepares questions for tagging by extracting distinct question
#' captions from the given flattened form tibble.  An optional (integer) slice
#' value (n) may be specified to select only n questions for tagging (used for demo
#' purposes to speed up the tagging pipeline).
#'
#' @param flattened_forms description
#' @param id_col description
#' @param text_col
#' @param n
#'
#' @importFrom
#' @importFrom
#'
#' @export
prepare_questions <- function(flattened_forms, id_col = NULL, text_col = "caption", n = NULL) {

  if (!is.null(n)) {
    # Keep only a n-question slice for demo purposes
    questions <- flattened_forms %>%
      distinct(caption) %>%
      slice_head(n = 300)
  } else {
    questions <- flattened_forms %>%
      distinct(caption)
  }

  questions <- tibble(form = list(questions))
  return(questions)
}
