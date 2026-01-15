#' Audit tag paths for hierarchy issues
#'
#' @param tag_matrix Tag matrix with tag_level_* columns.
#'
#' @return Tibble with per-question audit flags.
#'
#' @examples
#' tag_matrix <- tibble::tibble(
#'   id = c("q_001", "q_002"),
#'   caption = c("Age?", "Income?"),
#'   tag_level_1 = c("age", NA_character_),
#'   tag_level_2 = c("demographics", NA_character_)
#' )
#' audit_tag_matrix(tag_matrix)
#' @export
audit_tag_matrix <- function(tag_matrix) {
  tag_cols <- grep("^tag_level_", names(tag_matrix), value = TRUE)
  if (length(tag_cols) == 0) {
    stop("No tag_level_* columns found in tag_matrix.")
  }

  audit_rows <- purrr::map_dfr(seq_len(nrow(tag_matrix)), function(i) {
    row <- tag_matrix[i, ]
    tags <- unlist(row[tag_cols], use.names = FALSE)
    tags <- tags[!is.na(tags) & nzchar(tags)]

    duplicate_tags <- length(unique(tags)) < length(tags)
    missing_all <- length(tags) == 0

    tibble::tibble(
      id = row$id,
      caption = row$caption,
      duplicate_tags = duplicate_tags,
      missing_all = missing_all
    )
  })

  audit_rows
}

#' Audit tag paths against the ontology
#'
#' @param state A `tag_state`.
#' @param allow_manual Logical, whether to allow manual correction.
#'
#' @return Updated `tag_state` with `audit` tibble.
#'
#' @examples
#' state <- list(
#'   tag_matrix = tibble::tibble(
#'     id = "q_001",
#'     caption = "Age?",
#'     tag_level_1 = "age"
#'   )
#' )
#' class(state) <- "tag_state"
#' audit_tag_paths(state, allow_manual = FALSE)
#' @export
audit_tag_paths <- function(state, allow_manual = TRUE) {
  stopifnot(inherits(state, "tag_state"))
  stopifnot(!is.null(state$tag_matrix))

  audits <- audit_tag_matrix(state$tag_matrix)
  state$audit <- audits

  if (allow_manual) {
    message("Audit complete. Review state$audit for flagged rows.")
  }

  state
}
