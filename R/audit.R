#' Audit tag paths against the ontology
#'
#' @param state A `tag_state`.
#' @param allow_manual Logical, whether to allow manual correction.
#'
#' @return Updated `tag_state` with `audit` tibble (and optionally updated `tag_matrix`).
#' @export
audit_tag_paths <- function(state, allow_manual = TRUE) {
  stopifnot(inherits(state, "tag_state"))
  stopifnot(!is.null(state$tag_matrix))

  # 1) Build allowed tags per level
  allowed <- state$tag_matrix |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("tag_level_"),
      names_to = "level",
      values_to = "tag"
    ) |>
    dplyr::mutate(level_num = readr::parse_number(level)) |>
    dplyr::filter(!is.na(tag)) |>
    dplyr::distinct(level_num, tag)

  # 2) For each question, call LLM to confirm or adjust path
  audits <- purrr::pmap_dfr(
    list(
      id       = state$tag_matrix$id,
      caption  = state$tag_matrix$caption
      # plus the path per row
    ),
    ~ audit_single_question(..., allowed = allowed)
  )

  state$audit <- audits

  # Optionally, apply corrected paths, maybe after manual review.
  if (allow_manual) {
    # e.g. write audits to CSV, ask user to review, and re-import.
  }

  state
}
