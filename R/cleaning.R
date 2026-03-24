# tag_cleanup_synonyms.R

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(stringi)
library(tibble)

# -------------------------------------------------------------------
# 1) Deterministic cleaning
# -------------------------------------------------------------------

#' Canonicalize Tags for Deterministic Comparison
#'
#' @description
#' Normalizes tags as the first step in the tag cleanup pipeline.
#' Steps:
#' - convert to character
#' - trim whitespace
#' - lowercase
#' - transliterate to ASCII
#' - unify separators to spaces
#' - remove non-alphanumeric chars except spaces
#' - collapse repeated whitespace
#'
#' @param x Character vector of tags.
#'
#' @return Character vector of canonicalized tags.
#' @export
canon_tag <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)

  x <- str_trim(x)
  x[x == ""] <- NA_character_
  if (all(is.na(x))) return(x)

  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  # unify separators to spaces
  x <- str_replace_all(x, "[_\\-]+", " ")

  # keep only letters/digits/spaces
  x <- str_replace_all(x, "[^a-z0-9 ]+", " ")

  # collapse whitespace
  x <- str_squish(x)

  x[x == ""] <- NA_character_
  x
}

#' Collapse Consecutive Duplicate Tags
#'
#' @param tags Character vector without NAs/empties.
#'
#' @return Character vector with consecutive duplicates collapsed.
#' @export
collapse_consecutive <- function(tags) {
  if (length(tags) <= 1) return(tags)
  keep <- c(TRUE, tags[-1] != tags[-length(tags)])
  tags[keep]
}

#' Remove Duplicate Tags (keep first occurrence)
#'
#' @param tags Character vector of tags.
#' @param consecutive_first Logical; collapse consecutive duplicates first.
#'
#' @return Character vector with duplicates removed.
#' @export
remove_duplicates <- function(tags, consecutive_first = TRUE) {
  tags <- as.character(tags)

  tags <- tags[!is.na(tags)]
  tags <- stringr::str_trim(tags)
  tags <- tags[tags != ""]

  if (length(tags) == 0) return(character(0))

  if (consecutive_first) {
    tags <- collapse_consecutive(tags)
  }

  tags[!duplicated(tags)]
}

#' Clean a single tag row
#'
#' @param tags Raw tag vector of length k.
#' @param tag_cols Tag column names in order.
#'
#' @return Named list containing cleaned vector and audit fields.
#' @export
clean_tag_row <- function(tags, tag_cols) {
  canon <- canon_tag(tags)

  canon_non_na <- canon[!is.na(canon)]
  dedup <- remove_duplicates(canon_non_na)

  k <- length(tag_cols)
  padded <- c(dedup, rep(NA_character_, max(0, k - length(dedup))))[1:k]
  names(padded) <- tag_cols

  list(
    vec = padded,
    before_path = paste(canon_non_na, collapse = " > "),
    after_path  = paste(stats::na.omit(padded), collapse = " > "),
    n_before = length(canon_non_na),
    n_after  = sum(!is.na(padded)),
    n_removed = length(canon_non_na) - sum(!is.na(padded)),
    changed = !identical(canon_non_na, stats::na.omit(padded))
  )
}

#' Clean a full tag matrix
#'
#' @param tag_matrix Tag matrix with id, caption and tag_level_* columns.
#' @param tag_cols Optional tag columns.
#'
#' @return List with cleaned matrix and audit tibble.
#' @export
clean_tag_matrix <- function(tag_matrix, tag_cols = NULL) {
  if (is.null(tag_cols)) {
    tag_cols <- grep("^tag_level_", names(tag_matrix), value = TRUE)
  }
  stopifnot(length(tag_cols) > 0)

  cleaned <- tag_matrix %>%
    dplyr::mutate(
      .clean = purrr::pmap(
        dplyr::select(., dplyr::all_of(tag_cols)),
        ~ clean_tag_row(c(...), tag_cols = tag_cols)
      )
    ) %>%
    dplyr::mutate(
      n_before   = purrr::map_int(.clean, "n_before"),
      n_after    = purrr::map_int(.clean, "n_after"),
      n_removed  = purrr::map_int(.clean, "n_removed"),
      before_path = purrr::map_chr(.clean, "before_path"),
      after_path  = purrr::map_chr(.clean, "after_path"),
      changed    = purrr::map_lgl(.clean, "changed"),
      .vec       = purrr::map(.clean, "vec")
    ) %>%
    dplyr::select(-.clean) %>%
    dplyr::select(-dplyr::all_of(tag_cols)) %>%
    tidyr::unnest_wider(.vec) %>%
    dplyr::relocate(dplyr::all_of(tag_cols), .after = caption)

  audit <- cleaned %>%
    dplyr::select(id, caption, n_before, n_after, n_removed, changed, before_path, after_path)

  list(tag_matrix_clean = cleaned, audit = audit)
}

# -------------------------------------------------------------------
# 2) Build registry from cleaned tags
# -------------------------------------------------------------------

#' Build tag registry from cleaned tag matrix
#'
#' @param tag_matrix_clean Cleaned tag matrix.
#' @param tag_cols Optional tag columns.
#'
#' @return Tibble with columns tag and n.
#' @export
build_tag_registry_from_clean <- function(tag_matrix_clean, tag_cols = NULL) {
  if (is.null(tag_cols)) {
    tag_cols <- grep("^tag_level_", names(tag_matrix_clean), value = TRUE)
  }
  stopifnot(length(tag_cols) > 0)

  tag_matrix_clean %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(tag_cols),
      names_to = "level",
      values_to = "tag"
    ) %>%
    dplyr::mutate(tag = canon_tag(tag)) %>%
    dplyr::filter(!is.na(tag), nzchar(tag)) %>%
    dplyr::count(tag, name = "n", sort = TRUE)
}

# -------------------------------------------------------------------
# 3) Tag embeddings
# -------------------------------------------------------------------

#' Embed a single tag using an Ollama embedding model
#'
#' @param tag Tag text.
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#'
#' @return Numeric embedding vector.
#' @export
embed_single_tag <- function(tag, model, base_url, timeout_sec = 60) {
  ollama_embed(
    paste0("Survey question tag: ", tag),
    model = model,
    base_url = base_url,
    timeout_sec = timeout_sec
  )
}

#' Embed all unique tags in a tag registry
#'
#' @param tag_registry Tibble with column tag.
#' @param model Embedding model name.
#' @param base_url Ollama base URL.
#' @param timeout_sec Timeout in seconds.
#' @param progress Logical; print progress messages.
#'
#' @return Named list of embeddings keyed by tag.
#' @export
embed_tag_registry <- function(tag_registry,
                               model,
                               base_url,
                               timeout_sec = 60,
                               progress = TRUE) {
  stopifnot("tag" %in% names(tag_registry))

  tags <- tag_registry %>%
    dplyr::filter(!is.na(tag), nzchar(tag)) %>%
    dplyr::distinct(tag) %>%
    dplyr::pull(tag)

  out <- vector("list", length(tags))
  names(out) <- tags

  n <- length(tags)
  for (i in seq_along(tags)) {
    tg <- tags[[i]]
    if (progress) message(sprintf("[%d/%d] embedding tag: %s", i, n, tg))
    out[[tg]] <- embed_single_tag(
      tag = tg,
      model = model,
      base_url = base_url,
      timeout_sec = timeout_sec
    )
  }

  out
}

# -------------------------------------------------------------------
# 4) Similarity + pairwise candidate generation
# -------------------------------------------------------------------

#' Cosine similarity between two numeric vectors
#'
#' @param a Numeric vector.
#' @param b Numeric vector.
#'
#' @return Numeric cosine similarity or NA.
#' @keywords internal
cosine_sim <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  if (length(a) != length(b)) return(NA_real_)

  d <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (d == 0) return(NA_real_)

  sum(a * b) / d
}

#' Find embedding-based near-synonym tag candidates
#'
#' @description
#' Computes pairwise cosine similarity between tag embeddings and returns pairs
#' above a similarity threshold. These are only candidates for grouping.
#'
#' @param tag_registry Tibble with columns tag and n.
#' @param tag_emb Named list of embeddings keyed by tag.
#' @param min_sim Minimum cosine similarity threshold.
#' @param min_freq Minimum frequency for tags to be considered.
#' @param auto_sim Threshold used to label candidates as very_high confidence.
#'
#' @return Tibble of candidate pairs for review/grouping.
#' @export
find_synonym_candidates_embeddings <- function(tag_registry,
                                               tag_emb,
                                               min_sim = 0.88,
                                               auto_sim = 0.94,
                                               min_freq = 1) {
  stopifnot(all(c("tag", "n") %in% names(tag_registry)))

  reg <- tag_registry %>%
    dplyr::filter(!is.na(tag), nzchar(tag), n >= min_freq) %>%
    dplyr::distinct(tag, n) %>%
    dplyr::filter(tag %in% names(tag_emb))

  if (nrow(reg) < 2) {
    return(tibble::tibble(
      tag_a = character(),
      n_a = integer(),
      tag_b = character(),
      n_b = integer(),
      sim = numeric(),
      confidence_band = character()
    ))
  }

  tidyr::crossing(
    reg %>% dplyr::rename(tag_a = tag, n_a = n),
    reg %>% dplyr::rename(tag_b = tag, n_b = n)
  ) %>%
    dplyr::filter(tag_a < tag_b) %>%
    dplyr::mutate(
      sim = purrr::map2_dbl(tag_a, tag_b, ~ cosine_sim(tag_emb[[.x]], tag_emb[[.y]]))
    ) %>%
    dplyr::filter(!is.na(sim), sim >= min_sim) %>%
    dplyr::mutate(
      confidence_band = dplyr::if_else(sim >= auto_sim, "very_high", "review")
    ) %>%
    dplyr::arrange(dplyr::desc(sim), dplyr::desc(pmax(n_a, n_b)))
}

# Optional alias
#' @export
find_synonym_candidates <- find_synonym_candidates_embeddings

# -------------------------------------------------------------------
# 5) Group helpers
# -------------------------------------------------------------------

#' Build candidate synonym groups from pairwise edges
#'
#' @description
#' Forms provisional groups from pairwise similarity candidates using connected
#' components. These are only review candidates; the human review step decides
#' whether the group is valid and what the canonical tag should be.
#'
#' @param cands Tibble with columns tag_a, tag_b, sim.
#' @param tag_registry Tibble with columns tag and n.
#'
#' @return Tibble with one row per tag per provisional group.
#' @export
build_synonym_group_candidates <- function(cands, tag_registry) {
  stopifnot(all(c("tag_a", "tag_b", "sim") %in% names(cands)))
  stopifnot(all(c("tag", "n") %in% names(tag_registry)))

  if (nrow(cands) == 0) {
    return(tibble::tibble(
      group_id = integer(),
      tag = character(),
      n = integer(),
      approved_group = logical(),
      canonical_tag = character()
    ))
  }

  # Build adjacency list
  nodes <- sort(unique(c(cands$tag_a, cands$tag_b)))
  adj <- stats::setNames(vector("list", length(nodes)), nodes)

  for (i in seq_len(nrow(cands))) {
    a <- cands$tag_a[[i]]
    b <- cands$tag_b[[i]]
    adj[[a]] <- unique(c(adj[[a]], b))
    adj[[b]] <- unique(c(adj[[b]], a))
  }

  # Connected components by DFS
  visited <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  groups <- vector("list", 0)

  for (node in nodes) {
    if (visited[[node]]) next

    stack <- c(node)
    comp <- character()

    while (length(stack) > 0) {
      cur <- stack[[length(stack)]]
      stack <- stack[-length(stack)]

      if (visited[[cur]]) next
      visited[[cur]] <- TRUE
      comp <- c(comp, cur)

      nbrs <- adj[[cur]]
      nbrs <- nbrs[!visited[nbrs]]
      stack <- c(stack, nbrs)
    }

    groups[[length(groups) + 1]] <- sort(unique(comp))
  }

  group_tbl <- purrr::imap_dfr(groups, function(tags, idx) {
    tibble::tibble(group_id = idx, tag = tags)
  })

  out <- group_tbl %>%
    dplyr::left_join(tag_registry, by = "tag") %>%
    dplyr::mutate(
      approved_group = NA,
      canonical_tag = NA_character_
    ) %>%
    dplyr::arrange(group_id, dplyr::desc(n), tag)

  out
}

#' Add group-level summary statistics to synonym groups
#'
#' @param group_candidates Output of build_synonym_group_candidates().
#' @param cands Pairwise candidate tibble with tag_a, tag_b, sim.
#'
#' @return Tibble with group summary columns added to each row.
#' @export
summarise_synonym_groups <- function(group_candidates, cands) {
  stopifnot(all(c("group_id", "tag") %in% names(group_candidates)))
  stopifnot(all(c("tag_a", "tag_b", "sim") %in% names(cands)))

  if (nrow(group_candidates) == 0) return(group_candidates)

  pair_stats <- purrr::map_dfr(unique(group_candidates$group_id), function(gid) {
    tags <- group_candidates %>%
      dplyr::filter(group_id == gid) %>%
      dplyr::pull(tag)

    if (length(tags) <= 1) {
      return(tibble::tibble(
        group_id = gid,
        group_size = length(tags),
        group_min_sim = NA_real_,
        group_mean_sim = NA_real_,
        group_max_sim = NA_real_
      ))
    }

    sims <- cands %>%
      dplyr::filter(tag_a %in% tags, tag_b %in% tags) %>%
      dplyr::pull(sim)

    tibble::tibble(
      group_id = gid,
      group_size = length(tags),
      group_min_sim = if (length(sims) == 0) NA_real_ else min(sims),
      group_mean_sim = if (length(sims) == 0) NA_real_ else mean(sims),
      group_max_sim = if (length(sims) == 0) NA_real_ else max(sims)
    )
  })

  group_candidates %>%
    dplyr::left_join(pair_stats, by = "group_id") %>%
    dplyr::group_by(group_id) %>%
    dplyr::mutate(
      suggested_canonical = tag[which.max(n)]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(group_id, dplyr::desc(n), tag)
}

#' Build final tag map from reviewed synonym groups
#'
#' @description
#' Converts a reviewed group table into a named tag replacement map.
#' Only rows where approved_group is TRUE are used.
#' Every non-canonical member of an approved group maps directly to canonical_tag.
#'
#' @param reviewed_groups Tibble containing at least:
#'   group_id, tag, approved_group, canonical_tag
#'
#' @return Named character vector where names are source tags and values are replacements.
#' @export
build_tag_map_from_group_review <- function(reviewed_groups) {
  stopifnot(all(c("group_id", "tag", "approved_group", "canonical_tag") %in% names(reviewed_groups)))

  approved <- reviewed_groups %>%
    dplyr::mutate(
      tag = canon_tag(tag),
      canonical_tag = canon_tag(canonical_tag)
    ) %>%
    dplyr::filter(!is.na(approved_group), approved_group) %>%
    dplyr::filter(!is.na(tag), nzchar(tag)) %>%
    dplyr::filter(!is.na(canonical_tag), nzchar(canonical_tag))

  if (nrow(approved) == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  # Validate each approved group has exactly one canonical tag value
  group_check <- approved %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(
      n_canonical = dplyr::n_distinct(canonical_tag),
      .groups = "drop"
    )

  bad <- group_check %>% dplyr::filter(n_canonical != 1)
  if (nrow(bad) > 0) {
    stop("Each approved group must have exactly one canonical_tag value.", call. = FALSE)
  }

  # Build direct map from each non-canonical tag to canonical tag
  mapping <- approved %>%
    dplyr::group_by(group_id) %>%
    dplyr::mutate(group_canonical = canonical_tag[[1]]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tag != group_canonical) %>%
    dplyr::transmute(from = tag, to = group_canonical) %>%
    dplyr::distinct(from, .keep_all = TRUE)

  stats::setNames(mapping$to, mapping$from)
}

# -------------------------------------------------------------------
# 6) Apply synonym map to rows / full matrix
# -------------------------------------------------------------------

#' Apply tag replacement map to a tag vector and rerun duplicate removal
#'
#' @param tags Character vector of tags.
#' @param tag_map Named character vector where names are source tags and values are replacements.
#' @param dedup_after Logical; rerun remove_duplicates after replacement.
#'
#' @return Character vector of same length, padded with NAs if needed.
#' @export
apply_tag_map_to_tags <- function(tags, tag_map, dedup_after = TRUE) {
  out <- canon_tag(tags)

  if (length(tag_map) > 0) {
    stopifnot(is.character(tag_map), !is.null(names(tag_map)))
    names(tag_map) <- canon_tag(names(tag_map))
    unname_vals <- canon_tag(unname(tag_map))
    tag_map <- stats::setNames(unname_vals, names(tag_map))

    idx <- !is.na(out) & out %in% names(tag_map)
    out[idx] <- unname(tag_map[out[idx]])
  }

  out <- stringr::str_trim(out)
  out[out == ""] <- NA_character_

  if (!dedup_after) return(out)

  non_na <- out[!is.na(out)]
  dedup <- remove_duplicates(non_na, consecutive_first = TRUE)

  k <- length(out)
  c(dedup, rep(NA_character_, max(0, k - length(dedup))))[1:k]
}

#' Apply synonym collapse to a single cleaned tag row
#'
#' @param tags Cleaned tag vector.
#' @param tag_cols Tag column names in order.
#' @param tag_map Named character vector of approved replacements.
#'
#' @return List with updated vector and audit fields.
#' @export
collapse_synonyms_row <- function(tags, tag_cols, tag_map) {
  before <- canon_tag(tags)

  before_non_na <- before[!is.na(before)]
  before_non_na <- stringr::str_trim(before_non_na)
  before_non_na <- before_non_na[before_non_na != ""]

  after_vec <- apply_tag_map_to_tags(before, tag_map = tag_map, dedup_after = TRUE)

  # make sure unnest_wider() gets named elements
  names(after_vec) <- tag_cols

  list(
    vec = after_vec,
    before_path = paste(before_non_na, collapse = " > "),
    after_path  = paste(stats::na.omit(after_vec), collapse = " > "),
    n_before = length(before_non_na),
    n_after  = sum(!is.na(after_vec)),
    n_removed = length(before_non_na) - sum(!is.na(after_vec)),
    changed = !identical(before_non_na, stats::na.omit(after_vec))
  )
}

#' Apply human-approved synonym collapse to a cleaned tag matrix
#'
#' @param tag_matrix_clean Cleaned tag matrix.
#' @param tag_map Named character vector or data.frame/tibble with from and to columns.
#' @param tag_cols Optional tag columns.
#'
#' @return List with updated cleaned matrix and audit tibble.
#' @export
apply_synonym_collapse <- function(tag_matrix_clean, tag_map, tag_cols = NULL) {
  if (is.null(tag_cols)) {
    tag_cols <- grep("^tag_level_", names(tag_matrix_clean), value = TRUE)
  }
  stopifnot(length(tag_cols) > 0)

  if (is.data.frame(tag_map)) {
    stopifnot(all(c("from", "to") %in% names(tag_map)))
    tag_map <- stats::setNames(as.character(tag_map$to), as.character(tag_map$from))
  } else {
    stopifnot(is.character(tag_map), !is.null(names(tag_map)))
  }

  updated <- tag_matrix_clean %>%
    dplyr::mutate(
      .syn = purrr::pmap(
        dplyr::select(., dplyr::all_of(tag_cols)),
        ~ collapse_synonyms_row(c(...), tag_cols = tag_cols, tag_map = tag_map)
      )
    ) %>%
    dplyr::mutate(
      syn_n_before    = purrr::map_int(.syn, "n_before"),
      syn_n_after     = purrr::map_int(.syn, "n_after"),
      syn_n_removed   = purrr::map_int(.syn, "n_removed"),
      syn_before_path = purrr::map_chr(.syn, "before_path"),
      syn_after_path  = purrr::map_chr(.syn, "after_path"),
      syn_changed     = purrr::map_lgl(.syn, "changed"),
      .vec            = purrr::map(.syn, "vec")
    ) %>%
    dplyr::select(-.syn) %>%
    dplyr::select(-dplyr::all_of(tag_cols)) %>%
    tidyr::unnest_wider(.vec) %>%
    dplyr::relocate(dplyr::all_of(tag_cols), .after = caption)

  audit <- updated %>%
    dplyr::select(
      id, caption,
      syn_n_before, syn_n_after, syn_n_removed, syn_changed,
      syn_before_path, syn_after_path
    )

  list(tag_matrix_clean = updated, audit = audit)
}

