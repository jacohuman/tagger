library(httr2)
library(stringr)
library(tibble)
library(purrr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rlang)

# Group child tags under parent tag
build_parent_vocab_for_level <- function(question_tag_matrix, level) {
  stopifnot(level >= 1, level <= 7)  # parent exists up to level 7

  child_col  <- paste0("tag_level_", level)
  parent_col <- paste0("tag_level_", level + 1)

  question_tag_matrix |>
    count(
      parent_tag = .data[[parent_col]],
      tag        = .data[[child_col]],
      name       = "n"
    ) |>
    mutate(level_num = level) |>
    relocate(level_num, parent_tag, tag, n)
}

# Build the parent vocab across multiple levels
build_parent_vocab <- function(question_tag_matrix,
                               levels = 1:6) {
  purrr::map_dfr(levels, ~ build_parent_vocab_for_level(question_tag_matrix, .x))
}

llm_clean_parent_group <- function(vocab_group,
                                   model       = Sys.getenv("TAGGER_MODEL", "llama3.1:8b"),
                                   base_url    = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
                                   num_predict = 512,
                                   timeout_sec = 300) {
  level_num  <- unique(vocab_group$level_num)
  parent_tag <- unique(vocab_group$parent_tag)
  stopifnot(length(level_num) == 1, length(parent_tag) == 1)

  tag_lines <- paste0(
    "- ", vocab_group$tag, " (", vocab_group$n, ")",
    collapse = "\n"
  )

  prompt <- paste0(
    "You are cleaning an ontology vocabulary for survey questions. The ultimate goal is to extract an ontology from this tag hierarchy.\n",
    "All tags below are child tags that belong under the SAME parent tag.\n\n",
    "Hierarchy context:\n",
    "- level: ", level_num, "\n",
    "- parent tag: ", parent_tag, "\n\n",
    "Your tasks:\n",
    "1. Group tags that are minor spelling variants or obvious synonyms.\n",
    "2. For each group, choose ONE canonical child tag.\n",
    "3. Canonical name rules: 1–2 English words, lowercase, ASCII letters or underscores only.\n",
    "4. Without inventing new concepts; normalise/merge similar ones under a ontology level tag (i.e. if the child tags are too niche), while keeping it less general than the parent tag.\n",
    "5. If child tags cannot be semantically grouped, don't force groupings.\n\n",
    "IMPORTANT OUTPUT RULES (must follow):\n",
    "- Output ONLY valid JSON.\n",
    "- Do NOT write any explanation or markdown.\n",
    "- Do NOT use code fences.\n",
    "- Your first character MUST be '{' and your last character MUST be '}'.\n\n",
    "Return JSON exactly in this form:\n",
    "{ \"groups\": [\n",
    "  { \"canonical\": \"tag_name\", \"members\": [\"old1\", \"old2\"] },\n",
    "  { \"canonical\": \"another_tag\", \"members\": [\"old3\"] }\n",
    "]}\n\n",
    "Child tags under parent tag '", parent_tag, "' with frequencies:\n",
    tag_lines, "\n"
  )

  message("Cleaning parent group: level ", level_num,
          ", parent = '", parent_tag, "', ",
          nrow(vocab_group), " tags")
  print(prompt)

  resp <- request(paste0(base_url, "/api/generate")) |>
    req_timeout(timeout_sec) |>
    req_body_json(list(
      model       = model,
      prompt      = prompt,
      stream      = FALSE,
      format      = "json",
      num_predict = num_predict
    )) |>
    req_perform()

  body <- resp_body_string(resp)
  cat("Raw response body:\n", body, "\n\n")  # keep for debugging if needed

  out <- fromJSON(body, simplifyVector = FALSE)
  raw <- out$response %||% ""

  # Try to extract JSON block in case the model wraps it in text
  json_candidate <- str_extract(raw, "\\{[\\s\\S]*\\}")
  if (is.na(json_candidate)) {
    warning("No JSON found in response for level ", level_num,
            ", parent ", parent_tag, "; falling back to identity mapping.")
    return(vocab_group |>
             transmute(
               level_num     = level_num,
               old_tag       = tag,
               canonical_tag = tag
             ))
  }

  inner <- tryCatch(
    fromJSON(json_candidate, simplifyVector = FALSE),
    error = function(e) {
      warning("Failed to parse JSON for level ", level_num,
              ", parent ", parent_tag, ": ", conditionMessage(e),
              "; falling back to identity mapping.")
      NULL
    }
  )

  if (is.null(inner) || is.null(inner$groups)) {
    warning("Inner JSON has no 'groups' field for level ", level_num,
            ", parent ", parent_tag, "; falling back to identity mapping.")
    return(vocab_group |>
             transmute(
               level_num     = level_num,
               old_tag       = tag,
               canonical_tag = tag
             ))
  }

  groups <- inner$groups

  # If it's a single object, wrap in list
  if (!is.list(groups) || (!is.null(groups$canonical) && !is.null(groups$members))) {
    groups <- list(groups)
  }

  mapping <- map_dfr(groups, function(g) {
    tibble(
      level_num     = level_num,
      old_tag       = unlist(g$members, use.names = FALSE),
      canonical_tag = g$canonical
    )
  })

  mapping
}


build_tag_mapping_with_llm_parent <- function(parent_vocab,
                                              model       = Sys.getenv("TAGGER_MODEL", "llama3.1:8b"),
                                              num_predict = 512,
                                              timeout_sec = 300) {
  parent_vocab |>
    group_by(level_num, parent_tag) |>
    group_split() |>
    map_dfr(function(vg) {
      llm_clean_parent_group(
        vg,
        model       = model,
        num_predict = num_predict,
        timeout_sec = timeout_sec
      )
    }) |>
    ungroup() |>
    distinct(level_num, old_tag, canonical_tag)  # dedupe in case overlaps
}

# 1. Build parent vocab for the specified levels and drop all trivial groups
parent_vocab <- build_parent_vocab(question_tag_matrix, levels = 1:6)

parent_vocab_nontrivial <- parent_vocab |>
  group_by(level_num, parent_tag) |>
  filter(n() > 1) |>
  ungroup()

# 2. Build mapping per parent group
mapping <- build_tag_mapping_with_llm_parent(
  parent_vocab_nontrivial,
  model = "llama3.1:8b"
)

mapping
