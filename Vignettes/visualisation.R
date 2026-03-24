library(visNetwork)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(scales)

plot_tag_hierarchy_vis <- function(tag_matrix,
                                   root_name = "ROOT",
                                   tag_prefix = "tag_level_") {

  tag_cols <- grep(paste0("^", tag_prefix, "\\d+$"), names(tag_matrix), value = TRUE)
  tag_cols <- tag_cols[order(as.integer(sub(tag_prefix, "", tag_cols)))]

  stopifnot(length(tag_cols) > 0)

  # Long format: one row per question per level-tag
  long <- tag_matrix %>%
    select(id, all_of(tag_cols)) %>%
    pivot_longer(
      cols = all_of(tag_cols),
      names_to = "level_col",
      values_to = "tag"
    ) %>%
    mutate(
      level = as.integer(sub(tag_prefix, "", level_col)),
      tag = as.character(tag),
      tag = str_trim(tag),
      tag = ifelse(is.na(tag) | tag == "", NA_character_, tag)
    ) %>%
    filter(!is.na(tag)) %>%
    distinct(id, level, tag)

  if (nrow(long) == 0) {
    stop("No non-empty cleaned tags found to visualize.", call. = FALSE)
  }

  # Node ids are level-specific
  long <- long %>%
    mutate(node_id = paste0("L", level, ":", tag))

  # Node support = number of questions in which that level-tag appears
  node_stats <- long %>%
    count(node_id, level, tag, name = "n_questions") %>%
    mutate(
      label = tag,
      title = paste0(
        "<b>", tag, "</b><br>",
        "Level: ", level, "<br>",
        "Questions: ", comma(n_questions)
      ),
      value = n_questions,
      shape = "ellipse"
    )

  # Edges within each question path
  edges <- long %>%
    arrange(id, level) %>%
    group_by(id) %>%
    mutate(
      from = node_id,
      to   = lead(node_id)
    ) %>%
    ungroup() %>%
    filter(!is.na(to), from != to) %>%
    count(from, to, name = "weight")

  # Root -> top level
  top_level <- min(long$level, na.rm = TRUE)

  top_edges <- long %>%
    filter(level == top_level) %>%
    distinct(node_id) %>%
    transmute(from = root_name, to = node_id, weight = 1L)

  edges <- bind_rows(top_edges, edges) %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight), .groups = "drop")

  # Root node
  root_node <- tibble(
    node_id = root_name,
    level = 0L,
    tag = root_name,
    n_questions = n_distinct(long$id),
    label = root_name,
    title = paste0("<b>", root_name, "</b><br>Total questions: ", comma(n_distinct(long$id))),
    value = max(node_stats$n_questions, na.rm = TRUE),
    shape = "box"
  )

  nodes <- bind_rows(
    root_node %>%
      rename(id = node_id),
    node_stats %>%
      rename(id = node_id)
  ) %>%
    mutate(
      font = ifelse(id == root_name, list(size = 20), list(size = 16))
    )

  edges_vis <- edges %>%
    mutate(
      arrows = "to",
      width = rescale(weight, to = c(1, 10)),
      title = paste0("Questions following this transition: ", comma(weight))
    )

  visNetwork(nodes, edges_vis, height = "800px", width = "100%") %>%
    visHierarchicalLayout(
      direction = "UD",
      sortMethod = "directed",
      levelSeparation = 180,
      nodeSpacing = 180,
      treeSpacing = 220
    ) %>%
    visEdges(smooth = FALSE) %>%
    visNodes(scaling = list(min = 10, max = 40)) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visInteraction(
      navigationButtons = TRUE,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE
    ) %>%
    visPhysics(enabled = FALSE)
}

plot_tag_hierarchy_vis(state$tag_matrix_clean)
