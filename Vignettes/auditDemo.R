audit_final <- state$tag_matrix_clean %>%
  dplyr::select(id, caption) %>%
  dplyr::mutate(
    after_path = purrr::pmap_chr(
      dplyr::select(state$tag_matrix_clean, dplyr::matches("^tag_level_")),
      ~ paste(stats::na.omit(c(...)), collapse = " > ")
    )
  )

res_order <- reorder_tag_matrix_by_support(state$tag_matrix_clean)
state$tag_matrix_clean <- res_order$tag_matrix_clean
