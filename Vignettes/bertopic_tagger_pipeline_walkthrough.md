# BERTopic Tagger Pipeline Walkthrough

This walkthrough explains the execution flow of `run_bertopic_tagger_pipeline()` and lists each major function in call order, including the R source file where each function is defined.

## Entry point

1. `run_bertopic_tagger_pipeline(...)`  
   **File:** `R/bertopic_tagger_pipeline.R`  
   This is the orchestration function. It handles question extraction, BERTopic fitting/loading, hierarchical tagging, tag cleanup, synonym review/collapse, and final matrix ordering.

## Stage 1: Prepare question inputs

2. `unnest_questions(forms, limit_n = ...)`  
   **File:** `R/questions.R`  
   Flattens nested survey forms into a question-level tibble.

## Stage 2: Resume from progress or build fresh BERTopic state

### Resume branch (if `progress_path` exists)

3. `load_tag_state(progress_path)`  
   **File:** `R/stateHandler.R`  
   Loads a previously saved tagging state and validates class (`tag_state`).

### Fresh branch (if no saved state)

3. `embed_multiple_questions(texts = questions$caption, ...)`  
   **File:** `R/embedding.R`  
   Generates dense embeddings for all question captions.

4. `fit_bertopic_topics(texts = questions$caption, embeddings = embedding_matrix, bertopic_kwargs = ...)`  
   **File:** `R/bertopic_tagger_pipeline.R`  
   Fits BERTopic and returns per-question topic IDs.

   - `ensure_bertopic_python()`  
     **File:** `R/bert.R`  
     Ensures reticulate has the required Python modules.

5. `build_bertopic_hierarchy(questions, question_embeddings, topic_levels = ...)`  
   **File:** `R/bertopic_tagger_pipeline.R`  
   Converts BERTopic leaf topics into hierarchical cluster assignments.

   Internally this uses:

   - `infer_topic_levels(n_leaf)` *(if `topic_levels` is `NULL`)*  
     **File:** `R/bertopic_tagger_pipeline.R`
   - `build_cluster_index(assignments, clusters_by_level = topic_levels)`  
     **File:** `R/clustering.R`

6. `save_tag_state(state, progress_path)` *(optional, when `progress_path` is supplied)*  
   **File:** `R/stateHandler.R`

## Stage 3: Bottom-up cluster tagging

7. `tag_clusters_bottom_up(state, sample_size, progress_path, model, base_url)`  
   **File:** `R/tagging.R`  
   Calls the LLM-assisted tagger over hierarchical clusters and writes tag labels into the state.

## Stage 4: Build and clean question-tag matrix

8. `build_question_tag_matrix(state$assignments, state$clusters)`  
   **File:** `R/tagging.R`

9. `clean_tag_matrix(tag_matrix)`  
   **File:** `R/cleaning.R`

## Stage 5: Build tag registry and synonym candidates

10. `build_tag_registry_from_clean(clean_res$tag_matrix_clean)`  
    **File:** `R/cleaning.R`

11. `embed_tag_registry(tag_registry, ...)`  
    **File:** `R/embedding.R`

12. `find_synonym_candidates_embeddings(tag_registry, tag_emb, min_sim, auto_sim)`  
    **File:** `R/cleaning.R`

13. `build_synonym_group_candidates(cands = synonym_pairs, tag_registry = tag_registry)`  
    **File:** `R/cleaning.R`

14. `summarise_synonym_groups(group_candidates = synonym_groups, cands = synonym_pairs)`  
    **File:** `R/cleaning.R`

## Stage 6: Optional synonym collapse from reviewed groups

15. `build_tag_map_from_group_review(reviewed_groups)` *(only if `reviewed_groups` is not `NULL`)*  
    **File:** `R/cleaning.R`

16. `apply_synonym_collapse(tag_matrix_clean, tag_map)` *(only if `reviewed_groups` is not `NULL`)*  
    **File:** `R/cleaning.R`

## Stage 7: Final ordering and return object

17. `reorder_tag_matrix_by_support(collapsed)`  
    **File:** `R/audit.R`  
    Reorders tags by support/frequency.

18. Returns a named list containing:
    - BERTopic fit artifacts (`bertopic`)
    - final and intermediate state artifacts (`state`, `clean`, `synonym_*`, `reorder`)
    - final matrix (`tag_matrix_final`)

---

## Quick reference: function/file map

- `R/bertopic_tagger_pipeline.R`: `run_bertopic_tagger_pipeline`, `fit_bertopic_topics`, `build_bertopic_hierarchy`, `infer_topic_levels`
- `R/bert.R`: `ensure_bertopic_python`
- `R/questions.R`: `unnest_questions`
- `R/embedding.R`: `embed_multiple_questions`, `embed_tag_registry`
- `R/clustering.R`: `build_cluster_index`
- `R/stateHandler.R`: `load_tag_state`, `save_tag_state`
- `R/tagging.R`: `tag_clusters_bottom_up`, `build_question_tag_matrix`
- `R/audit.R`: `reorder_tag_matrix_by_support`
- `R/cleaning.R`: matrix cleanup + synonym candidate/grouping/collapse helpers
