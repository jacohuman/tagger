# Linked data publication

# ---- Config ----
gh_pages_path <- "~/Desktop/Nova_werk/Des_Nov_2025/surveyData/data"

# ---- Ensure GitHub Pages base directory exists ----
dir.create(gh_pages_path, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ----

# Get last path segment (for filename)

# Extract 5th path segment (e.g. "q", "proc", etc.)

# HTML generator with basic styling

# Save .jsonld and .html files to correct subdirectory in GitHub Pages repo

# ---- Loop over all entities and write ----
walk(all_graph_entities, write_entity_files)
