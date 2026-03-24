library(reclin2)
library(dplyr)
library(tibble)

# Example data: two sets of survey questions
df1 <- tibble(
  id = c("q1", "q2"),
  label = c("gps_location", "age"),
  text  = c("Please record the GPS location", "Enter the age of the respondent")
)

df2 <- tibble(
  id = c("q3", "q4"),
  label = c("gps_coordinates", "respondent_age"),
  text  = c("Capture GPS coordinates of the household", "How old is the respondent?")
)


df1 <- tibble(
  id = "q1",
  label = "gps_location",
  text  = "Please record the GPS location of the household"
)

df2 <- tibble(
  id = "q2",
  label = "gps_location",
  text  = "Record the GPS location of the household"
)


# Add dummy blocking column to force all-vs-all comparisons
df1$block <- 1
df2$block <- 1

# 1. Generate all pairs using blocking on dummy
pairs <- pair_blocking(df1, df2, on = "block")

# 2. Compare pairs using `compare_pairs()` and `jaro_winkler()`
pairs <- compare_pairs(
  pairs,
  on = c("label", "text"),
  comparators = list(
    label = cmp_jarowinkler(0.9),
    text  = cmp_jarowinkler(0.9)
  ),
  inplace = TRUE
)

pairs <- pairs %>%
  mutate(score = (label + text) / 2) %>%
  arrange(desc(score))

pairs %>% select(.x, .y, label, text, score)


pairs <- pairs %>%
  mutate(match_type = case_when(
    score > 0.95 ~ "skos:exactMatch",
    score > 0.85 ~ "skos:closeMatch",
    TRUE ~ NA_character_
  ))
