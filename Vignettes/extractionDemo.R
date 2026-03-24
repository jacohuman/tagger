library(GraphDB)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(novaRush)
library(httr)

# Using example URI's for now. (change later)
BASE_URI <- "http://example.org/data/"

df <- import_csv_files("/Users/jacohuman/Desktop/Nova_werk/Des_Nov_2025/Survey_data/1st_Stove_Build_Visit")

context <- set_survey_context()

extract_dataset = function(df, base_uri, study_id, context) {
  BASE_URI <- base_uri
  STUDY_ID <- study_id
  DATASET_ID <- study_id

  main_df <- df[["Main"]]
  questions_df <- df[["Questions"]]
  codebook_df <- df[["CodeBook"]]


  # Extract survey level entities.
  survey_level <- extract_survey_level_entities(
    questions = questions_df,
    main = main_df,
    base_uri = BASE_URI,
    study_id = STUDY_ID,
    dataset_id = DATASET_ID
  )

  procedure_uri <- survey_level$procedure$`@id`
  dataset_uri <- survey_level$dataset$`@id`

  # Extract participants.  (agents)
  participant_entities <- extract_participant_entities(main = main_df, base_uri = BASE_URI)

  # Extract questions.
  tag_set <- list2env(list())
  question_entities <- extract_question_entities(questions = questions_df, base_uri = BASE_URI, procedure_uri = procedure_uri, question_classifier, tag_set = tag_set)

  # Write the question entities to a file
  write(
    jsonlite::toJSON(question_entities, pretty = TRUE, auto_unbox = TRUE),
    file = paste0("mcp_test2", "questions.jsonld")
  )

  # Extract answers (options).
  closed_answer_entities <- extract_closed_answer_entities(codebook = codebook_df, base_uri = BASE_URI, procedure_uri = procedure_uri)

  # Extract submission (sur:CompletedQuestion) entities.
  submission_entities <- extract_submission_entities(
    main = main_df,
    questions_df = questions_df,
    codebook_df = codebook_df,
    base_uri = BASE_URI,
    procedure_uri = procedure_uri,
    dataset_uri = dataset_uri
  )

  # Combine all into @graph
  all_graph_entities <- c(
    list(survey_level$survey_ro),
    list(survey_level$procedure),
    list(survey_level$dataset),
    participant_entities,
    question_entities,
    closed_answer_entities,
    submission_entities
  )

  print("Starting data publication")

  # Save JSON-LD ontology file
  final_jsonld_list <- list(
    `@context` = context,
    `@graph` = all_graph_entities
  )

  unique_tags <- sort(ls(tag_set))
  cat("Unique tags used in this survey:\n")
  print(unique_tags)

  print("Saving tags")
  # Save tags for future reuse
  saveRDS(as.list(tag_set), file = paste0("Test2", "_tags.rds"))
  write.csv(data.frame(tag = sort(ls(tag_set))), file = paste0("Test2", "_tags.csv"), row.names = FALSE)

  write(
    jsonlite::toJSON(final_jsonld_list, pretty = TRUE, auto_unbox = TRUE),
    file = paste0("Test2", "_ontology_output.jsonld")
  )

  # print("Constructing transaction")
  # conf <- setConfig(host = datadudes2.xyz, ledger = "integrationDemo", signMessages = FALSE)
  # conf <- setContext(currentConfig = conf, context = context)
  # cat(sprintf("Sending %s data to Fluree", STUDY_ID))
  # createLedger(config = conf, transaction = NULL)
  # Transact(final_json_output)


  # Save to graph to a file.
  #write(final_json_output, paste0("Test_set", "_output.jsonld"))

  #print("The file was successfully transacted and saved")

  #print("Starting data publication")
  #walk(all_graph_entities, write_entity_files)
}

extract_dataset(df, BASE_URI, BASE_URI, context)

