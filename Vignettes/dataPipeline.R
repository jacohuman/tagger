
# IMPORT SURVEY DATA
# (the path to the data & the survey type should be specified)

# For eg. in this esample the raw survey files are stored in:
#               "~/Desktop/Nova_werk/Des_Nov_2025/Survey_Data"
# and the Mobenzi data format is used.

library(GraphDB)

if (Sys.info()[["user"]] != "christiaanpauw") {
  # For testing purposes, we use a local path
  svypath <- "~/Desktop/Nova_werk/Des_Nov_2025/Survey_data"
} else {
  # For production, we use the shared path
  svypath <- "/Users/christiaanpauw/Nova SA Dropbox/Christiaan Pauw/AIAO_Students/Mobenzi_data"
}

surveys <- GraphDB:::loadSurveys(svypath)

# SET BASE URI
# This uri will be used as the prefix when storing any and all of the extracted
# survey data. It is specifically used in the `@id` fields of each extracted
# entity (JSON object)

BASE_URI <- "http://nova.org.za/data/"

for (ds in names(surveys)) {
  STUDY_ID <- normalize_survey_name(ds)
  print("Extracting:")
  print(STUDY_ID)
  extract_dataset(df = surveys[[ds]], base_uri = BASE_URI, study_id = STUDY_ID)
}

