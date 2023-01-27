# Objective: define prevalent cases based on additional information reported at baseline.
install.packages("testthat",repos = "http://cran.us.r-project.org")
library(data.table)
library(testthat)
source("functions.R")

# Reading arguments
args <- commandArgs(trailingOnly = TRUE)
def_path <- as.character(args[1])
app_data_path <- as.character(args[2])

# Loading registries-based definitions
mydata <- readRDS("../Outputs/output_registries.rds")

# Summary of registries-based definitions
print("Number of cases identified in HES/death registries:")
print(sum(mydata$case, na.rm = TRUE))
print("Number of prevalent cases:")
print(sum(mydata$prevalent_case, na.rm = TRUE))
print("Number of incident cases:")
print(sum(mydata$incident_case, na.rm = TRUE))

# Identifying additional codes
mylist_codes <- list.files(path = def_path, pattern = "codes_")

# Running script only if additional codes to use
if (length(mylist_codes) > 0) {
  # Reading codes
  field_ids <- NULL
  for (i in 1:length(mylist_codes)) {
    tmp <- fread(paste0(def_path, "/", mylist_codes[i]), data.table = FALSE)
    tmp <- tmp[, 1]
    field_ids <- c(field_ids, gsub("\\.txt", "", gsub("codes_", "", mylist_codes[i])))
    print(paste0("Reading ", length(tmp), " codes for field ID ", field_ids[length(field_ids)]))
    assign(paste0("f", field_ids[length(field_ids)], "_codes"), tmp)
  }

  # Re-writing ICD codes (allowing to write e.g. 1-3 to say 1,2,3)
  for (i in 1:length(field_ids)) {
    type <- paste0("f", field_ids[i])
    input_codes <- eval(parse(text = paste0(type, "_codes")))
    full_list <- ExpandCodes(input_codes = input_codes)
    assign(paste0("full_", type), full_list)
  }

  # Definition of UK Biobank columns to use
  field_columns <- paste0(field_ids, "-0") # BASELINE ONLY FOR PREVALENT CASES (NB: OTHER INSTANCES MAY ONLY BE FOR A SUBSET OF PARTICIPANTS ANYWAY?)

  # Loading application data (relevant fields and eid only)
  print("Loading main UK Biobank dataset from:")
  print(app_data_path)
  column_names <- colnames(fread(app_data_path, data.table = FALSE, nrows = 0))
  field_column_ids <- grep("eid", column_names)
  for (i in 1:length(field_ids)) {
    field_column_ids <- c(field_column_ids, grep(field_columns[i], column_names))
  }
  ukb <- fread(app_data_path, data.table = FALSE, select = field_column_ids)
  rownames(ukb) <- ukb$eid
  ukb <- ukb[, -which(colnames(ukb) == "eid")]

  # Identifying prevalent cases based on additional criteria
  for (i in 1:ncol(ukb)) {
    tmp_field <- gsub("-.*", "", colnames(ukb)[i])
    print(paste0("Extracting based on field ", tmp_field, ", array ", gsub(".*//.", "", colnames(ukb)[i])))
    full_list <- eval(parse(text = paste0("f", tmp_field, "_codes")))
    pb <- txtProgressBar(style = 3)
    for (k in 1:length(full_list)) {
      # Extracting participant eids
      tmp_eid <- unique(rownames(ukb)[grepl(paste0("^", full_list[k]), ukb[, i])])

      # Defining corresponding participants as prevalent and not incident
      mydata[tmp_eid, "prevalent_case"] <- 1
      mydata[tmp_eid, "incident_case"] <- NA
      setTxtProgressBar(pb, k / length(full_list))
    }
    cat("\n")
  }

  # Updating the overall case/control status
  mydata$case[which(mydata$incident_case == 1)] <- 1
  mydata$case[which(mydata$prevalent_case == 1)] <- 1

  # Removing the time to diagnosis for prevalent cases
  mydata$time_to_diagnosis[which(mydata$prevalent_case == 1)] <- NA

  # Summary of final definitions
  print("Number of cases:")
  print(sum(mydata$case, na.rm = TRUE))
  print("Number of prevalent cases:")
  print(sum(mydata$prevalent_case, na.rm = TRUE))
  print("Number of incident cases:")
  print(sum(mydata$incident_case, na.rm = TRUE))
}

# Quality checks
test_that("None of the participants is both incident and prevalent", {
  expect_equal(sum(mydata$prevalent_case[which(mydata$incident_case == 1)], na.rm = TRUE), 0)
  expect_equal(sum(mydata$incident_case[which(mydata$prevalent_case == 1)], na.rm = TRUE), 0)
})
test_that("All incident cases have a date of/time to diagnosis", {
  expect_equal(sum(mydata$incident_case[is.na(as.numeric(mydata$date_diagnosis))], na.rm = TRUE), 0)
  expect_equal(sum(mydata$incident_case[is.infinite(as.numeric(mydata$date_diagnosis))], na.rm = TRUE), 0)
  expect_equal(sum(is.na(mydata$date_diagnosis[which(mydata$incident_case == 1)])), 0)
  expect_equal(sum(is.infinite(mydata$date_diagnosis[which(mydata$incident_case == 1)])), 0)
  expect_equal(sum(is.na(mydata$time_to_diagnosis[which(mydata$incident_case == 1)])), 0)
  expect_equal(sum(is.infinite(mydata$time_to_diagnosis[which(mydata$incident_case == 1)])), 0)
})
# NB: All incident cases must have a date of diagnosis (otherwise unknown if incident or prevalent)
# but prevalent cases do not necessarily have a date of diagnosis as they can be identified
# based on information provided at baseline (not only from HES data).

# Saving outputs
saveRDS(mydata, "../Outputs/output_final.rds")
