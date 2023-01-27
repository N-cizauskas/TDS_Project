# Objective: definition of disease status and time to diagnosis from diagnoses
# recorded in Hospital Episode Statistics and death registry data.

library(data.table)
source("functions.R")

# Reading arguments
args <- commandArgs(trailingOnly = TRUE)
def_path <- as.character(args[1])
app_data_path <- as.character(args[2])
death_main_path <- as.character(args[3])
death_cause_path <- as.character(args[4])

# Initialising empty lists of codes
icd10_codes <- NULL

# Reading input files
myfiles <- list.files(path = def_path)
if ("diagnoses_icd10.txt" %in% myfiles) {
  icd10_codes <- fread(paste0(def_path, "/diagnoses_icd10.txt"), data.table = FALSE)
  icd10_codes <- toupper(icd10_codes[, 1])
}

# Messages for quality check
print(paste0("Reading ", length(icd10_codes), " ICD10 codes"))

# Loading HES-based definitions
mydata <- readRDS("../Outputs/output_hes.rds")

# Summary of registries-based definitions
print("Number of cases identified in HES:")
print(sum(mydata$case, na.rm = TRUE))
print("Number of prevalent cases:")
print(sum(mydata$prevalent_case, na.rm = TRUE))
print("Number of incident cases:")
print(sum(mydata$incident_case, na.rm = TRUE))

# Loading death data
print("Loading main death dataset from:")
print(death_main_path)
death_full <- fread(death_main_path, data.table = FALSE)
print("Loading cause of death dataset from:")
print(death_cause_path)
death <- fread(death_cause_path, data.table = FALSE)

# Extracting relevant participants
death_full <- death_full[death_full$eid %in% rownames(mydata), ]
death <- death[death$eid %in% rownames(mydata), ]

# Removing duplicated rows (multiple sources)
death_full <- death_full[!duplicated(death_full$eid), ]
rownames(death_full) <- death_full$eid

# Setting as date object
death_full$date_of_death <- as.Date(death_full$date_of_death, format = "%d/%m/%Y")

# Storing date of death in dataset
mydata[rownames(death_full), "date_death"] <- death_full$date_of_death

# Re-writing ICD codes (allowing to write e.g. I20-I25 to say I20, I21, I22, I23, I24, I25)
type <- "icd10"
input_codes <- eval(parse(text = paste0(type, "_codes")))
if (length(input_codes) > 0) {
  full_list <- ExpandCodes(input_codes = input_codes)
} else {
  full_list <- input_codes
}
assign(paste0("full_", type), full_list)

# Definition of HES columns to use
hes_columns <- c("diag_icd10", "diag_icd9", "oper4")
names(hes_columns) <- c("icd10", "icd9", "opcs4")

# Using ICD10 codes
type <- "icd10"
full_list <- eval(parse(text = paste0("full_", type)))
if (length(full_list) > 0) {
  print(paste0("Extracting based on ", type, " codes"))
  pb <- txtProgressBar(style = 3)
  for (k in 1:length(full_list)) {
    # Extracting eid of diagnosed participants
    tmp_eid <- unique(as.character(death$eid[grepl(paste0("^", full_list[k]), death$cause_icd10)]))

    # Extracting dates of diagnoses (i.e. date of death)
    epistart <- death_full[tmp_eid, "date_of_death"]

    # Updating first date of diagnosis if before the current one
    mydata[tmp_eid, "date_diagnosis"] <- as.Date(ifelse((mydata[tmp_eid, "date_diagnosis"] >= epistart) | (is.na(mydata[tmp_eid, "date_diagnosis"])),
      yes = epistart,
      no = mydata[tmp_eid, "date_diagnosis"]
    ),
    origin = "1970-01-01"
    )

    # Setting participant as a case
    mydata[tmp_eid, "case"] <- 1

    setTxtProgressBar(pb, k / length(full_list))
  }
  cat("\n")
}

# Extracting ids of participants with unclear prevalent/incident status (missing dates of diagnosis)
unknown_ids <- which(is.na(mydata$prevalent_case))

# Definition of prevalent/incident cases based on dates of recruitment and first diagnosis
prevalent_ids <- which(mydata[, "date_diagnosis"] < mydata[, "date_recr"])
if (length(unknown_ids) > 0) {
  prevalent_ids <- prevalent_ids[!prevalent_ids %in% unknown_ids]
}
mydata[prevalent_ids, "prevalent_case"] <- 1
mydata[prevalent_ids, "incident_case"] <- NA
incident_ids <- which(mydata[, "date_diagnosis"] >= mydata[, "date_recr"])
if (length(unknown_ids) > 0) {
  incident_ids <- incident_ids[!incident_ids %in% unknown_ids]
}
mydata[incident_ids, "incident_case"] <- 1

# Definition of time to diagnosis from dates of recruitment and first diagnosis
mydata[incident_ids, "time_to_diagnosis"] <- mydata[incident_ids, "date_diagnosis"] - mydata[incident_ids, "date_recr"]

# Saving outputs
saveRDS(mydata, "../Outputs/output_registries.rds")
