# Recoding extracted categorical/continuous variables.
# Required inputs: outputs from 2-extract_selected.R

library(openxlsx)

# Loading outputs from previous steps
choices <- readRDS("../outputs/annot.rds")
mydata <- readRDS("../outputs/ukb_extracted.rds")

# Initialising objects
weird_recoding <- NULL
recoded_data <- mydata

# For loop over extracted columns
pb <- utils::txtProgressBar(style = 3)
for (k in 1:ncol(mydata)) {
  # Identifying corresponding coding name and coding ID
  tmp_coding_name <- gsub("\\..*", "", colnames(mydata)[k])
  tmp_coding_id <- choices[tmp_coding_name, "Coding"]

  if (!is.na(tmp_coding_id)) {
    # Extracting corresponding coding (if any)
    coding <- read.table(paste0("../parameters/codings/codes_", tmp_coding_id, ".txt"), header = TRUE, stringsAsFactors = FALSE)
    coding$RecodedValue[coding$RecodedValue == "NA"] <- NA
    coding$RecodedMeaning[coding$RecodedMeaning == "NA"] <- NA

    # Storing IDs of categorical variables for which some categories are not coded
    if (grepl("categorical", tolower(choices[tmp_coding_name, "ValueType"]))) {
      if (!all(as.character(na.exclude(mydata[, k])) %in% coding$OriginalValue)) {
        print(all(as.character(na.exclude(mydata[, k])) %in% coding$OriginalValue))
        weird_recoding <- c(weird_recoding, k)
      }
    }

    # Preparing recoding (applicable to any data type if a coding ID is provided)
    recoding <- coding$RecodedMeaning
    names(recoding) <- coding$OriginalValue
    recoded_data[, k] <- recoding[as.character(mydata[, k])]

    if (grepl("categorical", tolower(choices[tmp_coding_name, "ValueType"]))) {
      # Recoding for categorical variables: levels are ordered as indicated by RecodedValue
      recoded_data[, k] <- factor(recoded_data[, k], levels = unique(coding$RecodedMeaning[sort.list(as.numeric(coding$RecodedValue))]))
    }
  }

  if ((grepl("integer", tolower(choices[tmp_coding_name, "ValueType"]))) | (grepl("continuous", tolower(choices[tmp_coding_name, "ValueType"])))) {
    # Recoding for continuous/integers: as numeric if no character strings
    if (all(!grepl("\\D", recoded_data[, k]))) {
      recoded_data[, k] <- as.numeric(recoded_data[, k])
    }
  }

  if (grepl("date", tolower(choices[tmp_coding_name, "ValueType"]))) {
    # Recoding for dates
    recoded_data[, k] <- as.Date(recoded_data[, k], origin = "1970-01-01")
  }

  if (tolower(choices[tmp_coding_name, "ValueType"]) %in% c("time", "text", "compound")) {
    # Recoding for text/time/compound (rare types)
    recoded_data[, k] <- as.character(recoded_data[, k])
  }
  utils::setTxtProgressBar(pb, k / ncol(mydata))
}
cat("\n")
mydata <- recoded_data

# Quality check
if (length(weird_recoding) > 0) {
  print(paste0("Categories not described for ", length(weird_recoding), " fields:"))
  print(colnames(mydata)[weird_recoding])
}

# Additional recoding of continuous variables
continuous_codings <- list.files(path = "../parameters/codings", pattern = "codes_field")
if (length(continuous_codings) > 0) {
  print(paste0("Detected ", length(continuous_codings), " coding(s) for continuous variables:"))
  print(cbind(continuous_codings))

  # For loop over fields to recode
  fields_to_recode <- gsub("\\..*", "", gsub(".*_", "", continuous_codings))
  for (i in length(fields_to_recode)) {
    tmp_coding_name <- choices[which(choices$FieldID == fields_to_recode[i]), "CodingName"]
    coding <- read.table(paste0("../parameters/codings/", continuous_codings[i]),
      header = TRUE, stringsAsFactors = FALSE
    )
    ids <- which(gsub("\\..*", "", colnames(mydata)) == tmp_coding_name)
    recoded_data <- cbind(recoded_data, recoded_data[, ids, drop = FALSE])
    colnames(recoded_data)[(ncol(recoded_data) - length(ids) + 1):ncol(recoded_data)] <- sapply(strsplit(colnames(recoded_data), split = "\\."), FUN = function(x) {
      paste0(x[1], "_continuous", ".", x[2], ".", x[3])
    })[ids]
    for (j in ids) {
      # Allowing for NA in min/max (replacing them by actual min/max
      tmp_coding <- coding
      tmp_coding$MinValue[is.na(tmp_coding$MinValue)] <- min(mydata[, j], na.rm = TRUE) - 1
      tmp_coding$MaxValue[is.na(tmp_coding$MaxValue)] <- max(mydata[, j], na.rm = TRUE) + 1

      # Recoding each category
      for (k in 1:nrow(tmp_coding)) {
        tmp_cat_ids <- which((mydata[, j] >= tmp_coding[k, "MinValue"]) & (mydata[, j] < tmp_coding[k, "MaxValue"]))
        recoded_data[tmp_cat_ids, j] <- tmp_coding[k, "RecodedMeaning"]
      }

      # Factor levels are ordered as indicated by RecodedValue
      recoded_data[, j] <- factor(recoded_data[, j], levels = tmp_coding$RecodedMeaning[sort.list(as.numeric(tmp_coding$RecodedValue))])

      # Quality check
      print(colnames(mydata)[j])
      print(table(recoded_data[, j]))
    }
  }
}
mydata <- recoded_data

# Including ArrayList and ArrayMethod for selection at next step
choices$ArrayList <- rep(NA, nrow(choices))
choices$ArrayMethod <- rep(0, nrow(choices))

# Extracting available arrays
for (k in 1:nrow(choices)) {
  ids <- which(gsub("\\..*", "", colnames(mydata)) == choices$CodingName[k])
  choices[k, "ArrayList"] <- paste(unique(gsub(".*\\.", "", colnames(mydata)[ids])), collapse = ",")
}

# Preparing additional column information
write.xlsx(choices, "../parameters/parameters.xlsx")

# Saving extracted dataset
saveRDS(mydata, "../outputs/ukb_recoded.rds")
