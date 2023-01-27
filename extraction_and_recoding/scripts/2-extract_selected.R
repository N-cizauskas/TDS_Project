# Extracting selected variables at selected instances and giving them chosen coding and figure names
# Required inputs: the full UK Biobank dataset; data codings from UK Biobank; the data dictionary with choices in CodingName and FigureName

library(data.table)
library(openxlsx)

# Cleaning previous outputs
dir.create("../outputs", showWarnings = FALSE)
if (length(list.files("outputs")) > 0) {
  system("rm ../outputs/*")
}

# Creating directory for codings
dir.create("../parameters/codings", showWarnings = FALSE)

# Reading arguments
args <- commandArgs(trailingOnly = TRUE)
ukb_path <- as.character(args[1])

# Loading xlsx file with selections
choices <- read.xlsx("../parameters/selection.xlsx")

# Printing message
print(paste0("N=", sum(!is.na(choices$CodingName)), " field IDs have been selected for extraction:"))
tmpnames <- choices[, c("CodingName", "InstanceRequired")]
tmpnames <- tmpnames[!is.na(tmpnames[, 1]), ]
print(tmpnames)

# Checking there are no duplicated coding names
if (any(duplicated(choices$CodingName, incomparables = NA))) {
  stop(paste0("There are duplicated entries in CodingName (", choices$CodingName[duplicated(choices$CodingName, incomparables = NA)], "). Please change this in the input file."))
}

# Extracting relevant rows
choices <- choices[!is.na(choices$CodingName), ]
rownames(choices) <- choices$FieldID

# Loading relevant columns from UK Biobank data
ukb_main_columns <- colnames(fread(ukb_path, data.table = FALSE, nrows = 0))
ids <- grep("^eid", ukb_main_columns)
for (i in 1:nrow(choices)) {
  instance_vect <- unlist(strsplit(choices$InstanceRequired[i], split = ","))
  for (j in 1:length(instance_vect)) {
    ids <- c(ids, grep(paste0("^", choices$FieldID[i], "-", instance_vect[j], "\\."), ukb_main_columns))
  }
}
mydata <- fread(ukb_path, data.table = FALSE, select = ids)

# Re-formatting the data with eids as row names
rownames(mydata) <- mydata$eid
mydata <- mydata[, -1]
colnames(mydata) <- paste0(
  choices[gsub("-.*", "", colnames(mydata)), "CodingName"], ".",
  gsub(".*-", "", colnames(mydata))
)
rownames(choices) <- choices$CodingName


# Loading coding provided by UK Biobank
codings <- fread("../docs/Codings.csv", data.table = FALSE) # Codings for categorical outcomes: https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide

# Extracting the relevant data coding
coding_ids <- unique(choices$Coding)
coding_ids <- coding_ids[!is.na(coding_ids)]
pb <- utils::txtProgressBar(style = 3)
for (i in 1:length(coding_ids)) {
  codeID <- as.character(coding_ids[i])
  tmp <- codings[codings$Coding %in% codeID, -1]
  tmp <- cbind(tmp, tmp)
  colnames(tmp) <- c("OriginalValue", "OriginalMeaning", "RecodedValue", "RecodedMeaning")
  write.table(tmp, paste0("../parameters/codings/codes_", codeID, ".txt"),
    row.names = FALSE, col.names = TRUE
  )
  utils::setTxtProgressBar(pb, i / length(coding_ids))
}
cat("\n")

# Preparing template for recoding of continuous variables
tmp <- data.frame(MinValue = NA, MaxValue = NA, RecodedValue = NA, RecodedMeaning = NA)
write.table(tmp, paste0("../parameters/codings/codes_template_continuous.txt"),
  row.names = FALSE, col.names = TRUE
)

# Create figure names. If a manual figure name is not supplied, supply 'Field' as figure name
choices[which(is.na(choices$FigureName)),"FigureName"] = choices$Field

# Creating figure name including units
choices$FigureNameWithUnit <- choices$FigureName
unit_ids <- which(!is.na(choices$UnitInName))
choices$FigureNameWithUnit[unit_ids] <- paste0(choices$FigureName[unit_ids], " (", choices$UnitInName[unit_ids], ")")


# Print selected fields to help with manual definition of recoding
print(choices[,c("FieldID", "Coding")])

# Preparing additional column information
saveRDS(choices, "../outputs/annot.rds")

# Saving extracted dataset
saveRDS(mydata, "../outputs/ukb_extracted.rds")
