# Creating a data dict for current UKBB application.
# Aims: Obtain table with all variable info and empty columns to fill manually for naming guidance and instances to extract.

library(data.table)
library(openxlsx)

# Reading arguments
args <- commandArgs(trailingOnly = TRUE)
ukb_path <- as.character(args[1])

# Creating the parametrisation folder
dir.create("../parameters", showWarnings = FALSE)

# Loading the data
ukb_main_columns <- colnames(fread(ukb_path, data.table = FALSE, nrows = 0)) # unique IDS from current UKBB basket

# Loading UK Biobank files
dict <- fread("../docs/Data_Dictionary_Showcase.csv", data.table = FALSE) # Data dictionary from UKBB: https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide

# Extracting field IDs
IDs <- unique(gsub("-.*", "", gsub("X", "", ukb_main_columns))) # unique IDS from application. Removing array & instance info.
matchedDict <- dict[dict$FieldID %in% IDs, ]
print("Field IDs that could not be matched:")
print(IDs[!IDs %in% intersect(IDs, unique(dict$FieldID))])

# Identifying columns to keep from UK Biobank showcase file
requiredColumns <- c("Field", "Participants", "Notes", "Units", "Path", "Link", "FieldID", "ValueType", "Coding") # Value type (integer, factor etc.) can be used to filter table for pre-processing sep differences between continious and cat. variables
matchedDict <- matchedDict[, requiredColumns] # Dictionary uncluding current application variables only

# Extra columns
matchedDict$CodingName <- NA # variable is kept if not NA: shorter/ consice variable name for scripts - no white spaces etc.
matchedDict$FigureName <- NA # Names to put in figure - can include white spaces etc.
matchedDict$UnitInName <- tolower(matchedDict$Units) # Indicates if unit should be added in the FigureName. If NA or FALSE, it is not added. If TRUE, the unit provided in "Unit" is added in brackets at the end of "FigureName". User-defined units can be specified as characters and will be used instead of the value provided in "Unit".
matchedDict$InstanceList <- NA

# Defining ArrayList and InstanceList: lists of available arrays/instances
print("Defining available instances and arrays")
pb <- utils::txtProgressBar(style = 3)
for (i in 1:nrow(matchedDict)) {
  tmpname <- ukb_main_columns[grep(paste0("^", matchedDict[i, "FieldID"], "\\-"), ukb_main_columns)]
  tmpname <- gsub("-", ".", tmpname)
  matchedDict[i, "InstanceList"] <- paste(unique(sapply(strsplit(tmpname, split = "\\."), FUN = function(x) {
    x[2]
  })), collapse = ",")
  utils::setTxtProgressBar(pb, i / nrow(matchedDict))
}
cat("\n")

# Keeping all instances by default
matchedDict$InstanceRequired <- matchedDict$InstanceList # List of time points to keep. If NA, only baseline (0) is kept.
# e.g. To keep baseline and first repeat, use 0,1

# Re-ordering columns
matchedDict <- matchedDict[, c(
  "Field", "Participants", "Notes", "Units", "InstanceList", "Path", "Link",
  "FieldID", "ValueType", "Coding",
  "CodingName", "FigureName", "UnitInName", "InstanceRequired"
)]

# # Saving as csv file
# readr::write_csv(matchedDict, "../outputs/matchedDict.csv")

# Saving as excel file
wb <- createWorkbook()
addWorksheet(wb, "Fields")
writeData(wb, sheet = "Fields", matchedDict)
freezePane(wb, sheet = "Fields", firstRow = TRUE, firstCol = TRUE)
saveWorkbook(wb, "../parameters/selection.xlsx", overwrite = TRUE)
