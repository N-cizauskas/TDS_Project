# Recoding across variables with multiple arrays (multiple choice answers or repeated measurements)
# Required inputs: outputs from 3-recode_variables.R

library(data.table)
library(openxlsx)

# Loading outputs from previous steps
mydata <- readRDS("../outputs/ukb_recoded.rds")
choices <- read.xlsx("../parameters/parameters.xlsx")

# Identifying variables with multiple arrays
multiple_arrays_id <- grep(",", choices$ArrayList)

# ArrayMethod:
# 0: keep all arrays
# 1: take min over arrays
# 2: take max over arrays
# 3: take mean over arrays
# 4: take median over arrays
# 5: take first
# 6: take last

# For loop over fields with multiple arrays
data_recoded <- data.frame(row.names = rownames(mydata))
pb <- utils::txtProgressBar(style = 3)
for (i in 1:nrow(choices)) {
  instances <- strsplit(choices[i, "InstanceRequired"], split = ",")[[1]]
  for (j in instances) {
    column_ids <- grep(paste0("^", choices[i, "CodingName"], "\\.", j, "\\."), colnames(mydata))
    if (i %in% multiple_arrays_id) {
      if (choices[i, "ArrayMethod"] == 0) {
        data_recoded <- cbind(data_recoded, mydata[, column_ids, drop = FALSE])
      }
      if (choices[i, "ArrayMethod"] %in% c(1, 2, 3, 4)) {
        factor_variable <- FALSE
        if (is.factor(mydata[, column_ids[1]])) {
          mylevels <- levels(mydata[, column_ids[1]])
          factor_variable <- TRUE
        }

        # Making numeric
        for (k in 1:length(column_ids)) {
          mydata[, column_ids[k]] <- as.numeric(mydata[, column_ids[k]])
        }

        # Applying the selected function to the rows
        data_recoded <- cbind(data_recoded, rep(NA, nrow(data_recoded)))
        for (k in 1:nrow(mydata)) {
          if (choices[i, "ArrayMethod"] == 1) {
            data_recoded[k, ncol(data_recoded)] <- min(mydata[k, column_ids], na.rm = TRUE)
          }
          if (choices[i, "ArrayMethod"] == 2) {
            data_recoded[k, ncol(data_recoded)] <- max(mydata[k, column_ids], na.rm = TRUE)
          }
          if (choices[i, "ArrayMethod"] == 3) {
            data_recoded[k, ncol(data_recoded)] <- mean(mydata[k, column_ids], na.rm = TRUE)
          }
          if (choices[i, "ArrayMethod"] == 4) {
            data_recoded[k, ncol(data_recoded)] <- median(mydata[k, column_ids], na.rm = TRUE)
          }
        }

        # Assigning column name
        colnames(data_recoded)[ncol(data_recoded)] <- colnames(mydata)[column_ids[1]]

        # Transforming back to factor if needed
        if (factor_variable) {
          data_recoded[, ncol(data_recorded)] <- factor(data_recoded[, ncol(data_recorded)], levels = mylevels)
        }
      }
      if (choices[i, "ArrayMethod"] == 5) {
        id <- which.min(as.numeric(sapply(strsplit(colnames(mydata)[column_ids], split = "\\."), FUN = function(x) {
          x[3]
        })))
        data_recoded <- cbind(data_recoded, mydata[, column_ids[id]])
        colnames(data_recoded)[ncol(data_recoded)] <- colnames(mydata)[column_ids[id]]
      }
      if (choices[i, "ArrayMethod"] == 6) {
        id <- which.max(as.numeric(sapply(strsplit(colnames(mydata)[column_ids], split = "\\."), FUN = function(x) {
          x[3]
        })))
	data_recoded <- cbind(data_recoded, mydata[, column_ids[id]])
        colnames(data_recoded)[ncol(data_recoded)] <- colnames(mydata)[column_ids[id]]
      }
    } else {
      data_recoded <- cbind(data_recoded, mydata[, column_ids])
      colnames(data_recoded)[ncol(data_recoded)] <- colnames(mydata)[column_ids]
    }
  }
  utils::setTxtProgressBar(pb, i / nrow(choices))
}
cat("\n")

# Including continuous versions of categorised outcomes
data_recoded=cbind(data_recoded, mydata[,grep("_continuous", colnames(mydata)), drop=FALSE])
mydata=data_recoded

# Saving extracted dataset
saveRDS(mydata, "../outputs/ukb_final.rds")
