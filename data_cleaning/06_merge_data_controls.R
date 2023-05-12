## Script to merge cases and controls data with ukb_thinned 

# Load packages

library(dplyr)
library(stringr)

# set working directory

getwd()
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

# load data

ukb_thinned <- readRDS('ukb_thinned.rds')
output_registries <- readRDS("output_registeries.rds") # This data set combines the cases found in extract_hes.R with those found in the death registries.

# Make ukb index a column called eid - Do not need to run this as eid already in data

ukb <- cbind(eid = rownames(ukb), ukb)
rownames(ukb) <- 1:nrow(ukb)


# Merge output_registries data with ukb to include cases and controls in dataframe using eid as common column

ukb_thinned_controls <- merge(ukb_thinned, output_registries, by = "eid")

saveRDS(ukb_thinned_controls, file = "ukb_thinned_controls.rds")
