# step 1: import data
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")
ukb_thinned <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/ukb_thinned.rds")

# step 2: check for NAs and remove
sum(is.na(ukb_thinned$Sex)) # 0 NAs
sum(is.na(ukb_thinned$Year_birth)) # 3 NAs
ukb_thinned <- ukb_thinned[!is.na(ukb_thinned$Year_birth),]

# step 3: resave
saveRDS(ukb_thinned, file = "ukb_thinned.rds")