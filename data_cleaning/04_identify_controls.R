setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

# step 1: load packages
library(dplyr)
library(stringr)
library(table1)
library(parallel)
library(mice)
library(purrr)
library(rpart)

# step 2: data import
ukb_thinned <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/ukb_thinned.rds")

# step 3: determine cancer-less controls via icd10
unique(ukb_thinned$cancer_icd10)
result <- rep(FALSE, length(ukb_thinned$cancer_icd10))
count <- 0
for (i in ukb_thinned$cancer_icd10){
  #print(i)
  count <- count + 1
  if (i==""){
      result[count] <- TRUE
  }
}

table(result)

# step 4: determine hormone sensitive cancers via icd9
for (i in 1:length(result)){
  #print(i)
  if (result[i]==TRUE){
    if (!is.na(ukb_thinned$cancer_icd9[i])){
      result[i] <- FALSE
    }
  }
}

# step 5: check against prev data record
table(result)

# step 6: checking against controls @barbara ayo
ukb_thinned_controls <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/ukb_thinned_controls.rds")
ukb_thinned_controls$controlcheck <- result
table(ukb_thinned_controls$case, ukb_thinned_controls$controlcheck)
##    FALSE   TRUE
## 0  84829 381056
## 1  34119   2394

  # 84829 non cases found to be non controls

### saving controls
ukb_slimmed_controls <- subset(ukb_thinned_controls, subset = case == 0 & controlcheck == TRUE)

saveRDS(ukb_slimmed_controls, file = "controls.RDS")