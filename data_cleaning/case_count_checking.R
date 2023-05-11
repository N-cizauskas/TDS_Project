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

# step 3: determine hormone sensitive cancers via icd10
hormone_cancers <- c("C61", "C50", "C56", "C55", "C54", "C73")
unique(ukb_thinned$cancer_icd10)
result <- rep(FALSE, length(ukb_thinned$cancer_icd10))
count <- 0
for (i in ukb_thinned$cancer_icd10){
  print(i)
  count <- count + 1
  if (nchar(i)==3){
    if (i %in% hormone_cancers){
      result[count] <- TRUE
    }
  }
  
  if (nchar(i)==4){
    i <- substr(i, 1, 3)
    if (i %in% hormone_cancers){
      result[count] <- TRUE
    }
  }
  
}

table(result)

# step 4: determine hormone sensitive cancers via icd9
unique(ukb_thinned$cancer_icd9)
icd9_list <- c(174, 179, 182, 183, 185, 193)
for (i in 1:length(result)){
  print(i)
  if (result[i]==FALSE){
    code <- as.numeric(substr(ukb_thinned$cancer_icd9[i],1,3))
    print(code)
    if (code %in% icd9_list){
      result[i] <- TRUE
    }
  }
}

# step 5: check against prev data record
table(result)

# step 6: checking against controls @barbara ayo
ukb_thinned_controls <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/ukb_thinned_controls.rds")
ukb_thinned_controls$casecheck <- result
table(ukb_thinned_controls$case, ukb_thinned_controls$casecheck)
##     FALSE   TRUE
## 0 460094   5791
## 1   8067  28446

# 5791 non cases found to be cases
# 8067 cases found to be non cases

### SAVING TRUE CASES INTO RDS
ukb_slimmed_cases <- subset(ukb_thinned_controls, subset = case == 1 & casecheck == TRUE)

saveRDS(ukb_slimmed_cases, file = "cases.RDS")
