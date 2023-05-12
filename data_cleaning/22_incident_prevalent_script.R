# set working directory
setwd("/rds/general/user/dah22/projects/hda-22-23/live/TDS/Group8_Working/analysis/DATA")

# load libraries
library(dplyr)

# load data
female_test_icd <- readRDS("final_femaletest.rds")
male_test_icd <- readRDS("final_maletest.rds")
female_test <- readRDS("FINAL_DATA/female_test.rds")
male_test <- readRDS("FINAL_DATA/male_test.rds")

# Move rownames to a new column named 'eid' in female_test and male_test
female_test$eid <- rownames(female_test)
male_test$eid <- rownames(male_test)

# Move cancer_icd10 column from dataframes ending icd to the other two dataframes matched by 'eid'
female_test <- merge(female_test, female_test_icd[, c("eid", "incident_case", "prevalent_case")], by="eid")
male_test <- merge(male_test, male_test_icd[, c("eid", "incident_case", "prevalent_case")], by="eid")

# Move eid back to rownames for male and female_test dataframes
male_test2 <- male_test[,-1]
rownames(male_test2) <- male_test[,1]

female_test2 <- female_test[,-1]
rownames(female_test2) <- female_test[,1]

# Drop case column
prevalent_male <- male_test2 %>% dplyr::select(-c("case", "incident_case"))
prevalent_male <- prevalent_male %>% rename(case = prevalent_case)
incident_male <- male_test2 %>% dplyr::select(-c("case", "prevalent_case"))
incident_male <- incident_male %>% rename(case = incident_case)

prevalent_female <- female_test2 %>% dplyr::select(-c("case", "incident_case"))
prevalent_female <- prevalent_female %>% rename(case = prevalent_case)
incident_female <- female_test2 %>% dplyr::select(-c("case", "prevalent_case"))
incident_female <- incident_female %>% rename(case = incident_case)

# Save new dataframes as male and female_test_stratify
saveRDS(prevalent_male, "FINAL_DATA/male_prevalent_stratify.rds")
saveRDS(prevalent_female, "FINAL_DATA/female_prevalent_stratify.rds")
saveRDS(incident_male, "FINAL_DATA/male_incident_stratify.rds")
saveRDS(incident_female, "FINAL_DATA/female_incident_stratify.rds")

