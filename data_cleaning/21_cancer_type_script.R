# set working directory
setwd("/rds/general/user/dah22/projects/hda-22-23/live/TDS/Group8_Working/analysis/DATA")

# load libraries
library(dplyr)

# load data
female_test_icd <- readRDS("final_femaletest.rds")
male_test_icd <- readRDS("final_maletest.rds")
female_test <- readRDS("FINAL_DATA/female_test.rds")
male_test <- readRDS("FINAL_DATA/male_test.rds")
female_2402 <- readRDS("/rds/general/user/dah22/projects/hda-22-23/live/TDS/Group8_Working/analysis/female_2402.rds")
male_2402 <- readRDS("/rds/general/user/dah22/projects/hda-22-23/live/TDS/Group8_Working/analysis/male_2402.rds")

# Move rownames to a new column named 'eid' in female_test and male_test
female_test$eid <- rownames(female_test)
male_test$eid <- rownames(male_test)

# Move cancer_icd10 column from dataframes ending icd to the other two dataframes matched by 'eid'
female_test <- merge(female_test, female_test_icd[, c("eid", "cancer_icd10")], by="eid")
male_test <- merge(male_test, male_test_icd[, c("eid", "cancer_icd10")], by="eid")

# Move cancer_icd9 column from the 2402 dataframes to the female_test and male_test dataframes by matching on 'eid'
female_test <- merge(female_test, female_2402[, c("eid", "cancer_icd9")], by="eid")
male_test <- merge(male_test, male_2402[, c("eid", "cancer_icd9")], by="eid")

# Create new variables in male_test and female_test called cancer_type from the ICD-9 and 10 codes
female_test <- female_test %>%
  mutate(cancer_type = case_when(
    cancer_icd10 %in% c("C500", "C501", "C502", "C503", "C504", "C505", "C506", "C508", "C509") ~ "Breast",
    cancer_icd10 %in% c("C541", "C542", "C543", "C549", "C55") ~ "Uterine",
    cancer_icd10 == "C56" ~ "Ovarian",
    cancer_icd10 == "C73" ~ "Thyroid",
    cancer_icd9 %in% c("179", "182", "1820") ~ "Uterine",
    cancer_icd9 == "193" ~ "Thyroid",
    cancer_icd9 %in% c("1740", "1741", "1742", "1743", "1744", "1745", "1746", "1748", "1749") ~ "Breast",
    cancer_icd9 == "1830" ~ "Ovarian"
  ))

male_test <- male_test %>% 
  mutate(cancer_type = case_when(
    cancer_icd10 %in% c("C500", "C501", "C502", "C504", "C509") ~ "Breast",
    cancer_icd10 == "C61" ~ "Prostate",
    cancer_icd10 == "C73" ~ "Thyroid",
    cancer_icd9 == "185" ~ "Prostate",
    cancer_icd9 == "193" ~ "Thyroid"
  ))

# Move eid back to rownames for male and female_test dataframes
male_test2 <- male_test[,-1]
rownames(male_test2) <- male_test[,1]

female_test2 <- female_test[,-1]
rownames(female_test2) <- female_test[,1]

# Drop ICD 9 and 10 columns 
male_test2 <- male_test2 %>% 
  select(-c("cancer_icd10", "cancer_icd9"))

female_test2 <- female_test2 %>% 
  select(-c("cancer_icd10", "cancer_icd9"))

# Save new dataframes as male and female_test_stratify
saveRDS(female_test2, "FINAL_DATA/female_test_stratify.rds")
saveRDS(male_test2, "FINAL_DATA/male_test_stratify.rds")
