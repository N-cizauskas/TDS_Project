getwd()
setwd("/rds/general/project/hda-22-23/live/Comp_Epi/General/Group8_Working/analysis")

library(dplyr)

# Load data
male_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_maletest_2203.rds")
male_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_maletrain_2203.rds")
female_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_femaletrain_2203.rds")
female_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_femaletest_2203.rds")

date_recr_diff <- female_train$date_recr.y - as.Date(female_train$date_recr.x) 
table(date_recr_diff)

# Remove columns not needed for analysis currently (date_recr.y, cancer_icd10, Cancer_record_origin, incident_case, prevalent_case, bmi_bin)

female_train_final <- female_train %>% 
  select(-c(date_recr.y, cancer_icd10, Cancer_record_origin, incident_case, prevalent_case, bmi_bin, eid))

female_test_final <- female_train %>% 
  select(-c(date_recr.y, cancer_icd10, Cancer_record_origin, incident_case, prevalent_case, bmi_bin, eid))

male_train_final <- male_train %>% 
  select(-c(date_recr.y, cancer_icd10, Cancer_record_origin, incident_case, prevalent_case, bmi_bin, eid))

male_test_final <- male_test %>% 
  select(-c(date_recr.y, cancer_icd10, Cancer_record_origin, incident_case, prevalent_case, bmi_bin, eid))

# Save final datasets
saveRDS(female_train_final, file = "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_train_2403.rds")
saveRDS(female_test_final, file = "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_test_2403.rds")
saveRDS(male_train_final, file = "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_train_2403.rds")
saveRDS(male_test_final, file = "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_test_2403.rds")


# Save data as csv
write.csv(female_train_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_train.csv", row.names = TRUE)
write.csv(female_test_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_test.csv", row.names = TRUE)
write.csv(male_train_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_train.csv", row.names = TRUE)
write.csv(male_test_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_test.csv", row.names = TRUE)

# Load csv
female_train_csv <- read_csv("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_train.csv")
female_test_csv <- read_csv("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_test.csv")
male_train_csv <- read_csv("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_train.csv")
male_test_csv <- read_csv("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_test.csv")

# Convert first column to rownames
female_train_csv <- data.frame(female_train_csv, row.names = 1)
female_test_csv <- data.frame(female_test_csv, row.names = 1)
male_train_csv <- data.frame(male_train_csv, row.names = 1)
male_test_csv <- data.frame(male_test_csv, row.names = 1)

# Resave data  
write.csv(female_train_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_train.csv", row.names = TRUE)
write.csv(female_test_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_female_test.csv", row.names = TRUE)
write.csv(male_train_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_train.csv", row.names = TRUE)
write.csv(male_test_final, "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_test.csv", row.names = TRUE)

male_train_final <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_train_2403.rds")

# Change age_voice_broke, age_first_facial_hair and hair_bald in male data to categorical data

male_train_data_2503 <- male_train_data_2503 %>% mutate(age_first_facialhair = case_when(age_first_facialhair == 1 ~ 'Younger than average',
                                             age_first_facialhair == 2 ~ 'Average age',
                                             age_first_facialhair == 3 ~ 'Older than average',
                                             age_first_facialhair == -1 ~ 'Do not know',
                                             age_first_facialhair == -3 ~ 'Prefer not to say'))

male_train_final <- male_train_final %>% mutate(age_voice_broke = case_when(age_voice_broke == 1 ~ 'Younger than average',
                                                                                 age_voice_broke == 2 ~ 'About average age',
                                                                                 age_voice_broke == 3 ~ 'Older than average',
                                                                                 age_voice_broke == -1 ~ 'Do not know',
                                                                                 age_voice_broke == -3 ~ 'Prefer not to say'))


male_train_final <- male_train_final %>% mutate(hair_bald = case_when(hair_bald == 1 ~ 'Pattern 1',
                                                                            hair_bald == 2 ~ 'Pattern 2',
                                                                            hair_bald == 3 ~ 'Pattern 3',
                                                                            hair_bald == -1 ~ 'Do not know',
                                                                            hair_bald == -3 ~ 'Prefer not to say'))

male_test_final <- male_test_final %>% mutate(age_first_facialhair = case_when(age_first_facialhair == 1 ~ 'Younger than average',
                                                                                 age_first_facialhair == 2 ~ 'Average age',
                                                                                 age_first_facialhair == 3 ~ 'Older than average',
                                                                                 age_first_facialhair == -1 ~ 'Do not know',
                                                                                 age_first_facialhair == -3 ~ 'Prefer not to say'))

male_test_final <- male_test_final %>% mutate(age_voice_broke = case_when(age_voice_broke == 1 ~ 'Younger than average',
                                                                            age_voice_broke == 2 ~ 'About average age',
                                                                            age_voice_broke == 3 ~ 'Older than average',
                                                                            age_voice_broke == -1 ~ 'Do not know',
                                                                            age_voice_broke == -3 ~ 'Prefer not to say'))


male_test_final <- male_test_final %>% mutate(hair_bald = case_when(hair_bald == 1 ~ 'Pattern 1',
                                                                      hair_bald == 2 ~ 'Pattern 2',
                                                                      hair_bald == 3 ~ 'Pattern 3',
                                                                      hair_bald == -1 ~ 'Do not know',
                                                                      hair_bald == -3 ~ 'Prefer not to say'))

## num_in_house --> make all 10f+ into 10; PNTA, DNK - NA
male_train_final$num_in_house[male_train_final$num_in_house == "10+"] <- 10
male_train_final$num_in_house[male_train_final$num_in_house == "Prefer not to answer"] <- NA
male_train_final$num_in_house[male_train_final$num_in_house == "Do not know"] <- NA
male_train_final %>% count(num_in_house)
male_train_final$num_in_house <- as.numeric(male_train_final$num_in_house)
class(male_train_final$num_in_house)

## sleep_duration 11+ change and then impute
male_train_final %>% count(sleep_duration)
male_train_final$sleep_duration[male_train_final$sleep_duration == "11+"] <- 11
male_train_final$sleep_duration[male_train_final$sleep_duration == 1] <- 3
male_train_final$sleep_duration[male_train_final$sleep_duration == 2] <- 3
male_train_final$sleep_duration[male_train_final$sleep_duration == "Prefer not to answer"] <- NA
male_train_final$sleep_duration[male_train_final$sleep_duration == "Do not know"] <- NA
class(male_train_final$sleep_duration)
male_train_final$sleep_duration <- as.numeric(male_train_final$sleep_duration)

## num_in_house --> make all 10f+ into 10; PNTA, DNK - NA (test data)
male_test_final$num_in_house[male_test_final$num_in_house == "10+"] <- 10
male_test_final$num_in_house[male_test_final$num_in_house == "Prefer not to answer"] <- NA
male_test_final$num_in_house[male_test_final$num_in_house == "Do not know"] <- NA
male_test_final %>% count(num_in_house)
male_test_final$num_in_house <- as.numeric(male_test_final$num_in_house)
class(male_test_final$num_in_house)

## sleep_duration 11+ change and then impute (test data)
male_test_final %>% count(sleep_duration)
male_test_final$sleep_duration[male_test_final$sleep_duration == "11+"] <- 11
male_test_final$sleep_duration[male_test_final$sleep_duration == 1] <- 3
male_test_final$sleep_duration[male_test_final$sleep_duration == 2] <- 3
male_test_final$sleep_duration[male_test_final$sleep_duration == "Prefer not to answer"] <- NA
male_test_final$sleep_duration[male_test_final$sleep_duration == "Do not know"] <- NA
class(male_test_final$sleep_duration)
male_test_final$sleep_duration <- as.numeric(male_test_final$sleep_duration)

saveRDS(male_train_data_2503, file = "/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/male_train_data_2503.rds")
