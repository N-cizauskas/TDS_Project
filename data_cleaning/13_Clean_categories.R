# Set WD
getwd()
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA")

# Load libraries
library(dplyr)

# Load data
male_train <- readRDS("male_train_data_2503.rds")
male_test <- readRDS("male_test_data_2503.rds")
female_test <- readRDS("imputed_female_test_2403.rds")
female_train <- readRDS("imputed_female_train_2403.rds")

# View male data
str(male_train)

# View female data
str(female_train)

# Remove date_recr.x as not useful
male_train <- male_train %>% 
  select(-date_recr.x)

male_test <- male_test %>% 
  select(-date_recr.x)

# Change avg_household_income into numerical with hierarchy 
male_train <- male_train %>%
  mutate(avg_house_income = case_when(
    avg_house_income %in% c("Do not know", "Prefer not to answer") ~ -9999,
    avg_house_income == "Less than 18,000" ~ 1,
    avg_house_income == "18,000 to 30,999" ~ 2,
    avg_house_income == "31,000 to 51,999" ~ 3,
    avg_house_income == "52,000 to 100,000" ~ 4,
    avg_house_income == "Greater than 100,000" ~ 5,
    ))

male_test <- male_test %>%
  mutate(avg_house_income = case_when(
    avg_house_income %in% c("Do not know", "Prefer not to answer") ~ -9999,
    avg_house_income == "Less than 18,000" ~ 1,
    avg_house_income == "18,000 to 30,999" ~ 2,
    avg_house_income == "31,000 to 51,999" ~ 3,
    avg_house_income == "52,000 to 100,000" ~ 4,
    avg_house_income == "Greater than 100,000" ~ 5,
  ))

female_train <- female_train %>%
  mutate(avg_house_income = case_when(
    avg_house_income %in% c("Do not know", "Prefer not to answer") ~ -9999,
    avg_house_income == "Less than 18,000" ~ 1,
    avg_house_income == "18,000 to 30,999" ~ 2,
    avg_house_income == "31,000 to 51,999" ~ 3,
    avg_house_income == "52,000 to 100,000" ~ 4,
    avg_house_income == "Greater than 100,000" ~ 5,
  ))

female_test <- female_test %>%
  mutate(avg_house_income = case_when(
    avg_house_income %in% c("Do not know", "Prefer not to answer") ~ -9999,
    avg_house_income == "Less than 18,000" ~ 1,
    avg_house_income == "18,000 to 30,999" ~ 2,
    avg_house_income == "31,000 to 51,999" ~ 3,
    avg_house_income == "52,000 to 100,000" ~ 4,
    avg_house_income == "Greater than 100,000" ~ 5,
  ))

# Change education level into numerical with hierarchy (CSEs or equivalent, O levels/GCSEs or equivalent/NVQ or HND or HNC or equivalent all classified as 1 as similar) )
male_train <- male_train %>%
  mutate(Education_level = case_when(
    Education_level == "Prefer not to answer" ~ -9999,
    Education_level == "None of the above" ~ -9999,
    Education_level == "CSEs or equivalent" ~ 1,
    Education_level == "O levels/GCSEs or equivalent" ~ 1,
    Education_level == "NVQ or HND or HNC or equivalent" ~ 1,
    Education_level == "A levels/AS levels or equivalent" ~ 2,
    Education_level == "Other professional qualifications" ~ 3,
    Education_level == "College or University degree" ~ 4,
  ))

male_test <- male_test %>%
  mutate(Education_level = case_when(
    Education_level == "Prefer not to answer" ~ -9999,
    Education_level == "None of the above" ~ -9999,
    Education_level == "CSEs or equivalent" ~ 1,
    Education_level == "O levels/GCSEs or equivalent" ~ 1,
    Education_level == "NVQ or HND or HNC or equivalent" ~ 1,
    Education_level == "A levels/AS levels or equivalent" ~ 2,
    Education_level == "Other professional qualifications" ~ 3,
    Education_level == "College or University degree" ~ 4,
  ))

female_train <- female_train %>%
  mutate(Education_level = case_when(
    Education_level == "Prefer not to answer" ~ -9999,
    Education_level == "None of the above" ~ -9999,
    Education_level == "CSEs or equivalent" ~ 1,
    Education_level == "O levels/GCSEs or equivalent" ~ 1,
    Education_level == "NVQ or HND or HNC or equivalent" ~ 1,
    Education_level == "A levels/AS levels or equivalent" ~ 2,
    Education_level == "Other professional qualifications" ~ 3,
    Education_level == "College or University degree" ~ 4,
  ))

female_test <- female_test %>%
  mutate(Education_level = case_when(
    Education_level == "Prefer not to answer" ~ -9999,
    Education_level == "None of the above" ~ -9999,
    Education_level == "CSEs or equivalent" ~ 1,
    Education_level == "O levels/GCSEs or equivalent" ~ 1,
    Education_level == "NVQ or HND or HNC or equivalent" ~ 1,
    Education_level == "A levels/AS levels or equivalent" ~ 2,
    Education_level == "Other professional qualifications" ~ 3,
    Education_level == "College or University degree" ~ 4,
  ))

# Change traffic near road
male_train <- male_train %>%
  mutate(traffic_near_road = case_when(
    traffic_near_road == "[0,100)" ~ 1,
    traffic_near_road == "[100,1e+03)" ~ 2,
    traffic_near_road == "[1e+03,1e+04)" ~ 3,
    traffic_near_road == "[1e+04,1e+05)" ~ 4,
    traffic_near_road == "[1e+05,2e+05]" ~ 5,
  ))

male_test <- male_test %>%
  mutate(traffic_near_road = case_when(
    traffic_near_road == "[0,100)" ~ 1,
    traffic_near_road == "[100,1e+03)" ~ 2,
    traffic_near_road == "[1e+03,1e+04)" ~ 3,
    traffic_near_road == "[1e+04,1e+05)" ~ 4,
    traffic_near_road == "[1e+05,2e+05]" ~ 5,
  ))

female_train <- female_train %>%
  mutate(traffic_near_road = case_when(
    traffic_near_road == "[0,100)" ~ 1,
    traffic_near_road == "[100,1e+03)" ~ 2,
    traffic_near_road == "[1e+03,1e+04)" ~ 3,
    traffic_near_road == "[1e+04,1e+05)" ~ 4,
    traffic_near_road == "[1e+05,2e+05]" ~ 5,
  ))

female_test <- female_test %>%
  mutate(traffic_near_road = case_when(
    traffic_near_road == "[0,100)" ~ 1,
    traffic_near_road == "[100,1e+03)" ~ 2,
    traffic_near_road == "[1e+03,1e+04)" ~ 3,
    traffic_near_road == "[1e+04,1e+05)" ~ 4,
    traffic_near_road == "[1e+05,2e+05]" ~ 5,
  ))

# Traffic near major road
male_train <- male_train %>%
  mutate(traffic_near_major = case_when(
    traffic_near_major == "[0,100)" ~ 1,
    traffic_near_major == "[100,500)" ~ 2,
    traffic_near_major == "[500,1e+03)" ~ 3,
    traffic_near_major == "[1e+03,5e+03)" ~ 4,
    traffic_near_major == "[5e+03,1e+04)" ~ 5,
    traffic_near_major == "[1e+04,5e+04)" ~ 6,
    traffic_near_major == "[5e+04,1e+05)" ~ 7,
    traffic_near_major == "[1e+05,5e+05]" ~ 8,
  ))

male_test <- male_test %>%
  mutate(traffic_near_major = case_when(
    traffic_near_major == "[0,100)" ~ 1,
    traffic_near_major == "[100,500)" ~ 2,
    traffic_near_major == "[500,1e+03)" ~ 3,
    traffic_near_major == "[1e+03,5e+03)" ~ 4,
    traffic_near_major == "[5e+03,1e+04)" ~ 5,
    traffic_near_major == "[1e+04,5e+04)" ~ 6,
    traffic_near_major == "[5e+04,1e+05)" ~ 7,
    traffic_near_major == "[1e+05,5e+05]" ~ 8,
  ))

female_train <- female_train %>%
  mutate(traffic_near_major = case_when(
    traffic_near_major == "[0,100)" ~ 1,
    traffic_near_major == "[100,500)" ~ 2,
    traffic_near_major == "[500,1e+03)" ~ 3,
    traffic_near_major == "[1e+03,5e+03)" ~ 4,
    traffic_near_major == "[5e+03,1e+04)" ~ 5,
    traffic_near_major == "[1e+04,5e+04)" ~ 6,
    traffic_near_major == "[5e+04,1e+05)" ~ 7,
    traffic_near_major == "[1e+05,5e+05]" ~ 8,
  ))

female_test <- female_test %>%
  mutate(traffic_near_major = case_when(
    traffic_near_major == "[0,100)" ~ 1,
    traffic_near_major == "[100,500)" ~ 2,
    traffic_near_major == "[500,1e+03)" ~ 3,
    traffic_near_major == "[1e+03,5e+03)" ~ 4,
    traffic_near_major == "[5e+03,1e+04)" ~ 5,
    traffic_near_major == "[1e+04,5e+04)" ~ 6,
    traffic_near_major == "[5e+04,1e+05)" ~ 7,
    traffic_near_major == "[1e+05,5e+05]" ~ 8,
  ))

# dist to major road
male_train <- male_train %>%
  mutate(dist_major_road = case_when(
    dist_major_road == "[0,0.001)" ~ 1,
    dist_major_road == "[0.001,0.005)" ~ 2,
    dist_major_road == "[0.005,0.01)" ~ 3,
    dist_major_road == "[0.01,0.05)" ~ 4,
    dist_major_road == "[0.05,0.1)" ~ 5,
    dist_major_road == "[0.1,0.5)" ~ 6,
    dist_major_road == "[0.5,1]" ~ 7,
  ))

male_test <- male_test %>%
  mutate(dist_major_road = case_when(
    dist_major_road == "[0,0.001)" ~ 1,
    dist_major_road == "[0.001,0.005)" ~ 2,
    dist_major_road == "[0.005,0.01)" ~ 3,
    dist_major_road == "[0.01,0.05)" ~ 4,
    dist_major_road == "[0.05,0.1)" ~ 5,
    dist_major_road == "[0.1,0.5)" ~ 6,
    dist_major_road == "[0.5,1]" ~ 7,
  ))

female_train <- female_train %>%
  mutate(dist_major_road = case_when(
    dist_major_road == "[0,0.001)" ~ 1,
    dist_major_road == "[0.001,0.005)" ~ 2,
    dist_major_road == "[0.005,0.01)" ~ 3,
    dist_major_road == "[0.01,0.05)" ~ 4,
    dist_major_road == "[0.05,0.1)" ~ 5,
    dist_major_road == "[0.1,0.5)" ~ 6,
    dist_major_road == "[0.5,1]" ~ 7,
  ))

female_test <- female_test %>%
  mutate(dist_major_road = case_when(
    dist_major_road == "[0,0.001)" ~ 1,
    dist_major_road == "[0.001,0.005)" ~ 2,
    dist_major_road == "[0.005,0.01)" ~ 3,
    dist_major_road == "[0.01,0.05)" ~ 4,
    dist_major_road == "[0.05,0.1)" ~ 5,
    dist_major_road == "[0.1,0.5)" ~ 6,
    dist_major_road == "[0.5,1]" ~ 7,
  ))

# Tot_traffic_major
male_train <- male_train %>%
  mutate(tot_traffic_major = case_when(
    tot_traffic_major == "[0,1e+03)" ~ 1,
    tot_traffic_major == "[1e+03,1e+06)" ~ 2,
    tot_traffic_major == "[1e+06,1e+07]" ~ 3,
    ))

male_test <- male_test %>%
  mutate(tot_traffic_major = case_when(
    tot_traffic_major == "[0,1e+03)" ~ 1,
    tot_traffic_major == "[1e+03,1e+06)" ~ 2,
    tot_traffic_major == "[1e+06,1e+07]" ~ 3,
  ))

female_train <- female_train %>%
  mutate(tot_traffic_major = case_when(
    tot_traffic_major == "[0,1e+03)" ~ 1,
    tot_traffic_major == "[1e+03,1e+06)" ~ 2,
    tot_traffic_major == "[1e+06,1e+07]" ~ 3,
  ))

female_test <- female_test %>%
  mutate(tot_traffic_major = case_when(
    tot_traffic_major == "[0,1e+03)" ~ 1,
    tot_traffic_major == "[1e+03,1e+06)" ~ 2,
    tot_traffic_major == "[1e+06,1e+07]" ~ 3,
  ))

# Save new dataframes
saveRDS(male_train, file = "male_train_data_1504.rds")
saveRDS(male_test, file = "male_test_data_1504.rds")
saveRDS(female_train, file = "female_train_data_1504.rds")
saveRDS(female_test, file = "female_test_data_1504.rds")
