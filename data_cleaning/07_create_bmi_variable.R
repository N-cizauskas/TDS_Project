# Create BMI variable from Standing height and weight for male and female data

setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")
library(dplyr)

# Load data
ukb_male <- readRDS("male_2402.rds")
ukb_female <- readRDS("female_2402.rds")

# Number of missing data for weight variable 
sum(is.na(ukb_male$weight)) ## 1178 males
sum(is.na(ukb_female$weight)) ## 1119 females 

# Number of missing data for height variable 
sum(is.na(ukb_male$Standing_height)) ## 1174 males
sum(is.na(ukb_female$Standing_height)) ## 954 females

# Number of missing data for bmi variable
sum(is.na(ukb_male$bmi)) # 176634 males
sum(is.na(ukb_female$bmi)) # 216263

# Reassign bmi column with new BMI calculated from height and weight (BMI = kg/m^2) 
ukb_male$bmi <- ukb_male$weight / (ukb_male$Standing_height/100)^2 #BMI for males
ukb_female$bmi <- ukb_female$weight / (ukb_female$Standing_height/100)^2 #BMI for females

# Number of missing data for BMI variable
sum(is.na(ukb_male$bmi)) # 1385 males
sum(is.na(ukb_female$bmi)) # 1196 females

# Compare distribution of previous bmi and new BMI variable for males 
par(mfrow = c(1,2))
hist(ukb_male$bmi) # previous
hist(ukb_male$BMI) # new


# Compare distribution of previous bmi and new BMI variable for females 
par(mfrow = c(1,2))
hist(ukb_female$bmi) # previous 
hist(ukb_female$BMI) # new

# Drop BMI variable created earlier, but has now been replaced with bmi
ukb_female <- select(ukb_female, -BMI)
ukb_male <- select(ukb_male, -BMI)

# Save new datasets
saveRDS(ukb_male, file = "male_2802.rds")
saveRDS(ukb_female, file = "female_2802.rds")

boxplot(ukb_female$Seated_height)
boxplot(ukb_male$Seated_height)

boxplot(ukb_female$Standing_height)
boxplot(ukb_male$Standing_height)