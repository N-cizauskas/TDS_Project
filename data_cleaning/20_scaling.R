# set wd
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA")


# scaling one hot encoded data

female_train1 <- readRDS("female_train_1604_scaled.rds")
female_test1 <- readRDS("female_test_1604_scaled.rds")
male_train1 <- readRDS("male_train_1604_scaled.rds")
male_test1 <- readRDS("male_test_1604_scaled.rds")


### FEMALE TRAIN

## drop extra column

df1_not_in_df2 <- setdiff(names(female_train), names(female_test))
cat("Columns in df1 but not in df2: ", df1_not_in_df2, "\n")

library(dplyr)

female_train <- select(female_train, -c(age_last_cont..10.15.))


cols_to_scale <- 1:129
means_femaletrain <- apply(female_train[ , 1:128], 2, mean)
sds_femaletrain <- apply(female_train[ , 1:128], 2, sd)

female_test[ , 1:128] <- data.frame(scale(female_test[ , 1:128], center = means_femaletrain, scale = sds_femaletrain))
female_train[ , 1:128] <- data.frame(scale(female_train[ , 1:128], center = means_femaletrain, scale = sds_femaletrain))

means_femaletrain2 <- apply(female_train[ , 129:183], 2, mean)
sds_femaletrain2 <- apply(female_train[ , 129:183], 2, sd)
female_train[ , 129:183] <- data.frame(scale(female_train[ , 129:183], center = means_femaletrain2, scale = sds_femaletrain2))
female_test[ , 129:183] <- data.frame(scale(female_test[ , 129:183], center = means_femaletrain2, scale = sds_femaletrain2))


### MALE TRAIN
means_maletrain <- apply(male_train[ , 1:101], 2, mean)
sds_maletrain <- apply(male_train[ , 1:101], 2, sd)

male_test[ , 1:101] <- data.frame(scale(male_test[ , 1:101], center = means_maletrain, scale = sds_maletrain))
male_train[ , 1:101] <- data.frame(scale(male_train[ , 1:101], center = means_maletrain, scale = sds_maletrain))

means_maletrain2 <- apply(male_train[ , 102:154], 2, mean)
sds_maletrain2 <- apply(male_train[ , 102:154], 2, sd)
male_train[ , 102:154] <- data.frame(scale(male_train[ , 102:154], center = means_maletrain2, scale = sds_maletrain2))
male_test[ , 102:154] <- data.frame(scale(male_test[ , 102:154], center = means_maletrain2, scale = sds_maletrain2))

female_train$case <- female_train1$case
female_test$case <- female_test1$case
male_train$case <- male_train1$case
male_test$case <- male_test1$case


setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA")
saveRDS(female_train, file = "female_train.rds")
saveRDS(female_test, file = "female_test.rds")
saveRDS(male_train, file = "male_train.rds")
saveRDS(male_test, file = "male_test.rds")

write.csv(male_test, file = "male_test_scaled.csv", row.names=TRUE)
write.csv(male_train, file = "male_train_scaled.csv", row.names=TRUE)
write.csv(female_test, file = "female_test_scaled.csv", row.names=TRUE)
write.csv(female_train, file = "female_train_scaled.csv", row.names=TRUE)



