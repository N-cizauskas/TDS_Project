## more cleaning

## date.recr.x
final_female_train_2403 <- final_female_train_2403[ , -7]
  
## num_in_house --> make all 10f+ into 10; PNTA, DNK - NA
final_female_train_2403$num_in_house[final_female_train_2403$num_in_house == "10+"] <- 10
final_female_train_2403$num_in_house[final_female_train_2403$num_in_house == "Prefer not to answer"] <- NA
final_female_train_2403$num_in_house[final_female_train_2403$num_in_house == "Do not know"] <- NA
final_female_train_2403 %>% count(num_in_house)
final_female_train_2403$num_in_house <- as.numeric(final_female_train_2403$num_in_house)
class(final_female_train_2403$num_in_house)

## sleep_duration 11+ change and then impute
final_female_train_2403 %>% count(sleep_duration)
final_female_train_2403$sleep_duration[final_female_train_2403$sleep_duration == "11+"] <- 11
final_female_train_2403$sleep_duration[final_female_train_2403$sleep_duration == 1] <- 3
final_female_train_2403$sleep_duration[final_female_train_2403$sleep_duration == 2] <- 3
final_female_train_2403$sleep_duration[final_female_train_2403$sleep_duration == "Prefer not to answer"] <- NA
final_female_train_2403$sleep_duration[final_female_train_2403$sleep_duration == "Do not know"] <- NA
class(final_female_train_2403$sleep_duration)
final_female_train_2403$sleep_duration <- as.numeric(final_female_train_2403$sleep_duration)

## age_period start - impute
final_female_train_2403 %>% count(age_period_start)
final_female_train_2403$age_period_start[final_female_train_2403$age_period_start == "Prefer not to answer"] <- NA
final_female_train_2403$age_period_start[final_female_train_2403$age_period_start == "Do not know"] <- NA
class(final_female_train_2403$age_period_start)
final_female_train_2403$age_period_start <- as.numeric(final_female_train_2403$age_period_start)



### num_live_births --> 6+; impute only prefer not to answer
final_female_train_2403$number_live_births[final_female_train_2403$number_live_births == "6+"] <- 6
final_female_train_2403 %>% count(number_live_births)
final_female_train_2403$number_live_births[final_female_train_2403$number_live_births == "Prefer not to answer"] <- NA
class(final_female_train_2403$number_live_births)
final_female_train_2403$number_live_births <- as.numeric(final_female_train_2403$number_live_births)

## age first cont
final_female_train_2403 %>% count(age_first_cont)
final_female_train_2403$age_first_cont[final_female_train_2403$age_first_cont == "Prefer not to answer"] <- NA
final_female_train_2403$age_first_cont[final_female_train_2403$age_first_cont == "Do not known"] <- NA
final_female_train_2403$age_first_cont <- as.numeric(final_female_train_2403$age_first_cont)
final_female_train_2403$age_first_cont <- cut(final_female_train_2403$age_first_cont, breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55), include.lowest = TRUE, right = FALSE)
sum(is.na(final_female_train_2403$age_first_cont))

## age last cont --> bin every 5 years
final_female_train_2403 %>% count(age_last_cont)
final_female_train_2403$age_last_cont[final_female_train_2403$age_last_cont == "Prefer not to answer"] <- NA
final_female_train_2403$age_last_cont[final_female_train_2403$age_last_cont == "Do not know"] <- NA
final_female_train_2403$age_last_cont <- cut(final_female_train_2403$age_last_cont, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55), include.lowest = TRUE, right = FALSE)
final_female_train_2403$age_last_cont[final_female_train_2403$age_last_cont == "Still taking"] <- 0
final_female_train_2403$age_last_cont<- as.numeric(final_female_train_2403$age_last_cont)
class(final_female_train_2403$age_last_cont)

#final_female_train_2403$age_last_cont[final_female_train_2403$age_last_cont == "[0,5)"] <- "Still taking"

## record alcohol status
final_female_train_2403 %>% count(Alcohol_status)
final_female_train_2403$Alcohol_status[final_female_train_2403$Alcohol_status == -3] <- "Prefer not to answer"
final_female_train_2403$Alcohol_status[final_female_train_2403$Alcohol_status == 0] <- "Never"
final_female_train_2403$Alcohol_status[final_female_train_2403$Alcohol_status == 1] <- "Previous"
final_female_train_2403$Alcohol_status[final_female_train_2403$Alcohol_status == 2] <- "Current"

##FEMALE TEST

#final_femaletest_2203

## date.recr.x
final_femaletest_2203 <- final_femaletest_2203[ , -7]

## num_in_house --> make all 10f+ into 10; PNTA, DNK - NA
final_femaletest_2203$num_in_house[final_femaletest_2203$num_in_house == "10+"] <- 10
final_femaletest_2203$num_in_house[final_femaletest_2203$num_in_house == "Prefer not to answer"] <- NA
final_femaletest_2203$num_in_house[final_femaletest_2203$num_in_house == "Do not know"] <- NA
final_femaletest_2203 %>% count(num_in_house)
final_femaletest_2203$num_in_house <- as.numeric(final_femaletest_2203$num_in_house)
class(final_femaletest_2203$num_in_house)

## sleep_duration 11+ change and then impute
final_femaletest_2203 %>% count(sleep_duration)
final_femaletest_2203$sleep_duration[final_femaletest_2203$sleep_duration == "11+"] <- 11
final_femaletest_2203$sleep_duration[final_femaletest_2203$sleep_duration == 1] <- 3
final_femaletest_2203$sleep_duration[final_femaletest_2203$sleep_duration == 2] <- 3
final_femaletest_2203$sleep_duration[final_femaletest_2203$sleep_duration == "Prefer not to answer"] <- NA
final_femaletest_2203$sleep_duration[final_femaletest_2203$sleep_duration == "Do not know"] <- NA
class(final_femaletest_2203$sleep_duration)
final_femaletest_2203$sleep_duration <- as.numeric(final_femaletest_2203$sleep_duration)

## age_period start - impute
final_femaletest_2203 %>% count(age_period_start)
final_femaletest_2203$age_period_start[final_femaletest_2203$age_period_start == "Prefer not to answer"] <- NA
final_femaletest_2203$age_period_start[final_femaletest_2203$age_period_start == "Do not know"] <- NA
class(final_femaletest_2203$age_period_start)
final_femaletest_2203$age_period_start <- as.numeric(final_femaletest_2203$age_period_start)


### num_live_births --> 6+; impute only prefer not to answer
final_femaletest_2203$number_live_births[final_femaletest_2203$number_live_births == "6+"] <- 6
final_femaletest_2203 %>% count(number_live_births)
final_femaletest_2203$number_live_births[final_femaletest_2203$number_live_births == "Prefer not to answer"] <- NA
class(final_femaletest_2203$number_live_births)
final_femaletest_2203$number_live_births <- as.numeric(final_femaletest_2203$number_live_births)

## age first cont
final_femaletest_2203 %>% count(age_first_cont)
final_femaletest_2203$age_first_cont[final_femaletest_2203$age_first_cont == "Prefer not to answer"] <- NA
final_femaletest_2203$age_first_cont[final_femaletest_2203$age_first_cont == "Do not known"] <- NA
final_femaletest_2203$age_first_cont <- as.numeric(final_femaletest_2203$age_first_cont)
final_femaletest_2203$age_first_cont <- cut(final_femaletest_2203$age_first_cont, breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55), include.lowest = TRUE, right = FALSE)
sum(is.na(final_femaletest_2203$age_first_cont))

## age last cont --> bin every 5 years
final_femaletest_2203 %>% count(age_last_cont)
final_femaletest_2203$age_last_cont[final_femaletest_2203$age_last_cont == "Prefer not to answer"] <- NA
final_femaletest_2203$age_last_cont[final_femaletest_2203$age_last_cont == "Do not know"] <- NA
final_femaletest_2203$age_last_cont <- cut(final_femaletest_2203$age_last_cont, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55), include.lowest = TRUE, right = FALSE)
final_femaletest_2203$age_last_cont[final_femaletest_2203$age_last_cont == "Still taking"] <- 0
final_femaletest_2203$age_last_cont<- as.numeric(final_femaletest_2203$age_last_cont)
class(final_femaletest_2203$age_last_cont)

#final_female_train_2403$age_last_cont[final_female_train_2403$age_last_cont == "[0,5)"] <- "Still taking"

## record alcohol status
final_femaletest_2203 %>% count(Alcohol_status)
final_femaletest_2203$Alcohol_status[final_femaletest_2203$Alcohol_status == -3] <- "Prefer not to answer"
final_femaletest_2203$Alcohol_status[final_femaletest_2203$Alcohol_status == 0] <- "Never"
final_femaletest_2203$Alcohol_status[final_femaletest_2203$Alcohol_status == 1] <- "Previous"
final_femaletest_2203$Alcohol_status[final_femaletest_2203$Alcohol_status == 2] <- "Current"

## saveRDS
saveRDS(final_female_train_2403, file = "toimpute_femaletrain.rds")
saveRDS(final_femaletest_2203, file = "toimpute_femaletest.rds")
