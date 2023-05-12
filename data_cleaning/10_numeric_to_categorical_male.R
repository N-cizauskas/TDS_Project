setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


male_1003 <- readRDS("cleaned_male_1003.rds")

## num in house threshold
sum(is.na(male_1003$num_in_house))
male_1003 %>% count(num_in_house)
male_1003$num_in_house <- ifelse(male_1003$num_in_house > 9, "10+", male_1003$num_in_house)
male_1003$num_in_house[male_1003$num_in_house == -1] <- "Do not know"
male_1003$num_in_house[male_1003$num_in_house == -3] <- "Prefer not to answer"

## sleep duration
male_1003 %>% count(sleep_duration)
male_1003$sleep_duration <- ifelse(male_1003$sleep_duration > 10, "11+", male_1003$sleep_duration)
male_1003$sleep_duration[male_1003$sleep_duration == -1] <- "Do not know"
male_1003$sleep_duration[male_1003$sleep_duration == -3] <- "Prefer not to answer"

## accomodation
male_1003 %>% count(accommodation)
male_1003$accommodation[male_1003$accommodation == 1] <- "A house or bungalow"
male_1003$accommodation[male_1003$accommodation == 2] <- "A flat, maisonette or apartment"
male_1003$accommodation[male_1003$accommodation == 3] <- "Mobile or temporary structure"
male_1003$accommodation[male_1003$accommodation == 4] <- "Sheltered accommodation"
male_1003$accommodation[male_1003$accommodation == 5] <- "Care home"
male_1003$accommodation[male_1003$accommodation == -7] <- "None of the above"
male_1003$accommodation[male_1003$accommodation == -3] <- "Prefer not to answer"

## own or rent
male_1003 %>% count(own_or_rent)
male_1003$own_or_rent[male_1003$own_or_rent == 1] <- "Own outright"
male_1003$own_or_rent[male_1003$own_or_rent == 2] <- "Own with a mortgage"
male_1003$own_or_rent[male_1003$own_or_rent == 3] <- "Rent - from local authority, local council, housing association"
male_1003$own_or_rent[male_1003$own_or_rent == 4] <- "Rent - from private landlord or letting agency"
male_1003$own_or_rent[male_1003$own_or_rent == 5] <- "Pay part rent and part mortgage (shared ownership)"
male_1003$own_or_rent[male_1003$own_or_rent == 6] <- "Live in accommodation rent free"
male_1003$own_or_rent[male_1003$own_or_rent == -7] <- "None of the above"
male_1003$own_or_rent[male_1003$own_or_rent == -3] <- "Prefer not to answer"

## avg household income
male_1003 %>% count(avg_house_income)
male_1003$avg_house_income[male_1003$avg_house_income == 1] <- "Less than 18,000"
male_1003$avg_house_income[male_1003$avg_house_income == 2] <- "18,000 to 30,999"
male_1003$avg_house_income[male_1003$avg_house_income == 3] <- "31,000 to 51,999"
male_1003$avg_house_income[male_1003$avg_house_income == 4] <- "52,000 to 100,000"
male_1003$avg_house_income[male_1003$avg_house_income == 5] <- "Greater than 100,000"
male_1003$avg_house_income[male_1003$avg_house_income == -1] <- "Do not know"
male_1003$avg_house_income[male_1003$avg_house_income == -3] <- "Prefer not to answer"

## smoke household
male_1003 %>% count(smoke_household)
male_1003$smoke_household[male_1003$smoke_household == 1] <- "Yes, one household member smokes"
male_1003$smoke_household[male_1003$smoke_household == 2] <- "Yes, more than one household member smokes"
male_1003$smoke_household[male_1003$smoke_household == 0] <- "No"
male_1003$smoke_household[male_1003$smoke_household == -3] <- "Prefer not to answer"

## breastfed
male_1003 %>% count(breastfed)
male_1003$breastfed[male_1003$breastfed == 1] <- "Yes"
male_1003$breastfed[male_1003$breastfed == 0] <- "No"
male_1003$breastfed[male_1003$breastfed == -1] <- "Do not know"
male_1003$breastfed[male_1003$breastfed == -3] <- "Prefer not to answer"

## other medical conditions
male_1003 %>% count(other_medcond)
male_1003$other_medcond[male_1003$other_medcond == 1] <- "Yes"
male_1003$other_medcond[male_1003$other_medcond == 0] <- "No"
male_1003$other_medcond[male_1003$other_medcond == -1] <- "Do not know"
male_1003$other_medcond[male_1003$other_medcond == -3] <- "Prefer not to answer"

## for loop based on coding 100579

my_list <- c(1, 0, 2, 3, -3)

for (i in names(male_1003)) {
  if (all(male_1003[[i]] %in% my_list)) {
    print(i)
  }
}


## smoke_status
male_1003 %>% count(Smoke_status)

replacement_90 <- c("Never", "Previous", "Current", "Prefer not to answer")
my_list2 <- c(0, 1, 2, -3)

column_name2 <- "Smoke_status"
male_1003[[column_name2]] <- ifelse(male_1003[[column_name2]] %in% my_list2, 
                                      replacement_90[match(male_1003[[column_name2]], my_list2)], 
                                      male_1003[[column_name2]])


### alc status
male_1003 %>% count(Alcohol_status)
column_name3 <- "Alcohol_status"
male_1003[[column_name3]] <- ifelse(male_1003[[column_name3]] %in% my_list2, 
                                      replacement_90[match(male_1003[[column_name3]], my_list2)], 
                                      male_1003[[column_name3]])

## ever_smoked
male_1003 %>% count(Ever_smoked)
male_1003$Ever_smoked[male_1003$Ever_smoked == 0] <- "No"
male_1003$Ever_smoked[male_1003$Ever_smoked == 1] <- "Yes"


### ethnicity
male_1003 %>% count(ethnicity)
male_1003$ethnicity[male_1003$ethnicity == -3] <- "Prefer not to answer"
male_1003$ethnicity[male_1003$ethnicity == -1] <- "Do not know"
male_1003$ethnicity[male_1003$ethnicity == 1] <- "White"
male_1003$ethnicity[male_1003$ethnicity == 2] <- "Mixed"
male_1003$ethnicity[male_1003$ethnicity == 3] <- "Asian or Asian British"
male_1003$ethnicity[male_1003$ethnicity == 4] <- "Black or Black British"
male_1003$ethnicity[male_1003$ethnicity == 5] <- "Chinese"
male_1003$ethnicity[male_1003$ethnicity == 6] <- "Other ethnic group"
male_1003$ethnicity[male_1003$ethnicity == 1001] <- "White"
male_1003$ethnicity[male_1003$ethnicity == 1002] <- "White"
male_1003$ethnicity[male_1003$ethnicity == 1003] <- "White"
male_1003$ethnicity[male_1003$ethnicity == 2001] <- "Mixed"
male_1003$ethnicity[male_1003$ethnicity == 2002] <- "Mixed"
male_1003$ethnicity[male_1003$ethnicity == 2003] <- "Mixed"
male_1003$ethnicity[male_1003$ethnicity == 2004] <- "Mixed"
male_1003$ethnicity[male_1003$ethnicity == 3001] <- "Asian or Asian British"
male_1003$ethnicity[male_1003$ethnicity == 3002] <- "Asian or Asian British"
male_1003$ethnicity[male_1003$ethnicity == 3003] <- "Asian or Asian British"
male_1003$ethnicity[male_1003$ethnicity == 3004] <- "Asian or Asian British"
male_1003$ethnicity[male_1003$ethnicity == 4001] <- "Black or Black British"
male_1003$ethnicity[male_1003$ethnicity == 4002] <- "Black or Black British"
male_1003$ethnicity[male_1003$ethnicity == 4003] <- "Black or Black British"


### close_major_road
male_1003$close_major_road[male_1003$close_major_road == 0] <- "No"
male_1003$close_major_road[male_1003$close_major_road == 1] <- "Yes"


## stress_factor
list_100502 <- c(1, 2, 3, 4, 5, 6, -7, -3)
replacement_100502<- c("Serious illness, injury or assault to yourself", "Serious illness, injury or assault of a close relative", "Death of a close relative", "Death of a spouse or partner", "Marital separation/divorce", "Financial difficulties", "None of the above", "Prefer not to answer")

stress_factor <- "stress_factor"

male_1003[["stress_factor"]] <- ifelse(male_1003[["stress_factor"]] %in% list_100502,
                                         replacement_100502[match(male_1003[["stress_factor"]], list_100502)], 
                                         male_1003[["stress_factor"]])

male_1003 %>% count(stress_factor)


## education level
male_1003$Education_level[male_1003$Education_level == 1] <- "College or University degree"
male_1003$Education_level[male_1003$Education_level == 2] <- "A levels/AS levels or equivalent"
male_1003$Education_level[male_1003$Education_level == 3] <- "O levels/GCSEs or equivalent"
male_1003$Education_level[male_1003$Education_level == 4] <- "CSEs or equivalent"
male_1003$Education_level[male_1003$Education_level == 5] <- "NVQ or HND or HNC or equivalent"
male_1003$Education_level[male_1003$Education_level == 6] <- "Other professional qualifications"
male_1003$Education_level[male_1003$Education_level == -7] <- "None of the above"
male_1003$Education_level[male_1003$Education_level == -3] <- "Prefer not to answer"
male_1003 %>% count(Education_level)

## Family_history
replacement_1010 <- c("Prostate cancer",
                      "Severe depression",
                      "Parkinson's disease",
                      "Alzheimer's disease/dementia",
                      "Diabetes",
                      "High blood pressure",
                      "Chronic bronchitis/emphysema",
                      "Breast cancer",
                      "Bowel cancer",
                      "Lung cancer",
                      "Stroke",
                      "Heart disease",
                      "Do not know",
                      "Prefer not to answer",
                      "None of the above")
list_1010 <- c(13, 12, 11, 10, 9, 8, 6, 5, 4, 3, 2, 1, -11, -13, -17)

## father
male_1003 %>% count(Family_history_father)
male_1003[["Family_history_father"]] <- ifelse(male_1003[["Family_history_father"]] %in% list_1010,
                                                 replacement_1010[match(male_1003[["Family_history_father"]], list_1010)], 
                                                 male_1003[["Family_history_father"]])

## mother
male_1003 %>% count(Family_history_mother)
male_1003[["Family_history_mother"]] <- ifelse(male_1003[["Family_history_mother"]] %in% list_1010,
                                                 replacement_1010[match(male_1003[["Family_history_mother"]], list_1010)], 
                                                 male_1003[["Family_history_mother"]])


### urban vs rural
male_1003 %>% count(urban_rural)
replacement_91 <- c("England/Wales - Urban - sparse",
                    "England/Wales - Town and Fringe - sparse",
                    "England/Wales - Village - sparse",
                    "England/Wales - Hamlet and Isolated dwelling - sparse",
                    "England/Wales - Urban - less sparse",
                    "England/Wales - Town and Fringe - less sparse",
                    "England/Wales - Village - less sparse",
                    "England/Wales - Hamlet and Isolated Dwelling - less sparse",
                    "Postcode not linkable",
                    "Scotland - Large Urban Area",
                    "Scotland - Other Urban Area",
                    "Scotland - Accessible Small Town",
                    "Scotland - Remote Small Town",
                    "Scotland - Very Remote Small Town",
                    "Scotland - Accessible Rural",
                    "Scotland - Remote Rural",
                    "Scotland - Very Remote Rural")
list_91 <- c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18)
male_1003[["urban_rural"]] <- ifelse(male_1003[["urban_rural"]] %in% list_91,
                                       replacement_91[match(male_1003[["urban_rural"]], list_91)], 
                                       male_1003[["urban_rural"]])


## nitrogen dioxide means
male_1003$nit_dioxide_mean <- apply(male_1003[, 43:45], 1, mean)
male_1003$nit_dioxide_mean
sum(is.na(male_1003$nit_dioxide_mean))

## traffic near road
male_1003$traffic_near_road <- cut(male_1003$traffic_near_road, breaks = c(0, 100, 1000, 10000, 100000, 200000), include.lowest = TRUE, right = FALSE)

male_1003 %>% count(traffic_near_road)

## traffic near major

plot(male_1003$traffic_near_major)

male_1003$traffic_near_major <- cut(male_1003$traffic_near_major, breaks = c(0, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000), include.lowest = TRUE, right = FALSE)

male_1003 %>% count(traffic_near_major)

## dist major road

male_1003$dist_major_road <- cut(male_1003$dist_major_road, breaks = c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1), include.lowest = TRUE, right = FALSE)

male_1003 %>% count(dist_major_road)

plot(male_1003$dist_major_road)

## tot major traffic

summary(male_1003$tot_traffic_major)
male_1003$tot_traffic_major <- cut(male_1003$tot_traffic_major, breaks = c(0, 1000, 1000000, 10000000), include.lowest = TRUE, right = FALSE)

male_1003 %>% count(tot_traffic_major)
plot(male_1003$tot_traffic_major)

## drop columns

male_1003 <- select(male_1003, -c("non_cancer_year", "cancer_docdiag", "nit_dioxide_2005", "nit_dioxide_2006", "nit_dioxide_2007", "noise_pollution_eve", "noise_pollution_night"))


## drop rows --> urban and rural
male_1003 <- subset(male_1003, urban_rural != 4 | is.na(urban_rural))
male_1003 <- subset(male_1003, urban_rural != 2 | is.na(urban_rural))
male_1003 <- subset(male_1003, urban_rural != 1 | is.na(urban_rural))
male_1003 <- subset(male_1003, urban_rural != 18 | is.na(urban_rural))
male_1003 <- subset(male_1003, urban_rural != 9 | is.na(urban_rural))

male_1003 %>% count(urban_rural)

saveRDS(male_1003, file = "male_1603.rds")

### FEMALE

female_1003 <- readRDS("cleaned_female_1003.rds")

## nitrogen dioxide means
female_1003$nit_dioxide_mean <- apply(female_1003[, 48:50], 1, mean)
female_1003$nit_dioxide_femean
sum(is.na(female_1003$nit_dioxide_mean))

## drop columns

female_1003 <- select(female_1003, -c("non_cancer_year", "cancer_docdiag", "nit_dioxide_2005", "nit_dioxide_2006", "nit_dioxide_2007", "noise_pollution_eve", "noise_pollution_night"))

female_1003 %>% count(urban_rural)
## drop rows --> urban and rural
female_1003 <- subset(female_1003, urban_rural != 4 | is.na(urban_rural))
female_1003 <- subset(female_1003, urban_rural != 2 | is.na(urban_rural))
female_1003 <- subset(female_1003, urban_rural != 1 | is.na(urban_rural))
female_1003 <- subset(female_1003, urban_rural != 18 | is.na(urban_rural))
female_1003 <- subset(female_1003, urban_rural != 9 | is.na(urban_rural))
female_1003 <- subset(female_1003, urban_rural != 14 | is.na(urban_rural))

saveRDS(female_1003, file = "female_1603.rds")

