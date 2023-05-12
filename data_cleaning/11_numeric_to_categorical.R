library(dplyr)

### accommodation
female_train_all_data %>% count(accommodation)
female_train_all_data$accommodation[female_train_all_data$accommodation == 1] <- "A house or bungalow"
female_train_all_data$accommodation[female_train_all_data$accommodation == 2] <- "A flat, maisonette or apartment"
female_train_all_data$accommodation[female_train_all_data$accommodation == 3] <- "Mobile or temporary structure"
female_train_all_data$accommodation[female_train_all_data$accommodation == 4] <- "Sheltered accommodation"
female_train_all_data$accommodation[female_train_all_data$accommodation == 5] <- "Care home"
female_train_all_data$accommodation[female_train_all_data$accommodation == -7] <- "None of the above"
female_train_all_data$accommodation[female_train_all_data$accommodation == -3] <- "Prefer not to answer"

## own or rent
female_train_all_data %>% count(own_or_rent)
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 1] <- "Own outright"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 2] <- "Own with a mortgage"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 3] <- "Rent - from local authority, local council, housing association"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 4] <- "Rent - from private landlord or letting agency"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 5] <- "Pay part rent and part mortgage (shared ownership)"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == 6] <- "Live in accommodation rent free"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == -7] <- "None of the above"
female_train_all_data$own_or_rent[female_train_all_data$own_or_rent == -3] <- "Prefer not to answer"

## avg household income
female_train_all_data %>% count(avg_house_income)
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == 1] <- "Less than 18,000"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == 2] <- "18,000 to 30,999"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == 3] <- "31,000 to 51,999"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == 4] <- "52,000 to 100,000"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == 5] <- "Greater than 100,000"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == -1] <- "Do not know"
female_train_all_data$avg_house_income[female_train_all_data$avg_house_income == -3] <- "Prefer not to answer"

## num in household
female_train_all_data %>% count(num_in_house)
female_train_all_data$num_in_house[female_train_all_data$num_in_house == -1] <- "Do not know"
female_train_all_data$num_in_house[female_train_all_data$num_in_house == -3] <- "Prefer not to answer"

## sleep duration
female_train_all_data %>% count(sleep_duration)
female_train_all_data$sleep_duration[female_train_all_data$sleep_duration == -1] <- "Do not know"
female_train_all_data$sleep_duration[female_train_all_data$sleep_duration == -3] <- "Prefer not to answer"

## smoke household
female_train_all_data %>% count(smoke_household)
female_train_all_data$smoke_household[female_train_all_data$smoke_household == 1] <- "Yes, one household member smokes"
female_train_all_data$smoke_household[female_train_all_data$smoke_household == 2] <- "Yes, more than one household member smokes"
female_train_all_data$smoke_household[female_train_all_data$smoke_household == 0] <- "No"
female_train_all_data$smoke_household[female_train_all_data$smoke_household == -3] <- "Prefer not to answer"

## breastfed
female_train_all_data %>% count(breastfed)
female_train_all_data$breastfed[female_train_all_data$breastfed == 1] <- "Yes"
female_train_all_data$breastfed[female_train_all_data$breastfed == 0] <- "No"
female_train_all_data$breastfed[female_train_all_data$breastfed == -1] <- "Do not know"
female_train_all_data$breastfed[female_train_all_data$breastfed == -3] <- "Prefer not to answer"

## cancer diagnosed by doctor
female_train_all_data %>% count(cancer_docdiag)
female_train_all_data$cancer_docdiag[female_train_all_data$cancer_docdiag == 1] <- "Yes"
female_train_all_data$cancer_docdiag[female_train_all_data$cancer_docdiag == 0] <- "No"
female_train_all_data$cancer_docdiag[female_train_all_data$cancer_docdiag == -1] <- "Do not know"
female_train_all_data$cancer_docdiag[female_train_all_data$cancer_docdiag == -3] <- "Prefer not to answer"

## other medical conditions
female_train_all_data %>% count(other_medcond)
female_train_all_data$other_medcond[female_train_all_data$other_medcond == 1] <- "Yes"
female_train_all_data$other_medcond[female_train_all_data$other_medcond == 0] <- "No"
female_train_all_data$other_medcond[female_train_all_data$other_medcond == -1] <- "Do not know"
female_train_all_data$other_medcond[female_train_all_data$other_medcond == -3] <- "Prefer not to answer"

## age period start
female_train_all_data %>% count(age_period_start)
female_train_all_data$age_period_start[female_train_all_data$age_period_start == -1] <- "Do not know"
female_train_all_data$age_period_start[female_train_all_data$age_period_start == -3] <- "Prefer not to answer"

## for loop based on coding 100579

my_list <- c(1, 0, 2, 3, -3)

for (i in names(female_train_all_data)) {
  if (all(female_train_all_data[[i]] %in% my_list)) {
    print(i)
  }
}

## "menopause" "Smoke_status" "Alcohol_status" "Ever_smoked" "close_major_road" "case" "prevalent_case"

### menopause
replacement_100579 <- c("Yes", "No", "Not sure - had a hysterectomy", "Not sure - other reason", "Prefer not to answer")

column_name <- "menopause"
female_train_all_data[[column_name]] <- ifelse(female_train_all_data[[column_name]] %in% my_list, 
                            replacement_100579[match(female_train_all_data[[column_name]], my_list)], 
                            female_train_all_data[[column_name]])


female_train_all_data %>% count(menopause)

### smoke_status

female_train_all_data %>% count(Smoke_status)

replacement_90 <- c("Never", "Previous", "Current", "Prefer not to answer")
my_list2 <- c(0, 1, 2, -3)

column_name2 <- "Smoke_status"
female_train_all_data[[column_name2]] <- ifelse(female_train_all_data[[column_name2]] %in% my_list2, 
                                               replacement_90[match(female_train_all_data[[column_name2]], my_list2)], 
                                               female_train_all_data[[column_name2]])


### alc status
female_train_all_data %>% count(Alcohol_status)
column_name3 <- "Alcohol_status"
female_train_all_data[[column_name3]] <- ifelse(female_train_all_data[[column_name3]] %in% my_list2, 
                                                replacement_90[match(female_train_all_data[[column_name3]], my_list2)], 
                                                female_train_all_data[[column_name3]])


### Ever_smoked
female_train_all_data %>% count(Ever_smoked)
female_train_all_data$Ever_smoked[female_train_all_data$Ever_smoked == 0] <- "No"
female_train_all_data$Ever_smoked[female_train_all_data$Ever_smoked == 1] <- "Yes"

### number live births

number_live_births <- "number_live_births"

# Define the threshold number
threshold <- 5

# Define the replacement value
replacement1 <- "5+"

female_train_all_data[[number_live_births]] <- ifelse(female_train_all_data[[number_live_births]] > threshold, 
                            replacement1, 
                            female_train_all_data[[number_live_births]])

female_train_all_data %>% count(number_live_births)
female_train_all_data$number_live_births[female_train_all_data$number_live_births == -3] <- "Prefer not to answer"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 10] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 11] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 12] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 13] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 19] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 22] <- "5+"
female_train_all_data$number_live_births[female_train_all_data$number_live_births == 5] <- "5+"


## num_in_house #### PROBLEM
female_train_all_data %>% count(num_in_house)
female_train_all_data$num_in_house[female_train_all_data$num_in_house > 5 & female_train_all_data$num_in_house < 46] <- "6+"

num_in_house <- "num_in_house"

threshold2 <- 5

replacement2 <- "6+"

female_train_all_data[[num_in_house]] <- ifelse(female_train_all_data[[num_in_house]] > 5 & female_train_all_data[[num_in_house]] <= 45, 
                                                      replacement2, 
                                                      female_train_all_data[[num_in_house]])
  
### still birth misscarriage
replacement_100349 <- c("Yes", "No", "Do not know", "Prefer not to answer")
list_100349 <- c(1, 0, -1, -3)
replace_name <- "ever_stillbirth_miscarriage_abortion"
female_train_all_data[[replace_name]] <- ifelse(female_train_all_data[[replace_name]] %in% list_100349, 
                                                replacement_100349[match(female_train_all_data[[replace_name]], list_100349)], 
                                                female_train_all_data[[replace_name]])

female_train_all_data %>% count(ever_stillbirth_miscarriage_abortion)

### ever_cont
replace_name2 <- "ever_cont"
female_train_all_data[[replace_name2]] <- ifelse(female_train_all_data[[replace_name2]] %in% list_100349, 
                                                replacement_100349[match(female_train_all_data[[replace_name2]], list_100349)], 
                                                female_train_all_data[[replace_name2]])

### everHRT
replace_name3 <- "ever_HRT"
female_train_all_data[[replace_name3]] <- ifelse(female_train_all_data[[replace_name3]] %in% list_100349, 
                                                 replacement_100349[match(female_train_all_data[[replace_name3]], list_100349)], 
                                                 female_train_all_data[[replace_name3]])

### ethnicity
female_train_all_data %>% count(ethnicity)
female_train_all_data$ethnicity[female_train_all_data$ethnicity == -3] <- "Prefer not to answer"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == -1] <- "Do not know"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 1] <- "White"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 2] <- "Mixed"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 3] <- "Asian or Asian British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 4] <- "Black or Black British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 5] <- "Chinese"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 6] <- "Other ethnic group"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 1001] <- "White"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 1002] <- "White"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 1003] <- "White"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 2001] <- "Mixed"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 2002] <- "Mixed"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 2003] <- "Mixed"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 2004] <- "Mixed"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 3001] <- "Asian or Asian British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 3002] <- "Asian or Asian British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 3003] <- "Asian or Asian British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 3004] <- "Asian or Asian British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 4001] <- "Black or Black British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 4002] <- "Black or Black British"
female_train_all_data$ethnicity[female_train_all_data$ethnicity == 4003] <- "Black or Black British"


### close_major_road
female_train_all_data$close_major_road[female_train_all_data$close_major_road == 0] <- "No"
female_train_all_data$close_major_road[female_train_all_data$close_major_road == 1] <- "Yes"


## stress_factor
list_100502 <- c(1, 2, 3, 4, 5, 6, -7, -3)
replacement_100502<- c("Serious illness, injury or assault to yourself", "Serious illness, injury or assault of a close relative", "Death of a close relative", "Death of a spouse or partner", "Marital separation/divorce", "Financial difficulties", "None of the above", "Prefer not to answer")

stress_factor <- "stress_factor"

female_train_all_data[["stress_factor"]] <- ifelse(female_train_all_data[["stress_factor"]] %in% list_100502,
                                                replacement_100502[match(female_train_all_data[["stress_factor"]], list_100502)], 
                                                female_train_all_data[["stress_factor"]])

female_train_all_data %>% count(stress_factor)


## education level
female_train_all_data$Education_level[female_train_all_data$Education_level == 1] <- "College or University degree"
female_train_all_data$Education_level[female_train_all_data$Education_level == 2] <- "A levels/AS levels or equivalent"
female_train_all_data$Education_level[female_train_all_data$Education_level == 3] <- "O levels/GCSEs or equivalent"
female_train_all_data$Education_level[female_train_all_data$Education_level == 4] <- "CSEs or equivalent"
female_train_all_data$Education_level[female_train_all_data$Education_level == 5] <- "NVQ or HND or HNC or equivalent"
female_train_all_data$Education_level[female_train_all_data$Education_level == 6] <- "Other professional qualifications"
female_train_all_data$Education_level[female_train_all_data$Education_level == -7] <- "None of the above"
female_train_all_data$Education_level[female_train_all_data$Education_level == -3] <- "Prefer not to answer"
female_train_all_data %>% count(Education_level)

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
female_train_all_data %>% count(Family_history_father)
female_train_all_data[["Family_history_father"]] <- ifelse(female_train_all_data[["Family_history_father"]] %in% list_1010,
                                                   replacement_1010[match(female_train_all_data[["Family_history_father"]], list_1010)], 
                                                   female_train_all_data[["Family_history_father"]])

## mother
female_train_all_data %>% count(Family_history_mother)
female_train_all_data[["Family_history_mother"]] <- ifelse(female_train_all_data[["Family_history_mother"]] %in% list_1010,
                                                           replacement_1010[match(female_train_all_data[["Family_history_mother"]], list_1010)], 
                                                           female_train_all_data[["Family_history_mother"]])


### urban vs rural
female_train_all_data %>% count(urban_rural)
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
female_train_all_data[["urban_rural"]] <- ifelse(female_train_all_data[["urban_rural"]] %in% list_91,
                                                           replacement_91[match(female_train_all_data[["urban_rural"]], list_91)], 
                                                           female_train_all_data[["urban_rural"]])


