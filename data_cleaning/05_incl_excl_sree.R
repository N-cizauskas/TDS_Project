# step 1: import data
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

cases <- readRDS("cases.RDS")
controls <- readRDS("controls.RDS")

#step 2: drop columns casecheck and controlcheck

drops_a <- c("casecheck")
cases <- cases[ , !(names(cases) %in% drops_a)]

drops_b <- c("controlcheck")
controls <- controls[ , !(names(controls) %in% drops_b)]

# step 3: combine into single dataframe

ukb_data_2402 <- rbind(cases, controls)

saveRDS(ukb_data_2402, file = "ukb_data_2402.rds")

# step 4: male and females
ukb_data_2402$Sex[ukb_data_2402$Sex == "Females"] <- "Female"
ukb_data_2402$Sex[ukb_data_2402$Sex == "Males"] <- "Male"

female <- subset(ukb_data_2402, subset = Sex == "Female" )
male <- subset(ukb_data_2402, subset = Sex == "Male" )

# step 5: keeping relevant columns males

remove_male <- c("age_period_start", "menopause", "number_live_births", "first_birth", "first_birth", "ever_stillbirth_miscarriage_abortion", "ever_cont", "age_first_cont", "age_last_cont", "ever_HRT", "age_first_HRT", "age_menopause", "length_menscycle", "Stillbirth_count", "Miscarriage_count", "Abortion_count", "Progestan_contraceptive_type")
male <- male[ , !(names(male) %in% remove_male)]

# step 6: keeping relevant columns females

remove_female <- c("age_voice_broke", "age_first_facialhair")
female <- female[ , !(names(female) %in% remove_female)]

# step 7: save males and females as RDS

saveRDS(male, file = "male_2402.rds")
saveRDS(female, file = "female_2402.rds")
