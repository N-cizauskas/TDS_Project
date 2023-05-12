## Load Packages
library(dplyr)
library(ggplot2)

# Set working directory
getwd()
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

# Load data
females <- readRDS("DATA/female_train_all_data.rds")
males <- readRDS("DATA/male_train_all_data.rds")

# View data
str(females)
str(males)

# Change date from character to date
df$date_recr <- as.Date(df$date_recr)
df$waist_circumference <- as.integer(df$waist_circumference)
df$date_recr <- as.integer(df$date_recr)

# Create density plots

dir.create("Figures", showWarnings = FALSE)

pdf(("Figures/Distributions.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 1, 1))
for (k in 1:ncol(df)) {
  if (is.numeric(df[,k]))
  if(sum(!is.na(df[,k])) >= 2)
  plot(density(df[,k], na.rm = TRUE), col = "skyblue", xlab = "",
       main = "", ylab = colnames(df) [k])
  
}

dev.off()

str <- (str(df, list.len = ncol(df)))

# Categorise data 

Evnt_data <- df[ , c("own_or_rent", "num_in_house", 'walk_job', 'smoke_household', 'breastfed', 'stress_factor', 
                                  "freq_family_friend_visit", "urban_rural", "ever_worried_month", 'happiness', 'workplace_noise', 
                                  "Worked_with_chemicals", "Worked_with_smoking_people", "Worked_with_asbestos_materials", 
                                  "Worked_with_paints", "Worked_with_pesticides", "nit_dioxide_2010", "nit_oxide_2010", 
                                  "pollution_2010", "traffic_near_road", "traffic_near_major", "dist_major_road", 
                                  "tot_traffic_major", "close_major_road", "nit_dioxide_2005", "nit_dioxide_2006", 
                                  "nit_dioxide_2007", "pollution_2007", "noise_pollution_day", "noise_pollution_eve", 
                                  "noise_pollution_night", "greenspace_1000", "garden_perc_1000", "water_perc_1000", 
                                  "greenspace_300", "garden_perc_300", "water_perc_300", "nat_environ_1000", "nat_environ_300", 
                                  "dist_coast")]


Beh_data <- df[ , c("sleep_duration", "ever_cont", "age_first_cont", "age_last_cont", "ever_HRT", "age_first_hrt", 
                                 "age_last_hrt", 'Progestan_contraceptive_type','Smoke_status', 'Alcohol_status', 'Ever_smoked', 
                                 'Heavy_alcohol_frequency', 'drug_use', "alcohol_intake", "vigorous_exercise", "mod_exercise",
                                 "light_exercise" )]

Bibeh_data <- df[ , c("number_live_births", "first_birth", "ever_stillbirth_miscarriage_abortion", 'Stillbirth_count', 
                                   'Miscarriage_count', 'Abortion_count', 'Family_history_father', "Family_history_mother", "height",
                                   "weight", "bmi")]

Bio_data <- df[ , c("Sex", "waist_circumference", "Hip_circumference", 'Standing_height', 'Seated_height', "sys_BP", 
                                 "dias_BP", "age_first_facialhair", "age_voice_broke", "hair_bald", "other_medcond", 
                                 "age_period_start", "menopause", "htn_age", "age_menopause", "angina_age", "length_menscycle", 
                                 'mi_age', 'stroke_age', "illness_history", 'tot_chol', 'apoa1', 'fatty_acids', 'WBC_count', 
                                 "RBC_count", "Haematocrit_percentage", "Platelet_count", "Lymphocyte_count", "Neutrophil_count", 
                                 "apob", "cholesterol", "c_reactive_protein", "glucose", "hba1c", "hdl", "ifg1", "LDL", "Oestradiol", 
                                 "Testosterone", "triglyceride", "vit_d")]

Out_data <- df[ , c("date_recr", "first_cancer_year", "non_cancer_year", "cancer_docdiag", 
                                 'interpolated_age_first_cancer_diag', "Age_recr", "date_cancer_date", "cancer_icd10", "age_death", 
                                 "cancer_icd9", "Cancer_record_origin")]

Demo_data <- df[ , c("Year_birth", "place_of_birth_N", "place_of_birth_E", "townsend_index", "accommodation", 
                                  'avg_house_income', 'manual_work', 'shift_work', 'Private_healthcare', 'Education_level', 
                                  'ethnicity', 'year_of_birth', 'Job_coding', 'deprivation_score' )]

# Create distributions for each category

# Male train data
dir.create("Figures", showWarnings = FALSE)

pdf(("Figures/Distributions_males_train.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(males)) {
  if (is.numeric(males[,k]))
    if(sum(!is.na(males[,k])) >= 2){
      plot(density(males[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(males) [k])
  mtext("Males train distributions", outer=TRUE,  cex=1, line=-1.5)
    }
}


dev.off()

# Female train data
dir.create("Figures", showWarnings = FALSE)

pdf("Figures/Distributions_females_train.pdf")
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(females)) {
  if (is.numeric(females[,k]))
    if(sum(!is.na(females[,k])) >= 2){
      plot(density(females[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(females) [k])
  mtext("Females train distributions", outer = TRUE,  cex=1, line = -1.5)
    }
}


dev.off()

# environmental 

pdf(("Figures/Distributions_environment.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Evnt_data)) {
  if (is.numeric(Evnt_data[,k]))
    if(sum(!is.na(Evnt_data[,k])) >= 2)
      plot(density(Evnt_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Evnt_data) [k])
  mtext("Environmental", outer=TRUE,  cex=1, line=-1.5)
  
  
}

dev.off()

# Behavioural 

pdf(("Figures/Distributions_beh.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Beh_data)) {
  if (is.numeric(Beh_data[,k]))
    if(sum(!is.na(Beh_data[,k])) >= 2)
      plot(density(Beh_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Beh_data) [k])
  mtext("Behavioural", outer=TRUE,  cex=1, line=-1.5)
  
}

dev.off()

# Biobehavioural

pdf(("Figures/Distributions_biobehav.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Bibeh_data)) {
  if (is.numeric(Bibeh_data[,k]))
    if(sum(!is.na(Bibeh_data[,k])) >= 2)
      plot(density(Bibeh_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Bibeh_data) [k])
  mtext("Biobehavioural", outer=TRUE,  cex=1, line=-1.5)
  
}

dev.off()

# Biological 

pdf(("Figures/Distributions_biological.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Bio_data)) {
  if (is.numeric(Bio_data[,k]))
    if(sum(!is.na(Bio_data[,k])) >= 2)
      plot(density(Bio_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Bio_data) [k])
  mtext("Biological", outer=TRUE,  cex=1, line=-1.5)
  
}

dev.off()

# Outcome specific 

pdf(("Figures/Distributions_outcome.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Out_data)) {
  if (is.numeric(Out_data[,k]))
    if(sum(!is.na(Out_data[,k])) >= 2)
      plot(density(Out_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Out_data) [k])
  mtext("Outcome Specific", outer=TRUE,  cex=1, line=-1.5)
  
}

dev.off()

# Demographics 

pdf(("Figures/Distributions_demographic.pdf"))
par(mfrow = c(3, 3), mar = c(3, 4.5, 2, 1))
for (k in 1:ncol(Demo_data)) {
  if (is.numeric(Demo_data[,k]))
    if(sum(!is.na(Demo_data[,k])) >= 2)
      plot(density(Demo_data[,k], na.rm = TRUE), col = "skyblue", xlab = "",
           main = "", ylab = colnames(Demo_data) [k])
  mtext("Demographic", outer=TRUE,  cex=1, line=-1.5)
  
}

dev.off()
