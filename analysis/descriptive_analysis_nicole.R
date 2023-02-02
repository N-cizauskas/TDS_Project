setwd("/rds/general/user/sv619/projects/hda-22-23/live/TDS/Group8_Working")

getwd()



ukb_extracted <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/extraction_and_recoding/outputs/ukb_extracted.rds")

colnames(ukb_extracted)

output_final <- readRDS("/rds/general/user/sv619/projects/hda-22-23/live/TDS/sv619/outcome_definition/Outputs/output_final.rds")

view(output_final)

colnames(output_final)

output_final %>% count(prevalent_case)

output_final %>% count(incident_case)

hist(ukb_extracted$Year_birth.0.0)

ukb_extracted %>% count(Sex.0.0)

### adding eid as a column to ukb_extracted
eid <- rownames(ukb_extracted)
ukb_extracted$eid <- eid

### merging both data frames - ukb_extracted & output_final
merged_df <- merge(ukb_extracted, output_final, by = "eid")

### dataframes by cancer status

incident <- subset(merged_df, incident_case == 1)
notincident <- subset(merged_df, subset = incident_case == 0)

not <- subset(merged_df, subset = incident_case == 0 & prevalent_case == 0)

prevalent <- subset(merged_df, subset = prevalent_case == 1)
notprevalent <- subset(merged_df, subset = prevalent_case == 0 & incident_case == 0)


### adding health status
merged_df <- merged_df %>% mutate(health_status = ifelse(is.na(incident_case), "Prevalent", ifelse(incident_case==1, "Incident", "Healthy")))

merged_df %>% count(health_status)

hist(merged_df$first_cancer_year)

mutate(age_diag = ifelse(is.na(incident_case), "Prevalent", ifelse(incident_case==1, "Incident", "Healthy")))



### descriptive analysis - sex
incident %>% count(Sex.0.0)

not %>% count(Sex.0.0)

prevalent %>% count(Sex.0.0)


# BMI hist and mean + SD

hist(merged_df$bmi.1.0)




incident %>% select(bmi.1.0)  %>% summary()
incident %>% select(bmi.1.0) %>% summarize(stdev = sd(bmi.1.0, na.rm=T))

not %>% select(bmi.1.0)  %>% summary()
not %>% select(bmi.1.0) %>% summarize(stdev = sd(bmi.1.0, na.rm=T))

prevalent %>% select(bmi.1.0)  %>% summary()
prevalent %>% select(bmi.1.0) %>% summarize(stdev = sd(bmi.1.0, na.rm=T))

summary(merged_df$bmi.1.0)
sd(merged_df$bmi.1.0, na.rm=T)





# table1 - error using date_death (does not take dates)

table1(~ Sex.0.0 + bmi.1.0 | health_status, data=merged_df,
       overall=c(left="Total"))

