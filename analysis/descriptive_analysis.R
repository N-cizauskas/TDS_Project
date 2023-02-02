setwd("/rds/general/user/sv619/projects/hda-22-23/live/TDS/Group8_Working")

getwd()

colnames(ukb_extracted)

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

prevalent <- subset(merged_df, subset = prevalent_case == 1)
notprevalent <- subset(merged_df, subset = prevalent_case == 0 & incident_case == 0)


### descriptive analysis
incident %>% count(Sex.0.0)

incident <- c(13928, 244)

### adding health status
merged_df <- merged_df %>% mutate(health_status = ifelse(is.na(incident_case), "Prevalent", ifelse(incident_case==1, "Incident", "Healthy")))

merged_df %>% count(health_status)

hist(merged_df$first_cancer_year)

mutate(age_diag = ifelse(is.na(incident_case), "Prevalent", ifelse(incident_case==1, "Incident", "Healthy")))
