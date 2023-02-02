setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")#getwd()

# step 1: load packages
library(dplyr)
library(stringr)
library(table1)

# step 2: import data
ukb_extracted <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/extraction_and_recoding/outputs/ukb_extracted.rds")
#colnames(ukb_extracted)
output_final <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/outcome_definition/Outputs/output_final.rds")
#colnames(output_final)

# step 3: preliminary data analysis
output_final %>% count(prevalent_case)
output_final %>% count(incident_case)

hist(ukb_extracted$Year_birth.0.0)

ukb_extracted %>% count(Sex.0.0)

# step 4: merging to create new df
  ### adding eid as a column to ukb_extracted
eid <- rownames(ukb_extracted)
ukb_extracted$eid <- eid
  ### merging both data frames - ukb_extracted & output_final
merged_df <- merge(ukb_extracted, output_final, by = "eid")

# step 5: assign health status
### dataframes by cancer status
#incident <- subset(merged_df, incident_case == 1)
#notincident <- subset(merged_df, subset = incident_case == 0)
#prevalent <- subset(merged_df, subset = prevalent_case == 1)
#notprevalent <- subset(merged_df, subset = prevalent_case == 0 & incident_case == 0)

merged_df <- merged_df %>% 
  mutate(health_status = ifelse(is.na(incident_case), 
                                "Prevalent",  # if incident is NA
                                ifelse(incident_case==1,
                                       "Incident",  # if incident is not NA but 1
                                       "Healthy")))  # if incident is not NA but 0

merged_df %>% count(health_status)

# step 6: more analysis
### descriptive analysis
#incident %>% count(Sex.0.0)
#incident <- c(13928, 244)
# hist(merged_df$first_cancer_year)

merged_df[merged_df$Sex.0.0==0,]$Sex.0.0 <- 'Female'
merged_df[merged_df$Sex.0.0==1,]$Sex.0.0 <- 'Male'

table(merged_df$Sex.0.0, merged_df$health_status)

merged_df <- merged_df %>% mutate(age_cancer_diag = ifelse(is.na(age_cancer_diag.1.0),
                                                           age_cancer_diag.0.0,
                                                           age_cancer_diag.1.0))

merged_df <- merged_df %>% mutate(age_menopause = ifelse(is.na(age_menopause.1.0),
                                                         age_menopause.0.0,
                                                         age_menopause.1.0))
# added divya function
cumulate_df <- function(Y,X) {
  df <- Y
  output <- as.data.frame(df %>%
                            rowwise %>% 
                            mutate(m = {tmp <- c_across(starts_with(X))
                            tail(na.omit(tmp), 1)[1]}) %>%
                            ungroup)
  names(output)[names(output) == "m"] <- paste0(X,"_cumulative")
  return(output)
}
  # RUN CODE VIA cumulate_df(nameofdataframe, "startnameofcol")
    # example: merged_df <- cumulate_df(merged_df, "bmi")
merged_df <- cumulate_df(merged_df, "age_menopause")
saveRDS(merged_df, file = "merged_meno_df.rds")
merged_df <- cumulate_df(merged_df, "age_cancer_diag")
saveRDS(merged_df, file = "merged_cancer_diag_df.rds")
save.image(file = "my_work_space.RData")

# step 7: summary table
table1(~ Sex.0.0 + bmi.1.0 + age_cancer_diag + townsend_index.0.0 + Year_birth.0.0 + age_menopause.0.0| health_status, data=merged_df,
       overall=c(left="Total"))


