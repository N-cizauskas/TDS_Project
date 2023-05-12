setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

# step 1: load packages
library(dplyr)
library(stringr)
library(table1)
library(parallel)

# step 2: import data
ukb_extracted <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/extraction_and_recoding/outputs/ukb_extracted.rds")

# step 2a: edit col details
ukb_extracted$date_recr.0.0 <- as.character(ukb_extracted$date_recr.0.0)
ukb_extracted$date_cancer_date.0.0 <- as.character(ukb_extracted$date_cancer_date.0.0)
ukb_extracted$date_cancer_date.1.0 <- as.character(ukb_extracted$date_cancer_date.1.0)
ukb_extracted$date_cancer_date.2.0 <- as.character(ukb_extracted$date_cancer_date.2.0)
ukb_extracted$date_cancer_date.3.0 <- as.character(ukb_extracted$date_cancer_date.3.0)
ukb_extracted$date_cancer_date.4.0 <- as.character(ukb_extracted$date_cancer_date.4.0)
ukb_extracted$date_cancer_date.5.0 <- as.character(ukb_extracted$date_cancer_date.5.0)
ukb_extracted$date_cancer_date.6.0 <- as.character(ukb_extracted$date_cancer_date.6.0)
ukb_extracted$date_cancer_date.7.0 <- as.character(ukb_extracted$date_cancer_date.7.0)
ukb_extracted$date_cancer_date.8.0 <- as.character(ukb_extracted$date_cancer_date.8.0)
ukb_extracted$date_cancer_date.9.0 <- as.character(ukb_extracted$date_cancer_date.9.0)
ukb_extracted$date_cancer_date.10.0 <- as.character(ukb_extracted$date_cancer_date.10.0)
ukb_extracted$date_cancer_date.11.0 <- as.character(ukb_extracted$date_cancer_date.11.0)
ukb_extracted$date_cancer_date.12.0 <- as.character(ukb_extracted$date_cancer_date.12.0)
ukb_extracted$date_cancer_date.13.0 <- as.character(ukb_extracted$date_cancer_date.13.0)
ukb_extracted$date_cancer_date.14.0 <- as.character(ukb_extracted$date_cancer_date.14.0)
ukb_extracted$date_cancer_date.15.0 <- as.character(ukb_extracted$date_cancer_date.15.0)
ukb_extracted$date_cancer_date.16.0 <- as.character(ukb_extracted$date_cancer_date.16.0)

# step 3: get col list
check <- str_extract(colnames(ukb_extracted), "[.]+[0]+[.]+[0]")
wanted_cols <- colnames(ukb_extracted)[!is.na(check)]
ukb_thinned <- ukb_extracted %>% select(all_of(wanted_cols))
unique_cols <- unique(str_replace_all(colnames(ukb_thinned), "[.]+[0-9]+[.]+[0-9]?[0-9]", ""))
colnames(ukb_thinned) <- unique_cols

# step 3a: check for missingness
#unique_all <- unique(str_replace_all(colnames(ukb_extracted), "[.]+[0-9]+[.]+[0-9]?[0-9]", ""))
#length(unique_all)
#setdiff(unique_all, unique_cols) # whats in all that isnt in this
#ukb_extracted %>% select(starts_with("height")) %>% colnames() # height 2.0
#ukb_extracted %>% select(starts_with("bmi")) %>% colnames() # bmi 1.0

# step 3b: add in differently named ones
ukb_thinned$bmi <- ukb_extracted$bmi.1.0
ukb_thinned$height <- ukb_extracted$height.2.0

# step 3c: double check
rownames(ukb_extracted)==rownames(ukb_thinned) # all trues!

# step 3d: add rownames/index as new column for patient identification
ukb_thinned$eid <- rownames(ukb_thinned) 


cumulate_df <- function(X) {
  output <- as.data.frame(ukb_extracted %>%
                            rowwise %>% 
                            mutate(m = {tmp <- c_across(starts_with(X))
                            tail(na.omit(tmp), 1)[1]}) %>%
                            ungroup)
  return(output[,"m"])
}
sum(is.na(cumulate_df("height")))
sum(is.na(ukb_thinned$height))

# step 4: save
saveRDS(ukb_thinned, file="ukb_thinned.rds")
save.image(file = "my_work_space.RData")