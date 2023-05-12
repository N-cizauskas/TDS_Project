setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA")

# step 1: load packages
install.packages("dplyr",repos = "http://cran.us.r-project.org")
library(dplyr)
library(stringr)
library(parallel)
install.packages("mice",repos = "http://cran.us.r-project.org")
library(mice)
install.packages("purrr",repos = "http://cran.us.r-project.org")
library(purrr)
install.packages("rpart",repos = "http://cran.us.r-project.org")
library(rpart)

# step 2: data import
test_males <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/final_male_test_2403.rds")

# step 3: drop those with >30 missingness
# finding % missingness
male_miss <- sort((colMeans(is.na(test_males)))*100)
# dropping columns over 30%
drops <- c()
for (name in names(male_miss)){
  if (male_miss[name]> 30){
    #add over 30s to drops
    drops <- append(drops, name)
  } 
}
male_miss <- male_miss[!(names(male_miss) %in% drops)] 

# step 5: imputation
print('starting imputation')
cleaned_male_data <- test_males[!(names(test_males) %in% drops)]
imputed_male <-  mice(cleaned_male_data, method="cart")
full_male <- complete(imputed_male)
print('ending imputation')

# step 6: export
saveRDS(full_male, file = "male_test_data_2503.rds")
print('saved')