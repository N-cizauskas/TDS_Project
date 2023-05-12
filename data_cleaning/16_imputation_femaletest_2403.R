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
test_females <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/toimpute_femaletest.rds")

# step 3: drop those with >30 missingness
# finding % missingness
female_miss <- sort((colMeans(is.na(test_females)))*100)
# dropping columns over 30%
drops <- c()
for (name in names(female_miss)){
  if (female_miss[name]> 30){
    #add over 30s to drops
    drops <- append(drops, name)
  } 
}
female_miss <- female_miss[!(names(female_miss) %in% drops)] 

# step 5: imputation
print('starting imputation')
cleaned_female_data <- test_females[!(names(test_females) %in% drops)]
imputed_female <-  mice(cleaned_female_data, method="cart")
full_female <- complete(imputed_female)
print('ending imputation')

# step 6: export
saveRDS(full_female, file = "imputed_female_test_2403.rds")
print('saved')