setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

library(dplyr)
#install.packages("bnstruct", dependencies=TRUE)
library(bnstruct)


female_train_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/female_train_all_data.rds")
female_train_all_data %>% select_if(~!is.numeric(.x)) %>% colnames()
  # "Sex"; can drop
  # "date_recr.x" ; can drop
  # "cancer_icd10" ; can drop
  # "Cancer_record_origin" ; can drop
  # "date_recr.y" ; can drop
  # "bmi_bin" ; can drop as bmi included

data <- female_train_all_data %>% select_if(~is.numeric(.x))
data <- data %>% dplyr::select(-c("incident_case", "prevalent_case", "cancer_docdiag"))
# View(data) # what if i treat everything as continuous
var_count <- data %>% summarise_all(n_distinct)
var_count <- as.matrix(var_count)
colnames(var_count) <- NULL
var_count <- as.vector(var_count) # convert to vectors
str(var_count) 

dataset.from.data <- BNDataset(data = data,
                               discreteness = rep('c',80),
                               variables = colnames(data),
                               node.sizes = var_count)
show(dataset.from.data)
raw.data(dataset.from.data)
dataset.from.data <- complete(dataset.from.data)

#bn <- BN(dataset.from.data)

net.1 <- learn.network(dataset.from.data,
                       algo = "sem",
                       scoring.func = "AIC")

net.2 <- learn.network(dataset.from.data,
                       algo = "mmhc",
                       scoring.func = "BDeu")