setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


library(dplyr)
install.packages("glmnet")
library(glmnet)
install.packages("groupdata2")
library(groupdata2)
library(pROC)

# step 2: import data and prepare for modeling
female_test_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test_stratify.rds")
# Breast Ovarian Thyroid Uterine 
y <- "case"
x <- setdiff(colnames(female_test_stratify), c("case"))

male_test_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test_stratify.rds")
# Breast Prostate  Thyroid 
y <- "case"
x <- setdiff(colnames(male_test_stratify), c("case"))

male_thyroid <- subset(male_test_stratify, subset = cancer_type == "Thyroid" | is.na(cancer_type))
male_prostate <- subset(male_test_stratify, subset = cancer_type == "Prostate" | is.na(cancer_type))
male_breast <- subset(male_test_stratify, subset = cancer_type == "Breast" | is.na(cancer_type))

male_thyroid_y <- male_thyroid$case
male_thyroid_x <- subset(male_thyroid, select = -c(case, cancer_type))  

male_prostate_y <- male_prostate$case
male_prostate_x <- subset(male_prostate, select = -c(case, cancer_type))

male_breast_y <- male_breast$case
male_breast_x <- subset(male_breast, select = -c(case, cancer_type))

male_thyroid_x <- as.matrix(male_thyroid_x)

set.seed(123)
cvm <-function(alpha){
  model=cv.glmnet(x=x, 
                  y=sapply(male_train[153], as.numeric), 
                  alpha=alpha,
                  family = "binomial")
  with(model, cvm[which.min(lambda-lambda.1se)])
}

alpha.opt<-optimise(cvm, c(0,1))
alpha.opt

set.seed(123)
cv.elastic <- cv.glmnet(x = x,
                        y = sapply(male_train[153], as.numeric), 
                        alpha = alpha.opt$minimum, # lasso; alpha = 1;;; if ridge, alpha=0
                        family = "binomial",
                        nfolds = 10)
plot(cv.elastic)

model_elastic_pred = as.numeric(predict(cv.elastic, 
                                        s = cv.elastic$lambda.1se,
                                        newx = male_thyroid_x,
                                        type = "class"))

# Convert class probabilities to class predictions
threshold <- 0.5
pred <- ifelse(model_lasso_pred > threshold, 1, 0)


# Create confusion matrix
table(male_test$case, pred)



MSE_table['Elastic-net',] <- c(alpha.opt$minimum, cv.elastic$lambda.min, 
                               cv.elastic$lambda.1se,
                               sum(coef(cv.elastic)!=0)-1,
                               as.numeric(auc(male_test$case, model_elastic_pred)))
MSE_table

prop.table(table(male_test$case))
prop.table(table(male_train$case))
coef(cv.elastic)!=0

elastic_male <- as.data.frame(as.matrix(coef(cv.elastic)))