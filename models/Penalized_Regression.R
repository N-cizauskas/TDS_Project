#PR
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


library(dplyr)
install.packages("glmnet")
library(glmnet)
install.packages("groupdata2")
library(groupdata2)
library(pROC)

## male DATA
male_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_train.rds")
male_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test.rds")



## LASSO REG
set.seed(123) 

#need impute missingness wth makeX
x <- male_train[, -c(153)]
x <- makeX(x)
x<- as.matrix(x)

cv.lasso <- cv.glmnet(x,
                      y = sapply(male_train[153], as.numeric), 
                      alpha = 1, # lasso; alpha = 1;;; if ridge, alpha=0
                      family = "binomial",
                      nfolds = 10)
plot(cv.lasso)

#impute for newx
newx <- male_test[, -c(153)]
newx <- makeX(newx)
newx<- as.matrix(newx)


model_lasso_pred = as.numeric(predict(cv.lasso, 
                           s = cv.lasso$lambda.1se,
                           newx = newx,
                           type = "class"))


MSE_table <- as.data.frame(matrix(data = NA,
                                  nrow = 3,
                                  ncol = 5))
rownames(MSE_table) <- c("Ridge-regression", "Lasso", "Elastic-net")
colnames(MSE_table) <- c("alpha", "lamda_min", "lambda_1se", "Num_of_predictors", "AUC")
MSE_table['Lasso',] <- c(1, cv.lasso$lambda.min, 
                                    cv.lasso$lambda.1se,
                                    sum(coef(cv.lasso)!=0)-1,
                         as.numeric(auc(male_test$case, model_lasso_pred)))


MSE_table

## RIDGE REG
set.seed(123)
cv.ridge <- cv.glmnet(x,
                      y = sapply(male_train[153], as.numeric), 
                      alpha = 0, # lasso; alpha = 1;;; if ridge, alpha=0
                      family = "binomial",
                      nfolds = 10)
plot(cv.ridge)

model_ridge_pred = as.numeric(predict(cv.ridge, 
                                      s = cv.ridge$lambda.1se,
                                      newx = newx,
                                      type = "class"))

MSE_table['Ridge-regression',] <- c(0, cv.ridge$lambda.min, 
                                    cv.ridge$lambda.1se,
                         sum(coef(cv.ridge)!=0)-1,
                         as.numeric(auc(male_test$case, model_ridge_pred)))

## ELASTIC NET
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
                                      newx = newx,
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
coef(cv.lasso)!=0

lasso_lasso <- as.data.frame(as.matrix(coef(cv.lasso))) 
lasso_lasso$var <- rownames(lasso_lasso)
lasso_lasso[lasso_lasso$s1 != 0,]

elastic_male <- as.data.frame(as.matrix(coef(cv.elastic))) 


saveRDS(MSE_table, "Results_PR_male.rds")



# FEMALE 



## female DATA
female_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test.rds")
female_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_train.rds")

## Remove age_last_cont_10.15 from female_train
female_train <- female_train %>% 
  select(-'age_last_cont..10.15.')

## LASSO REG
set.seed(123) 

#need impute missingness wth makeX
x <- female_train[, -c(182)]
x <- makeX(x)
x<- as.matrix(x)

cv.lasso <- cv.glmnet(x,
                      y = sapply(female_train[182], as.numeric), 
                      alpha = 1, # lasso; alpha = 1;;; if ridge, alpha=0
                      family = "binomial",
                      nfolds = 10)
plot(cv.lasso)

#impute for newx
newx <- female_test[, -c(182)]
newx <- makeX(newx)
newx<- as.matrix(newx)


model_lasso_pred = as.numeric(predict(cv.lasso, 
                                      s = cv.lasso$lambda.1se,
                                      newx = newx,
                                      type = "class"))

lasso_female <- as.data.frame(as.matrix(coef(cv.lasso))) 


# Convert class probabilities to class predictions
threshold <- 0.5
pred <- ifelse(model_elastic_pred > threshold, 1, 0)

cases <- female_train$case

# Create confusion matrix
table(female_test$case, pred)


MSE_table <- as.data.frame(matrix(data = NA,
                                  nrow = 3,
                                  ncol = 5))
rownames(MSE_table) <- c("Ridge-regression", "Lasso", "Elastic-net")
colnames(MSE_table) <- c("alpha", "lamda_min", "lambda_1se", "Num_of_predictors", "AUC")
MSE_table['Lasso',] <- c(1, cv.lasso$lambda.min, 
                         cv.lasso$lambda.1se,
                         sum(coef(cv.lasso)!=0)-1,
                         as.numeric(auc(female_test$case, model_lasso_pred)))


MSE_table
## RIDGE REG
set.seed(123)
cv.ridge <- cv.glmnet(x,
                      y = sapply(female_train[182], as.numeric), 
                      alpha = 0, # lasso; alpha = 1;;; if ridge, alpha=0
                      family = "binomial",
                      nfolds = 10)
plot(cv.ridge)

model_ridge_pred = as.numeric(predict(cv.ridge, 
                                      s = cv.ridge$lambda.1se,
                                      newx = newx,
                                      type = "class"))

MSE_table['Ridge-regression',] <- c(0, cv.ridge$lambda.min, 
                                    cv.ridge$lambda.1se,
                                    sum(coef(cv.ridge)!=0)-1,
                                    as.numeric(auc(female_test$case, model_ridge_pred)))

## ELASTIC NET
set.seed(123)
cvm <-function(alpha){
  model=cv.glmnet(x=x, 
                  y=sapply(female_train[182], as.numeric), 
                  alpha=alpha,
                  family = "binomial")
  with(model, cvm[which.min(lambda-lambda.1se)])
}

alpha.opt<-optimise(cvm, c(0,1))
alpha.opt

set.seed(123)
cv.elastic <- cv.glmnet(x = x,
                        y = sapply(female_train[182], as.numeric), 
                        alpha = alpha.opt$minimum, # lasso; alpha = 1;;; if ridge, alpha=0
                        family = "binomial",
                        nfolds = 10)
plot(cv.elastic)

elastic_fem <- as.data.frame(as.matrix(coef(cv.elastic))) 


model_elastic_pred = as.numeric(predict(cv.elastic, 
                                        s = cv.elastic$lambda.1se,
                                        newx = newx,
                                        type = "class"))

MSE_table['Elastic-net',] <- c(alpha.opt$minimum, cv.elastic$lambda.min, 
                               cv.elastic$lambda.1se,
                               sum(coef(cv.elastic)!=0)-1,
                               as.numeric(auc(female_test$case, model_elastic_pred)))
MSE_table
prop.table(table(female_test$case))
prop.table(table(female_train$case))
coef(cv.elastic)!=0
coef(cv.lasso)!=0

lasso_lasso <- as.data.frame(as.matrix(coef(cv.lasso))) 
lasso_lasso$var <- rownames(lasso_lasso)
lasso_lasso[lasso_lasso$s1 != 0,]

cm = table(test_set[, 182], y_pred)

saveRDS(MSE_table, "Results_PR_female.rds")



# Filter out rows with coef = 0
male_lasso <- lasso_lasso[lasso_lasso$s1 != 0,]

# Create forest plot
ggplot(male_lasso, aes(x = s1, y = var)) +
  geom_point(size = 3, shape = 19) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  theme_classic() +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("Forest plot for non-zero coefficients")

# Filter out rows with coef = 0
elastic_male$var <- rownames(elastic_male)
male_elastic <- elastic_male[elastic_male$s1 != 0,]

# Create forest plot
ggplot(male_elastic, aes(x = s1, y = var)) +
  geom_point(size = 3, shape = 19) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  theme_classic() +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("Forest plot for non-zero coefficients")


# Filter out rows with coef = 0
lasso_female$var <- rownames(lasso_female)
female_lasso <- data.frame(lasso_female[lasso_female$s1 != 0,])

# Create forest plot
ggplot(female_lasso, aes(x = s1, y = var)) +
  geom_point(size = 3, shape = 19) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  theme_classic() +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("Forest plot for non-zero coefficients")


# Filter out rows with coef = 0
elastic_fem$var <- rownames(elastic_fem)
fem_elastic <- elastic_fem[elastic_fem$s1 != 0,]

# Create forest plot
ggplot(fem_elastic, aes(x = s1, y = var)) +
  geom_point(size = 3, shape = 19) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey")+
  theme_classic() +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("Forest plot for non-zero coefficients")

