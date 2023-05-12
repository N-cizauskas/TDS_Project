# Code adjusted from here:
# https://www.kaggle.com/code/rtatman/machine-learning-with-xgboost-in-r/notebook

library(xgboost) 
library(tidyverse) 
library(mltools)
library(data.table)

male_test_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test.rds")
male_train_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_train.rds")

# Get a subset of training dataframe without labels (ie. without case/control status)

male_train_case_removed <- male_train_all_data %>% 
  select(-"case")

male_test_case_removed <- male_test_all_data %>% 
  select(-"case")

# Create a vector with the labels
case <- male_train_all_data %>% 
  select("case")

case_test <- male_test_all_data %>% 
  select("case")

# Convert target feature to Boolean vector (True and False)
case$case <- as.logical(case$case)

case_test$case <- as.logical(case_test$case)

# Splitting the dataset into
# the Training set and Test set
#install.packages('caTools')
library(caTools)

training_set = male_train_case_removed
test_set = male_test_case_removed




# Create a vector with the labels
train_labels <- as.matrix(case)
test_labels <- as.matrix(case_test)


# Convert to matrix
training_set <- as.matrix(training_set)
test_set <- as.matrix(test_set)


# Convert dataframe into Dmatrices 

dtrain <- xgb.DMatrix(data = training_set, label= train_labels)
Sdtest <- xgb.DMatrix(data = test_set, label= test_labels)


xgmodel <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function
# train-logloss:0.591012 
# train-logloss:0.536861

# generate predictions for our held-out testing data
pred <- predict(xgmodel, Sdtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
#test-error= 0.192027270144873

## Tune the model 
# Improvement seen on the test set, so does not appear to be overfit
# Lets try to increase the number of rounds

# get the number of negative & positive cases in our data
negative_cases <- sum(train_labels == FALSE)
positive_cases <- sum(train_labels == TRUE)

# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 20, # number of boosting rounds
                       early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic") # the objective function
                       

# generate predictions for our held-out testing data
pred <- predict(model_tuned, Sdtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
# Max depth = 3 and nround 20, test error = 0.190985. This does not improve with more boosting rounds or by increasing/decreasing the depth

#install.packages("DiagrammeR")
#install.packages("htmltools")
library(DiagrammeR)

# plot the features! what's contributing most to our model?
xgb.plot.multi.trees(feature_names = names(training_set), 
                     model = model_tuned)

# retrying plot -
xgb.plot.tree(model = model_tuned, trees = 1)

predict(model_tuned, Sdtest, type="prob")

tree.preds <- predict(model_tuned, Sdtest, type="prob")

#install.packages("pROC")
library(pROC)
tree.roc <- roc(case_test$case, tree.preds)
print(tree.roc)
plot(tree.roc)



# confusion matrix
# you've to do your prediction here


# and transform them in a 0 1 variable, you can choose the value to get 1
pred <-  as.numeric(pred > 0.5)

#library(caret)
#confusionMatrix(factor(pred),factor(test_set$case))

pred <- factor(pred)

test_case <- factor(case_test$case)

confusion_mat = as.matrix(table(Actual_Values = test_case, Predicted_Values = pred)) 
print(confusion_mat)



# SHAP


load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))


shap_values=predict(model_tuned, test_set, predcontrib = TRUE, approxcontrib = F)

plot.shap.summary

























# FEMALE TIME 


library(xgboost) 
library(tidyverse) 
library(mltools)
library(data.table)

female_test_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test.rds")
female_train_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_train.rds")



# Get a subset of training dataframe without labels (ie. without case/control status)

female_train_case_removed <- female_train_all_data %>% 
  select(-"case")

female_test_case_removed <- female_test_all_data %>% 
  select(-"case")

# Create a vector with the labels
case <- female_train_all_data %>% 
  select("case")

case_test <- female_test_all_data %>% 
  select("case")

# Convert target feature to Boolean vector (True and False)
case$case <- as.logical(case$case)

case_test$case <- as.logical(case_test$case)

# Splitting the dataset into
# the Training set and Test set
#install.packages('caTools')
library(caTools)

training_set = female_train_case_removed
test_set = female_test_case_removed




# Create a vector with the labels


train_labels <- as.matrix(case)
test_labels <- as.matrix(case_test)


# Convert to matrix
training_set <- as.matrix(training_set)
test_set <- as.matrix(test_set)




# Convert dataframe into Dmatrices 

dtrain <- xgb.DMatrix(data = training_set, label= train_labels)
Sdtest <- xgb.DMatrix(data = test_set, label= test_labels)

# train a model using our training data
xgmodel <- xgboost(data = dtrain, # the data   
                   nround = 2, # max number of boosting iterations
                   objective = "binary:logistic")  # the objective function
# train-logloss:0.591012 
# train-logloss:0.536861

# generate predictions for our held-out testing data
pred <- predict(xgmodel, Sdtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
#test-error= 0.192027270144873

## Tune the model 
# Improvement seen on the test set, so does not appear to be overfit
# Lets try to increase the number of rounds

# get the number of negative & positive cases in our data
negative_cases <- sum(train_labels == FALSE)
positive_cases <- sum(train_labels == TRUE)

# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 20, # number of boosting rounds
                       early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic") # the objective function


# generate predictions for our held-out testing data
pred <- predict(model_tuned, Sdtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
# Max depth = 3 and nround 20, test error = 0.190985. This does not improve with more boosting rounds or by increasing/decreasing the depth

#install.packages("DiagrammeR")
#install.packages("htmltools")
library(DiagrammeR)

# plot the features! what's contributing most to our model?
xgb.plot.multi.trees(feature_names = names(training_set), 
                     model = model_tuned)

# retrying plot - N
xgb.plot.tree(model = model_tuned, trees = 1)

predict(model_tuned, Sdtest, type="prob")

tree.preds <- predict(model_tuned, Sdtest, type="prob")

#install.packages("pROC")
library(pROC)
tree.roc <- roc(case_test$case, tree.preds)
print(tree.roc)
plot(tree.roc)


# confusion matrix
# you've to do your prediction here


# and transform them in a 0 1 variable, you can choose the value to get 1
pred <-  as.numeric(pred > 0.5)

#library(caret)
#confusionMatrix(factor(pred),factor(test_set$case))

pred <- factor(pred)

test_case <- factor(case_test$case)

confusion_mat = as.matrix(table(Actual_Values = test_case, Predicted_Values = pred)) 
print(confusion_mat)

