imputed_femaletest_2402 <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test.rds")
imputed_femaletrain_2402 <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_train.rds")
female_train_all_data <- imputed_femaletrain_2402
female_test_all_data <- imputed_femaletest_2402






# Encoding the target feature as factor
female_test_all_data$case = factor(as.character(female_test_all_data$case),
                                   levels = c("0", "1"))

# Splitting the dataset into
# the Training set and Test set
#install.packages('caTools')
library(caTools)

training_set = female_train_all_data
test_set = female_test_all_data



# Fitting Decision Tree Classification
# to the Training set
# install.packages('rpart')
library(rpart)
training_set$case <- as.character(training_set$case)
classifier = rpart(formula = case ~ .,
                   method = "class",
                   data = training_set)
summary(classifier)
print(classifier)

# Predicting the Test set results
y_pred = predict(classifier,
                 newdata = test_set,
                 type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 182], y_pred)
cm




plot(classifier)
text(classifier)


rpart.plot(classifier)

binary.model <- rpart(case ~ ., data = test_set, cp = .002)
rpart.plot(binary.model)

# model pred 1

predict(binary.model, test_set, type="prob")

tree.preds <- predict(binary.model, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc) # 0.5801
plot(tree.roc)

# AUC - 0.571




# another method

predict(binary.model, test_set, type="prob")
tree.preds <- predict(tree.preds, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc)
plot(tree.roc)

# AUC - 0.5684


### better female decision tree; ONE WE USE
printcp(classifier)
min.x <- which.min(classifier$cptable[, 4])
best_cp <- classifier$cptable[min.x,1]
pruned_classifier = rpart(formula = case ~ ., 
                          method = "class",
                          data = training_set,
                          control = rpart.control(cp = best_cp))

# Predicting the Test set results
pruned_y_pred = predict(pruned_classifier,
                        newdata = test_set,
                        type = 'class')
print(paste0("accuracy is ", mean(pruned_y_pred == test_set$case))) # 0.688655862726406

# Making the Confusion Matrix
pruned_cm = table(test_set[, 182], pruned_y_pred)
pruned_cm

# get auc
tree.preds <- predict(pruned_classifier, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc) # 0.5649
plot(tree.roc)





















#### male


male_test_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test.rds")
male_train_all_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_train.rds")


# Encoding the target feature as factor
male_test_all_data$case = factor(as.character(male_test_all_data$case),
                                   levels = c("0", "1"))

# Splitting the dataset into
# the Training set and Test set
#install.packages('caTools')
library(caTools)

training_set = male_train_all_data
test_set = male_test_all_data



# Fitting Decision Tree Classification
# to the Training set
# install.packages('rpart')
library(rpart)
training_set$case <- as.character(training_set$case)
classifier = rpart(formula = case ~ ., # without adding cp, fit is not a tree, just a root
                   method = "class",
                   data = training_set,
                   control = rpart.control(cp = 0))
summary(classifier)
plot(classifier)
# Predicting the Test set results
y_pred = predict(classifier,
                 newdata = test_set,
                 type = 'class')
print(paste0("accuracy is ", mean(y_pred == test_set$case))) # 0.5780417

# Making the Confusion Matrix
cm = table(test_set[, 153], y_pred)
cm

# get initial auc
tree.preds <- predict(classifier, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc) # 0.5325
plot(tree.roc)


## better classifier; ONE WE USE
printcp(classifier)
min.x <- which.min(classifier$cptable[, 4])
best_cp <- classifier$cptable[min.x,1]
pruned_classifier = rpart(formula = case ~ ., 
                   method = "class",
                   data = training_set,
                   control = rpart.control(cp = best_cp))

# Predicting the Test set results
pruned_y_pred = predict(pruned_classifier,
                 newdata = test_set,
                 type = 'class')
print(paste0("accuracy is ", mean(pruned_y_pred == test_set$case))) # 0.6751942

# Making the Confusion Matrix
pruned_cm = table(test_set[, 153], pruned_y_pred)
pruned_cm

# get auc
tree.preds <- predict(pruned_classifier, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc) # 0.562
plot(tree.roc)
# end divya

plot(classifier)
text(classifier)


rpart.plot(classifier)

binary.model <- rpart(case ~ ., type="class", data = test_set, cp = .005)
rpart.plot(binary.model)



#another method

predict(tree.preds, test_set, type="prob")

tree.preds <- predict(tree.preds, test_set, type="prob")[, 2]

library(pROC)
tree.roc <- roc(test_set$case, tree.preds)
print(tree.roc)
plot(tree.roc)


