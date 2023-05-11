setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


library(readr)
library(dplyr)
library(h2o)
library(ggbeeswarm)
library(lares)
library(caTools)
h2o.init(nthreads = -1)

# import data (scaled)
male_train_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_train.rds")
male_test_data <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test.rds")
female_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test.rds")
female_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_train.rds")

## FEMALE  ############################################
training_set = female_train
test_set = female_test
# convert to h2o
train = as.h2o(training_set %>% select_if(~is.numeric(.x))) # does not include age_last_cont

test = as.h2o(test_set %>% select_if(~is.numeric(.x))) 

# MODELLING CODE START HERE

y <- "case"
x <- setdiff(colnames(test), c("case"))

train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])


# Number of CV folds (to generate level-one data for stacking)
nfolds <- 50

# A 2 stack from gbm and randomforest
# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                    y = y,
                    nfolds = nfolds,
                    seed = 1,
                    keep_cross_validation_predictions = TRUE,
                    training_frame = train)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf),
                                seed = 1)
model_path <- h2o.saveModel(object = ensemble, path = getwd(), force = TRUE)
print("Female Ensemble path is:")
print(model_path)

# Get training values
print(sprintf("Female RF Train AUC:  %s", h2o.auc(my_rf)))
print("RF Train confusion matrix")
h2o.confusionMatrix(my_rf)
print(sprintf("Female Ensemble Train AUC:  %s", h2o.auc(ensemble)))
print("Ensemble Train confusion matrix")
h2o.confusionMatrix(ensemble)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Female Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Female Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)
print(sprintf("Female Best Random Forest Test AUC:  %s", h2o.auc(perf_rf_test)))
print("RF confusion matrix")
h2o.confusionMatrix(my_rf, newdata=test)
png(file="female_rf_var_imp_1.png")
h2o.varimp_plot(my_rf, 20)
dev.off()

#pred <- h2o.predict(ensemble, newdata = test)
print("ensemble confusion matrix")
h2o.confusionMatrix(ensemble, newdata = test)

# interpretability plots
 # plot permutation importance (bar plot)
png(file="female_permutation_imp_bar_1.png")
h2o.permutation_importance_plot(ensemble, train, metric = "PR_AUC", n_samples=-1)
dev.off()

# plot permutation importance (box plot)
png(file="female_permutation_imp_box_1.png")
h2o.permutation_importance_plot(ensemble, train,metric = "PR_AUC", n_samples=-1, n_repeats=50)
dev.off()

# plot shap
png(file="female_shap_1.png")
#SHAP_values <- h2o_shap(my_rf)
h2o.shap_summary_plot(my_rf, newdata=test)
# Plot SHAP values (feature importance)
dev.off()





## MALE ############################################
training_set = male_train_data
test_set = male_test_data

train = as.h2o(training_set %>% select_if(~is.numeric(.x)))
test = as.h2o(test_set %>% select_if(~is.numeric(.x)))

# MODELLING CODE START HERE
y <- "case"
x <- setdiff(colnames(test), c("case"))


train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  nfolds = nfolds,
                  seed = 1,
                  keep_cross_validation_predictions = TRUE,
                  training_frame = train)
# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf),
                                seed = 1)
model_path <- h2o.saveModel(object = ensemble, path = getwd(), force = TRUE)
print("Male Ensemble path is:")
print(model_path)

# Get training values
print(sprintf("Male RF Train AUC:  %s", h2o.auc(my_rf)))
print("RF Train confusion matrix")
h2o.confusionMatrix(my_rf)
print(sprintf("Male Ensemble Train AUC:  %s", h2o.auc(ensemble)))
print("Ensemble Train confusion matrix")
h2o.confusionMatrix(ensemble)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Male Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
# [1] "Best Base-learner Test AUC:  0.556098320415567"
print(sprintf("Male Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Ensemble Test AUC:  0.559497412307603"

# Generate predictions on a test set (if neccessary)
print(sprintf("Male Best Random Forest Test AUC:  %s", h2o.auc(perf_rf_test)))
print("RF confusion matrix")
h2o.confusionMatrix(my_rf, newdata=test)
png(file="male_rf_var_imp_1.png")
h2o.varimp_plot(my_rf, 20)
dev.off()

#pred <- h2o.predict(ensemble, newdata = test)
print("male ensemble confusion matrix")
h2o.confusionMatrix(ensemble, newdata = test)

# interpretability plots
# plot permutation importance (bar plot)
png(file="male_permutation_imp_bar_1.png")
h2o.permutation_importance_plot(ensemble, train, metric = "PR_AUC", n_samples=-1)
dev.off()

# plot permutation importance (box plot)
png(file="male_permutation_imp_box_1.png")
h2o.permutation_importance_plot(ensemble, train, metric = "PR_AUC", n_samples=-1, n_repeats=50)
dev.off()

# plot shap
png(file="male_shap_1.png")
#SHAP_values <- h2o_shap(ensemble, test=test)
h2o.shap_summary_plot(my_rf, newdata=test)
# Plot SHAP values (feature importance)
dev.off()