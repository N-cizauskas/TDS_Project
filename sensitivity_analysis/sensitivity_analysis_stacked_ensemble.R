setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


library(readr)
library(dplyr)
library(h2o)
library(ggbeeswarm)
library(lares)
library(caTools)
library(ggplot2)
h2o.init(nthreads = -1)

# step 1: import model
female_model <- h2o.loadModel("StackedEnsemble_model_R_1683070681182_5162")
male_model <- h2o.loadModel("StackedEnsemble_model_R_1683070681182_33688")

# step 2: import data and prepare for modeling
female_test_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test_stratify.rds")
# Breast Ovarian Thyroid Uterine 
y <- "case"
x <- setdiff(colnames(female_test_stratify), c("case"))

female_breast <- as.h2o(female_test_stratify %>% filter(cancer_type == "Breast" | case == 0) %>% dplyr::select(-c("cancer_type")))
female_breast[, y] <- as.factor(female_breast[, y])
female_ovarian <- as.h2o(female_test_stratify %>% filter(cancer_type == "Ovarian" | case == 0) %>% dplyr::select(-c("cancer_type")))
female_ovarian[, y] <- as.factor(female_ovarian[, y])
female_thyroid <- as.h2o(female_test_stratify %>% filter(cancer_type == "Thyroid" | case == 0) %>% dplyr::select(-c("cancer_type")))
female_thyroid[, y] <- as.factor(female_thyroid[, y])
female_uterine <- as.h2o(female_test_stratify %>% filter(cancer_type == "Uterine"| case == 0) %>% dplyr::select(-c("cancer_type")))
female_uterine[, y] <- as.factor(female_uterine[, y])

male_test_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test_stratify.rds")
# Breast Prostate  Thyroid 
y <- "case"
x <- setdiff(colnames(male_test_stratify), c("case"))

male_breast <- as.h2o(male_test_stratify %>% filter(cancer_type == "Breast" | case == 0) %>% dplyr::select(-c("cancer_type")))
male_breast[, y] <- as.factor(male_breast[, y])
male_prostate <- as.h2o(male_test_stratify %>% filter(cancer_type == "Prostate" | case == 0) %>% dplyr::select(-c("cancer_type")))
male_prostate[, y] <- as.factor(male_prostate[, y])
male_thyroid <- as.h2o(male_test_stratify %>% filter(cancer_type == "Thyroid" | case == 0) %>% dplyr::select(-c("cancer_type")))
male_thyroid[, y] <- as.factor(male_thyroid[, y])

# step 3: predict model performance
perf <- h2o.performance(female_model, newdata = female_breast)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_breast Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_breast confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_breast)
png(file="sensitivity_female_breast.png")
h2o.permutation_importance_plot(female_model, female_breast, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(female_model, newdata = female_ovarian)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_ovarian Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_ovarian confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_ovarian)
png(file="sensitivity_female_ovarian.png")
h2o.permutation_importance_plot(female_model, female_ovarian, metric = "PR_AUC", n_samples=-1)
dev.off()


perf <- h2o.performance(female_model, newdata = female_thyroid)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_thyroid Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_thyroid confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_thyroid)
png(file="sensitivity_female_thyroid.png")
h2o.permutation_importance_plot(female_model, female_thyroid, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()


perf <- h2o.performance(female_model, newdata = female_uterine)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_uterine Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_uterine confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_uterine)
png(file="sensitivity_female_uterine.png")
h2o.permutation_importance_plot(female_model, female_uterine, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(male_model, newdata = male_breast)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("male_breast Ensemble Test AUC:  %s", ensemble_auc_test))
print("male_breast confusion matrix")
h2o.confusionMatrix(male_model, newdata=male_breast)
png(file="sensitivity_male_breast.png")
h2o.permutation_importance_plot(male_model, male_breast, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(male_model, newdata = male_prostate)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("male_prostate Ensemble Test AUC:  %s", ensemble_auc_test))
print("male_prostate confusion matrix")
h2o.confusionMatrix(male_model, newdata=male_prostate)
png(file="sensitivity_male_prostate.png")
h2o.permutation_importance_plot(male_model, male_prostate, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(male_model, newdata = male_thyroid)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("male_breast Ensemble Test AUC:  %s", ensemble_auc_test))
print("male_breast confusion matrix")
h2o.confusionMatrix(male_model, newdata=male_thyroid)
png(file="sensitivity_male_thyroid.png")
h2o.permutation_importance_plot(male_model, male_thyroid, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

# step 4: get a nice consolidated plot :>
varimp <- h2o.permutation_importance(female_model, female_breast, metric = "PR_AUC", n_samples=-1,  seed = 7)
first <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Breast Cancer",3)), Gender = c(rep("Female",3)))
varimp <- h2o.permutation_importance(female_model, female_ovarian, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Ovarian Cancer",3)), Gender = c(rep("Female",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(female_model, female_thyroid, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Thyroid Cancer",3)), Gender = c(rep("Female",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(female_model, female_uterine, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Uterine Cancer",3)), Gender = c(rep("Female",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(male_model, male_breast, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Breast Cancer",3)), Gender = c(rep("Male",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(male_model, male_prostate, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Prostate Cancer",3)), Gender = c(rep("Male",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(male_model, male_thyroid, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Cancer = c(rep("Thyroid Cancer",3)), Gender = c(rep("Male",3)))
first <- rbind(first, add)

saveRDS(first, "sensitivity_analysis_cancer_subtype.rds")

png(file = "sensitivity_analysis_ggplot.png")
ggplot(data = first) +
  geom_point(mapping = aes(y = Variable,
             x = Percentage,
             color = Cancer),
             size = 7) +
  scale_y_discrete(labels=c("Father's Prostate Cancer History", 
                            "Mother's History of Breast Cancer", 
                            "Glucose",
                            "Lymphocyte Count",
                            "No Menopause",
                            "Number of Live Births",
                            "Presence of Other Medical Conditions",
                            "No stress",
                            "Stress due to serious illness, injury or assault to oneself",
                            "Testosterone",
                            "Year of birth")) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = 0.5, color = "blue", linetype="dotted") + 
  annotate("text", x= 0.48, y = 3, label="Importance of 50%", angle=90, color = "blue") +
  facet_grid(cols = vars(Gender)) +
  ggtitle("Percentage Importance of Top 3 Variables per Cancer Subtype") +
  xlab("Percentage Importance of Variable")
dev.off()