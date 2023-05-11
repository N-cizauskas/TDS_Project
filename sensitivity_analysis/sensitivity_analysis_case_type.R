setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")


library(readr)
library(dplyr)
library(h2o)
library(ggbeeswarm)
library(lares)
library(caTools)
library(ggplot)
h2o.init(nthreads = -1)

# step 1: import model
female_model <- h2o.loadModel("StackedEnsemble_model_R_1683070681182_5162")
male_model <- h2o.loadModel("StackedEnsemble_model_R_1683070681182_33688")

# step 2: import data and prepare for modeling
female_incident_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_incident_stratify.rds")
y <- "case"
x <- setdiff(colnames(female_test_stratify), c("case"))
female_incident_stratify <- as.h2o(female_incident_stratify)
female_incident_stratify[, y] <- as.factor(female_incident_stratify[, y])

female_prevalent_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_prevalent_stratify.rds")
y <- "case"
x <- setdiff(colnames(female_prevalent_stratify), c("case"))
female_prevalent_stratify <- as.h2o(female_prevalent_stratify)
female_prevalent_stratify[, y] <- as.factor(female_prevalent_stratify[, y])

male_incident_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_incident_stratify.rds")
y <- "case"
x <- setdiff(colnames(male_incident_stratify), c("case"))
male_incident_stratify <- as.h2o(male_incident_stratify)
male_incident_stratify[, y] <- as.factor(male_incident_stratify[, y])

male_prevalent_stratify <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_prevalent_stratify.rds")
y <- "case"
x <- setdiff(colnames(male_prevalent_stratify), c("case"))
male_prevalent_stratify <- as.h2o(male_prevalent_stratify)
male_prevalent_stratify[, y] <- as.factor(male_prevalent_stratify[, y])



# step 3: predict model performance
perf <- h2o.performance(female_model, newdata = female_incident_stratify)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_incident_stratify Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_incident_stratify confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_incident_stratify)
png(file="sensitivity_female_incident.png")
h2o.permutation_importance_plot(female_model, female_incident_stratify, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(female_model, newdata = female_prevalent_stratify)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("female_prevalent_stratify Ensemble Test AUC:  %s", ensemble_auc_test))
print("female_prevalent_stratify confusion matrix")
h2o.confusionMatrix(female_model, newdata=female_prevalent_stratify)
png(file="sensitivity_female_prevalent.png")
h2o.permutation_importance_plot(female_model, female_prevalent_stratify, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(male_model, newdata = male_incident_stratify)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("male_incident_stratify Ensemble Test AUC:  %s", ensemble_auc_test))
print("male_incident_stratify confusion matrix")
h2o.confusionMatrix(male_model, newdata=male_incident_stratify)
png(file="sensitivity_male_incident.png")
h2o.permutation_importance_plot(male_model, male_incident_stratify, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()

perf <- h2o.performance(male_model, newdata = male_prevalent_stratify)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("male_prevalent_stratify Ensemble Test AUC:  %s", ensemble_auc_test))
print("male_prevalent_stratify confusion matrix")
h2o.confusionMatrix(male_model, newdata=male_prevalent_stratify)
png(file="sensitivity_male_prevalent.png")
h2o.permutation_importance_plot(male_model, male_prevalent_stratify, metric = "PR_AUC", n_samples=-1, n_repeats=1)
dev.off()



# step 4: get a nice consolidated plot :>
varimp <- h2o.permutation_importance(female_model, female_incident_stratify, metric = "PR_AUC", n_samples=-1, seed = 7)
first <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Case = c(rep("Incident Cases",3)), Gender = c(rep("Female",3)))
varimp <- h2o.permutation_importance(female_model, female_prevalent_stratify, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Case = c(rep("Prevalent Cases",3)), Gender = c(rep("Female",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(male_model, female_thyroid, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Case = c(rep("Incident Cases",3)), Gender = c(rep("Male",3)))
first <- rbind(first, add)
varimp <- h2o.permutation_importance(male_model, female_uterine, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Case = c(rep("Prevalent Cases",3)), Gender = c(rep("Male",3)))
first <- rbind(first, add)
saveRDS(first, "sensitivity_analysis_casetype.rds")

png(file = "sensitivity_analysis_casetype_ggplot.png")
ggplot(data = first) +
  geom_point(mapping = aes(y = Variable,
                           x = Percentage,
                           color = Case),
             size = 7) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(cols = vars(Gender)) +
  scale_y_discrete(labels=c("Age",
                            "Black or Black British Ethnicity", 
                            "Mother's History of Breast Cancer", 
                            "IFG1",
                            "LDL",
                            "Lymphocyte Count", 
                            "Number of Live Births",
                            "Lack of Other Medical Conditions",
                            "Standing Height",
                            "Stress due to serious illness, injury or assault to oneself")) +
  geom_vline(xintercept = 0.5, color = "blue", linetype="dotted") + 
  annotate("text", x= 0.48, y = 3, label="Importance of 50%", angle=90, color = "blue") +
  ggtitle("Percentage Importance of Top 3 Variables per Case Type") +
  xlab("Percentage Importance of Variable from Ensemble Method")
dev.off()


