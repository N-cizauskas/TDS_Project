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
male_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_train.rds")
male_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/male_test.rds")
female_test <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_test.rds")
female_train <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis/DATA/FINAL_DATA/female_train.rds")

y <- "case"
x <- setdiff(colnames(male_test), c("case"))
male_test <- as.h2o(male_test)
male_test[, y] <- as.factor(male_test[, y])

y <- "case"
x <- setdiff(colnames(female_test), c("case"))
female_test <- as.h2o(female_test)
female_test[, y] <- as.factor(female_test[, y])


# step 3: get a nice consolidated plot :>
varimp <- h2o.permutation_importance(female_model, female_test, metric = "PR_AUC", n_samples=-1,  seed = 7)
first <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Gender = c(rep("Female",3)))
varimp <- h2o.permutation_importance(male_model, male_test, metric = "PR_AUC", n_samples=-1,  seed = 7)
add <- cbind(as.data.frame(varimp)[1:3, c(1,4)], Gender = c(rep("Male",3)))
first <- rbind(first, add)

saveRDS(first, "stacked_ensemble_nicer_plot.rds")

png(file = "stacked_ensemble_ggplot.png")
ggplot(data = first) +
  geom_point(mapping = aes(x = Variable,
                           y = Percentage),
             size = 7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(rows = vars(Gender)) +
  scale_x_discrete(labels=c("Father's History of Prostate Cancer", 
                            "Mother's History of Breast Cancer", 
                            "Lymphocyte Count", 
                            "Stress due to serious illness, injury or assault to oneself",
                            "Testosterone")) +
  ggtitle("Percentage Importance of Top 3 Variables per Gender") +
  ylab("Percentage Importance of Variable")
dev.off()


newfirst <- first %>% arrange(desc(Percentage))
ggplot(data = newfirst) +
  geom_point(mapping = aes(x = Percentage,
                           y = Variable,
                           color = ifelse(Percentage < 0.5,'red', "black")),
             size = 7) +
  theme(legend.position="none") +
  geom_vline(xintercept = 0.5, color = "blue", linetype="dotted") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(cols = vars(Gender)) +
  scale_y_discrete(labels=c("Father's History of Prostate Cancer", 
                            "Mother's History of Breast Cancer", 
                            "Lack of Menopause", 
                            "Stress due to serious illness, injury or assault to oneself",
                            "Testosterone")) +
  annotate("text", x= 0.48, y = 3, label="Importance of 50%", angle=90, color = "blue") +
  ggtitle("Percentage Importance of Top 3 Variables per Gender") +
  xlab("Percentage Importance of Variable in Ensemble Method")