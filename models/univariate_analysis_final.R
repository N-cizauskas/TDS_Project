# Univariate analysis 

setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

#install.packages("gtsummary")
# Load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gtsummary)

# Load data
females <- readRDS("DATA/FINAL_DATA/female_train.rds")
males <- readRDS("DATA/FINAL_DATA/male_train.rds")


# Female univariate logistic regression with age (year_birth) and ethnicity as confounders

# Create a list of explanatory variables to remove from analysis
female_vars_conf <- females %>% 
  dplyr::select(-c("case")) %>% 
  colnames()

# iterate through each univariate formula
models_female_conf <- female_vars_conf %>% 
  map(
    .f = ~glm(                                                                     # pass the formulas one-by-one to glm()
      formula = as.formula(str_c("case ~ ", .x, "+ Year_birth")),      # within glm(), the string formula is .x
      family = "binomial",                                                         # specify type of glm (logistic)
      data = females)) 

# tidy up each of the glm regression outputs from above
models_female_conf <- models_female_conf %>% 
  map(
    .f = ~broom::tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE))              # return confidence intervals

# collapse the list of regression outputs in to one data frame
models_female_conf <- models_female_conf %>% 
  bind_rows() 

# round all numeric columns
models_female_conf <- models_female_conf %>% 
  mutate(across(where(is.numeric), round, digits = 3))

# Remove intercept rows from output dataframe
models_female_conf <- models_female_conf[models_female_conf$term!="(Intercept)",]

# Remove ethnicity and year of birth rows from output dataframe 
models_female_conf <- models_female_conf[models_female_conf$term!="ethnicity",]
models_female_conf <- models_female_conf[models_female_conf$term!="Year_birth",]

# Save univariate analysis with confounders data frame
saveRDS(models_female_conf, file = "univ_log_conf_female_scaled.rds")

# Load data 
models_female_conf <- readRDS("univ_log_conf_female_scaled.rds")

# Create a vector of all the variable names
all_vars <- c(names(males), names(females))

# Remove duplicates and sort alphabetically
unique_vars <- sort(unique(all_vars))

# Create a lookup table that maps each variable to a unique number
lookup_table <- data.frame(variable = unique_vars, number = 1:length(unique_vars))

# Volcano plot function
Volcano <- function(results, annot = NULL, lookup_table = NULL) {
  results$log2estimate <- log2(models_female_conf$estimate)
  par(mar = c(4.5, 4.5, 1, 1))
  plot(results$log2estimate, -log10(models_female_conf$p.value), pch = 19,
       las = 1, cex = 0.5, main = "Female Volcano Plot Univariate Logistic Regression", 
       xlab = expression(beta),
       ylab = expression(-log[10](p[value])),
       col = ifelse(models_female_conf$p.value < 0.05/nrow(results), "tomato", "darkgrey")) 
  sig_var <- results$term[models_female_conf$p.value < 0.05/nrow(results)]
  sig_labels <- match(sig_var, lookup_table$variable)
  text(results$log2estimate[models_female_conf$p.value < 0.05/nrow(results)], 
       -log10(models_female_conf$p.value[models_female_conf$p.value < 0.05/nrow(results)]), 
       pos = 3, offset = 0.2, cex = 0.5, labels = sig_labels)
  abline(v = 0, lty = 3)
  abline(h = -log10(0.05/nrow(results)), lty = 2,
         col = "darkred")
  legend("topright", col = c("darkred", "tomato", "darkgrey"), pch = c(NA, 19, 19), cex = 0.7, legend = c("Bonferroni threshold at 0.05",
                                                                                               "Bonferroni significant hits", "Not significant"), 
         lty = c(2, NA, NA))
}

# generate volcano plot for males
Volcano(models_female_conf, lookup_table = lookup_table)

### Now for males

male_vars_conf <- colnames(males)
male_vars_conf <- males %>% 
  dplyr::select(-c("case", "Year_birth")) %>% 
  colnames()


models_male_conf <- male_vars_conf %>% 
  map(
    .f = ~glm(
      formula = as.formula(str_c("case ~ ", .x, "+ Year_birth")),
      family = "binomial",
      data = males)) 

models_male_conf <- models_male_conf %>% 
  map(
    .f = ~broom::tidy(
      .x, 
      exponentiate = TRUE,           
      conf.int = TRUE))            

models_male_conf <- models_male_conf %>% 
  bind_rows() 

models_male_conf <- models_male_conf %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 3)))

# Remove intercept rows from dataframe
models_male_conf <- models_male_conf[models_male_conf$term!="(Intercept)",]

# Remove year of birth rows 
models_male_conf <- models_male_conf[seq(nrow(models_male_conf)) %% 2 != 0, ]

# Save univariate analysis with confounders data frame
saveRDS(models_male_conf, file = "univ_log_conf_male_scaled.rds")

# Load data 
models_male_conf <- readRDS("univ_log_conf_male_scaled.rds")

# Create volcano plot of univariate logistic regression with year of birth and ethnicity as confounders for males

Volcano <- function(results, annot = NULL, lookup_table = NULL) {
  results$log2estimate <- log2(results$estimate)
  par(mar = c(4.5, 4.5, 1, 1))
  plot(results$log2estimate, -log10(results$p.value), pch = 19,
       las = 1, cex = 0.5, main = "Volcano Plot Univariate Logistic Regression Males", 
       xlab = expression(beta),
       ylab = expression(-log[10](p[value])),
       col = ifelse(results$p.value < 0.05/nrow(results), "tomato", "darkgrey")) 
  sig_var <- results$term[results$p.value < 0.05/nrow(results)]
  sig_labels <- match(sig_var, lookup_table$variable)
  text(results$log2estimate[results$p.value < 0.05/nrow(results)], 
       -log10(results$p.value[results$p.value < 0.05/nrow(results)]), 
       pos = 3, offset = 0.2, cex = 0.5, labels = sig_labels)
  abline(v = 0, lty = 3)
  abline(h = -log10(0.05/nrow(results)), lty = 2,
         col = "darkred")
  legend("topright", col = c("darkred", "tomato", "darkgrey"), pch = c(NA, 19, 19), cex = 0.7, legend = c("Bonferroni threshold at 0.05",
                                                                                                          "Bonferroni significant hits", "Not significant"), 
         lty = c(2, NA, NA))
}

Volcano <- function(results, annot = NULL, lookup_table = NULL) {
  results$log2estimate <- log2(models_male_conf$estimate)
  par(mar = c(4.5, 4.5, 1, 1))
  plot(results$log2estimate, -log10(models_male_conf$p.value), pch = 19,
       las = 1, cex = 0.5, main = "Males Volcano Plot Univariate Logistic Regression", 
       xlab = expression(beta),
       ylab = expression(-log[10](p[value])),
       col = ifelse(models_male_conf$p.value < 0.05/nrow(results), "tomato", "darkgrey")) 
  sig_var <- results$term[models_male_conf$p.value < 0.05/nrow(results)]
  sig_labels <- match(sig_var, lookup_table$variable)
  text(results$log2estimate[models_male_conf$p.value < 0.05/nrow(results)], 
       -log10(models_male_conf$p.value[models_male_conf$p.value < 0.05/nrow(results)]), 
       pos = 3, offset = 0.2, cex = 0.5, labels = sig_labels)
  abline(v = 0, lty = 3)
  abline(h = -log10(0.05/nrow(results)), lty = 2,
         col = "darkred")
  legend("topright", col = c("darkred", "tomato", "darkgrey"), pch = c(NA, 19, 19), cex = 0.7, legend = c("Bonferroni threshold at 0.05",
                                                                                                          "Bonferroni significant hits", "Not significant"), 
         lty = c(2, NA, NA))
}
# generate volcano plot for males
Volcano(models_male_conf, lookup_table = lookup_table)

# Save look-up table
saveRDS(lookup_table, file = "lookup_table_univariate.rds")
