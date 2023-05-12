# Blood variables data cleaning 

# Set working directory
setwd("/rds/general/project/hda-22-23/live/TDS/Group8_Working/analysis")

# Load libraries
library(dplyr)
library(tidyverse)

# Load data
male <- readRDS("cleaned_male_0903.rds")
female <- readRDS("cleaned_female_0903.rds")

# Create histograms and boxplots of all the blood variables
blood_cols <- male[, 60:76]

pdf("Figures/blood_distributions.pdf")
par(mfrow = c(3, 2), mar = c(5, 5, 1, 1))
for (k in seq_along(blood_cols)) {
  col_data <- blood_cols[, k]
  col_name <- colnames(blood_cols)[k]
  hist(col_data, col = "skyblue", xlab = "",
       main = "", ylab = col_name)
  boxplot(col_data, xlab = "", main = "", ylab = col_name)
}
dev.off()
par(mfrow = c(1,2))

# Check ranges of all blood variables

for (k in blood_cols) {
  ranges <- range(k, na.rm = TRUE)
  print(ranges)
  
}

# Check percentages of columns with outliers > or < 1.5*IQR
for (k in blood_cols) {
  Q <- quantile(k, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(k, na.rm = TRUE)
  low <- sum((k < (Q[1] - 1.5*iqr)), na.rm = TRUE) / length(k) * 100
  high <-sum((k > (Q[2] + 1.5*iqr)), na.rm = TRUE) / length(k) * 100
  outliers <- low + high 
  print(outliers) 
}

# WBC - Normal = 4.5-11
hist(male$WBC_count)
boxplot(male$WBC_count)
range(male$WBC_count, na.rm = TRUE) # 0 - 74.08
sum(male$WBC_count > 11.155, na.rm = TRUE) # 3712
sum(male$WBC_count < 2.315, na.rm = TRUE) # 57
# 1 value of 0. Whilst possible due to unreadable value, neut and leuc also 0, so could work, but other bloods are normal which makes no sense
male <- subset(male, WBC_count != 0 | is.na(WBC_count)) # Removes one row

# RBC - Normal = 4-5.9
hist(male$RBC_count)
boxplot(male$RBC_count)
range(male$RBC_count, na.rm = TRUE) # 0.006 - 7.840
sum(male$RBC_count < 3.8135, na.rm = TRUE) #1432
sum(male$RBC_count > 5.6975, na.rm = TRUE) #1507
# RBC value 0.006 left in at the moment as all blood values for this individual very low
## WBC 0.08, platelet 0.3, haematocrit 0.05, lymph 0.01, Neut 0.04, CRP 0.77
## These are either incorrect or patient has a haematological disorder. 
## Not a case but are we cofident that all other cancer cases have been removed?

# Haematorit percentage 
## Normal = 41-50%
## Range = 0.05 - 67.9
## 0.05 is same individual with the lowest RBC count
## Also noted that the next lowes haematocrit is 0.18. Haematocrit level < 1% really shouldn't be possible
## These two individuals also both have the two lowest WBC, Hb, platelet
## Haematocrit < 1% is definitely not possible so these will be removed
male <- subset(male, Haematocrit_percentage > 1 | is.na(Haematocrit_percentage))  # Removes 2 rows

# Platelet count
## Normal = 150-400
## Range = 0.3 - 1215.0
## Minimum now = 7.8 after removal of previous rows
## 1215 is high, but not ridiculous after infection or trauma etc
## No further rows removed

# Lymphocyte count 
## Normal = 1-4
## Range = 0.0 - 55.5
## 3 values = 0, whilst this seems impossible, it is very rare, but in rare cases the lymphocyte count can be 
## undetectable due to extreme low levels, but still compatible with life, although suggests significant illness
## Could also be equipment issue
## They are not a case

# Neutrophil count
## Normal = 2-7.5
## Range = 0.0 - 18.7
## Again 2 counts of neut count 0, next lowest is 0.2
## Same as lymphocyte count, not impossible if undetectable, but suggests patient would be very unwell
## They are not cases 

# apob
# Normal = <1.3
# 0.401 - 2.000
# All seem reasonable so left

# cholesterol 
# < 5  
# 0.601 - 12.987
# Nothing unusual in this range - left as it is

# CRP
# Normal <10mg/L
# 0.08 - 79.49
# Nothing too abnormal here, however there are 8% of values 1.5*IQR above the 3rd quartile due to it not being uncommon to have raised CRP
# Left as it is, but may need to consider sensitivity analysis

# glucose
# Normal = 4-6 before meals and <8 after eating
# 0.995 - 36.813
# 17 values less than 2 suggesting very severe hypoglycaemic episode, if these patients are insulin dependent diabetics
# may be possible that they are not severely unwell, however usually these levels would induce coma
# 4 values > 30. This can be possible in untreated diabetes, but high risk of DKA
# Are all these patients having these results in a walk in clinic and not hospitalised?
# But all possible
# Again no cases

# hba1c
# >48 = diabetes
# 15.6 - 515.2
# The three highest 265 and 515 are not possible and both have normal glucose ? error
# Not cases
# Remove these rows
male <- subset(male, hba1c < 200 | is.na(hba1c))  # Removes 3 rows

# hdl
# Normal >1mmol/L
# 0.219 - 4.186
# Nothing particularly abnormal

# ifg1 (Insulin like growth factor)
# 1.909 - 124.818
# Whilst abnormal, very high values can be indicative of diseases such as acromegaly

# LDL
# Normal <3mmol/L
# 0.266 - 8.992
# Nothing too abnormal

# Testosterone
# 0.354 54.342
# Fairly normal distribution, not too abnormal - leave

# triglyceride
# Normal <1.7mmol/L
#0.233 - 11.278
# Slight right skew, but values are very plausible 

# vit d
# Normal 30-75/mmol/l
# 10 - 335
# 335 is very high but probably possible 

# No extreme values were cases 

# Save new dataframe
saveRDS(male, file = "cleaned_male_1003.rds")

#### Now do the same for females #### 

# Create histograms and boxplots of all the blood variables
blood_cols_fem <- female[, 65:81]

pdf("Figures/blood_distributions_female_preimpute.pdf")
par(mfrow = c(3, 2), mar = c(5, 5, 1, 1))
for (k in seq_along(blood_cols_fem)) {
  col_data <- blood_cols_fem[, k]
  col_name <- colnames(blood_cols_fem)[k]
  hist(col_data, col = "skyblue", xlab = "",
       main = "", ylab = col_name)
  boxplot(col_data, xlab = "", main = "", ylab = col_name)
}
dev.off()
par(mfrow = c(1,2))

# Check ranges of all blood variables
for (k in blood_cols_fem) {
  ranges <- range(k, na.rm = TRUE)
  print(ranges)
  
}

# Check percentages of columns with outliers > or < 1.5*IQR
for (k in blood_cols_fem) {
  Q <- quantile(k, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(k, na.rm = TRUE)
  low <- sum((k < (Q[1] - 1.5*iqr)), na.rm = TRUE) / length(k) * 100
  high <-sum((k > (Q[2] + 1.5*iqr)), na.rm = TRUE) / length(k) * 100
  outliers <- low + high 
  print(outliers) 
}

# WBC - Normal = 4.5-11
# 0.04 - 101.9
# All possible, but is right skewed due to 2 very high values

# RBC - Normal = 3.8-5.2
 # 0.006 - 6.74
# RBC value 0.006 left in at the moment as all blood values for this individual very low
## These are either incorrect or patient has a haematological disorder. 
## Not a case but are we confident that all other cancer cases have been removed?

# Haematorit percentage 
## Normal = 36-44%
## Range = 0.05 - 60.4
## 0.05 is same individual with the lowest RBC count, haematocrit 0.06 also has individual with RBC 0.006
# Haematocrit level < 1% really shouldn't be possible, probably shouldn't be compatibile with life <10%
## These two individuals also both have the two lowest WBC, Hb, platelet - either issue with blood machine or patient extremely unwell
## Haematocrit < 1% is definitely not possible so these will be removed -removes 5 observations
female <- subset(female, Haematocrit_percentage > 1 | is.na(Haematocrit_percentage)) # 5 rows removed

# Platelet count
## Normal = 150-400
## Range = 0.4 - 1033
## Minimum now = 3 after removal of previous rows
## 1033 is high, but not ridiculous after infection or trauma etc
## No further rows removed

# Lymphocyte count 
## Normal = 1-4
## Range = 0.0 - 85.71
## 3 values = 0, whilst this seems impossible, it is very rare, but in rare cases the lymphocyte count can be 
## undetectable due to extreme low levels, but still compatible with life, although suggests significant illness
## Could also be equipment issue
## They are not a case

# Neutrophil count
## Normal = 2-7.5
## Range = 0.0 - 25.1
## Again 2 counts of neut count 0, next lowest is 0.2
## Same as lymphocyte count, not impossible if undetectable, but suggests patient would be very unwell
## They are not cases 

# apob
# Normal = <1.3
# 0.401 - 2.000
# All seem reasonable so left, also normal distribution

# cholesterol 
# < 5  
# 1.801 - 13.719
# Nothing unusual in this range - left as it is, normal distribution

# CRP
# Normal <10mg/L
# 0.08 79.95
# Nothing too abnormal here, however there are 8% of values 1.5*IQR above the 3rd quartile due to it not being uncommon to have raised CRP
# Left as it is, but may need to consider sensitivity analysis
# Lots of values in 60's and 70's

# glucose
# Normal = 4-6 before meals and <8 after eating
# 1.414 34.478
# 8 values less than 2 suggesting very severe hypoglycaemic episode, if these patients are insulin dependent diabetics
# may be possible that they are not severely unwell, however usually these levels would induce coma
# 2 values > 30. This can be possible in untreated diabetes, but high risk of DKA
# Are all these patients having these results in a walk in clinic and not hospitalised?
# But all possible
# 2 cases within these values

# hba1c
# >48 = diabetes
# 15.3 296.1
# The highest 296 is not possible and has normal glucose ? error
# Not case
# Remove these row > 200
female <- subset(female, hba1c < 200 | is.na(hba1c))  # 1 removed

# hdl
# Normal >1mmol/L
# 0.228 4.129
# Nothing particularly abnormal, normal distribution

# ifg1 (Insulin like growth factor)
# 1.927 113.832
# Whilst abnormal, very high values can be indicative of diseases such as acromegaly

# LDL
# Normal <3mmol/L
# 0.751 9.797
# Nothing too abnormal

# Testosterone
# Normal <2.7
# 0.350 49.845
# Right skewed, 1 very high value but other bloods for this individual normal ? testosterone replacement


# triglyceride
# Normal <1.7mmol/L
# 0.238 11.245
# Slight right skew, but values are very plausible 

# vit d
# Normal 30-75/mmol/l
# 10 297
# Again whilst there are some very high values, all are likely plausible

# Save new dataframe
saveRDS(female, file = "cleaned_female_1003.rds")

#### Convert northing and easting coordinates to region ####
# Load packages
install.packages("units")
install.packages("sf", dependencies=True)
library(tidyverse)
library(sf)

# Create id in males df from rownames for merging at the end
male$id <- rownames(male)

# Subset columns required
utm <- subset(male, select = c("id", "place_of_birth_N", "place_of_birth_E"))

df = read.table(text = 'Easting Northing 
 320875 116975     
                320975 116975     
                320975 116925     
                321175 116925    
                321175 116875     
                321275 116875', header = TRUE)


#> Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.3
df %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()
utm <- utm[complete.cases(utm),]
utm1 <- data.frame(x=utm$Northing,y=utm$Easting) 
coordinates(utm1) <- ~x+y 
class(utm1)
proj4string(utm1) <- CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +ellps=WGS84") 
utm2 <- spTransform(utm1,CRS("+proj=longlat +datum=WGS84"))