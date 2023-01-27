##### Please copy this file to your own directory and do not edit
##### the file here. Thank you.

rm(list=ls())
library(data.table)

# Loading an extract of the data (500 rows and 1,000 columns)
mydata=fread("Data/ukb669759.csv", data.table=FALSE, nrows=500, select=1:1000)

