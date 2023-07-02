# Test on the trends in prevalence

#Exploratory data analysis
# Jonckheere-Terpstra is a nonparametric test based on ranks to detect trends in
# the data from multiple timepoints

#The hypothesis (H0) I am testing is that we have a significant 
#decreasing trend in the prevalence data.

#I will perform the test for both, for men and women

rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#Import packages
#install.packages("DescTools")
library(DescTools) #to perform the test

data_b<-read.table("../data/smoking_prevalence_both_2007-2020.txt",header=T, sep='',check.names = FALSE)
data_m<-read.table("../data/smoking_prevalence_males_2007-2020.txt",header=T, sep='',check.names = FALSE)
data_f<-read.table("../data/smoking_prevalence_females_2007-2020.txt",header=T, sep='',check.names = FALSE)



#########################################
############## BOTH #####################
#########################################
# Sample data 
data <- data.frame(t(data_b)) ####BOTH
covariate <- data

# Compute the differences between consecutive timepoints
differences <- covariate[, -1] - covariate[, -ncol(covariate)]

# Count the number of positive and negative differences
pos_diff <- sum(differences > 0)
neg_diff <- sum(differences < 0)


# Perform the Jonckheere-Terpstra test
result <- JonckheereTerpstraTest(covariate[, -1], nperm=10000, alternative=c('decreasing'))

# Print the results
cat("Jonckheere-Terpstra Test:\n")
cat("Test statistic:", result$statistic, "\n")
cat("p-value:", result$p.value, "\n")

# Interpret the results
if (result$p.value < 0.05) {
  cat("There is a significant decreasing trend in the covariate across the timepoints.\n")
} else {
  cat("There is no significant trend in the covariate across the timepoints.\n")
}



#########################################
############### MALES ###################
#########################################

# Sample data 
data <- data.frame(t(data_m)) ###MALES
covariate <- data

# Compute the differences between consecutive timepoints
differences <- covariate[, -1] - covariate[, -ncol(covariate)]

# Count the number of positive and negative differences
pos_diff <- sum(differences > 0)
neg_diff <- sum(differences < 0)


# Perform the Jonckheere-Terpstra test
result <- JonckheereTerpstraTest(covariate[, -1], nperm=10000, alternative=c('decreasing'))

# Print the results
cat("Jonckheere-Terpstra Test:\n")
cat("Test statistic:", result$statistic, "\n")
cat("p-value:", result$p.value, "\n")
#p-value: 1e-04 


# Interpret the results
if (result$p.value < 0.05) {
  cat("There is a significant decreasing trend in the covariate across the timepoints.\n")
} else {
  cat("There is no significant trend in the covariate across the timepoints.\n")
}
###################


#########################################
############## FEMALES ##################
#########################################

# Sample data 
data <- data.frame(t(data_f)) ###FEMALES
covariate <- data

# Compute the differences between consecutive timepoints
differences <- covariate[, -1] - covariate[, -ncol(covariate)]

# Count the number of positive and negative differences
pos_diff <- sum(differences > 0)
neg_diff <- sum(differences < 0)

# Perform the Jonckheere-Terpstra test

# Perform the Jonckheere-Terpstra test
result <- JonckheereTerpstraTest(covariate[, -1], nperm=10000, alternative=c('decreasing'))

# Print the results
cat("Jonckheere-Terpstra Test:\n")
cat("Test statistic:", result$statistic, "\n")
cat("p-value:", result$p.value, "\n")
#p-value: 3e-04 


# Interpret the results
if (result$p.value < 0.05) {
  cat("There is a significant decreasing trend in the covariate across the timepoints.\n")
} else {
  cat("There is no significant trend in the covariate across the timepoints.\n")
}



