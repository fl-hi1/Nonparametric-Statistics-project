rm(list=ls())

###Setting the working directory as the project directory
setwd("C:/Users/Val/OneDrive - Politecnico di Milano/travail Polimi/Nonparametric-Statistics-project/src")

inputpath = "../data"
outputpath = "../data"

#Imports
library(tidyr)
library(dplyr)
library(lme4)
library(pbapply)
library(mgcv)
library(conformalInference)
library(ggplot2)
library(progress)
library(parallel)
library(pbapply)

seed=2022
B=1000

#Import data
fulldata<-read.table("../data/final_dataset_2007-2020.txt",header=T)


fulldata$Year<-as.numeric(fulldata$Year)
fulldata$clust_5 <- as.factor(fulldata$clust_5)

final_model_females<-gam(
  Prevalence_females ~
    Year*clust_5 +
    Country+
    s(HDI, bs = 'cr') +
    Affordability,
  data=fulldata
)
summary(final_model_females)

###let's try a model with 10 years ahead predictions###
countries = unique(fulldata$Country)

df = data.frame(matrix(ncol = 8, nrow = 0))

for(count in countries){
  loc_df = fulldata[fulldata$Country==count,]
  loc_df = loc_df[c("Country","Year","Prevalence_females","Prevalence_males","Prevalence_both","clust_5")]
  change_females =  loc_df$Prevalence_females[8] - loc_df$Prevalence_females[2] #difference from 2008 to 2020
  change_males =  loc_df$Prevalence_males[8] - loc_df$Prevalence_males[2]
  change_both =  loc_df$Prevalence_both[8] - loc_df$Prevalence_both[2]
  change_females_perc =  (loc_df$Prevalence_females[8] - loc_df$Prevalence_females[2])/loc_df$Prevalence_females[2] #difference from 2008 to 2020
  change_males_perc =  (loc_df$Prevalence_males[8] - loc_df$Prevalence_males[2])/loc_df$Prevalence_males[2]
  change_both_perc =  (loc_df$Prevalence_both[8] - loc_df$Prevalence_both[2])/loc_df$Prevalence_both[2]
  clust = loc_df$clust_5[1]
  df = rbind(df, c(count,change_females,change_males,change_both,change_females_perc,change_males_perc,change_both_perc,clust))
}
u <- c("Country","change_females","change_males","change_both","change_females_perc","change_males_perc","change_both_perc","clust")
colnames(df)<- u
 
df$clust <- as.factor(df$clust)

plot(df$clust,df$change_females_perc)

plot(df$clust,df$change_both_perc)

plot(df$clust,df$change_males_perc)

model1 = lm(change_females ~ clust, data = df)
summary(model1)

model2 = lm(change_females_perc ~ clust, data = df)
summary(model)
