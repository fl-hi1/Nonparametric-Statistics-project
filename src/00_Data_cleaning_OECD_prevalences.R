rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata_1<-read.csv("../data/raw_datasets/OECD_data.csv",header=T)

rawdata<-rawdata_1[,c(1,6,7)]


library(tidyr)
library(dplyr)

data1 <- rawdata %>%
  pivot_wider(names_from = LOCATION, values_from = Value)

# Reorder columns by year
data1 <- data1[order(data1$TIME), ]
years<-data1$TIME
data<-data1[,-1]
data<-as.data.frame(data)
rownames(data)<-years





#Plot of the time points, points indicate the values
matplot(years,
        data,  
        type='l',
        xlab="years",
        ylab="Age-standardized percent",
        main="Age-standardized smoking prevalence")


variables<-colnames(rawdata)[2:10]

tax_percent=prices[,c(1,2,10)]

tax_percent$Most.sold.brand.of.cigarettes...price.in.currency.reported


#Saving the updated table
#write.table(taxes_table, 
#            "../data/cigarette_taxes_2008-2020.txt", 
#            sep = "\t")


