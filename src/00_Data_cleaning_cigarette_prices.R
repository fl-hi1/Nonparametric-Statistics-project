rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata<-read.csv("../data/raw_datasets/Cigarette_taxes_2008-2020.csv",header=T)

countries<-read.table("../data/OECD_countries_income_level",header=T)$Country

prices<-rawdata[rawdata$Country %in% countries,]




variables<-colnames(rawdata)[2:10]

tax_percent=prices[,c(1,2,10)]

tax_percent$Most.sold.brand.of.cigarettes...price.in.currency.reported

# 
library(tidyr)
library(dplyr)
taxes_table <- tax_percent%>%
  pivot_wider(names_from = Country, 
              values_from = Most.sold.brand.of.cigarettes...price.in.currency.reported)

#sum(is.na(taxes_table)) 0

#Saving the updated table
#write.table(taxes_table, 
#            "../data/cigarette_taxes_2008-2020.txt", 
#            sep = "\t")


