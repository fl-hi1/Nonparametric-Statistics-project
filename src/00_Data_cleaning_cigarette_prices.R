rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

library(tidyr)
library(dplyr)

rawdata<-read.csv("../data/raw_datasets/Cigarette_taxes_2008-2020.csv",header=T)

countries<-read.table("../data/OECD_countries_income_level",header=T)$Country
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Slovakia",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland",
             "Costa Rica")
#Extracting the variable of interest
prices<-rawdata[rawdata$Country %in% countries,]
tax_percent=prices[,c(1,2,8)]

#variables<-colnames(rawdata)[2:10]

taxes_table <- tax_percent%>%
  pivot_wider(names_from = Country, 
              values_from = Most.sold.brand.of.cigarettes...taxes.as.a...of.price...total.tax)

sum(is.na(taxes_table)) 

#Missing all values from 2007: imputing value from 2007 from 2008 
taxes_table[8,]<-taxes_table[taxes_table$Year=='2008',]
taxes_table[8,1]<-2007

#Saving the updated table with 2007 imputation
write.table(taxes_table, 
            "../data/cigarette_taxes_2007-2020.txt", 
            sep = "\t")


