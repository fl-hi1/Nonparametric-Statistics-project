rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata<-read.csv("../data/raw_datasets/Cigarettes_affordability_2010-2020.csv",header=T)


countries<-read.table("../data/OECD_countries_income_level",header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland")

affordability<-rawdata[rawdata$Country %in% countries,c(1,2,3)]


#First problem: NA estimate:
sum(is.na(affordability)) #NO NANs
affordability$Affordability...percentage.of.GDP.per.capita.required.to.purchase.2000.cigarettes.of.the.most.sold.brand

# Reorder columns by year
library(tidyr)

affordability_table <- affordability %>%
  pivot_wider(names_from = Country, 
              values_from = Affordability...percentage.of.GDP.per.capita.required.to.purchase.2000.cigarettes.of.the.most.sold.brand)


#Saving the updated table
write.table(affordability_table, 
            "../data/affordability_2010-2020.txt", 
           sep = "\t")


