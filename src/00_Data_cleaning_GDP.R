rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata<-read.csv("../data/raw_datasets/GDP_OECD_2000-2023_quarterly_expenditure_approach_constant_prices_constant_PPP.csv",header=T)

countries<-read.table("../data/OECD_countries_income_level",header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland")

GDP_measures<-rawdata[rawdata$Country %in% countries,c('Country','Year','Transaction','Value')]

GDP_allyears<-GDP_measures[GDP_measures$Transaction=='Gross domestic product (expenditure approach)',c(1,2,4)]


years_of_interest<-c(2007,2008,2010,2012,2014,2016,2018,2020)
GDP<-GDP_allyears[GDP_allyears$Year %in% years_of_interest, ]

#First problem: NA estimate:
sum(is.na(GDP)) #NO NANs


# Reorder columns by year
library(tidyr)

GDP_table <- GDP %>%
  pivot_wider(names_from = Country, 
              values_from = Value)

dim(GDP_table)

#Saving the updated table
#write.table(GDP_table, 
#            "../data/GDP_2007-2020.txt", 
#           sep = "\t")


