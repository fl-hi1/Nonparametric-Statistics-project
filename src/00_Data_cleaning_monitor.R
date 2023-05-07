rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata<-read.csv("../data/raw_datasets/mpower_overview_factor.csv",header=T)


countries<-read.table("../data/OECD_countries_income_level",header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland")

mpower<-rawdata[rawdata$Country %in% countries,]


#First problem: NA estimate:
sum(is.na(mpower$Protect.from.tobacco.smoke))
#All the NaN are in the Anti tobacco mass-media campains

#Rationale (may be overoptimistic)
#The NA info status is equal or lower to the latest one available


campaigns<-mpower[,c(1,2,9)]

# Reorder columns by year
library(tidyr)
library(dplyr)

campaigns_table <- campaigns %>%
  pivot_wider(names_from = Year, values_from = Anti.tobacco.mass.media.campaigns)


#Values need to be estimated for 2008 2007 for ALL countries
campaigns_table$'2008'=campaigns_table$'2010'
campaigns_table$'2007'=campaigns_table$'2010'

#Converting back 
library(tidyr)

original_table <- campaigns_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Anti.tobacco.mass.media.campaigns")

#Substituting appropriately
mpower$Anti.tobacco.mass.media.campaigns<-original_table


sum(is.na(mpower))

#Saving the updated table
#write.table(mpower, 
#            "mpower.txt", 
#            sep = "\t")


