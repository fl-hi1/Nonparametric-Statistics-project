rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

rawdata<-read.delim("../data/raw_datasets/Education_OECD_share_population_25-64_withtertiary_level.csv",
                    sep = ",", quote="", header=T)

rawdata_clean <- as.data.frame(lapply(rawdata, function(x) gsub('"', '', x)))

countries<-read.table("../data/OECD_countries_income_level", header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland")

#Select only countries of interest
education_raw<-rawdata_clean[rawdata_clean$X..Country.. %in% countries,]


#Filter by year
years_of_interest<-c(2007,2008,2010,2012,2014,2016,2018,2020)
education_selected_years<-education_raw[education_raw$X..Year..  %in% years_of_interest,c(2,6,8,10,14,21)]

#Filter only population from 25 to 64
education_selected_pop<-education_selected_years[education_selected_years$X..Indicator..=='Share of population 25 to 64 year-olds by educational attainment',-c(2)]

#Filter only for tertiary education
education_selected_tert<-education_selected_pop[education_selected_pop$X..Education.ISCED.level..== "Total tertiary education (ISCED2011 levels 5 to 8)",-c(2)]

#Reorder
education_reordered<-education_selected_tert[,c(1,3,2,4)]

#First problem: NA estimate:
sum(is.na(education_reordered)) #NO NANs

education_reordered[,c(2,4)] <- as.data.frame(lapply(education_reordered[,c(2,4)], as.numeric))

#Divide by gender
education_female<-education_reordered[education_reordered$X..Gender..=='Women',-c(3)]
education_male<-education_reordered[education_reordered$X..Gender..=='Men',-c(3)]
education_both<-education_reordered[education_reordered$X..Gender..=='Total',-c(3)]

dim(education_female)
dim(education_male)
dim(education_both) #has 3 more obs...


# Reorder columns by year
library(tidyr)

#Female
education_female_table <- education_female %>%
  pivot_wider(names_from = X..Country.., 
              values_from = X..Value..)

education_female_table$X..Year..<-as.integer(education_female_table$X..Year..)
education_female_table <- education_female_table %>%
  select(X..Year.., everything())


#Male
education_male_table <- education_male %>%
  pivot_wider(names_from = X..Country.., 
              values_from = X..Value..)

education_male_table$X..Year..<-as.integer(education_male_table$X..Year..)
education_male_table <- education_male_table %>%
  select(X..Year.., everything())

#Both
education_both_table <- education_both %>%
  pivot_wider(names_from = X..Country.., 
              values_from = X..Value..)

education_both_table$X..Year..<-as.integer(education_both_table$X..Year..)
education_both_table <- education_both_table %>%
  select(X..Year.., everything())


#HOW TO DEAL WITH MISSING VALUES?
#PROBLEMATIC COUNTRIES:
#Australia
#Israel
#Canada
#They may need to be removed as they have too many missing values,
#but for now we use the mean

# calculate column means


# replace NAs with column means
#First I need to convert it to a table
education_both_table <- as.data.frame(education_both_table)
col_means <- base::colMeans( education_both_table,na.rm = TRUE)
education_both_table <- base::replace(education_both_table, is.na(education_both_table), col_means[base::col(education_both_table)][is.na(education_both_table)])

education_male_table <- as.data.frame(education_male_table)
col_means <- base::colMeans( education_male_table,na.rm = TRUE)
education_male_table <- base::replace(education_male_table, is.na(education_male_table), col_means[base::col(education_male_table)][is.na(education_male_table)])

education_female_table <- as.data.frame(education_female_table)
col_means <- base::colMeans( education_female_table,na.rm = TRUE)
education_female_table <- base::replace(education_female_table, is.na(education_female_table), col_means[base::col(education_female_table)][is.na(education_female_table)])


#Saving the updated table
#write.table(education_both_table, 
#            "../data/education_both_2007-2020.txt", 
#          sep = "\t")
#
##Saving the updated table
#write.table(education_male_table, 
#            "../data/education_male_2007-2020.txt", 
#            sep = "\t")
#
##Saving the updated table
#write.table(education_female_table, 
#            "../data/education_female_2007-2020.txt", 
#            sep = "\t")
#

