rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#Imports
library(tidyr)
library(dplyr)

#Extracting rawdata- 
#Beware that here we are extracting the sets from 2007 to 2020
#With imputations
prevalence_table<-read.table("../data/smoking_prevalence_both_2007-2020.txt",header=T, sep='',check.names = FALSE)
affordability_table<-read.table("../data/affordability_2007-2020.txt",header=T, sep='',check.names = FALSE)
taxes_table<-read.table("../data/cigarette_taxes_2007-2020.txt",header=T,sep='',check.names = FALSE)
GDP_table<-read.table("../data/GDP_2007-2020.txt",header=T ,sep='',check.names = FALSE)
education_table<-read.table("../data/education_both_2007-2020.txt",header=T,sep='',check.names = FALSE)
colnames(education_table)[1]<-"Year"
bans_table<-read.table("../data/mpower_bans_2007-2020.txt",header=T,sep='',check.names = FALSE)
warn_table<-read.table("../data/mpower_warn_2007-2020.txt",header=T,sep='',check.names = FALSE)
help_table<-read.table("../data/mpower_help_2007-2020.txt",header=T,sep='',check.names = FALSE)
protect_table<-read.table("../data/mpower_protect_2007-2020.txt",header=T,sep='',check.names = FALSE)
campaigns_table<-read.table("../data/mpower_campaigns_2007-2020.txt",header=T,sep='',check.names = FALSE)


############Converting back MPOWER DATA
campaigns <- campaigns_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Campaigns")
protect <- protect_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Protect")
warn <- warn_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Warn")
help <- help_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Help")
bans <- bans_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Bans")



####MERGE ALL NPOWER DATA
####DO NOT RUN TWICE!
merged_mpower <- merge(campaigns, 
                       help,
                       by = c("Country", "Year"), 
                       all = TRUE)
merged_mpower <- merge(merged_mpower, 
                       warn,
                       by = c("Country", "Year"), 
                       all = TRUE)
merged_mpower <- merge(merged_mpower, 
                       bans,
                       by = c("Country", "Year"), 
                       all = TRUE)
merged_mpower <- merge(merged_mpower, 
                       protect,
                       by = c("Country", "Year"), 
                       all = TRUE)



#Converting back GDP, education data
GDP <- GDP_table %>%
  pivot_longer(cols = -Year, names_to = "Country", 
               values_to = "GDP")
education <- education_table %>%
  pivot_longer(cols = -Year, names_to = "Country", 
               values_to = "Education")

affordability <- affordability_table %>%
  pivot_longer(cols = -Year, names_to = "Country", 
               values_to = "Affordability")

taxes <- taxes_table %>%
  pivot_longer(cols = -Year, names_to = "Country", 
               values_to = "Cig taxes")





WHO_names<-colnames(taxes_table)
OECD_names<-colnames(GDP_table)

#I WILL USE THE OECD NAMES

setdiff(WHO_names,OECD_names)
setdiff(OECD_names,WHO_names)

merged_mpower[merged_mpower$Country=='Slovakia',1]<-'Slovak Republic'
affordability[affordability$Country=='Slovakia',2]<-'Slovak Republic'
taxes[taxes$Country=='Slovakia',2]<-'Slovak Republic'

merged_mpower[merged_mpower$Country=='Czechia',1]<-'Czech Republic'
affordability[affordability$Country=='Czechia',2]<-'Czech Republic'
taxes[taxes$Country=='Czechia',2]<-'Czech Republic'

merged_mpower[merged_mpower$Country=='United Kingdom of Great Britain and Northern Ireland',1]<-'United Kingdom'
affordability[affordability$Country=='United Kingdom of Great Britain and Northern Ireland',2]<-'United Kingdom'
taxes[taxes$Country=='United Kingdom of Great Britain and Northern Ireland',2]<-'United Kingdom'


merged_mpower[merged_mpower$Country=='United States of America',1]<-'United States'
affordability[affordability$Country=='United States of America',2]<-'United States'
taxes[taxes$Country=='United States of America',2]<-'United States'


merged_mpower[merged_mpower$Country=='Republic of Korea',1]<-'Korea'
affordability[affordability$Country=='Republic of Korea',2]<-'Korea'
taxes[taxes$Country=='Republic of Korea',2]<-'Korea'

#Saving the updated table with 2007 imputation and the right names
write.table(merged_mpower, 
            "../data/mpower_merged_2007-2020.txt", 
            sep = "\t")

#####

merged_gdp_edu<-merge(GDP, 
                      education,
                      by = c("Country", "Year"), 
                      all = TRUE)

merged_gdp_edu_aff<-merge(merged_gdp_edu, 
                      affordability,
                      by = c("Country", "Year"), 
                      all = TRUE)

merged_gdp_edu_aff_taxes<-merge(merged_gdp_edu_aff, 
                                taxes,
                                by = c("Country", "Year"), 
                                all = TRUE)

merged_gdp_edu_aff_taxes_power<-merge(merged_gdp_edu_aff_taxes, 
                                      merged_mpower,
                                      by = c("Country", "Year"), 
                                      all = TRUE)

#Addint the heavy artillery! the prevalence
rownames(prevalence_table)
prevalence_table2<-cbind(rownames(prevalence_table),prevalence_table)
colnames(prevalence_table2)[1]<-'Year'

prevalence<- prevalence_table2 %>%
  pivot_longer(cols = -Year, names_to = "Country", 
               values_to = "GDP")
unique(prevalence$Country)

prevalence[prevalence$Country=='Slovakia',2]<-'Slovak Republic'
prevalence[prevalence$Country=='New.Zealand',2]<-'New Zealand'
prevalence[prevalence$Country=='Czechia',2]<-'Czech Republic'
prevalence[prevalence$Country=='United.Kingdom.of.Great.Britain.and.Northern.Ireland',2]<-'United Kingdom'
prevalence[prevalence$Country=='United.States.of.America',2]<-'United States'
prevalence[prevalence$Country=='Republic.of.Korea',2]<-'Korea'
prevalence[prevalence$Country=='Costa.Rica',2]<-'Costa Rica'

merged_all<-merge(prevalence,merged_gdp_edu_aff_taxes_power, 
                  by = c("Country", "Year"), 
                  all = TRUE)

#Saving the updated table with 2007 imputation and the right names
write.table(merged_all, 
            "../data/final_dataset_2007-2020.txt", 
            sep = "\t")