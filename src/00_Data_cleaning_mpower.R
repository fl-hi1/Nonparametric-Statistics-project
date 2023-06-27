# Indicator description can be found at 
# https://www.who.int/data/gho/data/themes/topics/topic-details/GHO/gho-tobacco-conntrol-mpower-progress-towards-selected-tobacco-control-policies-for-demand-reduction

rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#Imports
library(tidyr)
library(dplyr)

#Extracting rawdata
rawdata<-read.csv("../data/raw_datasets/mpower_overview_factor.csv",header=T)

#Extrcting OECD countries and adding manually the countries with slightly different names
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

#Checking if there are mismatches
setdiff(countries, rawdata$Country)


#Ectracting mpower data only for OECD countries
mpower<-rawdata[rawdata$Country %in% countries,]

#Extracting each category separately
campaigns<-mpower[,c(1,2,9)]
protect<-mpower[,c(1,2,4)]
help<-mpower[,c(1,2,5)]
warn<-mpower[,c(1,2,6)]
bans<-mpower[,c(1,2,7)]
monitor<-mpower[,c(1,2,3)] #added now


# Reorder columns by year
campaigns_table <- campaigns %>% 
  pivot_wider(names_from = Year, values_from = Anti.tobacco.mass.media.campaigns)


# Trying to fill the NAs
# Example table with missing values

# Function to fill missing values with the average of adjacent elements
fill_missing_values <- function(row) {
  missing_indices <- which(row == '1')  # Identify elements with value 1
  for (i in missing_indices) {
    if (i == 1) {
      # If missing value is in the first position
      adjacent_indices <- c(i + 1)
    } else if (i == length(row)) {
      # If missing value is in the last position
      adjacent_indices <- c(i - 1)
    } else {
      # For missing values in other positions
      adjacent_indices <- c(i - 1, i + 1)
    }
    adjacent_values <- row[adjacent_indices]
    # Calculate the average of adjacent values
    average <- mean(adjacent_values)
    
    # Replace the missing value with the average
    row[i] <- average
  }
  
  return(row)
}

# Apply the fill_missing_values function to each row of the table
filled_table <- t(apply((campaigns_table[,2:7]), 1, fill_missing_values))

#Transforming the updated thing into a tibble
tibble_data <- as_tibble(filled_table)

#And truncating to the closest integer
tibble_data2 <- tibble_data %>%
  mutate_all(~as.integer(.))


campaigns_table[,2:7]<-tibble_data2


#Values need to be estimated for 2008 2007 for ALL countries
#First problem: NA estimate:
sum(is.na(mpower$Anti.tobacco.mass.media.campaigns))
#All the NaN are in the Anti tobacco mass-media campains

#Rationale (may be overoptimistic)
#The NA info status is equal or lower to the latest one available
campaigns_table$'2008'=campaigns_table$'2010'
campaigns_table$'2007'=campaigns_table$'2010'



########Transforming data into tables

protect_table <- protect %>%
  pivot_wider(names_from = Year, values_from = Protect.from.tobacco.smoke)

help_table <- help %>%
  pivot_wider(names_from = Year, values_from = Offer.help.to.quit.tobacco.use)

warn_table <- warn %>%
  pivot_wider(names_from = Year, values_from = Warn.about.the.dangers.of.tobacco)

bans_table <- bans %>%
  pivot_wider(names_from = Year, values_from = Enforce.bans.on.tobacco.advertising)

#added now
monitor_table <- monitor %>%
  pivot_wider(names_from = Year, values_from = Monitor)

sum(is.na(monitor))

#Converting back 
#######IGNORE THE NEXT TWO BLOCKS OF LINES
original_table <- campaigns_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Anti.tobacco.mass.media.campaigns")


original_protect_table <- protect_table %>%
  pivot_longer(cols = -Country, names_to = "Year", 
               values_to = "Protect.from.tobacco.smoke")

#Substituting appropriately
#mpower$Anti.tobacco.mass.media.campaigns<-original_table


######Saving the  tables

#campaings
write.table(campaigns_table, 
            "../data/mpower_campaigns_2007-2020.txt", 
            sep = "\t")

#protect
write.table(protect_table, 
            "../data/mpower_protect_2007-2020.txt", 
            sep = "\t")

#Offer Help
write.table(help_table, 
            "../data/mpower_help_2007-2020.txt", 
            sep = "\t")

#Warn
write.table(warn_table, 
            "../data/mpower_warn_2007-2020.txt", 
            sep = "\t")
#Bans
write.table(bans_table, 
            "../data/mpower_bans_2007-2020.txt", 
            sep = "\t")


#Monitor - new
write.table(monitor_table, 
            "../data/mpower_monitor_2007-2020.txt", 
            sep = "\t")


