#install.packages("package_name", lib = "path/to/your/R/project/library")


rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data/raw_datasets"
outputpath = "../data"


###Raw dataset upload
filename="Tobacco_prevalence_both_sexes.csv"
filepath <- file.path(inputpath, filename)
raw_prevalence <- read.csv(filepath)



###Extraction of relevant columns

raw_prevalence$GHO..DISPLAY.=factor(raw_prevalence$GHO..DISPLAY.)
raw_prevalence_smoke<-raw_prevalence[raw_prevalence$GHO..DISPLAY.=="Estimate of current tobacco smoking prevalence (%) (age-standardized rate)",]

colnames(raw_prevalence_smoke)
col_tokeep<-c("YEAR..DISPLAY.", "REGION..DISPLAY.","COUNTRY..DISPLAY.","SEX..DISPLAY.","Numeric")

#Selection of relevant features
data=raw_prevalence_smoke[,col_tokeep]


###Extraction of OECD countries
filename="OECD_countries_codes_income_groups.csv"
filepath <- file.path(inputpath, filename)
OECD_names <- read.csv(filepath)
countries=OECD_names$Country
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Slovak Republic",
             "Slovakia",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland",
             "Costa Rica")
#Saving
write.table(OECD_names, "../data/OECD_countries_income_level", 
                        sep = "\t")

#library(data.table)
prevalence_filtered <- data[data$COUNTRY..DISPLAY.%in% countries, ]

#Re-organizing for male, female, both
prevalence_males <- prevalence_filtered[prevalence_filtered$SEX..DISPLAY.=="Male",c(1,3,5) ]
prevalence_females <- prevalence_filtered[prevalence_filtered$SEX..DISPLAY.=="Female", c(1,3,5)]
prevalence_both<-prevalence_filtered[prevalence_filtered$SEX..DISPLAY.=="Both sexes", c(1,3,5)]


library(tidyr)
library(dplyr)

# Pivot the table to create a wide format dataset---------

# BOTH
prevalence_both_table <- prevalence_both %>%
  pivot_wider(names_from = COUNTRY..DISPLAY., values_from = Numeric)

# Reorder columns by year
prevalence_both_table <- prevalence_both_table %>%
  select(YEAR..DISPLAY., everything())

# View the resulting dataset
prevalence_both_table


# MALES
prevalence_male_table <- prevalence_males%>%
  pivot_wider(names_from = COUNTRY..DISPLAY., values_from = Numeric)

# Reorder columns by year
prevalence_male_table <- prevalence_male_table %>%
  select(YEAR..DISPLAY., everything())

# View the resulting dataset
prevalence_male_table

#FEMALES
prevalence_female_table <- prevalence_females%>%
  pivot_wider(names_from = COUNTRY..DISPLAY., values_from = Numeric)

# Reorder columns by year
prevalence_female_table <- prevalence_female_table %>%
  select(YEAR..DISPLAY., everything())

# View the resulting dataset
prevalence_female_table

#SAVING FILES
write.table(prevalence_female_table, 
            "../data/smoking_prevalence_f.txt", 
            sep = "\t")

write.table(prevalence_male_table, 
            "../data/smoking_prevalence_m.txt", 
            sep = "\t")

write.table(prevalence_both_table, 
            "../data/smoking_prevalence_b.txt", 
            sep = "\t")







