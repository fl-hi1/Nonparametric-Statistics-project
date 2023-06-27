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
             "Korea",
             "Slovak Republic",
             "Slovakia",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland",
             "Costa Rica",
             "Japan")

GDP_measures<-rawdata[rawdata$Country %in% countries,c('Country','Year','Transaction','Value')]

GDP<-GDP_measures[GDP_measures$Transaction=='Gross domestic product (expenditure approach)',c(1,2,4)]


# Reorder columns by year
library(tidyr)

GDP_table <- GDP %>%
  pivot_wider(names_from = Country, 
              values_from = Value)
GDP_table<-as.data.frame(GDP_table)

rownames(GDP_table)<-GDP_table[,1]
#do NOT run twice!
GDP_table<-GDP_table[,-1]
dim(GDP_table)

GDP_table<-as.data.frame(t(GDP_table))


#Saving the updated table
#write.table(GDP_table, 
#            "../data/GDP_allyears_2000-2020.txt", 
#            sep = "\t")
#

data<-GDP_table

library(roahd)


grid <- min(range(colnames(data))):max(range(colnames(data)))
grid
f_data <- fData(grid, data)

# DEPTH
band_depth <- BD(Data = f_data)
modified_band_depth <- MBD(Data = f_data)

# MEDIAN
median_curve <- median_fData(fData = f_data, type = "MBD") # still an fData object

# PLOT
plot(f_data, ylim=c(1e4,6e4))
lines(grid,median_curve$values,lwd=2.0) # superimpose median


# SPEARMAN CORRELATION OF A *MULTIVARIATE* (n>=2) FUNCTIONAL DATA
# here first 2 dimensions
bivariate_data <- as.mfData(list(mfD_healthy$fDList[[1]], mfD_healthy$fDList[[2]]))
plot(bivariate_data)
cor_spearman(bivariate_data, ordering='MHI')


# MAGNITUDE/AMPLITUDE OUTLIER
fb_plot = fbplot(f_data, main="Magnitude outliers") # Functional Box Plot
fb_plot$ID_outliers

# SHAPE OUTLIER
outliergram(f_data) # Outliergram
# get IDs
out_shape <- outliergram(f_data, display = FALSE)
out_shape$ID_outliers
# manual
MEI_f <- MEI(f_data)
MBD_f <- MBD(f_data)
a_0 <- a_2 <- -2/(f_data$N*(f_data$N-1))
a_1 <- 2*(f_data$N+1)/(f_data$N-1)
d_manual <- a_0+a_1*MEI_f+a_2*f_data$N^2*MEI_f^2-MBD_f
ID_outliers_manual <- which(d_manual>quantile(d_manual,probs = .75)+2*IQR(x = d_manual))
ID_outliers_manual
plot(MEI_f, MBD_f, col=ifelse(1:f_data$N %in% ID_outliers_manual,"red","black"), pch=19)

library(BNPTSclust)
years<-2000:2022
output<-tseriescm(t(GDP_table),
                   maxiter=1000,
                   burnin=10,
                   thinning=2,
                   level=FALSE,
                   trend=TRUE,
                   seasonality=FALSE,
                   priorb=TRUE,
                   b=0)


par(mfrow = c(1, 3))
matplot(
  years,
  t(GDP_table)[, output$gnstar == 1],
  type = 'l',
  col = "black",
  xlab = 'Years',
  ylab = 'GDP',
  main = "First Cluster"
)


dim(  GDP_table[, output$gnstar == 1])
length(  years)
matplot(
  years,
  GDP_table[, output$gnstar == 2],
  type = 'l',
  col = "black",
  xlab = 'Years',
  ylab = 'GDP',
  main = 'Second Cluster'
)

matplot(
  years,
  GDP_table[, output$gnstar == 3],
  type = 'l',
  col = "black",
  xlab = 'Years',
  ylab = 'GDP',
  main = 'Third Cluster'
)

matplot(
  years,
  GDP_table[, output$gnstar == 4],
  type = 'l',
  col = "black",
  xlab = 'Years',
  ylab = 'GDP',
  main = 'Third Cluster'
)

matplot(
  years,
  GDP_table[, output$gnstar == 5],
  type = 'l',
  col = "black",
  xlab = 'Years',
  ylab = 'GDP',
  main = 'Third Cluster'
)





rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"


#rawdata<-read.delim("../data/raw_datasets/Education_OECD_share_population_25-64_withtertiary_level.csv",
#                    sep = ",", quote="", header=T)

rawdata<-read.delim("../data/raw_datasets/education2.csv",
                    sep = ",", quote="", header=T)

#There are no data from japan!
jap<-read.delim("../data/raw_datasets/education_japan.csv",
                sep = ",", quote="", header=T)



rawdata_clean <- as.data.frame(lapply(rawdata, function(x) gsub('"', '', x)))

#Extracting countries from OECD database
countries<-read.table("../data/OECD_countries_income_level", header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Czechia", 
             "Republic of Korea",
             "Korea",
             "Slovak Republic",
             "Slovakia",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland",
             "Costa Rica",
             "Japan")
#
#Select only countries of interest
education_raw<-rawdata_clean[rawdata_clean$X.Region. %in% countries,]

mex<-education_raw[education_raw$X.Country.=='Mexico',]
Lat<-education_raw[education_raw$X.Country.=='Latvia',]
Est<-education_raw[education_raw$X.Country.=='Estonia',]
Lux<-education_raw[education_raw$X.Country.=='Luxembourg',]

mexicanregions<-c("MEX" )
latviaregions<-c("LVA" )
estoniaregions<-c("EST" )

luxembourgregions<-c("LUX" )

newmex<-as.data.frame(mex[mex$X.REG_ID %in% mexicanregions,])
newlatvia<-as.data.frame(Lat[Lat$X.REG_ID %in% latviaregions,])
newestonia<-as.data.frame(Est[Est$X.REG_ID %in% estoniaregions,])
newlouxembourg<-as.data.frame(Lux[Lux$X.REG_ID %in% luxembourgregions,])

education_raw <- education_raw %>% filter(X.Country. != "Mexico")
education_raw <- education_raw %>% filter(X.Country. != "Latvia")
education_raw <- education_raw %>% filter(X.Country. != "Estonia")
education_raw <- education_raw %>% filter(X.Country. != "Luxembourg")


education_raw<-rbind(education_raw,newmex)
education_raw<-rbind(education_raw,newlatvia)
education_raw<-rbind(education_raw,newestonia)
education_raw<-rbind(education_raw,newlouxembourg)


#Filter by year
education_selected_years<-education_raw

#Filter only population from 25 to 64
education_selected_pop<-education_selected_years[education_selected_years$X.Indicator.=='Share of population 25 to 64 year-olds by educational attainment',-c(2)]

#Filter only for tertiary education
education_selected_tert<-education_selected_pop[education_selected_pop$X.Education.ISCED.level.== "Total tertiary education (ISCED2011 levels 5 to 8)",-c(2)]

#Reorder
education_reordered<-education_selected_tert[,c(1,3,2,4)]

#First problem: NA estimate:
sum(is.na(education_reordered)) #NO NANs

education_reordered[,c(2,4)] <- as.data.frame(lapply(education_reordered[,c(2,4)], as.numeric))

#Divide by gender
education_female<-education_reordered[education_reordered$X.Gender.=='Women',-c(3)]
education_male<-education_reordered[education_reordered$X.Gender.=='Men',-c(3)]
education_both<-education_reordered[education_reordered$X.Gender.=='Total',-c(3)]

dim(education_female)
dim(education_male)
dim(education_both) #has 3 more obs...


# Reorder columns by year
library(tidyr)

#Female
education_female_table <- education_female %>%
  pivot_wider(names_from = X.Country., 
              values_from = X.Value.)

education_female_table$X.Year.<-as.integer(education_female_table$X.Year.)
education_female_table <- education_female_table %>%
  select(X.Year., everything())

#Male
education_male_table <- education_male %>%
  pivot_wider(names_from = X.Country., 
              values_from = X.Value.)

education_male_table$X.Year.<-as.integer(education_male_table$X.Year.)
education_male_table <- education_male_table %>%
  select(X.Year., everything())

#Both
education_both_table <- education_both %>%
  pivot_wider(names_from = X.Country., 
              values_from = X.Value.)


#HOW TO DEAL WITH MISSING VALUES?
#PROBLEMATIC COUNTRIES:
#Australia
#Israel
#Chile
#New Zealand
#Korea
#They may need to be removed as they have too many missing values,
#but for now we use the mean

# calculate column means


# replace NAs with column means
#First I need to convert it to a table
education_both_table <- as.data.frame(education_both_table)
colnames(education_both_table)[1]<-'Year'
col_means <- base::colMeans( education_both_table,na.rm = TRUE)
education_both_table <- base::replace(education_both_table, is.na(education_both_table), col_means[base::col(education_both_table)][is.na(education_both_table)])

education_male_table <- as.data.frame(education_male_table)
colnames(education_male_table)[1]<-'Year'
col_means <- base::colMeans( education_male_table,na.rm = TRUE)
education_male_table <- base::replace(education_male_table, is.na(education_male_table), col_means[base::col(education_male_table)][is.na(education_male_table)])

education_female_table <- as.data.frame(education_female_table)
colnames(education_female_table)[1]<-'Year'
col_means <- base::colMeans( education_female_table,na.rm = TRUE)
education_female_table <- base::replace(education_female_table, is.na(education_female_table), col_means[base::col(education_female_table)][is.na(education_female_table)])


#Saving the updated table
write.table(education_both_table, 
            "../data/education_both_2007-2020.txt", 
            sep = "\t")

#Saving the updated table
write.table(education_male_table, 
            "../data/education_male_2007-2020.txt", 
            sep = "\t")

#Saving the updated table
write.table(education_female_table, 
            "../data/education_female_2007-2020.txt", 
            sep = "\t")


