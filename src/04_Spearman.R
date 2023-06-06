rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")

inputpath = "../data"
outputpath = "../data"

#Imports
library(tidyr)
library(dplyr)
library(lme4)
library(pbapply)
library(mgcv)
library(conformalInference)
library(ggplot2)
library(progress)
library(parallel)
library(pbapply)
library(roahd)



#Import data
data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)


data_spearman<-data[data$Year!='2007',c("Country","Year","GDP","Education",
                                        "Prevalence_both",
                                        "Prevalence_males",
                                        "Prevalence_females")]

data_spearman$GDP<-log(data_spearman$GDP)

data_spearman_gdp<-data_spearman[,c("Country","Year","GDP")]
data_spearman_prevalence_b<-data_spearman[,c("Country","Year","Prevalence_both")]
data_spearman_prevalence_m<-data_spearman[,c("Country","Year","Prevalence_males")]
data_spearman_prevalence_f<-data_spearman[,c("Country","Year","Prevalence_females")]

data_spearman_education<-data_spearman[,c("Country","Year","Education")]


GDP_table <- data_spearman_gdp %>%
  pivot_wider(names_from = Year, 
              values_from = GDP)

GDP_table<-t(as.data.frame(GDP_table))
colnames(GDP_table)<-GDP_table[1,]
GDP_table<-t(GDP_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_gdp <- fData(grid,GDP_table)
quartz()
plot(f_data_gdp) # what happens if I do plot(data)?
#lines(grid,m, col="black", lwd=5)

#Computing band depth and modified band depth
band_depth <- BD(Data = f_data_gdp)
modified_band_depth <- MBD(Data = f_data_gdp)
#Computing the median curve
median_curve <- median_fData(fData = f_data_gdp, type = "MBD") # still an fData object
median_curve
plot(f_data_gdp) # what happens if I do plot(data)?
plot(median_curve)



Edu_table <- data_spearman_education %>%
  pivot_wider(names_from = Year, 
              values_from = Education)

Edu_table<-t(as.data.frame(Edu_table))
colnames(Edu_table)<-Edu_table[1,]
Edu_table<-t(Edu_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_edu <- fData(grid,Edu_table)
quartz()
plot(f_data_edu)


#Prevalence both
Prev_b_table <- data_spearman_prevalence_b %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_both)

Prev_b_table<-t(as.data.frame(Prev_b_table))
colnames(Prev_b_table)<-Prev_b_table[1,]
Prev_b_table<-t(Prev_b_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_b <- fData(grid,Prev_b_table)
quartz()
plot(f_data_prev_b)


#Prevalence males
Prev_m_table <- data_spearman_prevalence_m %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_males)

Prev_m_table<-t(as.data.frame(Prev_m_table))
colnames(Prev_m_table)<-Prev_m_table[1,]
Prev_m_table<-t(Prev_m_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_m <- fData(grid,Prev_m_table)
quartz()
plot(f_data_prev_m)


#Prevalence females
Prev_f_table <- data_spearman_prevalence_f %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_females)

Prev_f_table<-t(as.data.frame(Prev_f_table))
colnames(Prev_f_table)<-Prev_f_table[1,]
Prev_f_table<-t(Prev_f_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_f <- fData(grid,Prev_f_table)
quartz()
plot(f_data_prev_m)


###########Assessing Spearmann correlation

#Prevalence males-prevalence females
bivariate_data <- as.mfData(list(f_data_prev_m, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# 0.5416701
cor_spearman(bivariate_data, ordering='MEI')
# 0.5427426


#Correlation Edu-GDP
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_edu))

cor_spearman(bivariate_data, ordering='MHI')
#0.6098642
cor_spearman(bivariate_data, ordering='MEI')
#0.6067687

#There is clearly a strong positive correlation between the data



#GDP-Prevalence both
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_b))
cor_spearman(bivariate_data, ordering='MHI')
# -0.2121659
cor_spearman(bivariate_data, ordering='MEI')
#-0.2137018


#Education-Prevalence both
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_b))
cor_spearman(bivariate_data, ordering='MHI')
# -0.3150654
cor_spearman(bivariate_data, ordering='MEI')
#-0.3158375

#In general there seems to be a negative correlation (especially with tertiary ed level)




#GDP-prevalence males
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_m))
cor_spearman(bivariate_data, ordering='MHI')
# -0.3399437
cor_spearman(bivariate_data, ordering='MEI')
#-0.3399437


#Edu-prevalence males
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_m))
cor_spearman(bivariate_data, ordering='MHI')
# -0.234057
cor_spearman(bivariate_data, ordering='MEI')
#-0.2327447




#GDP-prevalence females
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# 0.07167501
cor_spearman(bivariate_data, ordering='MEI')
# 0.06687308

#Interesting... there does not seem to be such a huge correlation...


#Edu-prevalence females
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# -0.2667288
cor_spearman(bivariate_data, ordering='MEI')
# -0.2716218

#General comment
#GDP more correlated than education in males
#On the other hand there seem to be no correlation between females and GDP
#but only between female prev and edu...


#Can we find differenciated data for education for males and females?


#Here the correlation is suggesti
