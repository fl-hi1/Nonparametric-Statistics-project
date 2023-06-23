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





#Import data
data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)



#Logging the gdp
data$GDP<-log(data$GDP)
boxplot(data$GDP~data$Country)


plot(data$Education)
boxplot(data$Education~data$Country)

data$Year<-as.numeric(data$Year)
data$mpower_all=(data$Campaigns+data$Help+data$Warn+data$Bans+data$Protect)/25
boxplot(data$mpower_all~data$Country)

#Check the variable type
typeof(data$Year)
typeof(data$GDP)
typeof(data$Affordability)
typeof(data$Prevalence_both)
typeof(data$Cig_taxes)
data$Country<-as.factor(data$Country)
typeof(data$Country)
#data$Cig_taxes<-data$Cig_taxes*100



#Creating a unified index
min_index=12 #6*2 , since 1 is not possible
max_index=30 #6*5

data$mpower_all=((data$Campaigns+
                   data$Help+
                   data$Warn+
                   data$Bans+
                   data$Protect+
                   data$Monitor)-min_index)/25


#I start by considering values from 2010, as campaigns table 
#have problems
data_gam<-data
data_gam=data_gam[data_gam$Year!='2007',]
data_gam=data_gam[data_gam$Year!='2008',]

#I also remove japan as it has no education data
data_gam=data_gam[data_gam$Country!='Japan',]

########################################################################
############################# GAM MODEL 1 ##############################
########################################################################

#PREVALENCE BOTH VERSUS MPOWER FULL INDEX
#COUNTRIES ARE ADDED AS FACTOR

#Adding GDP and Education
#GAM model, smoothing term with cubic splines
#I start by adding the MPOWER index as a smooth term
#I will then see if I can reduce this term

model_gam_1 = mgcv::gam(
    
    data_gam$Prevalence_both ~ 
      
    s(data_gam$Year, bs='cr',k=3)+ #the behaviour is nearly linear, 
                                   # I can probably reduce the model
    data_gam$Country+
    
    s(data_gam$GDP, bs = 'cr') + 

    s(data_gam$Education, bs = 'cr') +
    
    s(data_gam$mpower_all, bs='cr')
)

plot(model_gam_1)
summary(model_gam_1)
plot(model_gam_1$residuals)
hist(model_gam_1$residuals)
#There do not seem to be autocorrelation patterns
#in the residuals.
#Also, the year relationship seem linear



#Can we do better???

#There might be gender differences not accounted for by this model
plot(data_gam$mpower_all,data_gam$Prevalence_males)
plot(data_gam$mpower_all,data_gam$Prevalence_females)



#####
model_gam_f = mgcv::gam(
  
  data_gam$Prevalence_females ~ 
    
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(data_gam$Education_females, bs = 'cr') +
    
    s(data_gam$mpower_all, bs='cr')
)

plot(model_gam_f)
summary(model_gam_f)
plot(model_gam_f$residuals)
hist(model_gam_f$residuals)
shapiro.test(model_gam_f$residuals)



#####
model_gam_m = mgcv::gam(
  
  data_gam$Prevalence_males ~ 
    
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(data_gam$Education_males, bs = 'cr') +
    
    s(data_gam$mpower_all, bs='cr')
)


plot(model_gam_m)

summary(model_gam_m)

plot(model_gam_m$residuals)
hist(model_gam_m$residuals)
shapiro.test(model_gam_m$residuals)


shapiro.test(data_gam$Prevalence_females)
hist(data_gam$Prevalence_both)
#####
model_gam_m = mgcv::gam(
  
  data_gam$Prevalence_males ~ 
    
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(data_gam$Education_males, bs = 'cr') +
    
    s(data_gam$mpower_all, bs='cr')
)


data_gam$Campaigns<-as.numeric(data_gam$Campaigns)
data_gam$ban<-as.numeric(data_gam$Bans)
data_gam$Monitor<-as.numeric(data_gam$Monitor)
data_gam$Help<-as.numeric(data_gam$Help)
data_gam$Protect<-as.numeric(data_gam$Protect)
data_gam$Warn<-as.numeric(data_gam$Warn)


model_gam_m = mgcv::gam(
  
  data_gam$Prevalence_males ~ 
    
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
   # data_gam$Country+
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(data_gam$Education_males, bs = 'cr') +
    
    data_gam$Campaigns+
    
    data_gam$Help+
    
    data_gam$Protect+
    
    data_gam$Monitor+
    
    data_gam$Warn+
    
    data_gam$Bans+
    
    data_gam$Country+
    
    data_gam$Cig_taxes+
    
    data_gam$Affordability
)

plot(model_gam_m)

summary(model_gam_m)

plot(model_gam_m$residuals)
hist(model_gam_m$residuals)
shapiro.test(model_gam_m$residuals)

#We are clearly overfitting...

#Probably the countries are too mani for the few data that we have

#We try to use another strategy... based on a different clustering

#We use the MHI and the clustering based on it

MHI_quantiles<-quantile(data_gam$HDI_MHI, probs=c(0.10,0.3,0.5,0.7,0.9))
MHI_quantiles
MHI_f<-data_gam$HDI_MHI


data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)



model_gam_f = mgcv::gam(
  
  data_gam$Prevalence_females ~ 
    
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model

    s(data_gam$GDP, bs = 'cr') + 
    
    s(data_gam$Education_females, bs = 'cr') +
    
    s(data_gam$HDI_MHI, bs = 'cr') +
    
    #data_gam$Country+
    
    data_gam$HDI_MHI_clustering+
    
    data_gam$Campaigns+
    
    data_gam$Help+
    
    data_gam$Protect+
    
    data_gam$Monitor+
    
    data_gam$Warn+
    
    data_gam$Bans
    
   # s(data_gam$Affordability, bs = 'cr') +
    
  #  s(data_gam$Cig_taxes, bs = 'cr') 
)

plot(model_gam_f)

summary(model_gam_f)

plot(model_gam_f$residuals)
hist(model_gam_f$residuals)
shapiro.test(model_gam_f$residuals)


