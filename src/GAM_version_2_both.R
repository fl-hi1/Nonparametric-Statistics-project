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

#creating unified mpower indes
#Creating a unified index
min_index=12 #6*2 , since 1 is not possible
max_index=30 #6*5

data$mpower_all=((data$Campaigns+
                    data$Help+
                    data$Warn+
                    data$Bans+
                    data$Protect+
                    data$Monitor)-min_index)/(max_index-min_index)
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



#I start by considering values from 2010, as campaigns table 
#have problems
data_gam<-data
data_gam=data_gam[data_gam$Year!='2007',]
data_gam=data_gam[data_gam$Year!='2008',]

#I also remove japan as it has no education data
data_gam=data_gam[data_gam$Country!='Japan',]

data_gam$Campaigns<-as.numeric(data_gam$Campaigns)
data_gam$ban<-as.numeric(data_gam$Bans)
data_gam$Monitor<-as.numeric(data_gam$Monitor)
data_gam$Help<-as.numeric(data_gam$Help)
data_gam$Protect<-as.numeric(data_gam$Protect)
data_gam$Warn<-as.numeric(data_gam$Warn)


########################################################################
############################# GAM MODELS ##############################
########################################################################

#PREVALENCE BOTH VERSUS MPOWER FULL INDEX
#COUNTRIES ARE ADDED AS FACTOR

#Adding GDP and Education
#GAM model, smoothing term with cubic splines
#I start by adding the MPOWER index as a smooth term
#I will then see if I can reduce this term

model_gam_1 = mgcv::gam(
    data_gam$Prevalence_both ~ 
    data_gam$Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
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
#however, a deviance so high is a clear indication of overfitting


################ Same on females
model_gam_f = mgcv::gam(
    data_gam$Prevalence_females ~ 
    data_gam$Year+ #the behaviour is nearly linear, 
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


################ Same on males
model_gam_m = mgcv::gam(
    data_gam$Prevalence_males ~ 
    data_gam$Year+ #the behaviour is nearly linear, 
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


##################### Model with MPOWER components separated

model_gam_2 = mgcv::gam(
    data_gam$Prevalence_both ~ 
    data_gam$Year+                  #the behaviour is nearly linear, 
    data_gam$Country+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education, bs = 'cr') +
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans
)
plot(model_gam_2)
summary(model_gam_2)
plot(model_gam_2$residuals)
hist(model_gam_2$residuals)
shapiro.test(model_gam_2$residuals)
#clearly, there are very long tails
#Also, a deviance explained by 99% is a clear indicator of overfitting


###Now I try to see the same for males and females separately

#Females
model_gam_3 = mgcv::gam(
    data_gam$Prevalence_females ~ 
    data_gam$Year+                  #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans
)
plot(model_gam_3)
summary(model_gam_3)
plot(model_gam_3$residuals)
hist(model_gam_3$residuals)
shapiro.test(model_gam_3$residuals)
#clearly, there are very long tails
#Also, a deviance explained by 99% is a clear indicator of overfitting


#Males
model_gam_4 = mgcv::gam(
    data_gam$Prevalence_males ~ 
    data_gam$Year+                  #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_males, bs = 'cr') +
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans
)
plot(model_gam_4)
summary(model_gam_4)
plot(model_gam_4$residuals)
hist(model_gam_4$residuals)
shapiro.test(model_gam_4$residuals)
#There are always outliers
#Also, the residuals are still collinear
#We are clearly overfitting...
#Probably the countries are too many for the few data that we have

#We try to use another strategy... based on a different clustering



################## ################## ################## 
################## MHI- based clustering ######################
##################  of countries ################## ##########

#We use the MHI and the clustering based on it

data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)
data_gam$HDI_MHI

model_gam_f = mgcv::gam(
    data_gam$Prevalence_females ~ 
    data_gam$Year+  
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    #data_gam$Country+
    #I introduce both the HDI MHI and the HDI_MHI clustering
    s(data_gam$HDI_MHI, bs = 'cr') +
    data_gam$HDI_MHI_clustering+
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans+
      data_gam$Cig_taxes+
      data_gam$Affordability
)

plot(model_gam_f)
summary(model_gam_f)

plot(model_gam_f$residuals)
hist(model_gam_f$residuals)
shapiro.test(model_gam_f$residuals)

#Males
model_gam_m = mgcv::gam(
  data_gam$Prevalence_males ~ 
    data_gam$Year+  
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_males, bs = 'cr') +
    #data_gam$Country+
    #I introduce both the HDI MHI and the HDI_MHI clustering
    s(data_gam$HDI_MHI, bs = 'cr') +
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
plot(model_gam_m)
summary(model_gam_m)
plot(model_gam_m$residuals)
hist(model_gam_m$residuals)
shapiro.test(model_gam_m$residuals)
#still a very high collinearity between residuals...


################## ################## ################## 
################## INTRODUCTION OF ################## 
################## LAGGED ######################
################## RESPONSE ################## ##########

data_gam<-data[data$Year!=2007,]
#data_gam<-data_gam[data_gam$Year!=2008,]
data_gam<-data_gam[data_gam$Country!='Japan',]


prevalence<-data_gam[,c(1:5)]
prevalence<-prevalence[prevalence$Year!='2008',]
prevalence[prevalence$Year=='2010',2]<-2008
prevalence[prevalence$Year=='2012',2]<-2010
prevalence[prevalence$Year=='2014',2]<-2012
prevalence[prevalence$Year=='2016',2]<-2014
prevalence[prevalence$Year=='2018',2]<-2016
prevalence[prevalence$Year=='2020',2]<-2018

data_gam<-data_gam[data_gam$Year!=2020,]

data_gam$lagged_prev_b<-prevalence$Prevalence_both
data_gam$lagged_prev_m<-prevalence$Prevalence_males
data_gam$lagged_prev_f<-prevalence$Prevalence_females

data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)
  
model_gam_lagged = mgcv::gam(
    data_gam$lagged_prev_b~ 
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education, bs = 'cr') +
    #s(data_gam$HDI_MHI, bs = 'cr') +
    #data_gam$Country+
    data_gam$HDI_MHI_clustering+
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans+
    data_gam$Affordability+
    s(data_gam$Cig_taxes, bs = 'cr') 
      
)
plot(model_gam_lagged)
summary(model_gam_lagged)
plot(model_gam_lagged$residuals)
hist(model_gam_lagged$residuals)
shapiro.test(model_gam_lagged$residuals)


###Doing the same for males and females separately

#Males

model_gam_lagged_m = mgcv::gam(
  data_gam$lagged_prev_m~ 
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_males, bs = 'cr') +
    s(data_gam$HDI_MHI, bs = 'cr') +
    #data_gam$Country+
    data_gam$HDI_MHI_clustering+
    data_gam$Campaigns+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans+
    data_gam$Affordability +
    data_gam$Cig_taxes,
)
plot(model_gam_lagged_m)
summary(model_gam_lagged_m)
plot(model_gam_lagged_m$residuals)
hist(model_gam_lagged_m$residuals)
shapiro.test(model_gam_lagged_m$residuals)

###LTS


model_lts_f = ltsReg(
  
    data_gam$Prevalence_females ~ 
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    
    data_gam$GDP+
    
    data_gam$Education_females+
    
    data_gam$HDI_MHI+
    data_gam$HDI_MHI_clustering+
    
    
    #data_gam$Country+
    
    data_gam$mpower_all,
    
    alpha=0.75
  
  # s(data_gam$Affordability, bs = 'cr') +
  
  #  s(data_gam$Cig_taxes, bs = 'cr') 
)

plot(model_lts_f)

summary(model_lts_f)

plot(model_lts_f$residuals)
hist(model_lts_f$residuals)
shapiro.test(model_lts_f$residuals)
