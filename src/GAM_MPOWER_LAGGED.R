##################### Model with MPOWER components separated
model_gam_2 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    data_gam$Year+                  
    data_gam$Country+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education, bs = 'cr') +
    data_gam$Taxes+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans,
  family = betar(link = "logit")
)
plot(model_gam_2)
summary(model_gam_2)
plot(model_gam_2$residuals)
hist(model_gam_2$residuals)
shapiro.test(model_gam_2$residuals)
#clearly, there are very long tails
#Also, a deviance explained by 99% is a clear indicator of overfitting


######
######Now I try to see the same for males and females separately
######
#Females
model_gam_3 = mgcv::gam(
  data_gam$Prevalence_females ~ 
    data_gam$Year+                  #the behaviour is nearly linear, 
    # I can probably reduce the model
    data_gam$Country+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    data_gam$Taxes+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans,
  family = betar(link = "logit")
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
    # s(data_gam$HDI, bs = 'cr') + 
    data_gam$Taxes+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans,
  family = betar(link = "logit")
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


#Seems like "protect" is important


#We try to use another strategy... based on a different clustering


############################################################### 
################## MHI- based clustering ######################
##################  of countries ##############################
############################################################### 
#We use the MHI and the clustering based on it

data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)
data_gam$HDI_MHI_clustering

#First we try the model with the composite index

model_gam_f = mgcv::gam(
  data_gam$Prevalence_females ~ 
    data_gam$Year+
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    #I introduce both the HDI MHI and the HDI_MHI clustering
    s(data_gam$HDI, bs = 'cr') +
    data_gam$HDI_MHI_clustering+
    data_gam$mpower_all:data_gam$HDI_MHI_clustering,
  family = betar(link = "logit")
)
plot(model_gam_f)
summary(model_gam_f)
plot(model_gam_f$residuals)
hist(model_gam_f$residuals)
shapiro.test(model_gam_f$residuals)



model_gam_f = mgcv::gam(
  data_gam$Prevalence_females ~ 
    data_gam$Year+  
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    #I introduce both the HDI MHI and the HDI_MHI clustering
    s(data_gam$HDI, bs = 'cr') +
    data_gam$HDI_MHI_clustering+
    data_gam$Taxes+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans,
  family = betar(link = "logit")
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
    data_gam$Country+
    #I introduce both the HDI MHI and the HDI_MHI clustering
    s(data_gam$HDI, bs = 'cr') +
    data_gam$HDI_MHI_clustering+
    data_gam$mpower_all
  #  data_gam$Taxes+
  #  data_gam$Help+
  #  data_gam$Protect+
  #  data_gam$Monitor+
  #  data_gam$Warn+
  #  data_gam$Bans 
  ,
  family = betar(link = "logit")
)
plot(model_gam_m)
summary(model_gam_m)
plot(model_gam_m$residuals)
hist(model_gam_m$residuals)
shapiro.test(model_gam_m$residuals)
#still a very high collinearity between residuals...


##################################################### 
################## INTRODUCTION OF ################## 
##################   LAGGED  ########################
##################  RESPONSE   ######################
##################################################### 

data_gam<-data[data$Year!=2007,]
data_gam<-data_gam[data_gam$Country!='Japan',]


prevalence<-data_gam[,c(1:5)]
prevalence<-prevalence[prevalence$Year!='2008',]
prevalence[prevalence$Year=='2010',2]<-2008
prevalence[prevalence$Year=='2012',2]<-2010
prevalence[prevalence$Year=='2014',2]<-2012
prevalence[prevalence$Year=='2016',2]<-2014
prevalence[prevalence$Year=='2018',2]<-2016
prevalence[prevalence$Year=='2020',2]<-2018

mpower_pre<-data_gam[,c(1,2,13,14,15,16,17,18,22)]
mpower_pre<-mpower_pre[mpower_pre$Year!='2020',]

mpower_pre[mpower_pre$Year=='2008',2]<-2010
mpower_pre[mpower_pre$Year=='2010',2]<-2012
mpower_pre[mpower_pre$Year=='2012',2]<-2014
mpower_pre[mpower_pre$Year=='2014',2]<-2016
mpower_pre[mpower_pre$Year=='2016',2]<-2018
mpower_pre[mpower_pre$Year=='2018',2]<-2020

data_gam<-data_gam[data_gam$Year!=2008,]
data_gam$mpower_pre<-mpower_pre$mpower_all
data_gam$Taxes_pre<-mpower_pre$Taxes
data_gam$Monitor_pre<-mpower_pre$Monitor
data_gam$Bans_pre<-mpower_pre$Bans
data_gam$Protect_pre<-mpower_pre$Protect
data_gam$Warn_pre<-mpower_pre$Warn
data_gam$Help_pre<-mpower_pre$Help


data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)

model_gam_lagged = mgcv::gam(
  Prevalence_females~ 
    Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    s(GDP, bs = 'cr') + 
    s(Education_females, bs = 'cr') +
    s(HDI, bs = 'cr') +
    #  data_gam$Country+
    data_gam$mpower_pre*HDI_MHI_clustering
  # Taxes_pre+
  #  Help_pre+
  #  Monitor_pre+
  #  Protect_pre+
  #  Warn_pre+
  #  Bans_pre#+
  ,
  data=data_gam,
  family = betar(link = "logit")
)


plot(model_gam_lagged)

summary(model_gam_lagged)
plot(model_gam_lagged$residuals)
hist(model_gam_lagged$residuals)
shapiro.test(model_gam_lagged$residuals)


###Doing the same for males and females separately

#Males

model_gam_lagged_m = mgcv::gam(
  data_gam$Prevalence_both~ 
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_males, bs = 'cr') +
    s(data_gam$HDI, bs = 'cr') +
    data_gam$HDI_MHI_clustering+
    data_gam$mpower_pre ,
  #data_gam$Country+
  family = betar(link = "logit")
)
plot(model_gam_lagged_m)
summary(model_gam_lagged_m)
plot(model_gam_lagged_m$residuals)
hist(model_gam_lagged_m$residuals)
shapiro.test(model_gam_lagged_m$residuals)



# Install and load the necessary packages
#install.packages("FactoMineR")
library(FactoMineR)

# Specify your data with ordinal variables
ordinal_data <- mpower_pre<-data_gam[,c(13,14,15,16,17,18)]
ordinal_data$Help<-as.factor(ordinal_data$Help)
#levels(ordinal_data$Help)<-1:5
ordinal_data$Bans<-as.factor(ordinal_data$Bans)
ordinal_data$Monitor<-as.factor(ordinal_data$Monitor)
ordinal_data$Warn<-as.factor(ordinal_data$Warn)
ordinal_data$Protect<-as.factor(ordinal_data$Protect)
ordinal_data$Taxes<-as.factor(ordinal_data$Taxes)




######
######
######


#data_gam<-data_gam[data_gam$Year!=2020,]

#data_gam$lagged_prev_m<-prevalence$Prevalence_males/100
#data_gam$lagged_prev_b<-prevalence$Prevalence_both/100
#data_gam$lagged_prev_f<-prevalence$Prevalence_females/100

model_gam_lagged = mgcv::gam(
  data_gam$lagged_prev_f~ 
    data_gam$Year+ #the behaviour is nearly linear, 
    # I can probably reduce the model
    s(data_gam$GDP, bs = 'cr') + 
    s(data_gam$Education_females, bs = 'cr') +
    s(data_gam$HDI, bs = 'cr') +
    data_gam$Country+
    # data_gam$HDI_MHI_clustering+
    data_gam$mpower_all
  #data_gam$Taxes+
  #data_gam$Help+
  #data_gam$Protect+
  #data_gam$Monitor+
  #data_gam$Warn+
  #data_gam$Bans#+
  ,
  family = betar(link = "logit")
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
    s(data_gam$HDI, bs = 'cr') +
    #data_gam$Country+
    data_gam$HDI_MHI_clustering+
    data_gam$Taxes+
    data_gam$Help+
    data_gam$Protect+
    data_gam$Monitor+
    data_gam$Warn+
    data_gam$Bans,
  family = betar(link = "logit")
)
plot(model_gam_lagged_m)
summary(model_gam_lagged_m)
plot(model_gam_lagged_m$residuals)
hist(model_gam_lagged_m$residuals)
shapiro.test(model_gam_lagged_m$residuals)

