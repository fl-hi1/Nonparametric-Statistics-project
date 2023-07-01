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

diagnostic_permutation <- function(T20, T2) {
  B <- length(T2)
  # Compare real test statistic with the ones given by the permuted data
  hist(T2,breaks = 100, xlim = range(c(T2, T20)))
  abline(v = T20, col = 3, lwd = 4)
  # Empirical cumulative distribution function
  plot(ecdf(T2))
  abline(v = T20, col = 3, lwd = 4)
  # P-value
  p_val <- sum(T2 >= T20) / B
  cat("p-value: ", p_val)
}


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
min_index=12 # since 1 is not possible
max_index=29 #6*5

data$mpower_all=((data$Taxes+
                    data$Help+
                    data$Warn+
                    data$Bans+
                    data$Protect+
                    data$Monitor)-min_index)/(max_index-min_index)
boxplot(data$mpower_all~data$Country)
boxplot(data$mpower_all~as.factor(data$Year))
#Seems that the countries have increased their policies across years!
#See "mpower_index_permutation_bootstrap" src file for analysis



#Check the variable type
typeof(data$Year)
typeof(data$GDP)
typeof(data$Affordability)
typeof(data$Prevalence_both)
data$Country<-as.factor(data$Country)
typeof(data$Country)



#I start by considering values from 2008
data_gam<-data
data_gam=data_gam[data_gam$Year!='2007',]
#data_gam=data_gam[data_gam$Year!='2008',]

#I also remove japan as it has no education data
data_gam=data_gam[data_gam$Country!='Japan',]

#data_gam$Campaigns<-as.numeric(data_gam$Campaigns)
data_gam$Bans<-as.numeric(data_gam$Bans)
data_gam$Monitor<-as.numeric(data_gam$Monitor)
data_gam$Help<-as.numeric(data_gam$Help)
data_gam$Protect<-as.numeric(data_gam$Protect)
data_gam$Warn<-as.numeric(data_gam$Warn)
data_gam$Taxes<-as.numeric(data_gam$Taxes)

#data_gam$Prevalence_both<-data_gam$Prevalence_both/100
#data_gam$Prevalence_males<-data_gam$Prevalence_males/100
#data_gam$Prevalence_females<-data_gam$Prevalence_females/100

data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)

#data_gam$mpower_all<-data_gam$mpower_all*100

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
    Prevalence_both~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(GDP, bs = 'cr') + 
    s(HDI, bs = 'cr') + 
    s(Education, bs = 'cr') +
    mpower_all,
    data=data_gam
  )
plot(model_gam_1)
summary(model_gam_1)
plot(model_gam_1$residuals)
hist(model_gam_1$residuals)
#There do not seem to be autocorrelation patterns
#in the residuals.
#however, a deviance so high is a clear indication of overfitting

#We try to reduce the model
model_gam_nogdp = mgcv::gam(
  Prevalence_both~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(HDI, bs = 'cr') + 
    s(Education, bs = 'cr') +
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdp)


T0<-abs(summary(model_gam_1)$s.table[1,3])
T0
    
fitted.values<-model_gam_nogdp$fitted.values
res<-model_gam_nogdp$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$Education, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)

#p-value:  0.991
#We cannot reject the null hypothesis that the GDP is 0
##We remove GDP

################################################
#We try to reduce the model removing edu
model_gam_nogdpnoedu = mgcv::gam(
  Prevalence_both~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(HDI, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdp)


T0<-abs(summary(model_gam_nogdp)$s.table[2,3])
T0


fitted.values<-model_gam_nogdpnoedu$fitted.values
res<-model_gam_nogdpnoedu$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)


#p-value:  0.052
#We can reject the null hypothesis that the edu is 0
##We keep edu 


#####################################
model_gam_nogdpnohdi = mgcv::gam(
  Prevalence_both~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(Education, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdpnohdi)


T0<-abs(summary(model_gam_nogdp)$s.table[1,3])
T0


fitted.values<-model_gam_nogdpnohdi$fitted.values
res<-model_gam_nogdpnohdi$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)
#0.071, we remove the HDI 


########################################
#We try to remove the year
model_gam_nogdpnohdinoyear = mgcv::gam(
  Prevalence_both~ 
    Country+
    s(Education, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdpnohdinoyear)


T0<-abs(summary(model_gam_nogdpnohdi)$p.table[2,3])
T0

fitted.values<-model_gam_nogdpnohdinoyear$fitted.values
res<-model_gam_nogdpnohdinoyear$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[2,3])
}
diagnostic_permutation(T0,T2)

#We definitely cannot remove the years



#############################################
#We try to remove MPOWER
model_gam_nogdpnohdinompower = mgcv::gam(
  Prevalence_both~
    Year+
    Country+
    s(Education, bs = 'cr'),
  data=data_gam
)
summary(model_gam_nogdpnohdinompower)


T0<-abs(summary(model_gam_nogdpnohdi)$p.table[39,3])
T0

fitted.values<-model_gam_nogdpnohdinompower$fitted.values
res<-model_gam_nogdpnohdinompower$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education, bs = 'cr')  +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0,T2)
###it seems like we can remove MPOWER


####################################
####################################
####################################


#We also try to do the same for males and females separately


####################################
############# FEMALES ##############
####################################


model_gam_f = mgcv::gam(
  Prevalence_females~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(GDP, bs = 'cr') + 
    s(HDI, bs = 'cr') + 
    s(Education_females, bs = 'cr') +
    mpower_all,
  data=data_gam
)
plot(model_gam_f)
summary(model_gam_f)
plot(model_gam_f$residuals)
hist(model_gam_f$residuals)



#We try to reduce the model
model_gam_nogdp = mgcv::gam(
  Prevalence_females~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(HDI, bs = 'cr') + 
    s(Education_females, bs = 'cr') +
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdp)


T0<-abs(summary(model_gam_f)$s.table[1,3])
T0

fitted.values<-model_gam_nogdp$fitted.values
res<-model_gam_nogdp$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education_females, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)

#p-value:  0.049
#We reject the null hypothesis that the GDP is 0
##We cannot remove GDP

################################################
#We try to reduce the model removing edu
model_gam_noedu = mgcv::gam(
  Prevalence_females~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(GDP, bs = 'cr') + 
    s(HDI, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_noedu)


T0<-abs(summary(model_gam_f)$s.table[3,3])
T0


fitted.values<-model_gam_noedu$fitted.values
res<-model_gam_noedu$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education_females, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[3,3])
}
diagnostic_permutation(T0,T2)


#p-value:  0.07
#We cannot reject the null hypothesis that the edu is 0
##We remove edu 


#####################################
model_gam_noeduohdi = mgcv::gam(
  Prevalence_females~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(GDP, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_noeduohdi)


T0<-abs(summary(model_gam_noedu)$s.table[2,3])
T0


fitted.values<-model_gam_noeduohdi$fitted.values
res<-model_gam_noeduohdi$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$HDI, bs = 'cr') +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)
#We cannot reject H0, we KEEP HDI 0.003



########################################
#We try to remove the year
model_gam_noedunoyear = mgcv::gam(
  Prevalence_females~ 
    Country+
    s(GDP, bs = 'cr') + 
    s(HDI, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_noedunoyear)



T0<-abs(summary(model_gam_noedu)$p.table[2,3])
T0


fitted.values<-model_gam_nogdpnohdinoyear$fitted.values
res<-model_gam_nogdpnohdinoyear$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr') +  
      s(data_gam$HDI, bs = 'cr') +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[2,3])
}
diagnostic_permutation(T0,T2)
#p 0.03
#We definitely cannot remove the years



#############################################
#We try to remove MPOWER
model_gam_noedunompower = mgcv::gam(
  Prevalence_females~
    Year+
    Country+
    s(GDP, bs = 'cr')+
    s(HDI, bs = 'cr'),
  data=data_gam
)
summary(model_gam_noedunompower)


T0<-abs(summary(model_gam_noedu)$p.table[39,3])
T0

fitted.values<-model_gam_noedunompower$fitted.values
res<-model_gam_noedunompower$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr')  +
      s(data_gam$HDI, bs = 'cr')  +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0,T2)
###it seems like we can remove MPOWER




####################################
############# MALES ##############
####################################




model_gam_m = mgcv::gam(
  Prevalence_males~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(GDP, bs = 'cr') + 
    s(HDI, bs = 'cr') + 
    s(Education_males, bs = 'cr') +
    mpower_all,
  data=data_gam
)
plot(model_gam_m)
summary(model_gam_m)
plot(model_gam_m$residuals)
hist(model_gam_m$residuals)



#We try to reduce the model
model_gam_nogdp = mgcv::gam(
  Prevalence_males~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(HDI, bs = 'cr') + 
    s(Education_males, bs = 'cr') +
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdp)


T0<-abs(summary(model_gam_m)$s.table[1,3])
T0

fitted.values<-model_gam_nogdp$fitted.values
res<-model_gam_nogdp$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education_males, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)

#p-value:  0.141
#We cannot reject the null hypothesis that the GDP is 0
##We remove GDP

################################################
#We try to reduce the model removing edu
model_gam_nogdpnoedu = mgcv::gam(
  Prevalence_males~ 
    Year+                                              
    Country+
    s(HDI, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdpnoedu)


T0<-abs(summary(model_gam_nogdp)$s.table[1,3])
T0


fitted.values<-model_gam_nogdpnoedu$fitted.values
res<-model_gam_nogdpnoedu$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education_males, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)


#p-value:  0.006
#We cannot reject the null hypothesis that the edu is 0
##We keep edu 


#####################################
model_gam_nogdpohdi = mgcv::gam(
  Prevalence_females~ 
    Year+                  #the behaviour is nearly linear,                                    # I can probably reduce the model
    Country+
    s(Education_males, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdpohdi)


T0<-abs(summary(model_gam_nogdp)$s.table[2,3])
T0


fitted.values<-model_gam_nogdpohdi$fitted.values
res<-model_gam_nogdpohdi$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education_males, bs = 'cr') +
      s(data_gam$HDI, bs = 'cr') +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)
#p-value 0.143
#We cannot reject H0, we KEEP HDI 0.003



########################################
#We try to remove the year
model_gam_nogdpnohdinoyear = mgcv::gam(
  Prevalence_males~ 
    Country+
    s(Education_males, bs = 'cr') + 
    mpower_all,
  data=data_gam
)
summary(model_gam_nogdpnohdinoyear)



T0<-abs(summary(model_gam_nogdpnohdi)$p.table[2,3])
T0


fitted.values<-model_gam_nogdpnohdinoyear$fitted.values
res<-model_gam_nogdpnohdinoyear$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education_males, bs = 'cr') +  
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[2,3])
}
diagnostic_permutation(T0,T2)
#p  0.002
#We definitely cannot remove the years



#############################################
#We try to remove MPOWER
model_gam_nogdpnohdinompower = mgcv::gam(
  Prevalence_females~
    Year+
    Country+
    s(Education_males, bs = 'cr'),
  data=data_gam
)
summary(model_gam_nogdpnohdinompower)


T0<-abs(summary(model_gam_nogdpnohdi)$p.table[39,3])
T0

fitted.values<-model_gam_nogdpnohdinompower$fitted.values
res<-model_gam_nogdpnohdinompower$residuals

seed<-2022
B<-1000
set.seed(seed)
T2<-numeric(B)
n<-nrow(data_gam)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res[permutation]
  response.perm <- fitted.values + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education_males, bs = 'cr')  +
      data_gam$mpower_all
  )
  T2[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0,T2)
#p-value:  0.474
###it seems like we can remove MPOWER



#Conclusion: from this type of analysis, MPOWER does not seem to be a significant 
#factor. on the other hand, year has a negative impact on smoking and at least one
#sociodemographic indicator counts
