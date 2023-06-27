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


# Try a different strategy: create lagged predictors

data_gam<-data[data$Year!='2007',]


lag_order<-1
lagged_data <- data_gam
for (i in 3:ncol(data_gam)) {
  lagged_data[, i] <- c(rep(NA, lag_order), head(data_gam[, i], -lag_order))
}

#Removing 2008 - DO NOT RUN TWICE!
lagged_data=lagged_data[lagged_data$Year!='2008',]
lagged_data=lagged_data[lagged_data$Year!='2010',]
lagged_data=lagged_data[lagged_data$Country!='Japan',]

data_gam=data_gam[data_gam$Year!='2008',]
data_gam=data_gam[data_gam$Year!='2010',]
data_gam=data_gam[data_gam$Country!='Japan',]


model_gam4 = mgcv::gam(
                   data_gam$Prevalence_both ~ 
                   
                   data_gam$Cig_taxes+ 
                   lagged_data$Cig_taxes + 
                   
                   s(data_gam$GDP, bs = 'cr') + 
                   s(lagged_data$GDP, bs = 'cr') + 
                   
                   s(data_gam$Education, bs = 'cr') +
                   s(lagged_data$Education, bs = 'cr') +
                   
                   s(data_gam$Affordability, bs='cr') +
                   s(lagged_data$Affordability, bs='cr') +
                 
                   s(data_gam$mpower_all, bs='cr') +
                   s(lagged_data$mpower_all, bs='cr') 
                    
                  # data_gam$Country
                  )

quartz()
par(mfrow = c(2,4))
plot(model_gam4)



#Permutational inference to simplify the model---------------

library(pbapply)
library(mgcv)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$p.table[2,3]) #associated t value
T.obs

#H0: cig taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     #no cig taxes, only lagged ones
                     
                     lagged_data$Cig_taxes + 
                     
                     s(data_gam$GDP, bs = 'cr') + 
                     s(lagged_data$GDP, bs = 'cr') + 
                     
                     s(data_gam$Education, bs = 'cr') +
                     s(lagged_data$Education, bs = 'cr') +
                     
                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') 
                    # data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes+ 
      lagged_data$Cig_taxes + 
      
      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(data_gam$Education, bs = 'cr') +
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
      
     # data_gam$Country
  )
  return(abs(summary(gam.perm)$p.table[2,3]))
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)


quartz()
par(mfrow=c(2,1))
hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '')
abline(v = T.obs, col = 'red', lwd = 4)

plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)

p_value=sum(T2>=T.obs)/1000
p_value

#As the model is extremely complex, I set the alpha of 0.1
#So I can reject H0



#We cannot reject H0







################################################
###Now I perform on the lagged cig taxes
################################################


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    lagged_data$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$GDP, bs = 'cr') + 
    
    s(data_gam$Education, bs = 'cr') +
    s(lagged_data$Education, bs = 'cr') +
    
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
    
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$p.table[3,3]) #associated t value
T.obs

#H0: cig taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     #no lagged cigtaxes
                     data_gam$Cig_taxes + 

                     s(data_gam$GDP, bs = 'cr') + 
                     s(lagged_data$GDP, bs = 'cr') + 
                     
                     s(data_gam$Education, bs = 'cr') +
                     s(lagged_data$Education, bs = 'cr') +
                     
                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                     
                     #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      lagged_data$Cig_taxes + 
      
      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(data_gam$Education, bs = 'cr') +
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$p.table[3,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '')
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.359 
#I cannot reject H0--> I remove lagged cig


#NOW I HAVE REMOVED LAGGED CIGARETTES





#--------------------------------------------------
# H0: Education is 0 ---------------
#-------------------------------------------------




model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 

    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$GDP, bs = 'cr') + 
    
    s(data_gam$Education, bs = 'cr') +
    s(lagged_data$Education, bs = 'cr') +
    
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[3,3]) #associated t value
T.obs

#H0: Education does not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 

                     s(data_gam$GDP, bs = 'cr') + 
                     s(lagged_data$GDP, bs = 'cr') + 
                     
                     #no Education 
                     s(lagged_data$Education, bs = 'cr') +
                     
                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +

      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(data_gam$Education, bs = 'cr') +
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[3,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '')
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.132 

#I cannot reject H0, so I remove the predictor


#I NOW REMOVE EDUCATION



#--------------------------------------------------
# H0: Lagged Education is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[3,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 

                     s(data_gam$GDP, bs = 'cr') + 
                     s(lagged_data$GDP, bs = 'cr') + 
                     
                     #no lagge Education 

                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
                   #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[3,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
# p- value is 0, lagged education has an impact!

#H0 is rejected,
#I cannot remove lagged edu



#--------------------------------------------------
# H0: GDP is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[1,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                     
                     #No GDP
                     s(lagged_data$GDP, bs = 'cr') + 
                     
                     s(lagged_data$Education, bs = 'cr') + 
                     
                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
                   #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[1,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.056
#I reject H0...

#I cannot remove GDP!




#--------------------------------------------------
# H0: LAGGED GDP is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[2,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                   
                     s(data_gam$GDP, bs='cr') +
                     #No LAGGED GDP                     
                     s(lagged_data$Education, bs = 'cr') + 
                     
                     s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
                   #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      s(lagged_data$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[2,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.263

#I cannot reject H0

#So I can remove lagged GDP





#--------------------------------------------------
# H0: Affordability is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    s(data_gam$GDP, bs = 'cr') + 
    s(lagged_data$Education, bs = 'cr') +
    s(data_gam$Affordability, bs='cr') +
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[3,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                     
                     s(data_gam$GDP, bs='cr') +

                     s(lagged_data$Education, bs = 'cr') + 
                     
                    # NO AFFORDABILITY s(data_gam$Affordability, bs='cr') +
                     s(lagged_data$Affordability, bs='cr') +
                     
                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
                   #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 

      s(lagged_data$Education, bs = 'cr') +
      
      s(data_gam$Affordability, bs='cr') +
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[3,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.314

#I cannot reject H0

#So I can remove Affordability



#--------------------------------------------------
# H0: Lagged Affordability is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,4))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[3,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                     
                     s(data_gam$GDP, bs='cr') +
                     
                     s(lagged_data$Education, bs = 'cr') + 
                     

                     s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
                   #data_gam$Country
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[3,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.012

#I reject H0 and keep lagged affordability




#--------------------------------------------------
# H0: Mpower (this...) is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(lagged_data$Affordability, bs='cr') +
    
    s(data_gam$mpower_all, bs='cr') +
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,3))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[4,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                     
                     s(data_gam$GDP, bs='cr') +
                     
                     s(lagged_data$Education, bs = 'cr') + 
                     
                     s(lagged_data$Affordability, bs = 'cr') + 
                     
                  #   s(data_gam$mpower_all, bs='cr') +
                     s(lagged_data$mpower_all, bs='cr') #+
                   
)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(lagged_data$Affordability, bs='cr') +
      
      s(data_gam$mpower_all, bs='cr') +
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[4,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#0.227

#I cannot reject H0... I remove MPOWER



#--------------------------------------------------
# H0: LAGGED MPOWER (this...) is 0 ---------------
#-------------------------------------------------


model_gam4 = mgcv::gam(
  data_gam$Prevalence_both ~ 
    
    data_gam$Cig_taxes + 
    
    s(data_gam$GDP, bs = 'cr') + 
    
    s(lagged_data$Education, bs = 'cr') +
    
    s(lagged_data$Affordability, bs='cr') +
    
    s(lagged_data$mpower_all, bs='cr') 
  
)

quartz()
par(mfrow = c(2,3))
plot(model_gam4)

sum<-summary(model_gam4)
#Table for nonparametric model part
sum$s.table
#Table for parametric model part
head(sum$p.table)
T.obs<-abs(sum$s.table[4,3]) #associated t value
T.obs

#H0: lagged education taxes do not impact
gam.H0 = mgcv::gam(data_gam$Prevalence_both ~
                     
                     data_gam$Cig_taxes + 
                     
                     s(data_gam$GDP, bs='cr') +
                     
                     s(lagged_data$Education, bs = 'cr') + 
                     
                     s(lagged_data$Affordability, bs = 'cr') 
                     

)

res.H0 = gam.H0$residuals
plot(res.H0)
hist(res.H0)


perm_wrapper = function() {
  res.H0 = gam.H0$residuals
  n<-length(res.H0)
  permutation = sample(n)
  res.H0.perm = res.H0[permutation] 
  Y.perm.H0 = gam.H0$fitted + res.H0.perm 
  
  #Full model with permuted residuals
  gam.perm = mgcv::gam(
    Y.perm.H0 ~  
      
      data_gam$Cig_taxes +
      
      s(data_gam$GDP, bs = 'cr') + 
      
      s(lagged_data$Education, bs = 'cr') +
      
      s(lagged_data$Affordability, bs='cr') +
      
      s(lagged_data$mpower_all, bs='cr') 
  )
  return(abs(summary(gam.perm)$s.table[4,3])) ###change here
}


B<-1000
seed<-2023
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper"))
set.seed(seed)
T2 <- pbreplicate(1000, perm_wrapper(), simplify='vector')
stopCluster(cl)

hist(sort(T2)[-1000],
     breaks = 100,
     main = 'Permutational distribution of test statistics',
     xlab = '',xlim=c(0,12))
abline(v = T.obs, col = 'red', lwd = 4)


plot(ecdf(sort(T2)[-1000]), main = 'ECDF of test statistics')
abline(v = T.obs, col = 'red', lwd = 4)


p_value=sum(T2>=T.obs)/1000
p_value
#I cannot reject an impact with p-value 0.075

#So I keep them



#So my model is now that














#--------------------------------------------------
#Running the model for females only ---------------
#-------------------------------------------------

set.seed(1)
model_gam = mgcv::gam(data_gam$Prevalence_females ~
                        
                        data_gam$Cig_taxes+ 
                        lagged_data$Cig_taxes + 
                        
                        s(data_gam$GDP, bs = 'cr') + 
                        s(lagged_data$GDP, bs = 'cr') + 
                        
                        s(data_gam$Education, bs='cr') +
                        s(lagged_data$Education, bs='cr') +
                        
                        s(data_gam$Affordability, bs='cr') +
                        s(lagged_data$Affordability, bs='cr') +
                        
                        s(data_gam$mpower_all, bs='cr') +
                        s(lagged_data$mpower_all, bs='cr'), 
                      data=data_gam
)















#Running the model for males only---------------
