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

seed=2022
B=1000


####Permutational test diagnostics
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
data$Year<-as.numeric(data$Year)

#creating unified mpower indes
#Creating a unified index
min_index=12 # since 1 is not possible
max_index=29 #6*5 minus 1 for monitor which is up to 5

data$mpower_all=((data$Taxes+
                    data$Help+
                    data$Warn+
                    data$Bans+
                    data$Protect+
                    data$Monitor)-min_index)/(max_index-min_index)
boxplot(data$mpower_all~data$Country)
boxplot(data$mpower_all~as.factor(data$Year))
#Seems that the countries have increased their policies across years!
data$Year<-as.numeric(data$Year)

data_gam<-data
data_gam=data_gam[data_gam$Year!='2007',]
data_gam=data_gam[data_gam$Year!='2008',]

#I also remove japan as it has no education data
data_gam=data_gam[data_gam$Country!='Japan',]

#data_gam$Campaigns<-as.numeric(data_gam$Campaigns)
data_gam$Bans<-as.numeric(data_gam$Bans)
data_gam$Monitor<-as.numeric(data_gam$Monitor)
data_gam$Help<-as.numeric(data_gam$Help)
data_gam$Protect<-as.numeric(data_gam$Protect)
data_gam$Warn<-as.numeric(data_gam$Warn)
data_gam$Taxes<-as.numeric(data_gam$Taxes)


########################################################################
############################# GAM MODELS ##############################
########################################################################

#PREVALENCE BOTH VERSUS AFFORDABILITIES
#COUNTRIES ARE ADDED AS FACTOR

#Adding GDP and Education
#GAM model, smoothing term with cubic splines
#I will then see if I can reduce this term


##Run once!
data_gam$GDP<-log(data_gam$GDP)

model_gam_both <- gam(
  Prevalence_both ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(GDP, bs = 'cr') +
    s(Education, bs = 'cr') +  
   Affordability,
  data = data_gam
)
summary(model_gam_both)
plot(model_gam_both)
plot(model_gam_both$residuals)

#Comment: the countries clearly explain the majority of the variance in the data
#Let's see if first we can remove the GDP from the model
#H0:GDP term is 0, vs H1 is different than zero


# Compute the distributions
summ<-summary(model_gam_both)
T0<-abs(summ$s.table[2,3])
T0

model_gam_noGDP<-gam(
  Prevalence_both ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(Education, bs = 'cr') +  
    Affordability,
  data = data_gam
)
summary(model_gam_noGDP)
  
res<-model_gam_noGDP$residuals
fitted.values<-model_gam_noGDP$fitted.values

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)
#
#p-value:  0.099
#We cannot reject the null hypothesis that the GDP is 0



model_gam_noGDPnoEDU<-gam(
  Prevalence_both ~
    Year +
    Country+
    s(HDI, bs = 'cr')+
    Affordability,
  data=data_gam
)

res<-model_gam_noGDPnoEDU$residuals
fitted.values<-model_gam_noGDPnoEDU$fitted.values

T0<-summary(model_gam_noGDP)$s.table[2,3]
T0

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2)
#p-value 0.029 so we cannot remove it
##so we keep education

########
model_gam_noGDPnoHDI<-gam(
  Prevalence_both ~
    Year +
    Country+
    s(Education, bs = 'cr')+
    Affordability,
  data=data_gam
)

summ<-summary(model_gam_noGDP)
T0<-summ$s.table[1,3]
T0


res<-model_gam_noGDPnoHDI$residuals
fitted.values<-model_gam_noGDPnoHDI$fitted.values

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)
#p-value 0.084 we cannot reject it is 0 and we remove HDI



#Now I test the affordability
model.gam.noaffordability<-gam(
  Prevalence_both ~
    Year +
    Country+
    s(Education, bs = 'cr'),
  data=data_gam
)
summary(model.gam.noaffordability)
plot(model.gam.noaffordability)
T0<-abs(summary(model_gam_noGDP)$p.table[39,3])
T0

res<-model.gam.noaffordability$residuals
fitted.values<-model.gam.noaffordability$fitted.values

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0,T2) 
# 0.067
# It is not significant at alpha=0.1, I can remove it
# At the net of the analysis it seems that the affordability
# significantly and negatively on prevalence


####So the final model for is:
model.final.both<-gam(
  data_gam$Prevalence_both ~
    data_gam$Year +
    data_gam$Country+
    s(data_gam$Education, bs = 'cr')  
)
summary(model.final.both)




#############################
#############################
# On males
#############################
#############################
model_gam_males <- gam(
  Prevalence_males ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(GDP, bs = 'cr') +
    s(Education_males, bs = 'cr') +  
    Affordability,
  data = data_gam
)
summary(model_gam_males)
plot(model_gam_males)

summ_males<-summary(model_gam_males)
T0.males<-abs(summ_males$s.table[2,3])
T0.males


model_gam_noGDP.males<-gam(
  Prevalence_males ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(Education_males, bs = 'cr') +  
    Affordability,
  data = data_gam
)
summary(model_gam_noGDP.males)

res.males<-model_gam_noGDP.males$residuals
fitted.values.males<-model_gam_noGDP.males$fitted.values


T2.males<-numeric(B)
n<-nrow(data_gam)
set.seed(seed)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.males[permutation]
  response.perm <- fitted.values.males + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$Education_males, bs = 'cr') +  
      data_gam$Affordability
  )
  T2.males[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0.males,T2.males) 
# p-value 0.089
#We cannot reject H0, hence we can simplify the model
#and remove GDP


#Now we try to remove the HDI variable-----------------------
model.gam.noHDI<-gam(
  Prevalence_males ~
    Year +
    Country+
    s(Education_males, bs = 'cr')+
    Affordability,  
  data = data_gam
)
summary(model.gam.noHDI)
T0<-summary(model_gam_noGDP.males)$s.table[1,3]
T0


res<-model.gam.noHDI$residuals
fitted.values<-model.gam.noHDI$fitted.values

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)
#p-value 0.151, so we can remove HDI
#####---------------------------------------------

#Now we try to remove the education variable
model.gam.noedu.males<-gam(
  Prevalence_males ~
    Year +
    Country+
    Affordability,  
  data = data_gam
)
summary(model.gam.noedu.males)

T0<-abs(summary(model.gam.noHDI)$s.table[1,3])
T0

res<-model.gam.noedu.males$residuals
fitted.values<-model.gam.noedu.males$fitted.values

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
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2)
# p-value 0.003 so we cannot reduce the model 
# and we should keep the education

##########Now we try to remove affordability

#Now we try to remove the affordability variable
model.gam.noaffordability<-gam(
  Prevalence_males ~
    Year +
    Country+
    s(Education_males, bs = 'cr'),  
  data = data_gam
)
summary(model.gam.noaffordability)

T0<-abs(summary(model.gam.noHDI)$p.table[39,3])
T0

res.males<-model.gam.noaffordability$residuals
fitted.values.males<-model.gam.noaffordability$fitted.values


T2<-numeric(B)
n<-nrow(data_gam)

for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.males[permutation]
  response.perm <- fitted.values.males + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$Education_males, bs = 'cr') +  
      data_gam$Affordability
  )
  T2[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0,T2) 
#p-value 0.306
#So we can remove affordability
#So we have that we cannot say affordability impacts on males

#Final model on smoking prevalence for males:
summary(model.gam.noaffordability)





#############################
#############################
# On females
#############################
#############################

model_gam_females1 <- gam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(GDP, bs = 'cr') +
    s(Education_females, bs = 'cr') +  
    Affordability,
    data = data_gam
)
summary(model_gam_females1)
plot(model_gam_females1)


# Compute the distributions
summ_females<-summary(model_gam_females1)
T0.females<-summ_females$s.table[2,3]
T0.females

model_gam_noGDP.females<-gam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    s(Education_females, bs = 'cr') +  
    Affordability,
    data = data_gam
)
summary(model_gam_noGDP.females)

res.females<-model_gam_noGDP.females$residuals
fitted.values.females<-model_gam_noGDP.females$fitted.values


T2.females<-numeric(B)
n<-nrow(data_gam)

for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.females[permutation]
  response.perm <- fitted.values.females + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$GDP, bs = 'cr') +
      s(data_gam$Education_females, bs = 'cr') +  
      data_gam$Affordability
  )
  T2.females[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0.females,T2.females)
#p-value 0.166
#We cannot reject H0, hence we can simplify the model
#We remove the GDP
######

##Now we try to remove education
model_gam_females.noedu <- gam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    Affordability,  
  data = data_gam
)

summary(model_gam_females.noedu)


summ_females<-summary(model_gam_noGDP.females)

T0.females<-abs(summ_females$s.table[2,3])
T0.females

res.females<-model_gam_females.noedu$residuals
fitted.values.females<-model_gam_females.noedu$fitted.values

T2.females<-numeric(B)
n<-nrow(data_gam)

for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.females[permutation]
  response.perm <- fitted.values.females + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      s(data_gam$Education_females, bs = 'cr') +  
      data_gam$Affordability
  )
  T2.females[perm] <- abs(summary(model.perm)$s.table[2,3])
}
diagnostic_permutation(T0,T2.females)
#0.419
#So we cannot reject H0, we remove education


##Now we try to remove HDI
model_gam_females.nohdi <- gam(
  Prevalence_females ~
    Year +
    Country+
    Affordability,
  data = data_gam
)
summary(model_gam_females.nohdi)


T0<-summary(model_gam_females.noedu)$s.table[1,3]
T0

res.females<-model_gam_females.nohdi$residuals
fitted.values.females<-model_gam_females.nohdi$fitted.values

T2.females<-numeric(B)
n<-nrow(data_gam)
set.seed(seed)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.females[permutation]
  response.perm <- fitted.values.females + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      data_gam$Affordability
  )
  T2.females[perm] <- abs(summary(model.perm)$s.table[1,3])
}
diagnostic_permutation(T0,T2.females)
#p-value 0.01 so we cannot remove it

#####Now we try to remove affordability

model_gam_females.noaffordability <- gam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr'),
  data = data_gam
)
summary(model_gam_females.noaffordability)
plot(model_gam_females.noaffordability)

summ_females<-summary(model_gam_females.noedu)
T0.females<-abs(summ_females$p.table[39,3])
T0.females

res.females<-model_gam_females.noaffordability$residuals
fitted.values.females<-model_gam_females.noaffordability$fitted.values

T2.females<-numeric(B)
n<-nrow(data_gam)
set.seed(seed)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.females[permutation]
  response.perm <- fitted.values.females + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      data_gam$Affordability
  )
  T2.females[perm] <- abs(summary(model.perm)$p.table[39,3])
}
diagnostic_permutation(T0.females,T2.females)
# p-value is ###0.034 so I can reject h0 and I keep affordability
####I will run confint with that 


#Finally, I try to remove the year-----
model_gam_females.noyear <- gam(
  Prevalence_females ~
    Country+
    s(HDI, bs = 'cr')+
    Affordability,
  data = data_gam
)
summary(model_gam_females.noyear)
plot(model_gam_females.noaffordability)

summ_females<-summary(model_gam_females.noaffordability)
T0.females<-abs(summ_females$p.table[2,3])
T0.females

res.females<-model_gam_females.noyear$residuals
fitted.values.females<-model_gam_females.noyear$fitted.values

T2.females<-numeric(B)
n<-nrow(data_gam)
set.seed(seed)
for(perm in 1:B){
  permutation <- sample(n)
  res.perm <- res.females[permutation]
  response.perm <- fitted.values.females + res.perm
  model.perm<-gam(
    response.perm ~
      data_gam$Year +
      data_gam$Country+
      s(data_gam$HDI, bs = 'cr') +
      data_gam$Affordability
  )
  T2.females[perm] <- abs(summary(model.perm)$p.table[2,3])
}
diagnostic_permutation(T0.females,T2.females)
#p-value is 0 so we should keep the year


###Final model females
final_model_females<-gam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    Affordability,
   data=data_gam
 #  try to remove the outliers with respect to HDI- but that does not change much
 # data= data_gam[!data_gam$Country %in% c("Mexico", "Chile", "Colombia","Costa Rica"), ]
)
summary(final_model_females)

plot(final_model_females,  main="HDI smooth term")

prevf<-data_gam[data_gam$Country=="France",3][1]
prevf
target<- prevf-0.3*prevf
target

new_obs<-data.frame(Country="France",Year=2025,HDI=0.902,Affordability=3.2)
predict(final_model_females,new_obs,se=TRUE)


#dataprova<-data_gam[data_gam$Country!=c("Mexico","Chile","Colombia"),]
xnew = new_obs 
T.obs = predict(final_model_females,newdata=xnew)
#For smoothing splines (maybe not necessary to write xnew as dataframe)
#T.obs = predict(m_loc,x=valueofnewpoint)

fitted.obs<-predict(final_model_females, data_gam) 
#For smoothing splines 
#predict(fit_smooth_splines, df$x)$y #change x
res.obs<- data_gam$Prevalence_females-fitted.obs #change y

######Nonparallel
T.boot <- numeric(B)
set.seed(seed)
pb = progress::progress_bar$new(total = B,
                                format = " Processing [:bar] :percent eta: :eta")
for(i in 1:B){
  #### IF YOU HAVE A RESPONSE PERMUTE THE RESIDUALS
  response.b <- fitted.obs + sample(res.obs, replace = T)
  
  #Change here! - chosen model but with response.b as response
  model.boot <- gam(response.b ~ 
                      Year +
                      Country+
                      s(HDI, bs = 'cr') +
                      Affordability,
                    data=data_gam)
  #change here!
  T.boot[i] <- predict(model.boot,newdata=xnew)
  #If the model booted is a smooth splines
  #T.boot[i] <-predict(model.boot, x = valueofnewpoint)$y
  pb$tick()
}
myalpha=0.1
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
target





prevf<-data_gam[data_gam$Country=="Türkiye",3][1]
prevf
target<- prevf-0.3*prevf
target
new_obs<-data.frame(Country="Türkiye",Year=2025,HDI=0.85,Affordability=3.8)
xnew = new_obs 
T.obs = predict(final_model_females,newdata=xnew)
#For smoothing splines (maybe not necessary to write xnew as dataframe)
#T.obs = predict(m_loc,x=valueofnewpoint)

fitted.obs<-predict(final_model_females, data_gam) 
#For smoothing splines 
#predict(fit_smooth_splines, df$x)$y #change x
res.obs<- data_gam$Prevalence_females-fitted.obs #change y

######Nonparallel
T.boot <- numeric(B)
set.seed(seed)
pb = progress::progress_bar$new(total = B,
                                format = " Processing [:bar] :percent eta: :eta")
for(i in 1:B){
  #### IF YOU HAVE A RESPONSE PERMUTE THE RESIDUALS
  response.b <- fitted.obs + sample(res.obs, replace = T)
  
  #Change here! - chosen model but with response.b as response
  model.boot <- gam(response.b ~ 
                            Year +
                            Country+
                            s(HDI, bs = 'cr') +
                            Affordability,
                          data=data_gam)
  #change here!
  T.boot[i] <- predict(model.boot,newdata=xnew)
  #If the model booted is a smooth splines
  #T.boot[i] <-predict(model.boot, x = valueofnewpoint)$y
  pb$tick()
}
myalpha=0.1
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
target



########FRANCE

prevf<-data_gam[data_gam$Country=="Norway",3][1]
prevf
target<- prevf-0.3*prevf
target

new_obs<-data.frame(Country="Norway",Year=2025,HDI=0.966,Affordability=2.22)
predict(final_model_females,new_obs,se=TRUE)
xnew = new_obs 
T.obs = predict(final_model_females,newdata=xnew)
#For smoothing splines (maybe not necessary to write xnew as dataframe)
#T.obs = predict(m_loc,x=valueofnewpoint)

fitted.obs<-predict(final_model_females, data_gam) 
#For smoothing splines 
#predict(fit_smooth_splines, df$x)$y #change x
res.obs<- data_gam$Prevalence_females-fitted.obs #change y

######Nonparallel
T.boot <- numeric(B)
set.seed(seed)
pb = progress::progress_bar$new(total = B,
                                format = " Processing [:bar] :percent eta: :eta")
for(i in 1:B){
  #### IF YOU HAVE A RESPONSE PERMUTE THE RESIDUALS
  response.b <- fitted.obs + sample(res.obs, replace = T)
  
  #Change here! - chosen model but with response.b as response
  model.boot <- gam(response.b ~ 
                      Year +
                      Country+
                      s(HDI, bs = 'cr') +
                      Affordability,
                    data=data_gam)
  #change here!
  T.boot[i] <- predict(model.boot,newdata=xnew)
  #If the model booted is a smooth splines
  #T.boot[i] <-predict(model.boot, x = valueofnewpoint)$y
  pb$tick()
}
myalpha=0.1
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
target



################################################################################
################################################################################
#######                              QGAM                            ##########
################################################################################
################################################################################

install.packages("qgam")
library(qgam)

final_model_females_qgam<-qgam(
  Prevalence_females ~
    Year +
    Country+
    s(HDI, bs = 'cr') +
    Affordability,
  data=data_gam,
  qu=0.5
  #  try to remove the outliers with respect to HDI- but that does not change much
  # data= data_gam[!data_gam$Country %in% c("Mexico", "Chile", "Colombia","Costa Rica"), ]
)

summary(final_model_females_qgam)
plot(final_model_females_qgam,main="qgam")
#plot(final_model_females_qgam, scale = FALSE, pages = 1)

prevf<-data_gam[data_gam$Country=="Türkiye",3][1]
prevf
target<- prevf-0.3*prevf
target

new_obs<-data.frame(Country="Türkiye",Year=2025,HDI=0.843,Affordability=3.1)
predict(final_model_females_qgam,new_obs,se=TRUE)


################################################################################
################################################################################
#######                              EXTRA                            ##########
################################################################################
################################################################################

# Specify the number of folds for cross-validation
num_folds <- 5

# Create an empty vector to store the evaluation metric for each fold
evaluation_metric <- numeric(num_folds)

# Perform cross-validation
set.seed(2022)  # For reproducibility

# Create indices for cross-validation folds
folds <- sample(rep(1:num_folds, length.out = nrow(data_gam)))

mpe <- function(actual, predicted) {
  n <- length(actual)
  mpe_val <- (1/n) * sum(abs((actual - predicted)) / actual) * 100
  return(mpe_val)
}

# Iterate over  each fold
for (i in 1:num_folds) {
  # Split the data into training and validation sets based on the fold
  train_data <- data_gam[folds != i, ]  # Training set
  valid_data <- data_gam[folds == i, ]  # Validation set
  
  # Fit the GAM model with GCV using the training set
  gam_model <- gam(Prevalence_females ~
                     Year +
                     Country+
                     s(HDI, bs = 'cr') +
                    # s(Education_females, bs = 'cr') +
                     Affordability,
                   data = train_data)
  
  # Make predictions on the validation set
  predicted_values <- predict(gam_model, newdata = valid_data)
  
  # Calculate the evaluation metric (e.g., MSE) for the fold
  evaluation_metric[i] <- mpe(valid_data$Prevalence_females, predicted_values)
}
plot(valid_data$Prevalence_males,predicted_values)
# Calculate the average evaluation metric across all folds
average_metric <- (mean(evaluation_metric))

# Print the average evaluation metric
cat("Average Evaluation Metric:", average_metric, "\n")
# 0.0001605886



data_gam$HDI_MHI_clustering<-as.factor(data_gam$HDI_MHI_clustering)
#######
model_gam_both <- gam(
  Prevalence_both ~
    Year +
    s(HDI, bs = 'cr') +
    s(GDP, bs = 'cr') +
    s(Education, bs = 'cr') +  
    HDI_MHI_clustering+
    Affordability,
  data = data_gam
)

summary(model_gam_both)


set.seed(2022)  # For reproducibility

# Create indices for cross-validation folds

data_gam$Prevalence_females<-data_gam$Prevalence_females
num_folds=5
folds <- sample(rep(1:num_folds, length.out = nrow(data_gam)))
evaluation_metric<-numeric(num_folds)
# Iterate over each fold
for (i in 1:num_folds) {
  # Split the data into training and validation sets based on the fold
  train_data <- data_gam[folds != i, ]  # Training set
  valid_data <- data_gam[folds == i, ]  # Validation set

  # Fit the GAM model with GCV using the training set
  gam_model <- gam(Prevalence_females ~
                     Year+
                     Country+
                     s(HDI, bs = 'cr') +
                     Affordability,
                   data = train_data
                   )
  # Make predictions on the validation set
  predicted_values <- predict(gam_model, newdata = valid_data)
  #predicted_values <- plogis(predicted_values_l)
  
  #Calculate the evaluation metric (e.g., MSE) for the fold
  evaluation_metric[i] <- mean((valid_data$Prevalence_females - (predicted_values))^2)
}

# Calculate the average evaluation metric across all folds (e.g. RMSE)
average_metric <- (sqrt(mean(evaluation_metric)))

# Print the average evaluation metric
cat("Average Evaluation Metric:", average_metric, "\n") #1.250058
