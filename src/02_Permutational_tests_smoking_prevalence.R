rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"


females<-read.table("../data/smoking_prevalence_females_2007-2020.txt",header=T)
males<-read.table("../data/smoking_prevalence_males_2007-2020.txt",header=T)
countries<-read.table("../data/OECD_countries_income_level",header=T)$Country

library(progress)


# TWO POPULATION PAIRED CASE
# H0: mean(male) = mean(female)
# H1: mean(male) ≠ mean(female)
# can be reformulated as
# H0: mean(male-female) = mu
# H1: mean(male-female) ≠ mu
# with mu = c(0)
# ==> center of simmetry of one univariate population


#Dataset of differences
df<-males-females

df_2007<-df[1,]
df_2008<-df[2,]
df_2010<-df[3,]
df_2012<-df[4,]
df_2014<-df[5,]
df_2016<-df[6,]
df_2018<-df[7,]
df_2020<-df[8,]

# Set parameters
alpha <- 0.05
test_statistic <- mean
seed = 1996
set.seed(seed)
B=100000
mu0 <- 0


##########################  Utils  ###########################

perm_t_test_paired <- function(diff, mu0, iter = 10000, test_statistic = mean){
  T20 <- abs(test_statistic(diff) - mu0)
  n <- length(diff) ###DA RICONTROLLARE
  T2 <- numeric(iter)
  for(perm in 1:iter)
  {
    # Random permutation
    # obs: exchanging data within couples means changing the sign of the difference
    signs.perm <- rbinom(n, 1, 0.5)*2 - 1
    diff_perm <- (diff) * matrix(signs.perm,nrow=n ,ncol=1,byrow=FALSE) #controllare
    diff.mean_perm <- test_statistic(diff_perm)
    T2[perm] <- abs(diff.mean_perm-mu0)
  }
  hist(T2, xlim=c(0,T20))
  abline(v=T20, col='green')
  
  p.value <- sum(T2 >= T20) / B
  return(p.value)
}


#####################################################

##### 2007
mydf<-t(df_2007)
quartz()
perm_t_test_paired(mydf, mu0, B, mean)
p_val_2007<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2007

# Empirical type-1 Error with confidence interval
estimated.alpha <- sum(p_val_2007 < alpha) / B
estimated.alpha
c(estimated.alpha - sqrt(estimated.alpha * (1 - estimated.alpha) / B) * qnorm(1-alpha/2), estimated.alpha, estimated.alpha + sqrt(estimated.alpha * (1 - estimated.alpha) / B) * qnorm(1-alpha/2))

##### 2008
mydf<-t(df_2008)
p_val_2008<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2008

##### 2010
mydf<-t(df_2010)
p_val_2010<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2010

##### 2012
mydf<-t(df_2012)
p_val_2012<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2012

##### 2014
mydf<-t(df_2014)
p_val_2014<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2014

##### 2016
mydf<-t(df_2016)
p_val_2016<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2016

##### 2018
mydf<-t(df_2018)
p_val_2018<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2018

##### 2020
mydf<-t(df_2020)
p_val_2020<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2020

c(p_val_2007,
  p_val_2008,
  p_val_2010, 
  p_val_2012, 
  p_val_2014,
  p_val_2016,
  p_val_2018,
  p_val_2020)


#Conclusion: 
#the difference is significant for all years, so it may make sense to
#perform two separate regressions


#Question: is it better to do so or to run two different regressions separately?




