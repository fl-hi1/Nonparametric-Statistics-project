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



###Compute bootstrap confint on the differences
diagnostic_bootstrap <- function(distro, obs, alpha) {  #distro = T.boot, 
  #obs = predict(fit,x)$y, 
  #      or T.obs
  #x potrebbe essere un nuovo punto
  variance_pred <- var(distro)
  print(paste("Variance: ", variance_pred))
  sd_pred <- sd(distro)
  print(paste("Standard deviation: ", sd_pred))
  bias_pred <- mean(distro) - obs
  print(paste("Bias: ", bias_pred))
  MSE_pred <- variance_pred + bias_pred^2
  print(paste("MSE: ", MSE_pred))
  # computing quantiles
  right.quantile <- quantile(distro, 1 - alpha / 2)
  left.quantile <- quantile(distro, alpha / 2)
  # REVERSE!!!!-percentile ----- guardare come si fanno i non reverse!!!!!credo invertendo i due
  CI <- c(
    obs - (right.quantile - obs),
    obs,
    obs - (left.quantile - obs)
  )
  names(CI) <- c("lower", "center", "upper")
  print(CI)
  plot(ecdf(distro), main = "Bootstrap distribution")
  abline(v = CI[2], col = 3, lwd = 2)
  abline(v = CI[c(1, 3)], lty = 3)
}










#####################################################

##### 2007
mydf<-t(df_2007)
quartz()
perm_t_test_paired(mydf, mu0, B, mean)
p_val_2007<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2007


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
B=1000
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
# [1] "Variance:  9.67520582271875"
# [1] "Standard deviation:  3.11049928833278"
# [1] "Bias:  -2.510348"
# [1] "MSE:  15.9770529038228"
# lower center  upper 
# 15.924 17.878 25.348 



##### 2008
mydf<-t(df_2008)
p_val_2008<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2008


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
# [1] "Variance:  10.4477461859049"
# [1] "Standard deviation:  3.23229735419019"
# [1] "Bias:  -2.444159"
# [1] "MSE:  16.4216594031859"
# lower center  upper 
# 15.624 17.288 24.388




##### 2010
mydf<-t(df_2010)
p_val_2010<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2010


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)

#[1] "Variance:  8.50940760197699"
#[1] "Standard deviation:  2.91708889168242"
#[1] "Bias:  -2.120825"
#[1] "MSE:  13.007306282602"
#lower center  upper 
#14.650 16.075 22.375 



##### 2012
mydf<-t(df_2012)
p_val_2012<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2012


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
# [1] "Variance:  9.22251810629728"
# [1] "Standard deviation:  3.03685990890217"
# [1] "Bias:  -1.334803"
# [1] "MSE:  11.0042171551063"
# lower    center     upper 
# 8.421575 14.547000 19.846900 



##### 2014
mydf<-t(df_2014)
p_val_2014<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2014


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
# [1] "Variance:  7.50849421469371"
# [1] "Standard deviation:  2.74016317300516"
# [1] "Bias:  -0.951211000000004"
# [1] "MSE:  8.41329658121471"
# lower  center   upper 
# 10.4915 13.1980 17.4820


##### 2016
mydf<-t(df_2016)
p_val_2016<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2016


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)
# [1] "Variance:  6.79557390544986"
# [1] "Standard deviation:  2.60683215904858"
# [1] "Bias:  -0.466230000000001"
# [1] "MSE:  7.01294431834986"
# lower    center     upper 
# 7.677778 11.865556 15.522222



##### 2018
mydf<-t(df_2018)
p_val_2018<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2018


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)


# [1] "Variance:  6.45530084834838"
# [1] "Standard deviation:  2.54072840900958"
# [1] "Bias:  0.145450000000002"
# [1] "MSE:  6.47645655084838"
# lower center  upper 
# 3.225 10.625 13.750 



##### 2020
mydf<-t(df_2020)
p_val_2020<-perm_t_test_paired(mydf, mu0, B, mean)
p_val_2020


#######Parallel (ma nonparallel funziona meglio)
compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:length(df), replace = T) #or length(df) if numeric vector
  df.boot <- df[permutation] #or without comma if numeric vector
  #CHANGE HERE!
  quant0.75 <- quantile(df.boot)[4]
  return(quant0.75) # this can be 2D etc.
}

n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))
#Change here with the name of parameters called in compute bootstrap 
#(method_name only if with mediam)
clusterExport(cl, varlist = list("mydf",  #or "df"
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(mydf), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
T.obs=compute_bootstrap_sample(mydf)
T.obs
diagnostic_bootstrap(T.boot, T.obs, alpha = myalpha)

# [1] "Variance:  5.50458355793291"
# [1] "Standard deviation:  2.34618489423424"
# [1] "Bias:  0.253225"
# [1] "MSE:  5.56870645855791"
# lower center  upper 
# 4.050  9.725 12.350


#P-value
c(p_val_2007,
  p_val_2008,
  p_val_2010, 
  p_val_2012, 
  p_val_2014,
  p_val_2016,
  p_val_2018,
  p_val_2020)


#Bootstrap

# Year lower center  upper 
# 2010 14.650 16.075 22.375 
# 2012 8.421575 14.547000 19.846900 
# 2014 10.4915 13.1980 17.4820 ---->2014
# 2016 7.677778 11.865556 15.522222
# 2018 3.225 10.625 13.750 
#c2020  4.050  9.725 12.350


#Conclusion: 
#the difference is significant for all years, so it may make sense to
#perform two separate regressions

#Question: is it better to do so or to run two different regressions separately?
