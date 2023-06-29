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
dataraw<-read.table("../data/affordability_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)

aff<-dataraw[-c(8,7),]

#Import data
data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)
data$Year<-as.numeric(data$Year)
data<-data[data$Year!='2007',]
data<-data[data$Year!='2008',]

boxplot(data$Affordability~as.factor(data$Year))
#Seems that the countries have increased their policies across years!


#Can we say that we have increased over time?---------
#We use two approaches:

df<-t(aff[1,-1]-aff[6,-1])
#First we check that the difference between  2020 and 2007 is 0

compute_t_stat=function(df,med0){
  med_diff<-median((df)-med0)
  return (abs(med_diff))
}

med0<-0
T0<-compute_t_stat(df,med0)
T0

symmetric_perm_wrapper = function(df, med0) {
  n = dim(df)[1]
  p =  dim(df)[2] #2 #I am confronting two things
  
  med0matrix <- matrix(med0, 
                       nrow = n, 
                       ncol = p, 
                       byrow = TRUE)
  
  # In this case we use changes of signs in place of permutations
  signs.perm <- rbinom(n, 1, 0.5) * 2 - 1
  df_perm <- med0matrix + (as.numeric(df) - med0) * matrix(signs.perm, 
                                                           nrow = n, 
                                                           ncol = p, 
                                                           byrow = FALSE)
  compute_t_stat(df_perm, med0matrix)
}

# parallel
n_cores <- detectCores()
cl = makeCluster(n_cores)
clusterExport(cl, varlist = list("symmetric_perm_wrapper", "df", 
                                 "med0", 
                                 "compute_t_stat","seed"))
set.seed(seed)
T2 <- pbreplicate(1e3, symmetric_perm_wrapper(df, med0), cl = cl)
stopCluster(cl)


diagnostic_permutation <- function(T20, T2) {
  B <- length(T2)
  # Compare real test statistic with the ones given by the permuted data
  hist(T2, xlim = range(c(T2, T20)))
  abline(v = T20, col = 3, lwd = 4)
  # Empirical cumulative distribution function
  plot(ecdf(T2))
  abline(v = T20, col = 3, lwd = 4)
  # P-value
  p_val <- sum(T2 >= T20) / B
  cat("p-value: ", p_val)
}

diagnostic_permutation(T0, T2)

#So the difference is significant! I can reject the null hypothesis

#Now I want to compute a bootstrap confidence interval on this difference

#An alternative approach is to use bootstrap on the differences


###########Bootstrap on the differences


diagnostic_bootstrap <- function(distro, obs, alpha) { 
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

T0=median(df)
T0

compute_bootstrap_sample <- function(df) {
  permutation <- sample(1:nrow(df), replace = T) 
  df.boot <- df[permutation, ] 
  #Using the median
  med <- median(df.boot)
  return(med) 
}
n_cores <- detectCores()
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(np)))


clusterExport(cl, varlist = list("df", 
                                 "compute_bootstrap_sample", 
                                 "seed"))
set.seed(seed)
T.boot <- pbreplicate(B, compute_bootstrap_sample(df), cl = cl)
stopCluster(cl)

myalpha=0.05 #change!
diagnostic_bootstrap(T.boot, T0, alpha = myalpha)
# [1] "Variance:  0.00861563603603604"
# [1] "Standard deviation:  0.0928204505270042"
# [1] "Bias:  -0.00464000000000009"
# [1] "MSE:  0.00863716563603604"
# lower center  upper 
# 0.275  0.465  0.650 

#The bootstrap interval does no contain 0, so we can say that the 
# median difference betwee affordability value at 20010 and at 2020 
# is greater than 0



#####Can we move to a functional setting?

#removing the year column and year 2007 observations
aff <- aff[order(aff$Year, decreasing = FALSE), ]
mpower_table_f<-as.data.frame(aff[,-1])

grid <- seq(2010, 2020, by=2)
#Create functional data

f_data <- fData(grid,t(mpower_table_f))


plot(f_data,
     xlab="Year", 
     ylab="Affordability composite measure",
     main="Affordability")

fb_plot = fbplot(f_data, main="Magnitude outliers - MBD", Depths = "MBD")  # Functional Box Plot
fb_plot$ID_outliers
# Israel      Japan   Korea   Mexico   Switzerland   Türkiye   United States 
MBD_f<-as.data.frame(t(MBD(f_data)))
MBD_f[fb_plot$ID_outliers]
aff[,"New Zealand"]

#https://tobaccocontrol.bmj.com/content/early/2022/08/25/tc-2021-057232
#Between 2010 and 2020, 
#New Zealand-based tobacco companies used differential price shifting 
#to reduce the impact of annual tobacco excise tax increases on lower priced brands 
#compared with higher priced brands.

#Has experienced a very steep increase in affordability


newz<-data[data$Country=='New Zealand',]
italy<-data[data$Country=='Italy',]


#Is the prevalence decrease of new zealand above average?






