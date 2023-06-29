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

#Import data
data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)


data$Year<-as.numeric(data$Year)

#creating unified mpower indes
#Creating a unified index
min_index=12 # since 1 indicates lack of measures possible so we start from 2
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


#Can we say that we have increased over time?---------
#We use two approaches:

dataset_mpower<-data[,c(1,2,22)]
#Both
mpower_table <- dataset_mpower %>%
  pivot_wider(names_from = Country, 
              values_from = mpower_all)
mpower_table<-as.data.frame(mpower_table)

df<-t(mpower_table[8,-1]-mpower_table[2,-1])
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
#[1] "Variance:  0.000542379749992204"
#[1] "Standard deviation:  0.0232890478549941"
#[1] "Bias:  -0.00899999999999998"
#[1] "MSE:  0.000623379749992204"
#lower    center     upper 
#0.1470588 0.1764706 0.2352941

#The bootstrap interval does no contain 0, so we can say that the 
# median difference betwee mpower value at 2007 and at 2020 
# is greater than 0

#Namely, #So there has been an increase of around 14.6-0.23 in the MPOWER
#compound score


###################################
############ Can we move to a functional setting?
###################################


#removing the year column and year 2007 observations
mpower_table_f<-as.data.frame(mpower_table[-1,-1])

grid <- seq(2008, 2020, by=2)
#Create functional data

f_data <- fData(grid,t(mpower_table_f))


plot(f_data,
     xlab="Year", 
     ylab="Mpower composite measure",
     main="Mpower")
fb_plot = fbplot(f_data, main="Magnitude outliers - MBD", Depths = "MBD")  # Functional Box Plot
fb_plot$ID_outliers
# Israel      Japan   Korea   Mexico   Switzerland   Türkiye   United States 
MBD_f<-as.data.frame(t(MBD(f_data)))
MBD_f[fb_plot$ID_outliers]
#Israel       Japan    Korea    Mexico   Switzerland Türkiye       United States
# 0.2031599 0.143416 0.261075 0.1958443   0.2072749 0.1199451      0.193101


#These countries seem to increased less than others their MPOWER composite measure
mpower_table$Israel
mpower_table$Japan
mpower_table$Korea
mpower_table$Mexico
mpower_table$Switzerland

mpower_table$`United States`

#Turkiye has had a very steep increase
mpower_table$Türkiye

#In Turkey, the adoption of MPOWER measures in 2008 
#led to a significant reduction in smoking. 
#By 2012 data showed that smoking decreased by 13.4% 
#and exposure to second-hand smoke was on the decline too. 
#Turkey became the third country in Europe to go 100% smoke-free indoors 
#and the first country to achieve all six MPOWER measures at the highest level.

#The MPOWER measures have been life-changing for Turkey’s people, 
#reducing the likelihood of heart disease, lung cancer, and other chronic diseases. 
#Progress is evident, with studies showing a 20% decline in 2012 
#in the number of citizens admitted to hospital for smoking-related diseases.



############
###Can we classify data based on HDI MHI?
############


names<-colnames(mpower_table)%in%(data[data$HDI_MHI_clustering==c(1,2),1])
df_low<-mpower_table[,c(names)]

names<-colnames(mpower_table)%in%(data[data$HDI_MHI_clustering==c(3,4),1])
df_high<-mpower_table[,c(names)]

####First question:do the low countries have a #no potrei fqre le medie delle diff
diff_low<-df_low[8,]-df_low[2,]
median1<-median(as.numeric(diff_low[1,]))
diff_high<-df_high[8,]-df_high[2,]
median2<-median(as.numeric(diff_high[1,]))
#have the low countries and high countries experienced
#a different increase over the period under exam?
T0=abs(median1-median2) 
T0









