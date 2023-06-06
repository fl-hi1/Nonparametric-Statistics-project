#Exploratory data analysis
# Visualise data - for now we ignore the dependence, 
#the idea is to search for anomalous behaviours

rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#Import packages
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)

#Read data and add column which will be used for labelling
data<-read.table("../data/final_dataset_2007-2020.txt",header=T, sep='',check.names = FALSE)

data$Cig_taxes=data$Cig_taxes*100
data$merged<-paste(data$Country, "_", data$Year) #for text
data$mpower_all=(data$Campaigns+data$Help+data$Warn+data$Bans+data$Protect)/25


# Multivariate case
bagplot_matrix <- aplpack::bagplot.pairs(data[,c(3,4,5,6,7,8,9)])
#Comment:I use this bagplot to identify variables and countries with strange behaviour


#############################GDP VS PREVALENCE###############################


# Choose methos
#method_name = 'Mahalanobis'
method_name = 'Tukey'
#method_name = 'Projection'


data$GDP<-log(data$GDP)
# Depths
the_depth = depth(u=cbind(data$GDP,data$Prevalence_both),method=method_name)
df<-data.frame(data$GDP,data$Prevalence_both)

# Median
depthMedian(df, depth_params = list(method=method_name))

# or, if you already have the depth
df[which.max(the_depth),]

# Plots depth (only if the dataset is BIVARIATE)
depthContour(df,depth_params = list(method=method_name)) # ,points=TRUE
depthPersp(df,depth_params = list(method=method_name))
#depthPersp(df,depth_params = list(method=method_name), plot_method = 'rgl')

# Plots Bagplot more closely
bagplot(df)
#bagplot(df, factor = 1.3)
aplpack::bagplot(df,show.whiskers = F, main="GDP-Prevalence",xlab="GDP",ylab="Prevalence")
text(df$data.GDP,df$data.Prevalence_both, 1:nrow(df),pos=1,labels = data$merged)
#Sunburst plot
aplpack::bagplot(df,show.loophull = F, main="Sunburst plot")
text( df$data.GDP, df$data.Prevalence_both,1:nrow(df),pos=1,labels = data$merged)

# Outlier identification
(bagplot_cont <- bagplot(df))
#text(df$x1,df$x2,1:nrow(df),pos=1) # superimpose indexes
outlying_obs <- bagplot_cont$pxy.outlier
ind_outlying_obs <- which(apply(df,1,function(x) all(x %in% outlying_obs)))

outliers<-data[ind_outlying_obs,]
outliers

#Generally low GDP and low prevalence
#Poorer countries from center or Southern America
#Costa Rica
#Colombia
#Mexico

#Outstanding for the very high GDP but average prevalence

#Not outliers but of interest:
#Greece
#Chile 

#Relative high prevalence compared to the GDP




################GDP AND AFFORDABILITY################################

data$Education[is.na(data$Education)]<-0

data<-data[data$Country!='Japan',]

# Choose methos
#method_name = 'Mahalanobis'
method_name = 'Tukey'
#method_name = 'Projection'
data$Prevalence<-data$Prevalence_both
plot(data$Education, data$Prevalence)
# Depths
the_depth = depth(u=cbind(data$Education,data$Prevalence),method=method_name)
df<-data.frame(data$Education,data$Prevalence)

# Median
depthMedian(df, depth_params = list(method=method_name))

# or, if you already have the depth
df[which.max(the_depth),]

# Plots depth (only if the dataset is BIVARIATE)
depthContour(df,depth_params = list(method=method_name)) # ,points=TRUE
depthPersp(df,depth_params = list(method=method_name))
#depthPersp(df,depth_params = list(method=method_name), plot_method = 'rgl')

# Plots Bagplot more closely
bagplot(df)
bagplot(df, factor = 1.3)
aplpack::bagplot(df,show.whiskers = F, main="Affordability-Prevalence",xlab="Affordability (%of GDP)",ylab="Prevalence")
text(df$data.Education,df$data.Prevalence, 1:nrow(df),pos=1,labels = data$merged)
#Sunburst plot
aplpack::bagplot(df,show.loophull = F, main="Affordability-Prevalence",xlab="Affordability (%of GDP)",ylab="Prevalence")
text( df$data.Education, df$data.Prevalence,1:nrow(df),pos=1,labels = data$merged)

# Outlier identification
(bagplot_cont <- bagplot(df))
#text(df$x1,df$x2,1:nrow(df),pos=1) # superimpose indexes
outlying_obs <- bagplot_cont$pxy.outlier
ind_outlying_obs <- which(apply(df,1,function(x) all(x %in% outlying_obs)))

outliers<-data[ind_outlying_obs,]
outliers


#Comments:
#Interesting that 
#New zealand has a low prevalence despite the higher (increasing!) affordability
#Luxembourg has an extremely low affordability

#How about cig taxes?

#######CIG TAXES - PREVALENCES################

# Choose methos
method_name = 'Mahalanobis'
#method_name = 'Tukey'
#method_name = 'Projection'


# Depths
the_depth = depth(u=cbind(data$Cig_taxes,data$Prevalence),method=method_name)
df<-data.frame(data$Cig_taxes,data$Prevalence)

# Median
depthMedian(df, depth_params = list(method=method_name))

# or, if you already have the depth
df[which.max(the_depth),]

# Plots depth (only if the dataset is BIVARIATE)
depthContour(df,depth_params = list(method=method_name)) # ,points=TRUE
depthPersp(df,depth_params = list(method=method_name))
#depthPersp(df,depth_params = list(method=method_name), plot_method = 'rgl')


# Plots Bagplot more closely 
#From a first sight it seems that countries with higher prevalence in general have high taxes.#
# In this case, it will be interesting to seethe component of taxes on the decrease of prevalence in time within each nation
#We see that the US has (as expected) relatively low taxes, even in they increase gradually with years


bagplot(df)
bagplot(df, factor = 1.3)
aplpack::bagplot(df,show.whiskers = F, main="Taxes on cigarettes-Prevalence",xlab="Taxes on cigarettes (%of xxx)",ylab="Prevalence")
text(df$data.Cig_taxes,df$data.Prevalence, 1:nrow(df),pos=1,labels = data$merged)
#Sunburst plot
aplpack::bagplot(df,show.loophull = F, main="Taxes on cigarettes-Prevalence",xlab="Taxes on cigarettes (%of xxx)",ylab="Prevalence")
text(df$data.Prevalence,1:nrow(df),pos=1,labels = data$merged)

# Outlier identification
(bagplot_cont <- bagplot(df))
#text(df$x1,df$x2,1:nrow(df),pos=1) # superimpose indexes
outlying_obs <- bagplot_cont$pxy.outlier
ind_outlying_obs <- which(apply(df,1,function(x) all(x %in% outlying_obs)))

outliers<-data[ind_outlying_obs,]
outliers


#Comments:
#Interesting that 
#New zealand has a low prevalence despite the higher (increasing!) affordability
#Luxembourg has an extremely low affordability
