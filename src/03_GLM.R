rm(list=ls())

###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#Imports
library(tidyr)
library(dplyr)

data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)


library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)

# Method name
method_name = 'Mahalanobis'
method_name = 'Tukey'
method_name = 'Projection'

# Visualise bivariate data
plot(data$Prevalence,data$GDP)

# Depths
the_depth = depth(u=cbind(data$Prevalence,data$GDP),method=method_name)
df<-data.frame(data$Prevalence,data$GDP)

# Depth of new point relative to a sample
p = c(3,5) # INPUT
depth(u=p, X=df, method=method_name)

# Median
depthMedian(df, depth_params = list(method=method_name))
# or, if you already have the depth
df[which.max(the_depth),]

# Plots depth (only if the dataset is BIVARIATE)
depthContour(df,depth_params = list(method=method_name)) # ,points=TRUE
depthPersp(df,depth_params = list(method=method_name))
depthPersp(df,depth_params = list(method=method_name), plot_method = 'rgl')

# Plots Bagplot
bagplot(df)
bagplot(df, factor = 1.3)
aplpack::bagplot(df,show.whiskers = F, main="Bagplot")
aplpack::bagplot(df,show.loophull = F, main="Sunburst plot")
# multivariate
bagplot_matrix <- aplpack::bagplot.pairs(data[,3:5])

# ddPlot
ddPlot(x=df$data.Prevalence, y=df$data.GDP, depth_params=list(method=method_name))


# Outlier removal
(bagplot_cont <- bagplot(df))
#text(df$x1,df$x2,1:nrow(df),pos=1) # superimpose indexes
outlying_obs <- bagplot_cont$pxy.outlier
ind_outlying_obs <- which(apply(df,1,function(x) all(x %in% outlying_obs)))
unname(ind_outlying_obs)
#outl <- sort(unique(outl))
df_clean <- df[-ind_outlying_obs,]