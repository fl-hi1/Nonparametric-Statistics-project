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
library(ggplot2)

#Read data and add column which will be used for labelling
data<-read.table("../data/final_dataset_2007-2020.txt",header=T, sep='',check.names = FALSE)

data<-data[data$Year=='2020',]
data$Cig_taxes=data$Cig_taxes*100
data$merged<-paste(data$Country, "_", data$Year) #for text
data$mpower_all=((data$Taxes+data$Help+data$Warn+data$Bans+data$Protect)-12)/(29-12)


# Multivariate case
#bagplot_matrix <- aplpack::bagplot.pairs(data[data$Year=='2020',c(3,4,5,6,7,8,9)])
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
#New zealand has a low prevalence with affordability
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




##
countries<-data[data$Year=='2008',1]

HDI_MHI<-data[data$Year=='2008',"HDI_MHI"]

data_2008<-data[data$Year=='2008',5]
data_2020<-data[data$Year=='2020',5]

data_2008_males<-data[data$Year=='2008',4]
data_2020_males<-data[data$Year=='2020',4]

data_2008_females<-data[data$Year=='2008',3]
data_2020_females<-data[data$Year=='2020',3]


df1<-cbind(data_2008,data_2020)
df_f<-cbind(data_2008_females,data_2020_females)
df_m<-cbind(data_2008_males,data_2020_males)



par(mfrow=c(1,3))
#Both
bagplot(df1,
        factor=1.5,
        xlim=c(5,55),
        ylim=c(5,55),
        cex=1.2,
        xlab='Smoking prevalence in 2008 (%)',
        ylab="Smoking prevalence in 2020 (%)",
        main="Smoking prevalence 2008-2020")
par(cex.axis = 1.4,cex.lab=1.5, cex.main=1.4)
lines(x=0:50, y=0:50, col='red',lty=2)
text(data_2008,
     data_2020,
     pos=1,
     cex=1.2,
     labels = countries)

#Females
bagplot(df_f,
        factor=1.5,
        xlim=c(5,55),
        ylim=c(5,55),
        cex=1.2,
        xlab='Smoking prevalence in 2008 (%)',
        ylab="Smoking prevalence in 2020 (%)",
        main="Female prevalence 2008-2020")
par(cex.axis = 1.4,cex.lab=1.5,cex.main=1.4)
lines(x=0:50, y=0:50, col='red',lty=2)
text(data_2008_females,
     data_2020_females,
     pos=1,
     labels = countries)

#Males
bagplot(df_m,
        factor=1.5,
        xlim=c(5,55),
        ylim=c(5,55),
        cex=1.2,
        xlab='Smoking prevalence in 2008 (%)',
        ylab="Smoking prevalence in 2020 (%)",
        main="Male prevalence 2008-2020")
par(cex.axis = 1.4,cex.lab=1.5,cex.main=1.4)
lines(x=0:50, y=0:50, col='red',lty=2)
text(data_2008_males,
     data_2020_males,
     pos=1,
     labels = countries,
     cex=1.2)


#####Bubbleplot - from GGPLOT 2
databub<-data.frame(data_2008=data_2008,
                    data_2020=data_2020,
                    HDI_MHI=HDI_MHI)


# Create a 2D bubble plot for both sexes
ggplot(data=databub,aes(x = data_2008, y = data_2020,  color = HDI_MHI)) +
  geom_point() +
  geom_text(aes(label = countries), vjust = 1.5) +  # Add labels with the 'Country' variable
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the y = x line
  scale_color_gradient(low = "red", high = "blue") +
  labs(x = "Smoking prevalence in 2008 (%)", 
       y = "Smoking prevalence in 2020 (%)", 
       size = "Population", 
       color = "HDI_MHI") +
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence in 2008 vs 2020 in OECD Countries")



# Create a 2D bubble plot for females
ggplot(data=databub,aes(x = data_2008_females, 
                        y = data_2020_females,
                        color = HDI_MHI)) +
  geom_point() +
  geom_text(aes(label = countries), vjust = 1.5) +  # Add labels with the 'Country' variable
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the y = x line
  scale_color_gradient(low = "red", high = "blue") +
  labs(x = "Smoking prevalence in 2008 (%)", 
       y = "Smoking prevalence in 2020 (%)", 
       size = "Population", 
       color = "HDI_MHI") +
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for females in 2008 vs 2020 in OECD Countries")


# Create a 2D bubble plot for males
ggplot(data=databub,aes(x = data_2008_males, 
                        y = data_2020_males,
                        color = HDI_MHI)) +
  geom_point() +
  geom_text(aes(label = countries), vjust = 1.5) +  # Add labels with the 'Country' variable
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +  # Add the y = x line
  scale_color_gradient(low = "red", high = "blue") +
  labs(x = "Smoking prevalence in 2008 (%)", 
       y = "Smoking prevalence in 2020 (%)", 
       size = "Population", 
       color = "HDI_MHI") +
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for males in 2008 vs 2020 in OECD Countries")


#Comment on Korea strange case
#Female smoking is perceived very negatively in East Asian countries 
#such as South Korea, Japan, and China, 
#as well as in Islamic countries. 
#These countries’ self-reported surveys (SRs) 
#tend to produce results that underestimate 
#the number of smokers, owing to the social 
#desirability response bias. 
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4319222/


cluster1<-data[data$Year=='2020',"clust_3"]
cluster2<-data[data$Year=='2020',"clust_5"]

data$mpower_all=((data$Taxes+data$Help+data$Warn+data$Bans+data$Protect)-12)/(29-12)
databub$mpower_all_2020<-data[data$Year=='2020',"mpower_all"]
databub$mpower_all_2008<-data[data$Year=='2008',"mpower_all"]
databub$clust<-as.factor(data[data$Year=='2008',"HDI_MHI_clustering"])

databub_f<-databub
databub_f$data_2008<-data_2008_females
databub_f$data_2020<-data_2020_females


databub_m<-databub
databub_m$data_2008<-data_2008_males
databub_m$data_2020<-data_2020_males



#####INTRODUCING ALSO MPOWER------------

# Create a 2D bubble plot - after removing costarica
ggplot(data=databub,aes(x = (data_2020-data_2008)/data_2008*100, 
                             y = ((mpower_all_2020-mpower_all_2008)/mpower_all_2008)*100,
                             color = HDI_MHI, 
                             size=mpower_all_2008)) +
  geom_point() +
  geom_text(aes(label = countries), vjust = 1.5,size=4) +  # Add labels with the 'Country' variable
  scale_size(range = c(0.5,10), 
             breaks=c(0.10, 0.2, 0.3, 0.4 ,0.5, 0.75),
             labels = c("0.1", "0.2","0.3", "0.4", "0.5", "0.75")) +
  scale_color_gradient(low = "red", high = "blue") +
  labs(x = "Smoking percentual change with respect to 2008 (%)", 
       y = "mpower_all_scores percentual change with respect to 2008 (%)", 
       size = "MPOWER score 2008", 
       color = "HDI_MHI") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for males in 2008 vs 2020 in OECD Countries")


# Create a 2D bubble plot - after removing costarica
ggplot(data=databub[-7,],aes(x = (data_2020-data_2008)/data_2008*100, 
                        y = ((mpower_all_2020-mpower_all_2008)/mpower_all_2008)*100,
                        color = data_2008, 
                        size=mpower_all_2008)) +
  geom_point() +
  geom_text(aes(label = countries[-7]), 
            vjust = 1.5,
            size=4, 
            col='black') +  # Add labels with the 'Country' variable
  scale_size(range = c(0.5,10), 
             breaks=c(0.10, 0.2, 0.3, 0.4 ,0.5, 0.75),
             labels = c("0.1", "0.2","0.3", "0.4", "0.5", "0.75")) +
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Smoking percentual change with respect to 2008 (%)", 
       y = "mpower_all_scores percentual change with respect to 2008 (%)", 
       size = "MPOWER score 2008 (0-1)", 
       color = "Smoking prevalence 2008 (%)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for males in 2008 vs 2020 in OECD Countries")



# Create a 2D bubble plot for males
ggplot(data=databub_m[-7,],aes(x = (data_2020-data_2008)/data_2008*100, 
                        y = (mpower_all_2020-mpower_all_2008)/mpower_all_2008*100,
                        color = data_2008,
                        size=mpower_all_2008)) +
  geom_point() +
  geom_text(aes(label = countries[-7]), 
            vjust = 1.5,
            size=4,
            col="black") +  # Add labels with the 'Country' variable
  scale_size(range = c(0.5,10), 
             breaks=c(0.10, 0.2, 0.3, 0.4 ,0.5, 0.75),
             labels = c("0.1", "0.2","0.3", "0.4", "0.5", "0.75")) +
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Smoking percentual change with respect to 2008 (%)", 
       y = "mpower_all_scores percentual change with respect to 2008 (%)", 
       size = "MPOWER score 2008", 
       color = "HDI_MHI") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for males in 2008 vs 2020 in OECD Countries")



# Create a 2D bubble plot for females
ggplot(data=databub_f[-7,],aes(x = (data_2020-data_2008)/data_2008*100, 
                        y = (mpower_all_2020-mpower_all_2008)/mpower_all_2008*100,
                        color = data_2008,
                        size=mpower_all_2008)) +
  geom_point() +
  geom_text(aes(label = countries[-7]), 
            vjust = 1.5,
            size=4,
            col="black") +  # Add labels with the 'Country' variable
  scale_size(range = c(0.5,10), 
             breaks=c(0.10, 0.2, 0.3, 0.4 ,0.5, 0.75),
             labels = c("0.1", "0.2","0.3", "0.4", "0.5", "0.75")) +
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Smoking percentual change with respect to 2008 (%)", 
       y = "mpower_all_scores percentual change with respect to 2008 (%)", 
       size = "MPOWER score 2008", 
       color = "Prevalence 2008 (%)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
  ggtitle("Smoking Prevalence for females in 2008 vs 2020 in OECD Countries")


#
#databub_diff<-databub_f
#databub_diff$data_2008<-databub_m$data_2008-databub_f$data_2008
#databub_diff$data_2020<-databub_m$data_2020-databub_f$data_2020
#
#
#
## Create a 2D bubble plot for diff
#ggplot(data=databub_diff[-7,],aes(x = (data_2020-data_2008)/data_2008*100, 
#                               y = (mpower_all_2020-mpower_all_2008)/mpower_all_2008*100,
#                               color = HDI_MHI,
#                               size=data_2008)) +
#  geom_point() +
#  geom_text(aes(label = countries[-7]), 
#            vjust = 1.5,
#            size=4,
#            col="black") +  # Add labels with the 'Country' variable
#  scale_size(range = c(0.5,10), 
#             breaks=c(0, 2.5, 5, 10,15, 20, 30),
#             labels = c(0, 2.5, 5, 10,15, 20, 30)) +
#  scale_color_gradient(low = "red", high = "blue") +
#  labs(x = "Smoking percentual change with respect to 2008 (%)", 
#       y = "mpower_all_scores percentual change with respect to 2008 (%)", 
#       size = "MPOWER score 2008", 
#       color = "HDI_MHI (%)") +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
#  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add the y = x line
#  theme(axis.title = element_text(size = 12)) + # Increase the axis label size
#  ggtitle("Smoking Prevalence diff in 2008 vs 2020 in OECD Countries")