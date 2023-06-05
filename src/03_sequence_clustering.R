rm(list=ls())
###Setting the working directory as the project directory
setwd("C:/Users/Val/OneDrive - Politecnico di Milano/travail Polimi/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

library(dplyr)
library("cluster")
library(TraMineR)
library(dplyr)


fulldata<-read.table("../data/final_dataset_2007-2020.txt",header=T)

oecd_tabac = c("Australia", "Austria","Belgium","Canada","Chile","Colombia","Costa Rica","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Republic of Korea","Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden","Switzerland", "Türkiye","United Kingdom of Great Britain and Northern Ireland","United States of America")
#38 paeses in totale

countries = unique(fulldata$Country)


#Load the library usefull for the analysis
library("TraMineR")

#We need to creat a table in order to compute the sequence,
#let's do it for campaign first

campaign = fulldata[c("Country","Year","Campaigns")]

data_seq = data.frame(matrix(ncol = 9, nrow = 0))



for(count in countries){
  sub_row = c(count,campaign[campaign$Country == count,]$Campaigns[-1])
  data_seq = rbind(data_seq, sub_row)
}
u <- c("Country",seq(2008, 2020, by=2))
colnames(data_seq)<- u

#now let's define the parameter in order to create the sequence

camp.alpha <- seq(1,5)
camp.lab <- seq(1,5) #long label
camp.scode <- seq(1,5) #short label, use by states argument
camp.seq <- seqdef(data_seq, 2:8, alphabet = camp.alpha, states = camp.scode, labels = camp.lab)

#then, let's compute the dissimilarity matrix with the Optimal matching with a proper distance
z<- matrix(c(0,1,2,3,4,1,0,1,2,3,2,1,0,1,2,3,2,1,0,1,4,3,2,1,0), nrow = 5, ncol = 5, byrow = F)
z
camp.om <- seqdist(camp.seq, method = "OM", indel = 2, sm = z) #mi sambra buono =)

#then, let's do clustering, first ward method
clusterward <- agnes(camp.om, diss = TRUE, method = "ward")

X11()
plot(clusterward, ask=TRUE, which.plots=2) #let's do 3 cluster, clearly

camp.cl3 <- cutree(clusterward, k = 3) 
cl3.lab <- factor(camp.cl3, labels = paste("Cluster", 1:3))



data_clustering = data_seq
data_clustering['ward_clustering'] = camp.cl3

#let's do it with an other method, average for instance average
clusteraverage <- agnes(camp.om, diss = TRUE, method = "average")

X11()
plot(clusteraverage, ask=TRUE, which.plots=2) #let's do 3 cluster, clearly

camp.cl3a <- cutree(clusteraverage, k = 3) 
cl3a.lab <- factor(camp.cl3a, labels = paste("Cluster", 1:3))

data_clustering['average_clustering'] = camp.cl3a

#
#
#let's do it now for warm
warn = fulldata[c("Country","Year","Warn")]
data_seq = data.frame(matrix(ncol = 9, nrow = 0))



for(count in countries){
  sub_row = c(count,warn[warn$Country == count,]$Warn[-1])
  data_seq = rbind(data_seq, sub_row)
}
u <- c("Country",seq(2008, 2020, by=2))
colnames(data_seq)<- u

#now let's define the parameter in order to create the sequence

warn.alpha <- seq(1,5)
warn.lab <- seq(1,5) #long label
warn.scode <- seq(1,5) #short label, use by states argument
warn.seq <- seqdef(data_seq, 2:8, alphabet = warn.alpha, states = warn.scode, labels = warn.lab)
#then, let's compute the dissimilarity matrix with the Optimal matching with a proper distance
z<- matrix(c(0,1,2,3,4,1,0,1,2,3,2,1,0,1,2,3,2,1,0,1,4,3,2,1,0), nrow = 5, ncol = 5, byrow = F)
z
warn.om <- seqdist(warn.seq, method = "OM", indel = 1, sm = z) #mi sambra buono =)

#then, let's do clustering, first ward method
clusterward <- agnes(warn.om, diss = TRUE, method = "ward")

X11()
plot(clusterward, ask=TRUE, which.plots=2) #let's do 3 cluster, clearly

camp.cl3 <- cutree(clusterward, k = 2) 
cl5.lab <- factor(camp.cl5, labels = paste("Cluster", 1:5))



data_clustering = data_seq
data_clustering['ward_clustering_on_warn_2'] = camp.cl3
