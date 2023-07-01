rm(list=ls())
###Setting the working directory as the project directory
#install.packages("sets")
#install.packages("wpa")
#setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
setwd("C:/Users/Val/OneDrive - Politecnico di Milano/travail Polimi/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

library(dplyr)
library("cluster")
library(TraMineR)
library(dplyr)
library("colorspace")
library(wpa)

fulldata<-read.table("../data/final_dataset_2007-2020.txt",header=T)

countries = unique(fulldata$Country)

mpower = fulldata[c("Country","Year","Campaigns","Help","Warn","Bans","Protect")]
mpower[109,3] = 2 #change the problematic value for greece from 1 to 2

data_seq = data.frame(matrix(ncol = 9, nrow = 0))


library(sets)


#FLAVIA: CHANGING CAMPAIGNS WITH TAXES

for(count in countries){
  row_camp = c(count,mpower[mpower$Country == count,]$Campaigns[-1])
  row_help = c(count,mpower[mpower$Country == count,]$Help[-1])
  row_warn = c(count,mpower[mpower$Country == count,]$Warn[-1])
  row_protect = c(count,mpower[mpower$Country == count,]$Protect[-1])
  n = length(row_camp)
  sub_row = c(count) #let's built the row made of tuple
  for(index in 2:n){
    value = toString(c(row_camp[index], row_help[index], row_warn[index], row_protect[index]))
    sub_row = c(sub_row, value)
  }
  data_seq = rbind(data_seq, sub_row)
}
u <- c("Country",seq(2008, 2020, by=2))
colnames(data_seq)<- u

#let's creat the list of all possible states
alphaB = c()
for(i in 2:5){
  for(j in 2:5){
    for(k in 2:5){
      for(l in 2:5){
        alphaB = c(alphaB, toString(c(i,j,k,l)))
      }
    }
  }
}

#definition of component-wize state value
alphaB_1 = c()
alphaB_2 = c()
alphaB_3 = c()
alphaB_4 = c()
for(i in 2:5){
  for(j in 2:5){
    for(k in 2:5){
      for(l in 2:5){
        alphaB_1 = c(alphaB_1, i)
        alphaB_2 = c(alphaB_2, j)
        alphaB_3 = c(alphaB_3, k)
        alphaB_4 = c(alphaB_4, l)
      }
    }
  }
}

#let's modify the color, in order to have nice representation of the states
pal <- colorRamp(c("red", "green"))#from red for the worth to green the best

color_states_01 = numeric(256)
for(i in 1:256){
  color_states_01[i] = (alphaB_1[i]+alphaB_2[i]+alphaB_3[i]+alphaB_4[i]-8)/12
}

color_states <- vector(mode="character", length=256)
for (i in 1:256){
  color_states[i] = rgb2hex(pal(color_states_01[i]))
}


all.seq = seqdef(data_seq, 2:8, alphabet = alphaB, cpal = color_states, id = countries)


#let's creat the dissimilarity matrix
z = matrix(0.,nrow = 256,ncol = 256)

for(i in 1:256){
  for(j in 1:256){
    z[i,j] = abs(alphaB_1[i] - alphaB_1[j]) + abs(alphaB_2[i] - alphaB_2[j]) +
      abs(alphaB_3[i] - alphaB_3[j]) + abs(alphaB_4[i] - alphaB_4[j]) 
  }
}

all.om <- seqdist(all.seq, method = "OM", indel = 6, sm = z) #we choose an indel of 6 because the max cost is 12 (half of it)

clusterward <- agnes(all.om, diss = TRUE, method = "ward")
clustercomp <- agnes(all.om, diss = TRUE, method = "complete")

X11()
plot(clusterward, ask=TRUE, which.plots=2)#2 or 3 cluster might work
plot(clustercomp, ask=TRUE, which.plots=2)

clust3 <- cutree(clusterward, k = 3) 
clust2 <- cutree(clusterward, k = 2) 

clustcomp4 = cutree(clustercomp, k = 4) 
clustcomp2 = cutree(clustercomp, k = 2) 


X11()
seqplot(all.seq) #to much state, non ci capice nulla

#compute the proportion of state for each week
X11()
seqdplot(all.seq)

#represantative plot
X11()
seqplot(all.seq, group = clust2, type = "r", diss=all.om)

X11()
seqplot(all.seq, group = clust3, type = "r", diss=all.om)

#full sequence plot
X11()
seqplot(all.seq, group = clust2, type = "I")

X11()
seqplot(all.seq, group = clust3, type = "I")

X11()
seqplot(all.seq, group = clustcomp4, type = "I")

X11()
seqplot(all.seq, group = clustcomp2, type = "I")


#frequency plot
quartz()
X11()
seqdplot(all.seq, group = clust2)

X11()
seqdplot(all.seq, group = clust3)
