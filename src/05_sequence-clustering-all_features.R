rm(list=ls())
###Setting the working directory as the project directory
#install.packages("sets")
#install.packages("wpa")
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

mpower = fulldata[c("Country","Year","Campaigns","Help","Warn","Bans","Protect","Taxes")]
mpower[109,3] = 2 #change the problematic value for greece from 1 to 2

data_seq = data.frame(matrix(ncol = 9, nrow = 0))


library(sets)

for(count in countries){
  row_camp = c(count,mpower[mpower$Country == count,]$Campaigns[-1])
  row_help = c(count,mpower[mpower$Country == count,]$Help[-1])
  row_warn = c(count,mpower[mpower$Country == count,]$Warn[-1])
  row_ban = c(count,mpower[mpower$Country == count,]$Bans[-1])
  row_protect = c(count,mpower[mpower$Country == count,]$Protect[-1])
  row_tax = c(count,mpower[mpower$Country == count,]$Taxes[-1])
  n = length(row_camp)
  sub_row = c(count) #let's built the row made of tuple
  for(index in 2:n){
    value = toString(c(row_camp[index], row_help[index], row_warn[index], row_ban[index], row_protect[index], row_tax[index]))
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
        for(m in 2:5){
          for(n in 2:5){
            alphaB = c(alphaB, toString(c(i,j,k,l,m,n)))
          }
        }
      }
    }
  }
}

#definition of component-wize state value
alphaB_1 = c()
alphaB_2 = c()
alphaB_3 = c()
alphaB_4 = c()
alphaB_5 = c()
alphaB_6 = c()
for(i in 2:5){
  for(j in 2:5){
    for(k in 2:5){
      for(l in 2:5){
        for(m in 2:5){
          for(n in 2:5){
        alphaB_1 = c(alphaB_1, i)
        alphaB_2 = c(alphaB_2, j)
        alphaB_3 = c(alphaB_3, k)
        alphaB_4 = c(alphaB_4, l)
        alphaB_5 = c(alphaB_5, m)
        alphaB_6 = c(alphaB_6, n)
          }
        }
      }
    }
  }
}

#let's modify the color, in order to have nice representation of the states
pal <- colorRamp(c("red", "green"))#from red for the worth to green the best

color_states_01 = numeric(4096)
for(i in 1:4096){
  color_states_01[i] = (alphaB_1[i]+alphaB_2[i]+alphaB_3[i]+alphaB_4[i]+alphaB_5[i]+alphaB_6[i]-12)/18
}

color_states <- vector(mode="character", length=4096)
for (i in 1:4096){
  color_states[i] = rgb2hex(pal(color_states_01[i]))
}


all.seq = seqdef(data_seq, 2:8, alphabet = alphaB, cpal = color_states, id = countries)


#let's creat the dissimilarity matrix
z = matrix(0.,nrow = 4096,ncol = 4096)

for(i in 1:4096){
  for(j in 1:4096){
    z[i,j] = abs(alphaB_1[i] - alphaB_1[j]) + abs(alphaB_2[i] - alphaB_2[j]) +
    abs(alphaB_3[i] - alphaB_3[j]) + abs(alphaB_4[i] - alphaB_4[j]) +
    abs(alphaB_5[i] - alphaB_5[j]) + abs(alphaB_6[i] - alphaB_6[j])
  }
}

all.om <- seqdist(all.seq, method = "OM", indel = 9, sm = z) #we choose an indel of 6 because the max cost is 12 (half of it)

clusterward <- agnes(all.om, diss = TRUE, method = "ward")
clustercomp <- agnes(all.om, diss = TRUE, method = "complete")
clusterav <- agnes(all.om, diss = TRUE, method = "average")
clustersin <- agnes(all.om, diss = TRUE, method = "single")

X11()
plot(clusterward, ask=TRUE, which.plots=2)#2 or 3 cluster might work
plot(clustercomp, ask=TRUE, which.plots=2)#2 or 3 cluster might work
plot(clusterav, ask=TRUE, which.plots=2)# is a shit
plot(clustersin, ask=TRUE, which.plots=2)#is the shit, the recurrent probleme of single linkage happened

clust3 <- cutree(clusterward, k = 3) 
clust2 <- cutree(clusterward, k = 2) 

clustcomp3 = cutree(clustercomp, k = 3) 
clustcomp2 = cutree(clustercomp, k = 2) 


"""X11()
seqplot(all.seq) #to much state, non ci capice nulla

#compute the proportion of state for each week
X11()
seqdplot(all.seq)"""

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
seqplot(all.seq, group = clustcomp3, type = "I")

X11()
seqplot(all.seq, group = clustcomp2, type = "I")


#frequency plot
X11()
seqdplot(all.seq, group = clust2)

X11()
seqdplot(all.seq, group = clust3)

#let's do it with the z^2 distance matrix
z2 = matrix(0.,nrow = 4096,ncol = 4096)

for(i in 1:4096){
  for(j in 1:4096){
    z2[j,i] = sqrt((alphaB_1[i] - alphaB_1[j])^2 + (alphaB_2[i] - alphaB_2[j])^2 +
    (alphaB_3[i] - alphaB_3[j])^2 + (alphaB_4[i] - alphaB_4[j])^2 +
    (alphaB_5[i] - alphaB_5[j])^2 + (alphaB_6[i] - alphaB_6[j])^2 )
  }
}

all.om2 <- seqdist(all.seq, method = "OM", indel = 4, sm = z2) #we choose an indel of 6 because the max cost is 12 (half of it)

clusterward <- agnes(all.om2, diss = TRUE, method = "ward")
clustercomp <- agnes(all.om2, diss = TRUE, method = "complete")
clusterav <- agnes(all.om2, diss = TRUE, method = "average")
clustersin <- agnes(all.om2, diss = TRUE, method = "single")

X11()
plot(clusterward, ask=TRUE, which.plots=2)#2 or 3 or 5 cluster might work
plot(clustercomp, ask=TRUE, which.plots=2)#3 cluster might work
plot(clusterav, ask=TRUE, which.plots=2)# 3 or 4
plot(clustersin, ask=TRUE, which.plots=2)#is the shit, the recurrent probleme of single linkage happened


clust5.2 <- cutree(clusterward, k = 5)
clust3.3 <- cutree(clusterward, k = 3) 
clust2.2 <- cutree(clusterward, k = 2)

X11()
seqplot(all.seq, group = clust5.2, type = "I")
X11()
seqplot(all.seq, group = clust3.2, type = "I")
X11()
seqplot(all.seq, group = clust2.2, type = "I")

#we select the cluster composed of 5 elements, and add it to the data_seq
country_clust = data.frame(Country = countries, Cluster_5 = clust5.2)

#add the data clustering to the full dataset
clust_add = c()

for(i in 1:length(clust5.2)){
  clust_add = c(clust_add, rep(clust5.2[i],8))
}

fulldata["clust_5"] = clust_add

write.table(x = fulldata, file = "../data/final_dataset_2007-2020.txt" )
