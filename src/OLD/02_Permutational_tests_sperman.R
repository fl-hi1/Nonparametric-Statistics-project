rm(list=ls())
###Setting the working directory as the project directory
setwd("C:/Users/Val/OneDrive/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#load the datas

females<-read.table("../data/smoking_prevalence_females_2007-2020.txt",header=T)
males<-read.table("../data/smoking_prevalence_males_2007-2020.txt",header=T)
df<-read.table("../data/smoking_prevalence_both_2007-2020.txt",header=T)

countries<-read.table("../data/OECD_countries_income_level",header=T)$Country
education <- read.table("../data/education_both_2007-2020.txt", header = T)
GDP <- read.table("../data/GDP_2007-2020.txt", header = T)

#drop the columns year of GDP
GDP <- GDP[,-1]

#fixing the education dataset
data_2016 = education[1,]
data_2016
data_2018 = education[7,]
data_2020 = education[8,]

#education = education[-c(1,7,8),]

education <- rbind(education,data_2016)
education <- rbind(education,data_2018)
education <- rbind(education,data_2020)

education = education[,-1]

#order the dataset to get the countries in the same position
df <- df[ , order(names(df))]
education <- education[ , order(names(education))]
GDP <- GDP[ , order(names(GDP))]

#check the matching countries
names(GDP)
names(education)
names(df)

int1 = intersect(names(GDP), names(df))
int2 = intersect(names(df), names(education))
int2
library(progress)

#Beginning of the correlation analysis year by year

df_2007<-df[1,]
df_2008<-df[2,]
df_2010<-df[3,]
df_2012<-df[4,]
df_2014<-df[5,]
df_2016<-df[6,]
df_2018<-df[7,]
df_2020<-df[8,]

GDP_2007<-GDP[1,]
GDP_2008<-GDP[2,]
GDP_2010<-GDP[3,]
GDP_2012<-GDP[4,]
GDP_2014<-GDP[5,]
GDP_2016<-GDP[6,]
GDP_2018<-GDP[7,]
GDP_2020<-GDP[8,]

education_2007<-education[1,]
education_2008<-education[2,]
education_2010<-education[3,]
education_2012<-education[4,]
education_2014<-education[5,]
education_2016<-education[6,]
education_2018<-education[7,]
education_2020<-education[8,]

### sperman test try, let's start with GDP and smokers
int1 = intersect(names(GDP), names(df))
int2 = intersect(names(df), names(education))

names.df1 <- names(df)[names(df) %in% int1]
names.GDP1 <- names(GDP)[names(GDP) %in% int1]

dim(df_2007[,names.df1])
dim(GDP_2007[,names.GDP1])

names(df_2007[,names.df1]) == names(GDP_2007[,names.GDP1]) #Va bene

#Let's do it yearly
cor.test(unname(unlist(df_2007[,names.df1])), unname(unlist(GDP_2007[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2008[,names.df1])), unname(unlist(GDP_2008[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2010[,names.df1])), unname(unlist(GDP_2010[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2012[,names.df1])), unname(unlist(GDP_2012[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2014[,names.df1])), unname(unlist(GDP_2014[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2016[,names.df1])), unname(unlist(GDP_2016[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2018[,names.df1])), unname(unlist(GDP_2018[,names.GDP1])), method = "spearman")
cor.test(unname(unlist(df_2020[,names.df1])), unname(unlist(GDP_2020[,names.GDP1])), method = "spearman")

#all together, let's see
cor.test(unname(unlist(df[,names.df1])), unname(unlist(GDP[,names.GDP1])), method = "spearman")


#let's compare smoker's and education

int2 = intersect(names(df), names(education))

names.df2 <- names(df)[names(df) %in% int2]
names.edu2 <- names(education)[names(education) %in% int2]

dim(df_2007[,names.df2])
dim(education_2007[,names.edu2])

names(df_2007[,names.df2]) == names(education_2007[,names.edu2]) #Va bene anche

cor.test(unname(unlist(df_2007[,names.df2])), unname(unlist(education_2007[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2008[,names.df2])), unname(unlist(education_2008[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2010[,names.df2])), unname(unlist(education_2010[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2012[,names.df2])), unname(unlist(education_2012[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2014[,names.df2])), unname(unlist(education_2014[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2016[,names.df2])), unname(unlist(education_2016[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2018[,names.df2])), unname(unlist(education_2018[,names.edu2])), method = "spearman")
cor.test(unname(unlist(df_2020[,names.df2])), unname(unlist(education_2020[,names.edu2])), method = "spearman")

#all together, let's see
cor.test(unname(unlist(df[,names.df2])), unname(unlist(education[,names.edu2])), method = "spearman")

#compare GDP and education
int3 = intersect(names(GDP), names(education))

names.GDP3 <- names(GDP)[names(GDP) %in% int3]
names.edu3 <- names(education)[names(education) %in% int3]

cor.test(unname(unlist(GDP[,names.GDP3])), unname(unlist(education[,names.edu3])), method = "spearman")
