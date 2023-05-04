rm(list=ls())
#Setting the working directory
setwd("~/Desktop/_potential_datasets_Nonpara/HFA Dataset")

#Importing the first dataset
df <- read.csv("~/Desktop/_potential_datasets_Nonpara/HFA Dataset/HFA Data (pivoted) part 2.csv", 
               stringsAsFactors=TRUE)
head(df)
nrow(df)
ncol(df)

df2 <- read.csv("~/Desktop/_potential_datasets_Nonpara/HFA Dataset/HFA Data (pivoted) part 3.csv", 
               stringsAsFactors=TRUE)
head(df2)
nrow(df2)
ncol(df2)
df2

# Df1 has 10 rows more, which correspond to years 1960-1969, which I need to cut if I 
# want a more homogeneous dataset

df1_clean<-df[,-c(5:14)]
summary(df1_clean)
df1<-df1_clean


#I remove the variables which do not match between df 1 and 2
# before joining them

#I remove the variable "place residence" from df1
df1<-df1[,-c(3)]
summary(df1)
colnames(df1)

#I remove the variable "YES/NO" from df2
df2<-df2[,-c(3)]
summary(df2)
colnames(df2)


#Now the number of columns match and I can join the two df
ncol(df2)
ncol(df1)

df3<-rbind(df1,df2)
head(df3)

library(readxl)
#Now from df 3 I filter the rows I am interested in
#To select them, I import the metadata file
metadata_row <- read_excel("HFA Metadata.xlsx", 
                       sheet = "Labels")
#And only keep the relevant columns and rename
metadata_row<-metadata_row[,c(1,2)]
head(metadata_row)
names<-metadata_row[1,]
names
colnames(metadata_row)<-names
rm(names)
metadata<-metadata_row[c(-1),]
head(metadata)

#library(data.table)
library(dplyr)
#selectedRows <- metadata[ , metadata$Label %like% "cigarette"]

#Looking for the word "cigarettes" or "cigars"
cigar<-metadata %>% filter(grepl("cigar", Label))
cigar
cig<-cigar[1:2,]
cig
#I get HFA_625 and HFA_626

#Looking for the word "smoke" o similar
smok<-metadata %>% filter(grepl("smok", Label))
smok
#I get HFA_625 and HFA_626

tob<-metadata %>% filter(grepl("toba", Label))
tob

#I get the codes to extract from df3
codes<-c(cig$Code,smok$Code)
codes


#And I select the correct rows from the merged dataset
df_clean<-df3[df3$Measure.code %in% codes, ]
summary(df_clean)
colnames(df_clean)
nrow(df_clean)

df_clean

#Counting NaNs
colSums(is.na(df_clean))/nrow(df_clean)

nrow(df_clean)

df_ita<-df_clean[df_clean$COUNTRY_REGION=="ITA",]

df_fra<-df_clean[df_clean$COUNTRY_REGION=="FRA",]


nrow(df_ita)
