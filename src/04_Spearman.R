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
library(roahd)



#Import data
data<-read.table("../data/final_dataset_2007-2020.txt",
                 header=T, 
                 sep='',
                 check.names = FALSE)


data_spearman<-data[data$Year!='2007',c("Country","Year","GDP","Education",
                                        "Affordability","Cig_taxes",
                                        "Prevalence_both",
                                        "Prevalence_males",
                                        "Prevalence_females")]
#DO NOT FORGET TO LOG!
data_spearman$GDP<-log(data_spearman$GDP)
data_spearman_gdp<-data_spearman[,c("Country","Year","GDP")]
data_spearman_affordability<-data_spearman[,c("Country","Year","Affordability")]
data_spearman_cig<-data_spearman[,c("Country","Year","Cig_taxes")]
data_spearman_prevalence_b<-data_spearman[,c("Country","Year","Prevalence_both")]
data_spearman_prevalence_m<-data_spearman[,c("Country","Year","Prevalence_males")]
data_spearman_prevalence_f<-data_spearman[,c("Country","Year","Prevalence_females")]
data_spearman_education<-data_spearman[,c("Country","Year","Education")]
data_spearman_education_f<-data_spearman[,c("Country","Year","Education")]



GDP_table <- data_spearman_gdp %>%
  pivot_wider(names_from = Year, 
              values_from = GDP)

GDP_table<-t(as.data.frame(GDP_table))
colnames(GDP_table)<-GDP_table[1,]
GDP_table<-t(GDP_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_gdp <- fData(grid,GDP_table)
quartz()
plot(f_data_gdp) # what happens if I do plot(data)?
#lines(grid,m, col="black", lwd=5)

#Computing band depth and modified band depth
band_depth <- BD(Data = f_data_gdp)
modified_band_depth <- MBD(Data = f_data_gdp)
#Computing the median curve
median_curve <- median_fData(fData = f_data_gdp, type = "MBD") # still an fData object
median_curve
plot(f_data_gdp) # what happens if I do plot(data)?
#lines(median_curve)



Edu_table <- data_spearman_education %>%
  pivot_wider(names_from = Year, 
              values_from = Education)

Edu_table<-t(as.data.frame(Edu_table))
colnames(Edu_table)<-Edu_table[1,]
Edu_table<-t(Edu_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_edu <- fData(grid,Edu_table)
quartz()
plot(f_data_edu)


#Prevalence both
Prev_b_table <- data_spearman_prevalence_b %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_both)

Prev_b_table<-t(as.data.frame(Prev_b_table))
colnames(Prev_b_table)<-Prev_b_table[1,]
Prev_b_table<-t(Prev_b_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_b <- fData(grid,Prev_b_table)
quartz()
plot(f_data_prev_b)


#Prevalence males
Prev_m_table <- data_spearman_prevalence_m %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_males)

Prev_m_table<-t(as.data.frame(Prev_m_table))
colnames(Prev_m_table)<-Prev_m_table[1,]
Prev_m_table<-t(Prev_m_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_m <- fData(grid,Prev_m_table)
quartz()
plot(f_data_prev_m)


#Prevalence females
Prev_f_table <- data_spearman_prevalence_f %>%
  pivot_wider(names_from = Year, 
              values_from = Prevalence_females)

Prev_f_table<-t(as.data.frame(Prev_f_table))
colnames(Prev_f_table)<-Prev_f_table[1,]
Prev_f_table<-t(Prev_f_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)
#transforming data into fd
f_data_prev_f <- fData(grid,Prev_f_table)
quartz()
plot(f_data_prev_f)



#AFFORDABILITY - DATA FROM 2O10
affordability_table <- data_spearman_affordability %>%
  pivot_wider(names_from = Year, 
              values_from = Affordability)

affordability_table<-t(as.data.frame(affordability_table))
colnames(affordability_table)<-affordability_table[1,]
affordability_table<-t(affordability_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_aff <- fData(grid,affordability_table)
quartz()
plot(f_data_aff) # what happens if I do plot(data)?
#lines(grid,m, col="black", lwd=5)


#Cig taxes
cig_table <- data_spearman_cig %>%
  pivot_wider(names_from = Year, 
              values_from = Cig_taxes)

cig_table<-t(as.data.frame(cig_table))
colnames(cig_table)<-cig_table[1,]
cig_table<-t(cig_table[-1,])
#I need to reorganize in a table using dplyr
grid <-  seq( 2008, 2020, length.out =  7)

#transforming data into fd
f_data_cig<- fData(grid,cig_table)
quartz()
plot(f_data_cig) # what happens if I do plot(data)?
#lines(grid,m, col="black", lwd=5)


#Identify outliers

###########Assessing Spearmann correlation

#Prevalence males-prevalence females
bivariate_data <- as.mfData(list(f_data_prev_m, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# 0.5416701
cor_spearman(bivariate_data, ordering='MEI')
# 0.5427426


library(parallel)
library(pbapply)
library(progress) #for progress bar


diagnostic_permutation <- function(T20, T2) {
  B <- length(T2)
  
  # Compare real test statistic with the ones given by the permuted data
  hist(T2, xlim = range(c(T2, T20)))
  abline(v = T20, col = 3, lwd = 4)
  
  # Empirical cumulative distribution function
  plot(ecdf(T2),main="ECDF(T2)")
  abline(v = T20, col = 3, lwd = 4)
  
  # P-value
  p_val <- sum(T2 >= T20) / B
  cat("p-value: ", p_val)
}

compute_t_stat <- function(df1, df2,grid) {
  df1_f<- roahd::fData(grid,df1)
  df2_f<- roahd::fData(grid,df2)
  bivariate_data <-roahd::as.mfData(list(df1_f, df2_f))
  spearman_f<-roahd::cor_spearman(bivariate_data, ordering='MHI')
  return(abs(spearman_f))
}


# observed test statistics
df1<-Prev_m_table
df2<-Prev_f_table
T20 = compute_t_stat(df1, df2,grid)
T20

perm_wrapper = function(df1,df2,grid) {
  df_pooled = rbind(df1, df2)
  n = nrow(df_pooled)
  n1 = nrow(df1)
  permutation = sample(n)
  df_perm = df_pooled[permutation, ]
  df1_perm = df_perm[1:n1, ]
  df2_perm = df_perm[(n1 + 1):n, ]
  compute_t_stat(df1_perm, df2_perm,grid)
}

seed=2022
B=1000
# parallel
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("perm_wrapper", "df1", "df2", "grid", "compute_t_stat"))
set.seed(seed)
T2 <- pbreplicate(10000, perm_wrapper(df1, df2, grid), cl = cl)
stopCluster(cl)

par(mfrow=c(1,2))
diagnostic_permutation(T20,T2)
#p-value:  0


#Correlation Edu-GDP
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_edu))

cor_spearman(bivariate_data, ordering='MHI')
#0.6098642
cor_spearman(bivariate_data, ordering='MEI')
#0.6067687

#There is clearly a strong positive correlation between the data



#GDP-Prevalence both
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_b))
cor_spearman(bivariate_data, ordering='MHI')
# -0.2121659
cor_spearman(bivariate_data, ordering='MEI')
# -0.2137018


#Education-Prevalence both
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_b))
cor_spearman(bivariate_data, ordering='MHI')
# -0.3150654
cor_spearman(bivariate_data, ordering='MEI')
#-0.3158375

#In general there seems to be a negative correlation (especially with tertiary ed level)




#GDP-prevalence males
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_m))
cor_spearman(bivariate_data, ordering='MHI')
# -0.3399437
cor_spearman(bivariate_data, ordering='MEI')
#-0.3399437


#Edu-prevalence males
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_m))
cor_spearman(bivariate_data, ordering='MHI')
# -0.234057
cor_spearman(bivariate_data, ordering='MEI')
#-0.2327447




#GDP-prevalence females
bivariate_data <- as.mfData(list(f_data_gdp, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# 0.07167501
cor_spearman(bivariate_data, ordering='MEI')
# 0.06687308

#Interesting... there does not seem to be such a huge correlation...


#Edu-prevalence females
bivariate_data <- as.mfData(list(f_data_edu, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# -0.2667288
cor_spearman(bivariate_data, ordering='MEI')
# -0.2716218

#General comment
#GDP more correlated than education in males
#On the other hand there seem to be no correlation between females and GDP
#but only between female prev and edu...


education_females<-read.table("../data/education_female_2007-2020.txt",
                                header=T, 
                                sep='',
                                check.names = FALSE)

education_females<-education_females[education_females$Year!='2007',]
education_females <- education_females[order(education_females$Year), ]

education_females<-t(as.data.frame(education_females))
colnames(education_females)<-education_females[1,]
education_females<-t(education_females[-1,])


edu_female_f<- fData(grid,t(education_females))
quartz()
plot(edu_female_f) # what happens if I do plot(data)?
#lines(grid,m, col="black", lwd=5)


education_females[education_females$Country=='Slovakia',2]<-'Slovak Republic'
education_females[education_females$Country=='New.Zealand',2]<-'New Zealand'
education_females[education_females$Country=='Czechia',2]<-'Czech Republic'
education_females[education_females$Country=='United.Kingdom.of.Great.Britain.and.Northern.Ireland',2]<-'United Kingdom'
education_females[education_females$Country=='United.States.of.America',2]<-'United States'
education_females[education_females$Country=='Republic.of.Korea',2]<-'Korea'
education_females[education_females$Country=='Costa.Rica',2]<-'Costa Rica'

#Edu-prevalence females
bivariate_data <- as.mfData(list(edu_female_f, 
                                 f_data_prev_f))
cor_spearman(bivariate_data, ordering='MHI')
# -0.2667288
cor_spearman(bivariate_data, ordering='MEI')
# -0.2716218


#Can we find differenciated data for education for males and females?




####Now I investigate affordability and cig taxes

#Cig-prevalence males
bivariate_data <- as.mfData(list(f_data_aff, 
                                 f_data_prev_m))
cor_spearman(bivariate_data, ordering='MHI')
# 0.100648
cor_spearman(bivariate_data, ordering='MEI')
# 0.1033026
#Similar results (unshown) for males and females, slightly stronger for males



#Cig-prevalence both
bivariate_data <- as.mfData(list(f_data_cig, 
                                 f_data_prev_b))
cor_spearman(bivariate_data, ordering='MHI')
# 0.6147264
cor_spearman(bivariate_data, ordering='MEI')
# 0.6230917

#There (strange) seem to be a positive correlation... VERY STRANGE
#Similar results (unshown) for males and females, slightly stronger for males
