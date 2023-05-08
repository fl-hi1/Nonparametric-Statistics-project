rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

# Load package fda
#install.packages('fda')
library(fda)

#####Read data and extract values and years, plus reordering years in the correct order
rawdata<-read.table("../data/semiprocessed_datasets/smoking_prevalence_b.txt",header=T)
#rawdata<-read.table("../data/semiprocessed_datasets/smoking_prevalence_f.txt",header=T)
#rawdata<-read.table("../data/semiprocessed_datasets/smoking_prevalence_m.txt",header=T)

data<-rawdata[c(2:7,1),2:37]
head(data)
dim(data)
years<-rawdata[c(2:7,1),1]
head(years)

#Give appropriate name to the dataset rows  
rownames(data)<-years

#Plot of the time points, points indicate the values
matplot(years,data,  
        type = "l",
        lty="dashed",
        xlab="years",
        ylab="Age-standardized percent",
        main="Age-standardized smoking prevalence")

countryrange<-1:36

for (i in countryrange){
  points(years, data[,i], pch = 20) # add blue points to second line
}

########################################################################
########################################################################
##############                 SMOOTHING                   #############
########################################################################
########################################################################

##install.packages('mgcv')
library(mgcv)
##install.packages('splines')
library(splines)


# Create bspline basis imposing the passage by the points
myrange<-c(2000,2020) 
myknots = c(2000,2010, 2015, 2018, 2019, 2020)
myorder=3
nbasis = myorder + length(myknots) -2

basis <- create.bspline.basis(rangeval=myrange,
                              breaks=myknots,
                              nbasis=nbasis, 
                              norder=myorder)
#Plot basis
quartz()
plot(basis)

# Evaluate bspline basis on abscissa
basismat <- eval.basis(years[1:length(years)], basis)
#Dimensional check - c(number of timepoints,  number of basis)
c(dim(data)[1], nbasis) 


# Estimate coefficients 
est_coef = lsfit(basismat, data, intercept=FALSE)$coef #help(lsfit) ls fit
est_coef
dim(est_coef)

#Assess smoothed values (they are of course the same of the real ones)
smoothed <- basismat %*% est_coef
head(smoothed)
dim(smoothed)

#Xsp0<-data.frame(Xsp0)
row.names(smoothed)<-years

#Plot smoothed function
matplot(years,smoothed, 
        type="l",
        xlab="years",
        ylab="Age-standardized percent estimate",
        main="Age-standardized estimate of smoking prevalence")

for (i in countryrange){
  points(years[1:length(years)], data[1:length(years),i], pch = 20) # add blue points to second line
}
#Double visual check that the smoothing points exactly superimpose the original ones
for (i in countryrange){
  points(years[1:length(years)], smoothed[1:length(years),i], 
         col='green',
         pch = 1) 
}
#Triple numerical check: smoothed data points - original data points
#are null (at the net of some approximation error)
smoothed-data

# Predict values for new years
new_years <- c(2007, 2008, 2012, 2014, 2016)
new_years_basismat <- eval.basis(new_years, basis)
predicted_values <- new_years_basismat %*% est_coef

# Assign correct column name and print predicted values
row.names(predicted_values)<-new_years
predicted_values

#Plot smoothed function
matplot(years,smoothed,
        type="l",
        xlab="years",
        ylab="Age-standardized percent estimate",
        main="Age-standardized estimate of smoking prevalence")

for (i in countryrange){
  points(years[1:length(years)], data[1:length(years),i], 
         pch = 20) # add blue points to second line
}
for (i in countryrange){
  points(new_years[1:length(new_years)], predicted_values[1:length(new_years),i], 
         pch = 20,col='red') # add blue points to second line
}

predicted_values

#MERGE DATA INTO A SINGLE ONE
chosen_years <- c(2007, 2008, 2010, 2012, 2014, 2016,2018,2020)
data<-data.frame(data)
predicted_values<-data.frame(predicted_values)
colnames(predicted_values)<-colnames(data)

final_dataset<-rbind(data[rownames(data)%in% chosen_years,],
                 predicted_values[rownames(predicted_values)%in% chosen_years,])

final_dataset <- final_dataset[order(rownames(final_dataset)),]


#SAVING FILES
#write.table(final_dataset, 
#            "../datasmoking_prevalence_males_2007-2020.txt", 
#            sep = "\t")
#
#write.table(final_dataset, 
#            "../data/smoking_prevalence_females_2007-2020.txt", 
#            sep = "\t")
#
#write.table(final_dataset, 
#            "../data/smoking_prevalence_both_2007-2020.txt", 
#            sep = "\t")
#

