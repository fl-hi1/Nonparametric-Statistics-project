library(roahd)

rm(list=ls())
###Setting the working directory as the project directory
setwd("~/Documents/GitHub/Nonparametric-Statistics-project/src")
inputpath = "../data"
outputpath = "../data"

#rawdata<-read.delim("../data/raw_datasets/Education_OECD_share_population_25-64_withtertiary_level.csv",
#                    sep = ",", quote="", header=T)

rawdata<-read.delim("../data/raw_datasets/HDR21-22_Composite_indices.csv",
                    sep = ",", quote="", header=T)



#Extracting countries from OECD database
countries<-read.table("../data/OECD_countries_income_level", header=T)$Country

#Adding manually the countries with slightly different names
countries<-c(countries,
             "Korea (Republic of)",
             "Czechia", 
             "Republic of Korea",
             "Korea",
             "Slovak Republic",
             "Slovakia",
             "Türkiye",
             "United States of America",
             "United Kingdom of Great Britain and Northern Ireland",
             "Costa Rica",
             "Japan")
#


hdi_raw<-rawdata[rawdata$country %in% countries,]


hdi<-hdi_raw[,c(2,23,24,25,26,27,28,29,30,31,32,33,34,35,36)]

grid <- seq(2007, 2020, by=1)



#Create functional data
f_data <- fData(grid, hdi[,-c(1)])

plot(f_data,xlab="Year", ylab="HDI")

# MEDIAN - type can be MBD, BD etc
median_curve <- median_fData(fData = f_data, type = "MBD") # still an fData object
#median_curve_manual <- univariate_fdata[which.max(modified_band_depth),] # still an fData object


median_curve$values

# PLOT FD WITH MEDIAN
plot(f_data)
lines(grid,median_curve$values,lwd=2.0) # superimpose median



# Functional Box Plot
# MAGNITUDE/AMPLITUDE OUTLIER 

# MBD depth
fb_plot_MBD = fbplot(f_data, main="Magnitude outliers", Depths = "MBD")  # Functional Box Plot
fb_plot_MBD$ID_outliers
hdi$country[fb_plot_MBD$ID_outliers]
#Amplitude outliers according to MBD
#"Colombia" "Mexico"   "Turkey"  
# with lower amplitude


#But we are more interested in the order of curves, to stratify based on
# increasing values of curves
#MHI depth

fb_plot_MHI = fbplot(f_data, main="Magnitude outliers", Depths = "MHI")  # Functional Box Plot
fb_plot_MHI$ID_outliers
hdi$country[fb_plot_MHI$ID_outliers]
#Amplitude outliers according to MHI are
#"Chile"      "Colombia"   "Costa Rica" "Hungary"    "Mexico"    
#"Turkey"

#MEI depth
fb_plot_MEI = fbplot(f_data, main="Magnitude outliers", Depths = "MEI")  # Functional Box Plot
fb_plot_MEI$ID_outliers
hdi$country[fb_plot_MEI$ID_outliers]
#No outliers according to MEI

hdi_index_yearselected<-hdi_raw[,c(2,23,24,26,28,30,32,34,35,36)]


# Manual 
MHI_f <- MHI(f_data)
hist(MHI_f)


#20% is based on the number of outliers in MHI, 80% is by eye
#MBD_quantiles<-quantile(MBD_f, probs=c(0.10,0.75))
MHI_quantiles<-quantile(MHI_f, probs=c(0.10,0.5,0.9))
#MHI_quantiles2<-quantile(MHI_f, probs=c(0.10,25,0.5,0.75,0.9))
MHI_quantiles2<-MHI_quantiles

hdi_raw$MHI<-0
hdi_raw$MHI[MHI_f<=MHI_quantiles2[1]]<-1
hdi_raw$MHI[MHI_f>MHI_quantiles2[1]]<-2
hdi_raw$MHI[MHI_f>MHI_quantiles2[2]]<-3
hdi_raw$MHI[MHI_f>MHI_quantiles2[3]]<-4
#hdi_raw$MHI[MHI_f>MHI_quantiles2[4]]<-5
#hdi_raw$MHI[MHI_f>MHI_quantiles2[5]]<-6



hdi_raw$MHI


#Plot
plot(f_data, col=hdi_raw$MHI)


#We will add this MHI-based clustering structure to
#see if we can reach a good interpretabilotu



hdi_index_yearselected<-hdi_raw[,c(2,23,24,26,28,30,32,34,36)]
selected_years<-c("2007","2008",
                  "2010","2012",
                  "2014","2016",
                  "2018","2020")
colnames(hdi_index_yearselected)[2:9]<-c("2007","2008",
                                          "2010","2012",
                                          "2014","2016",
                                          "2018","2020")
hdi_index<-cbind(hdi_index_yearselected,
                 HDI_MHI=MHI_f)

hdi_index_final<-cbind(hdi_index,
                       HDI_MHI_clustering=hdi_raw$MHI)


#Saving the updated table
write.table(hdi_index_final, 
            "../data/HDI_2007-2020.txt", 
            sep = "\t")

#We will try to use the dataset







