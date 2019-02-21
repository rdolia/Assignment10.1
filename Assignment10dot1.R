library(xlsx)
#a. Read the file in Zip format and get it into R
zipF<- "C:\\Users\\ra306656\\Documents\\AirQualityUCI.zip"
outDir<-"D:\\Rachit\\Acadgild\\ASSIGNMENTS\\Assignment10.1"
unzip(zipF,exdir=outDir)
#file saved in above outDir location.
data <- read.xlsx("AirQualityUCI.xlsx",1)
View(data)
head(data)
#b. Create Univariate for all the columns.
library(tidyr)
library(lattice)
#1for column CO.GT.
histogram(~CO.GT.,data)
densityplot(~CO.GT.,data)
qqmath(~CO.GT.,data)
bwplot(~CO.GT.,data)
#2for column PT08.S1.CO

histogram(~PT08.S1.CO.,data)
densityplot(~PT08.S1.CO.,data)
qqmath(~PT08.S1.CO.,data)
bwplot(~PT08.S1.CO.,data)
# 3for column NMHC.GT.  
histogram(~NMHC.GT.,data)
densityplot(~NMHC.GT.,data)
qqmath(~NMHC.GT.,data)
bwplot(~NMHC.GT.,data)
# 4 for column NMHC.GT.  
histogram(~NMHC.GT.,data)
densityplot(~NMHC.GT.,data)
qqmath(~NMHC.GT.,data)
bwplot(~NMHC.GT.,data)
#5for column C6H6.GT.
histogram(~C6H6.GT.,data)
densityplot(~C6H6.GT.,data)
qqmath(~C6H6.GT.,data)
bwplot(~C6H6.GT.,data)
#6for column PT08.S2.NMHC. 
histogram(~PT08.S2.NMHC. ,data)
densityplot(~PT08.S2.NMHC. ,data)
qqmath(~PT08.S2.NMHC. ,data)
bwplot(~PT08.S2.NMHC. ,data)
#7for column NOx.GT.  
histogram(~NOx.GT.,data)
densityplot(~NOx.GT.,data)
qqmath(~NOx.GT. ,data)
bwplot(~NOx.GT. ,data)
#8for column PT08.S3.NOx.   
histogram(~PT08.S3.NOx. ,data)
densityplot(~PT08.S3.NOx. ,data)
qqmath(~PT08.S3.NOx. ,data)
bwplot(~PT08.S3.NOx. ,data)
#9for column NO2.GT.    
histogram(~NO2.GT. ,data)
densityplot(~NO2.GT. ,data)
qqmath(~NO2.GT. ,data)
bwplot(~NO2.GT. ,data)
#10for column PT08.S4.NO2.     
histogram(~PT08.S4.NO2.,data)
densityplot(~PT08.S4.NO2. ,data)
qqmath(~PT08.S4.NO2.,data)
bwplot(~PT08.S4.NO2. ,data)
#11for column PT08.S5.O3.           
histogram(~PT08.S5.O3.,data)
densityplot(~PT08.S5.O3.,data)
qqmath(~PT08.S5.O3.,data)
bwplot(~PT08.S5.O3.,data)
#12for column RH           
histogram(~RH,data)
densityplot(~RH,data)
qqmath(~RH,data)
bwplot(~RH,data)
#13for column AH           
histogram(~AH,data)
densityplot(~AH,data)
qqmath(~AH,data)
bwplot(~AH,data)
 
#c. Check for missing values in all columns.
#Missing values are given value -200. Will replace them with NA
library(naniar)
data <- replace_with_na_all(data, condition = ~.x == -200)
#Visualise missing values
vis_miss(data)
#remove columns NMHC.GT. as it as 90 % missing values
data <- data[-5]
#remove columns NA. and NA..1 as they have 100% missing values.
data <- data[-c(15,16)]
#removing rows 9358 till end as all are NA.
data <- data[-c(9358:9471),] 
#visualise again
vis_miss(data)

# Impute the missing values using appropriate methods
library(mice)
md.pattern(data)
str(data)
#running the mice function
data_temp <- mice(data,m=5,maxit=20,seed=500,method = 'cart')
data_complete <- complete(data_temp,1)
md.pattern(data_complete)


# Create bi-variate analysis for all relationships
#removing first 2 columns for bi- variate analysis
data1 <- data_complete[-c(1,2)]
library(corrplot)

corrplot(cor(data1))
#We can see higher correlations between attributes with darker and bigger circles

#Test relevant hypothesis for valid relation


cor.test(data1$PT08.S1.CO.,data1$PT08.S2.NMHC.)
#The p-value of the test is  2.2e-16, which is less than the significance level alpha = 0.05.
#We can conclude that PT08.S1.CO. and PT08.S2.NMHC. are significantly 
#correlated with a correlation coefficient of0.9331013.
#doing the same with blue to dark blue circles in the corrplot will determine 
#significance of correlation for each attribute pair.

# Create cross tabulations with derived variables

str(data_complete)
data_complete_derived <- data_complete

data_complete_derived$Year<- format(as.Date(data_complete_derived$Date), "%Y")
data_complete_derived$Month<- format(as.Date(data_complete_derived$Date), "%m")


#cross tabulating to show records in each month. 
table(data_complete_derived$Month)
#cross tabulating to show records in each year. 
table(data_complete_derived$Year)

#check for trends and patterns in time series
#COGT columm
data3 <-data_complete[,c(1,3)]
head(data3)
#CONVERTING COGT Values to time series
COGT <- aggregate(CO.GT. ~ Date, data3, mean)
COGT1 <-ts(COGT$CO.GT.,start = c(2004,3,10),frequency = 365)
plot(COGT1)
#We can see by the plot that this column doesnt have any seasonal pattern
#or variance hence it's stationary.

# PT08.S1.CO.columm
data4 <-data_complete[,c(1,4)]
head(data4)
#computing mean value for each day/date
PT08.S1.CO <- aggregate(PT08.S1.CO. ~ Date, data4, mean)
#converting to time series
PT08.S1.CO1 <-ts(PT08.S1.CO$PT08.S1.CO.,start = c(2004,3,10),frequency = 365)
#plotting
plot(PT08.S1.CO1)
#Again no seasonal or any other pattern for this column.

# C6H6.GT.columm
data5 <-data_complete[,c(1,5)]
head(data5)
#computing mean value for each day/date
C6H6.GT <- aggregate(C6H6.GT. ~ Date, data5, mean)
#converting to time series
C6H6.GT1 <-ts(C6H6.GT$C6H6.GT.,start = c(2004,3,10),frequency = 365)
#plotting
plot(C6H6.GT1)

#PT08.S2.NMHC. columm
data6 <-data_complete[,c(1,6)]
head(data6)
#computing mean value for each day/date
PT08.S2.NMHC <- aggregate(PT08.S2.NMHC. ~ Date, data6, mean)
#converting to time series
PT08.S2.NMHC1 <-ts(PT08.S2.NMHC$PT08.S2.NMHC.,start = c(2004,3,10),frequency = 365)
#plotting
plot(PT08.S2.NMHC1)
#Hence in this manner we can plot for each of the columns/gases
#and identify any seasonal patterns or variance.
