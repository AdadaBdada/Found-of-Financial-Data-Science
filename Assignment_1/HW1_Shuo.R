
#######################################################################################################
## load data Combine five dataset as one dataset#######################################################
#######################################################################################################

setwd("/Users/jinshuo/SIT/FE582/HW1_F18")
# install.packages("gdata")
# gdata: Various R programming tools for data manipulation 
library('gdata')
library('plyr')

data_Bronx <- read.xls("rollingsales_bronx.xls",skip=4,header=TRUE) # 2
data_Brooklyn <- read.xls("rollingsales_brooklyn.xls",skip=4,header=TRUE) # 3
data_Manhattan <-read.xls("rollingsales_manhattan.xls",skip=4,header=TRUE) # 1
data_Queens <- read.xls("rollingsales_queens.xls",skip=4,header=TRUE) # 4
data_Staten_Island <- read.xls("rollingsales_statenisland.xls",skip=4,header=TRUE) # 5


#######################################################################################################
## 1.Manhattan Analysis #################################################################################
#######################################################################################################

names(data_Manhattan) <- tolower(names(data_Manhattan))
# Covert the column to a factor
data_Manhattan$borough <- factor(data_Manhattan$borough)
data_Manhattan$zip.code <- factor(data_Manhattan$zip.code)
str(data_Manhattan)

# gsub: Pattern Matching and Replacement
data_Manhattan$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", data_Manhattan$sale.price))
data_Manhattan$sale.price.n
count(is.na(data_Manhattan$sale.price.n))

## clean/format the data with regular expressions
names(data_Manhattan)
data_Manhattan$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Manhattan$gross.square.feet))
data_Manhattan$land.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Manhattan$land.square.feet))
data_Manhattan$sale.date <- as.Date(data_Manhattan$sale.date)
data_Manhattan$sale.year <- as.Date(data_Manhattan$sale.date,"%Y")
data_Manhattan$year.built <- as.numeric(as.character(data_Manhattan$year.built))

## do a bit of exploration to make sure the data is ok
attach(data_Manhattan)
names(data_Manhattan)
# Check the price boxplot we found the borough 1 have more higher price than the other four borough

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
gross.sqft[sale.price.n==0]

detach(data_Manhattan)

## keep only the actual sales
Manhattan.sale <- data_Manhattan[data_Manhattan$sale.price.n!=0,]
plot(Manhattan.sale$gross.sqft,Manhattan.sale$sale.price.n)
plot(log(Manhattan.sale$gross.sqft),log(Manhattan.sale$sale.price.n))
names(Manhattan.sale)

## for now, let's look at 1, 2, and 3 family homes, coop, condos 
nrow(Manhattan.sale)
Manhattan.homes <- Manhattan.sale[which(grepl("FAMILY|COOP|CONDOS",Manhattan.sale$building.class.category)),]
nrow(Manhattan.homes)
plot(log(Manhattan.homes$gross.sqft),log(Manhattan.homes$sale.price.n))

# remove the outlier
Manhattan <- subset(Manhattan.homes, Manhattan.homes$sale.price.n > 10000 & Manhattan.homes$gross.sqft < 20000 & Manhattan.homes$sale.price.n < 90000000)
nrow(Manhattan)
summary(Manhattan$sale.price.n)
plot(Manhattan$gross.sqft,Manhattan$sale.price.n)


plot(summaryBy(sale.price.n~building.class.category,data=Manhattan, FUN = mean))
ggplot(Manhattan, aes(x= building.class.category, y = sale.price.n,fill = building.class.category)) + geom_boxplot()  + coord_flip()

#######################################################################################################
## 2.Bronx Analysis ###################################################################################
#######################################################################################################

names(data_Bronx) <- tolower(names(data_Bronx))
# Covert the column to a factor
data_Bronx$borough <- factor(data_Bronx$borough)
data_Bronx$zip.code <- factor(data_Bronx$zip.code)
str(data_Bronx)

# gsub: Pattern Matching and Replacement
data_Bronx$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", data_Bronx$sale.price))
data_Bronx$sale.price.n
count(is.na(data_Bronx$sale.price.n))

## clean/format the data with regular expressions
names(data_Bronx)
data_Bronx$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Bronx$gross.square.feet))
data_Bronx$land.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Bronx$land.square.feet))
data_Bronx$sale.date <- as.Date(data_Bronx$sale.date)
data_Bronx$sale.year <- as.Date(data_Bronx$sale.date,"%Y")
data_Bronx$year.built <- as.numeric(as.character(data_Bronx$year.built))

## do a bit of exploration to make sure the data is ok
attach(data_Bronx)
names(data_Bronx)
# Check the price boxplot we found the borough 1 have more higher price than the other four borough

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
gross.sqft[sale.price.n==0]

detach(data_Bronx)

## keep only the actual sales
Bronx.sale <- data_Bronx[data_Bronx$sale.price.n!=0,]
plot(Bronx.sale$gross.sqft,Bronx.sale$sale.price.n)
plot(log(Bronx.sale$gross.sqft),log(Bronx.sale$sale.price.n))
names(Bronx.sale)

## for now, let's look at 1, 2, and 3 family homes, coop, condos 
nrow(Bronx.sale)
Bronx.homes <- Bronx.sale[which(grepl("FAMILY|COOP|CONDOS",Bronx.sale$building.class.category)),]
nrow(Bronx.homes)
plot(log(Bronx.homes$gross.sqft),log(Bronx.homes$sale.price.n))

# remove the outlier
Bronx <- subset(Bronx.homes, Bronx.homes$sale.price.n > 100 & Bronx.homes$gross.sqft < 10000 & Bronx.homes$sale.price.n < 1500000)
nrow(Bronx)
summary(Bronx$sale.price.n)
plot(Bronx$gross.sqft,Bronx$sale.price.n)


summaryBy(sale.price.n~building.class.category,data=Bronx, FUN = mean)
ggplot(Bronx, aes(x= building.class.category, y = sale.price.n,fill = building.class.category)) + geom_boxplot()  + coord_flip()

#######################################################################################################
## 3.Brooklyn Analysis ################################################################################
#######################################################################################################

names(data_Brooklyn) <- tolower(names(data_Brooklyn))
# Covert the column to a factor
data_Brooklyn$borough <- factor(data_Brooklyn$borough)
data_Brooklyn$zip.code <- factor(data_Brooklyn$zip.code)
str(data_Brooklyn)

# gsub: Pattern Matching and Replacement
data_Brooklyn$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", data_Brooklyn$sale.price))
data_Brooklyn$sale.price.n
count(is.na(data_Brooklyn$sale.price.n))

## clean/format the data with regular expressions
names(data_Brooklyn)
data_Brooklyn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Brooklyn$gross.square.feet))
data_Brooklyn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Brooklyn$land.square.feet))
data_Brooklyn$sale.date <- as.Date(data_Brooklyn$sale.date)
data_Brooklyn$sale.year <- as.Date(data_Brooklyn$sale.date,"%Y")
data_Brooklyn$year.built <- as.numeric(as.character(data_Brooklyn$year.built))

## do a bit of exploration to make sure the data is ok
attach(data_Brooklyn)
names(data_Brooklyn)
# Check the price boxplot we found the borough 1 have more higher price than the other four borough

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
gross.sqft[sale.price.n==0]

detach(data_Brooklyn)

## keep only the actual sales
Brooklyn.sale <- data_Brooklyn[data_Brooklyn$sale.price.n!=0,]
plot(Brooklyn.sale$gross.sqft,Brooklyn.sale$sale.price.n)
plot(log(Brooklyn.sale$gross.sqft),log(Brooklyn.sale$sale.price.n))
names(Brooklyn.sale)

## for now, let's look at 1, 2, and 3 family homes, coop, condos 
nrow(Brooklyn.sale)
Brooklyn.homes <- Brooklyn.sale[which(grepl("FAMILY|COOP|CONDOS",Brooklyn.sale$building.class.category)),]
nrow(Brooklyn.homes)
plot(log(Brooklyn.homes$gross.sqft),log(Brooklyn.homes$sale.price.n))

# remove the outlier
Brooklyn <- subset(Brooklyn.homes, Brooklyn.homes$sale.price.n > 100 & Brooklyn.homes$gross.sqft < 12000 & Brooklyn.homes$sale.price.n < 8000000 )
nrow(Brooklyn)
summary(Brooklyn$sale.price.n)
plot(Brooklyn$gross.sqft,Brooklyn$sale.price.n)


summaryBy(sale.price.n~building.class.category,data=Brooklyn, FUN = mean)
ggplot(Brooklyn, aes(x= building.class.category, y = sale.price.n,fill = building.class.category)) + geom_boxplot()  + coord_flip()


#######################################################################################################
## 4.Queens Analysis ##################################################################################
#######################################################################################################

names(data_Queens) <- tolower(names(data_Queens))
# Covert the column to a factor
data_Queens$borough <- factor(data_Queens$borough)
data_Queens$zip.code <- factor(data_Queens$zip.code)
str(data_Queens)

# gsub: Pattern Matching and Replacement
data_Queens$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", data_Queens$sale.price))
data_Queens$sale.price.n
count(is.na(data_Queens$sale.price.n))

## clean/format the data with regular expressions
names(data_Queens)
data_Queens$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Queens$gross.square.feet))
data_Queens$land.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Queens$land.square.feet))
data_Queens$sale.date <- as.Date(data_Queens$sale.date)
data_Queens$sale.year <- as.Date(data_Queens$sale.date,"%Y")
data_Queens$year.built <- as.numeric(as.character(data_Queens$year.built))

## do a bit of exploration to make sure the data is ok
attach(data_Queens)
names(data_Queens)
# Check the price boxplot we found the borough 1 have more higher price than the other four borough

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
gross.sqft[sale.price.n==0]

detach(data_Queens)

## keep only the actual sales
Queens.sale <- data_Queens[data_Queens$sale.price.n!=0,]
plot(Queens.sale$gross.sqft,Queens.sale$sale.price.n)
plot(log(Queens.sale$gross.sqft),log(Queens.sale$sale.price.n))
names(Queens.sale)

## for now, let's look at 1, 2, and 3 family homes, coop, condos 
nrow(Queens.sale)
Queens.homes <- Queens.sale[which(grepl("FAMILY|COOP|CONDOS",Queens.sale$building.class.category)),]
nrow(Queens.homes)
plot(log(Queens.homes$gross.sqft),log(Queens.homes$sale.price.n))

# remove the outlier
Queens <- subset(Queens.homes, Queens.homes$sale.price.n > 100 & Queens.homes$gross.sqft > 100 & Queens.homes$gross.sqft < 8000 )
nrow(Queens)
summary(Queens$sale.price.n)
plot(Queens$gross.sqft,Queens$sale.price.n)


summaryBy(sale.price.n~building.class.category,data=Queens, FUN = mean)
ggplot(Queens, aes(x= building.class.category, y = sale.price.n,fill = building.class.category)) + geom_boxplot()  + coord_flip()


#######################################################################################################
## 5.Staten_Island Analysis ###########################################################################
#######################################################################################################

names(data_Staten_Island) <- tolower(names(data_Staten_Island))
# Covert the column to a factor
data_Staten_Island$borough <- factor(data_Staten_Island$borough)
data_Staten_Island$zip.code <- factor(data_Staten_Island$zip.code)
str(data_Staten_Island)

# gsub: Pattern Matching and Replacement
data_Staten_Island$sale.price.n <- as.numeric(gsub("[^[:digit:]]","", data_Staten_Island$sale.price))
data_Staten_Island$sale.price.n
count(is.na(data_Staten_Island$sale.price.n))

## clean/format the data with regular expressions
names(data_Staten_Island)
data_Staten_Island$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Staten_Island$gross.square.feet))
data_Staten_Island$land.sqft <- as.numeric(gsub("[^[:digit:]]","",data_Staten_Island$land.square.feet))
data_Staten_Island$sale.date <- as.Date(data_Staten_Island$sale.date)
data_Staten_Island$sale.year <- as.Date(data_Staten_Island$sale.date,"%Y")
data_Staten_Island$year.built <- as.numeric(as.character(data_Staten_Island$year.built))

## do a bit of exploration to make sure the data is ok
attach(data_Staten_Island)
names(data_Staten_Island)
# Check the price boxplot we found the borough 1 have more higher price than the other four borough

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
gross.sqft[sale.price.n==0]

detach(data_Staten_Island)

## keep only the actual sales
Staten_Island.sale <- data_Staten_Island[data_Staten_Island$sale.price.n!=0,]
plot(Staten_Island.sale$gross.sqft,Staten_Island.sale$sale.price.n)
plot(log(Staten_Island.sale$gross.sqft),log(Staten_Island.sale$sale.price.n))
names(Staten_Island.sale)

## for now, let's look at 1, 2, and 3 family homes, coop, condos 
nrow(Staten_Island.sale)
Staten_Island.homes <- Staten_Island.sale[which(grepl("FAMILY|COOP|CONDOS",Staten_Island.sale$building.class.category)),]
nrow(Staten_Island.homes)
plot(log(Staten_Island.homes$gross.sqft),log(Staten_Island.homes$sale.price.n))

# remove the outlier
Staten_Island <- subset(Staten_Island.homes, Staten_Island.homes$sale.price.n > 100 & Staten_Island.homes$gross.sqft < 8000  & Staten_Island.homes$sale.price.n < 2000000)
nrow(Staten_Island)
summary(Staten_Island$sale.price.n)
plot(Staten_Island$gross.sqft,Staten_Island$sale.price.n)


summaryBy(sale.price.n~building.class.category,data=Staten_Island, FUN = mean)
ggplot(Staten_Island, aes(x= building.class.category, y = sale.price.n,fill = building.class.category)) + geom_boxplot()  + coord_flip()


#######################################################################################################
## 6.rbin all data set ################################################################################
#######################################################################################################

data <- rbind(Bronx,Brooklyn,Manhattan,Queens, Staten_Island)

ggplot(data,aes(x=building.class.category, y=log(sale.price.n),fill=building.class.category))+geom_boxplot()+coord_flip() + ggtitle("Boxplot of Building Type")
ggplot(data,aes(x=borough, y=log(sale.price.n),fill=borough))+geom_boxplot() + ggtitle("Boxplot of Borough Prices")
ggplot(data, aes(sale.date, log(sale.price.n))) + geom_point(aes(colour = factor(borough)))


#######################################################################################################
# Problem 2
#######################################################################################################

data1 <- read.csv("nyt1.csv")
data2 <- read.csv("nyt2.csv")
data3 <- read.csv("nyt3.csv")

# 1.Create a new variable, age_group, that categorizes users as “<20”, “20-29”, “30-39”, “40-49”, “50-59”, “60-69”, and “70+”.

data1$agecat <- cut(data1$Age, c(-Inf,20,29,39,49,59,69,Inf),labels = c("<20","20-29","30-39","40-49","50-59","60-69","70+"))
data1$day <- 1
data2$agecat <- cut(data2$Age, c(-Inf,20,29,39,49,59,69,Inf),labels = c("<20","20-29","30-39","40-49","50-59","60-69","70+"))
data2$day <- 2
data3$agecat <- cut(data3$Age, c(-Inf,20,29,39,49,59,69,Inf),labels = c("<20","20-29","30-39","40-49","50-59","60-69","70+"))
data3$day <- 3
data_2 <- rbind(data1,data2,data3)

library("doBy")
siterange <- function(x) {
  c(length(x),min(x),mean(x),max(x))
}
summaryBy(Age~agecat, data=data1, Fun=siterange)
summaryBy(Age~agecat, data=data2, Fun=siterange)
summaryBy(Age~agecat, data=data3, Fun=siterange)

# Plot the distribution of number of impressions and click-through-rate (CTR = #clicks / #impressions) for these age categories
library("ggplot2")
names(data1)
str(data1)
###### Impression #########################################
ggplot(data_2,aes(x=agecat, y=Impressions ))+ geom_boxplot(aes(colour = factor(day)))+ ggtitle("Impression Distribution of Each Day")

###### CTR ################################################
data1$hasimp <- cut(data1$Impressions,c(-Inf,0,Inf))
data2$hasimp <- cut(data2$Impressions,c(-Inf,0,Inf))
data3$hasimp <- cut(data3$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimp, data=data1, FUN=siterange)
summaryBy(Clicks~hasimp, data=data2, FUN=siterange)
summaryBy(Clicks~hasimp, data=data3, FUN=siterange)

ggplot(subset(data1, Impressions >0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()+ggtitle("Day1 CTR")
ggplot(subset(data2, Impressions >0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()+ggtitle("Day2 CTR")
ggplot(subset(data3, Impressions >0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()+ggtitle("Day3 CTR")

# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

data2$scode[data2$Impressions==0] <- "NoImps"
data2$scode[data2$Impressions >0] <- "Imps"
data2$scode[data2$Clicks >0] <- "Clicks"

data3$scode[data3$Impressions==0] <- "NoImps"
data3$scode[data3$Impressions >0] <- "Imps"
data3$scode[data3$Clicks >0] <- "Clicks"

data1$scode <- factor(data1$scode)
data2$scode <- factor(data2$scode)
data3$scode <- factor(data3$scode)


# Explore the data and make visual and quantitative comparisons across user segments/demographics
# (<20-year-old males versus <20-year-old females or logged-in versus not, for example).

data_3 = rbind(data1,data2,data3)
head(data_3)
str(data_3)

ggplot(subset(data_3, Impressions > 0),aes(x=agecat,y = log(Clicks/Impressions),colour=factor(Gender)))+geom_boxplot()


