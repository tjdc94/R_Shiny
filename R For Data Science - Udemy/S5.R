#Method 1: Select file manually
stats <- read.csv(file.choose())
stats 

#Method 2: Set WD & Read Data
getwd()
#Mac:
setwd("/Users/tonye.cole/Documents/Personal Development/R/R For Data Science - Udemy")

getwd()
rm(stats)
stats <- read.csv("DemographicData.csv", stringsAsFactors=TRUE)
stats

#-------------------------------------- Exploring Data

nrow(stats)
#Imported 195 Rows
ncol(stats)
#Imported 5 columns
head(stats, n=10)
#Shows top 6 rows by default
tail(stats, n=8)
#Shows bottom 6 rows by default
str(stats)
#Structure 
summary(stats)

#--------------------------------------  Using the $ sign
stats
stats[3,3]
stats$Internet.users
stats$Internet.users[2]
stats[,"Internet.users"]
levels(stats$Income.Group)

#--------------------------------------  Basic Operations with a DF

stats[1:10,]
stats[c(4,100),]
#Remember propertoies of []
stats[1,]
is.data.frame(stats[1,])
stats[,1, drop=F]
is.data.frame(stats[,1, drop=F])
#multiply columns
stats$Birth.rate * stats$Internet.users
#Add a column
stats$MyCalc <- stats$Birth.rate * stats$Internet.users
head(stats)
#Remove a column
stats$MyCalc <- NULL
head(stats)

#--------------------------------------  Filtering Dataframes
filter <- stats$Internet.users < 2
stats[filter,]

stats[stats$Birth.rate < 40,]
stats[stats$Birth.rate < 40 & stats$Internet.users < 2,]
stats[stats$Income.Group == "High income",]
levels(stats$Income.Group)

stats[stats$Country.Name == "Malta",]

#-------------------------------------- Intro to qplot()
library(ggplot2)
?qplot()
qplot(data=stats, x=Internet.users, )
qplot(data=stats, x=Income.Group, y=Birth.rate)
qplot(data=stats, x=Income.Group, y=Birth.rate, size=I(3), colour=I("blue"))
qplot(data=stats, x=Income.Group, y=Birth.rate, geom="boxplot")

#--------------------------------------  Visualisations
qplot(data=stats, x=Internet.users, y=Birth.rate)
qplot(data=stats, x=Internet.users, y=Birth.rate,
      colour=Income.Group, size=I(2)) 

head(stats)

#--------------------------------------  Creating Dataframes

mydf <- data.frame(Countries_2012_Dataset, Codes_2012_Dataset, Regions_2012_Dataset)
head(mydf)
# colnames(mydf) <- c("Country","Code","Region")
# head(mydf)
rm(mydf)

mydf <- data.frame(Country=Countries_2012_Dataset, Code=Codes_2012_Dataset, Region=Regions_2012_Dataset)
head(mydf)
summary(mydf)

#--------------------------------------  Merging Dataframes

merged <- merge(stats, mydf, by.x = "Country.Code", by.y = "Code")
head(merged)

merged$Country <- NULL
str(merged)
tail(merged)

#--------------------------------------  Visualising 

qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Region, size=I(2)) 
#Shapes
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Region, size=I(2), shape=I(17)) 
#Transparency
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Region, size=I(2), shape=I(19), alpha=I(0.5)) 
#Title
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Region, size=I(2), shape=I(19), alpha=I(0.5), main="Birth Rate vs Internet Users")
