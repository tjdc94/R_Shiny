df60 <- data.frame(Code=Country_Code, Life_Expectancy= Life_Expectancy_At_Birth_1960) 
head(df60)

df13 <- data.frame(Code=Country_Code, Life_Expectancy=Life_Expectancy_At_Birth_2013)
head(df13)


setwd("/Users/tonye.cole/Documents/Personal Development/R/R For Data Science - Udemy")
getwd()
data <- read.csv("S5_Homework_Data.csv", stringsAsFactors=TRUE)
head(data)

filter <- data$Year == "1960"
data60 <- data[filter,]
head(data60)

filter <- data$Year == "2013"
data13 <- data[filter,]
head(data13)

head(df13)
head(data13)

merged60 <- merge(data60, df60, by.x = "Country.Code", by.y = "Code")
head(merged60)

merged13 <- merge(data13, df13, by.x = "Country.Code", by.y = "Code")
head(merged13)

qplot(data=merged60, x=Fertility.Rate, y=Life_Expectancy,
      colour=Region, size=I(2), shape=I(19), alpha=I(0.5), main="Life Expectancy vs Fertility Rate 1960")

qplot(data=merged13, x=Fertility.Rate, y=Life_Expectancy,
      colour=Region, size=I(2), shape=I(19), alpha=I(0.5), main="Life Expectancy vs Fertility Rate 2013")