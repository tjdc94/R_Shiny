getwd()
setwd("/Users/tjdcole/Documents/GitHub/R_Shiny/R For Data Science - Udemy")
movies <- read.csv("S6-Homework-Data.csv", stringsAsFactors=TRUE)
head(movies)
summary(movies)
str(movies)

library("tidyverse")
library("ggplot2")


t <- movies %>% 
  rename(GrossMill = Gross...mill., GrossUSPercentage = Gross...US, BudgetMill = Budget...mill.) %>%
  select(Genre, Studio, GrossMill, GrossUSPercentage, BudgetMill) %>%
  filter(Genre == "action" | Genre == "adventure"| Genre == "animation" | Genre == "comedy" | Genre == "drama", na.rm = TRUE) %>%
  filter(Studio == "Buena Vista Studios" |Studio == "Fox" | Studio == "Paramount Pictures" | Studio == "Sony" | Studio == "Universal" | Studio == "WB", na.rm = TRUE)

head(t)
summary(t)
str(t)

u <- ggplot(data=t,aes(x=Genre, y=GrossUSPercentage))
u + geom_jitter(aes(colour=Studio, size=BudgetMill)) + geom_boxplot(size=1.2, alpha=0.3) +
  xlab("Genre") + 
  ylab("Gross % US") +
  ggtitle("Domestic Gross % by Genre") +
  theme(axis.title.x = element_text(colour="blue", size=15),
        axis.title.y = element_text(colour="blue", size=15),
        axis.text.x = element_text(size=10), #tick mark formatting
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 15), #legend formatting
        legend.text = element_text(size=10),
        #legend.position = c(1,1),
        #legend.justification = c(1,1),
        
        plot.title = element_text(colour="Black",
                                  size=25,
                                  family= "Courier"))




t <- ggplot(data=movies, aes(x=Genre, y=CriticRating, colour=Genre))
t + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)