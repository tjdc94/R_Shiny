head(data)
str(data)
summary(data)

#Avg fertility rate
mean(data$Fertility.Rate)

install.packages("tidyverse")
library(dplyr)

#How to do this in base R
which(data$Region == "Africa")
data[which(data$Region == "Africa"), ]

#Using the TIdyverse package is much cleaner!
data %>%
  filter(Region == "Africa") %>%
  select(-Region) %>%
  top_n(10)

#---------------------------- Top 10 fertility rates in Africa w * 10 column
fertility10 <- data %>%
  filter(Region == "Africa") %>%
  select(-Region) %>% #Hides this column
  mutate(fertility10 = Fertility.Rate * 10)  %>% #Adds new column that is a function of existing variables
  arrange(desc(Fertility.Rate)) %>% #Changes row order
           top_n(10)  #Top 10
fertility10

#---------------------------- 
data %>% select(Region)

avg_fertility <- data %>%
  group_by(Region) %>%
    summarise(average_fertility = mean(Fertility.Rate, na.rm = TRUE))
head(avg_fertility) #This outputs as a Tibble

#merges data w avg_fertility by Region
merged <- merge(data, avg_fertility, by = "Region")
head(merged)

#Best performer compared to average

merged %>%
mutate(difference = Fertility.Rate - average_fertility) %>%
arrange(desc(difference)) %>%
top_n(10) 


# top_n(merged,10)

