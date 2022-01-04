getwd()
setwd("/Users/tonye.cole/Documents/Personal Development/R/Spotify")
getwd()
spotify <- read.csv("spotify.csv")
head(spotify)
summary(spotify)
str(spotify)

library("tidyverse")
library("ggplot2")

liked <- spotify %>%
  filter(target == 1 )
head(liked)


#Plot two histograms for 0 & 1 target on the same scale 
ggplot() +  geom_histogram(data= spotify %>%
                             filter(target == 1 ), 
                           aes(x=danceability, y= ..density..), alpha= 0.5,binwidth = 0.05, fill= "green", colour="black") +
  geom_density(data = spotify, aes(x=danceability)) +
  geom_histogram(data= spotify %>%
                   filter(target == 0 ), aes(x=danceability, y= ..density..), alpha = 0.5, binwidth = 0.05, fill = "red", colour="black")


#Plots two histograms on top of each other one for 0 target & the other for 1 target
ggplot(data=spotify) + geom_histogram(aes(x=danceability, y= ..density.., fill = target), colour = "black") + facet_wrap(~target, nrow = 2)

#head(spotify)


#ggplot(data=spotify) + geom_histogram(aes(x=danceability, y= ..density.., fill = target), colour = "black") + facet_wrap(~target, nrow = 2) 
  
#plots a different histogram for each metric all using one ggplot  
x <- spotify %>% select(-song_title,-artist) %>%
  pivot_longer(!X, names_to = "metric", values_to = "rating")

ggplot(x) + geom_histogram(aes(x=rating, y= ..density.., fill = metric), colour = "black") + facet_wrap(~metric, nrow=10, scales = "free")


#t + geom_histogram(binwidth=10,
#                   aes(x=CriticRating),
 #                  fill="White", colour="Red")


(target_counts <- spotify %>% group_by(target) %>% summarise(n = n()))
ggplot(target_counts) + 
    geom_col(aes(x = target, y = n, fill = as.character(target)))
