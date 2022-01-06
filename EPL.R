getwd()
setwd("/Users/tjdcole/Documents/GitHub/R_Shiny")
getwd()
epl <- read.csv("EPL_20_21.csv", stringsAsFactors=TRUE)
head(epl)
summary(epl)
str(epl)

install.packages("tidyverse")
library("tidyverse")
library("ggplot2")
library("dplyr")

xg_xa <- epl %>%
  select(Name, xG, Position, xA, Club) %>%
  filter(Position != "GK") %>%
  separate(col = Position, c("Position1", "Position2"), extra = "drop", fill = "right")
head(xg_xa)
str(xg_xa)
summary(xg_xa)

p <- ggplot(data= xg_xa %>%
         select(Name, xG, xA),aes(x=xG, y=xA)) 

p + geom_point(colour = "red", alpha = 0.5)
