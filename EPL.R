getwd()
setwd("/Users/tjdcole/Documents/GitHub/R_Shiny")
getwd()
epl <- read.csv("EPL_20_21.csv", stringsAsFactors=TRUE)
head(epl)
summary(epl)
str(epl)

install.packages("tidyverse")
install.packages("plotly")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("plotly")

xg_xa <- epl %>%
  select(Name, xG, Position, xA, Club) %>%
  filter(Position != "GK") %>%
  separate(col = Position, c("Position1", "Position2"), 
           extra = "drop", fill = "right") %>%
  mutate_if(is.character,as.factor)

# head(xg_xa)
# str(xg_xa)
# summary(xg_xa)

p <- ggplot(data= xg_xa,aes(x=xG, y=xA, colour=Position1, text = Name))

q <- p + geom_point(alpha=0.5) + 
  xlab("Expected Goals") + ylab("Expected Assists") +
  ggtitle("xG vs xA for each outfield player in the EPL 20/21")
  #geom_text(hjust = 0, nudge_x = 0.05, aes(label = Name))
ggplotly(q, tooltip = c(Name, xG, xA))
#Attempting to allow name of the player to show when you hover over them

#Updated version
