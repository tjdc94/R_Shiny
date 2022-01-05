getwd()
setwd("/Users/tjdcole/Documents/GitHub/R_Shiny")
getwd()
epl <- read.csv("EPL_20_21.csv")
head(epl)
summary(epl)
str(epl)

install.packages("tidyverse")
library("tidyverse")
library("ggplot2")
library("dplyr")

xg_xa <- epl %>%
  select(Name, xG, xA)
head(xg_xa)

ggplot(data=)
