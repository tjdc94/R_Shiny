#install.packages("rmarkdown")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("evaluate")
#install.packages("maptools")
#install.packages('rgeos', type='source')
#install.packages('rgdal', type='source')
#install.packages('sf')
#install.packages('tmap')


library("ggmap")
library("shiny")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")
library("rgdal")
library("broom")
library("maptools")
library("leaflet")
library("sf")
library("tmap")
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

#Load Data
setwd("/Users/tonye.cole/Documents/Coding/R/datasets")

countries <- my_spdf$NAME
write.csv(countries, "countries.csv")

#Read shapefile with rgdal library
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp"), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE)

#Clean the data object
my_spdf@data$POP2005[ which(my_spdf@data$POP2005 == 0)] = NA
my_spdf@data$POP2005 <- as.numeric(as.character(my_spdf@data$POP2005)) / 1000000 %>% round(2)


#spdf_fortified <- tidy(my_spdf, region = "NAME")

#Create a colour palette
mypalette <- colorNumeric( palette="viridis", domain=my_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

#Basic choropleth
m <- leaflet(my_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

#Change colour scale to Quantile
m <- leaflet(my_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )

