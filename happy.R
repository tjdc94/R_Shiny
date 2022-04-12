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
library("RColorBrewer")
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

#Global Variables
setwd("/Users/tonye.cole/Documents/Coding/R/datasets")
happy <- read_csv("happy_2019.csv", 
                     col_types = cols(`Perceptions of corruption` = col_double()))

gdp <- read.csv("gdp-per-capita-worldbank.csv", stringsAsFactors=FALSE)


#world_df <- st_read(dsn = "ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp"), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE)

#UI Code
ui <- fluidPage(
  titlePanel("World Happiness Insights"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choice"),
      leafletOutput("mymap"),
    ),
    mainPanel(
      plotlyOutput('plotly',
                   width = "100%",
                   height = "400px",
                   inline = FALSE,
                   reportTheme = TRUE
      ),
      # verbatimTextOutput('select')
      # verbatimTextOutput('plotly')
    )
  )
)
#Server Code
server <- shinyServer(function(input, output) {
  
  #Data Manipulation
  happy_df <- happy %>%
    select(`Country or region`, Score, `GDP per capita`, `Social support`, `Healthy life expectancy`, `Freedom to make life choices`, Generosity, `Perceptions of corruption`) %>%
    rename(Country = "Country or region") %>%
    rename(GDP_score = "GDP per capita") %>%
    filter(Country != "Kosovo") %>%
    filter(Country != "South Sudan") %>%
    filter(Country != "Northern Cyprus") %>%
    filter(Country != "Vietnam") %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE))
  
  happy_df$Country[happy_df$Country == "Ivory Coast"] <- "Cote d'Ivoire"
  happy_df$Country[happy_df$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
  happy_df$Country[happy_df$Country == "Congo (Brazzaville)"] <- "Congo"
  happy_df$Country[happy_df$Country == "Libya"] <- "Libyan Arab Jamahiriya"
  happy_df$Country[happy_df$Country == "Laos"] <- "Lao People's Democratic Republic"
  happy_df$Country[happy_df$Country == "Moldova"] <- "Republic of Moldova"
  happy_df$Country[happy_df$Country == "Myanmar"] <- "Burma"
  happy_df$Country[happy_df$Country == "Iran"] <- "Iran (Islamic Republic of)"
  happy_df$Country[happy_df$Country == "South Korea"] <- "Korea, Republic of"
  happy_df$Country[happy_df$Country == "North Macedonia"] <- "The former Yugoslav Republic of Macedonia"
  happy_df$Country[happy_df$Country == "Syria"] <- "Syrian Arab Republic"
  happy_df$Country[happy_df$Country == "Palestinian Territories"] <- "Palestine"
  happy_df$Country[happy_df$Country == "Trinidad & Tobago"] <- "Trinidad and Tobago"
  happy_df$Country[happy_df$Country == "Tanzania"] <- "United Republic of Tanzania"
  
  gdp_df <- gdp %>%
    select(Entity, Year, GDP.per.capita..PPP..constant.2017.international...) %>%
    rename(GDP_per_capita = "GDP.per.capita..PPP..constant.2017.international...") %>%
    filter(Year == "2019")
  
  
  gdp_df$Entity[gdp_df$Entity == "Moldova"] <- "Republic of Moldova"
  gdp_df$Entity[gdp_df$Entity == "Myanmar"] <- "Burma"
  gdp_df$Entity[gdp_df$Entity == "Laos"] <- "Lao People's Democratic Republic"
  gdp_df$Entity[gdp_df$Entity == "South Korea"] <- "Korea, Republic of"
  gdp_df$Entity[gdp_df$Entity == "Iran"] <- "Iran (Islamic Republic of)"
  gdp_df$Entity[gdp_df$Entity == "Ivory Coast"] <- "Cote d'Ivoire"
  gdp_df$Entity[gdp_df$Entity == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
  gdp_df$Entity[gdp_df$Entity == "North Macedonia"] <- "The former Yugoslav Republic of Macedonia"
  gdp_df$Entity[gdp_df$Entity == "Syria"] <- "Syrian Arab Republic"
  gdp_df$Entity[gdp_df$Entity == "Palestinian Territories"] <- "Palestine"
  gdp_df$Entity[gdp_df$Entity == "Trinidad & Tobago"] <- "Trinidad and Tobago"
  gdp_df$Entity[gdp_df$Entity == "Tanzania"] <- "United Republic of Tanzania"
  gdp_df$Entity[gdp_df$Entity == "Congo (Brazzaville)"] <- "Congo"
  gdp_df$Entity[gdp_df$Entity == "Libya"] <- "Libyan Arab Jamahiriya"
  
  
  #Combine happy_df with shapefile
  merged_df <- merge(happy_df, gdp_df,
                    by.x = "Country", by.y = "Entity")
  map_spdf <- merge(world_spdf, merged_df,
                    by.x = "NAME", by.y = "Country")
  
  #Create custom bins & colour palette
  mybins <- cut(happy_df$Score, 5) 
  
  #Change bins so they change dynamically binwidth or number of bins
  mypalette <- colorBin( palette="YlOrBr", domain=map_spdf@data$Score, na.color="transparent", bins=mybins)
  #Make a mypalllet function if using a specific score use a different score 
  
  #Prepare text for Tooltips
  mytext <- paste(
    "Country: ", map_spdf@data$NAME,"<br/>", 
    "Happiness Score: ", map_spdf@data$Score, "<br/>", 
    "GDP: $", round(map_spdf@data$GDP_per_capita,2),  #This is a GDP score between 0 & 1.68 Need to get real GDP data and merge to this
    sep="") %>%
    lapply(htmltools::HTML)
  
  
  #Final Plot
  output$mymap <- renderLeaflet({
    leaflet(map_spdf) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
      fillColor = ~mypalette(Score), 
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="white", 
      weight=0.3,
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Happiness Score", position = "bottomleft" )
  })
})
  
 # output$choice <- renderUI({
  #   c <-  happy_df %>%
  #     select(Score, GDP_per_capita) %>%
  #     distinct(.keep_all = FALSE)
  #   selectInput("select", h3("Category"), 
  #               choices = c)
  # })
  
# Return a Shiny app object
shinyApp(ui = ui, server = server)
