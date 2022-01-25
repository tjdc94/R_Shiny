#Packages

#install.packages("tidyverse")
#install.packages("plotly")
#install.packages('rsconnect')
library("shiny")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("plotly")
library("rsconnect")
#deployApp()

#Global Variables
setwd("/Users/tonye.cole/Documents/GitHub/R_Shiny")
epl <- read.csv("EPL_20_21.csv", stringsAsFactors=FALSE)


#UI Code
ui <- fluidPage(
  titlePanel("EPL Insights"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choice"),
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


# Server code
server <- function(input, output) {
  
  xg_xa <- epl %>%
    select(Name, xG, Position, xA, Club, Mins) %>%
    filter(Position != "GK" & Mins >= 180) %>%
    separate(col = Position, c("Position", "Position2"), 
             extra = "drop", fill = "right")
  
  output$plotly<- renderPlotly({
 #   
    d <- xg_xa %>% filter(Position == input$select | Club == input$select)
     
      p <- ggplot(data= d, aes(x=xG, y=xA, colour=Position, text = Name)) + 
        geom_point(alpha=0.5) + xlab("Expected Goals per 90") + ylab("Expected Assists per 90") + 
     ggtitle("Per 90 xG vs xA for each outfield player in the EPL 20/21") 
      
      ggplotly(p, tooltip = c("Name", "xG", "xA"))
     
   })
  

  
  output$choice <- renderUI({
    c <-  xg_xa %>%
      select(Position, Club) %>%
     distinct(.keep_all = FALSE)
    selectInput("select", h3("Category"), 
                choices = c)
  })
  
}


#Attempting to allow name of the player to show when you hover over them

# Return a Shiny app object
shinyApp(ui = ui, server = server)
