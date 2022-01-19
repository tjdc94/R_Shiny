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
setwd("/Users/tonye.cole/Documents/Personal Development/R/GitHub/R_Shiny")
epl <- read.csv("EPL_20_21.csv", stringsAsFactors=TRUE)

#UI Code
ui <- fluidPage(
  titlePanel("EPL Insights"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choice"),
    ),
    mainPanel(
      plotOutput('plot'),
      plotlyOutput('plotly',
        width = "100%",
        height = "400px",
        inline = FALSE,
        reportTheme = TRUE
      ),
      verbatimTextOutput('select')
    )
  )
)


# Server code
server <- function(input, output) {
  
  xg_xa <- epl %>%
    select(Name, xG, Position, xA, Club) %>%
    filter(Position != "GK") %>%
    separate(col = Position, c("Position", "Position2"), 
             extra = "drop", fill = "right") %>%
    mutate_if(is.character,as.factor)
  
 output$plotly<- renderPlotly({
   
   d <- xg_xa %>%
     filter(Position == input$select | Club == input$select)
   
   p <- ggplot(data= d, aes(x=xG, y=xA, colour=Position, text = Name)) + 
     geom_point(alpha=0.5) + xlab("Expected Goals") + ylab("Expected Assists") 
   
   p + ggtitle("xG vs xA for each outfield player in the EPL 20/21")
   #geom_text(hjust = 0, nudge_x = 0.05, aes(label = Name))
   
   ggplotly(p, tooltip = c("Name", "xG", "xA"))
   
 }, env = parent.frame(), quoted = FALSE)
  
  # output$plot <- renderPlot({
  #   
  #   d <- xg_xa %>%
  #     filter(Position == input$select | Club == input$select)
  # 
  # 
  #   p <- ggplot(data= d, aes(x=xG, y=xA, colour=Position, text = Name)) + 
  #     geom_point(alpha=0.5) + xlab("Expected Goals") + ylab("Expected Assists") 
  #     
  #   p + ggtitle("xG vs xA for each outfield player in the EPL 20/21")
  #   #geom_text(hjust = 0, nudge_x = 0.05, aes(label = Name))
  #   
  #   ggplotly(p, tooltip = c("Name", "xG", "xA"))
  # })
  
  output$select <- renderPrint({
    
    input$select
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
