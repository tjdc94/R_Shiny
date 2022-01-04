#install.packages("tidyverse")
#install.packages('rsconnect')
library("shiny")
library("tidyverse")
library("rsconnect")
#deployApp()

# Global variables can go here
setwd("/Users/tonye.cole/Documents/Personal Development/R/datasets/Spotify")
spotify <- as.data.frame(read.csv("data.csv"))


# Define the UI
ui <- fluidPage(
  titlePanel("Spotify Insights"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("control"),
      uiOutput("choice"),
    ),
      mainPanel(
      verbatimTextOutput('inputValues'),
      verbatimTextOutput('select'),
      plotOutput('plot')
  )
)
)


# Define the server code
server <- function(input, output) {
  
  output$control <- renderUI(
    sliderInput("d", "danceability", min(spotify$danceability), max(spotify$danceability), value = c(0.2, 0.4))
  #Input ID d and labelled danceability. Value is a vector of two values as we want a range slider  
  )
  
 #<- renderUI(
   #selectInput("select", h2("categories"), )
 #)
  
  
  output$plot <- renderPlot({
    
    x <- spotify%>%
       filter (input$d[1] <= danceability, input$d[2] >= danceability)
  #filter (input$select == key) #attempting to filter the data by the key in the dropdown
  #Here attempt to equate the input from d as the danceability so the histogram changes it's sample based on the slider
    
   ggplot(x) + geom_histogram(aes_string(x = input$select)) #+ xlim(0.0, 1.0)
   #passed in the colnames as a string using aes_string to change the x axis based on drop-down
   
   # hist(x$acousticness, col = "#75AADB", border = "white")
    #after filtering based on danceability we extracted the acoustic column 
  })
  
  
  
  output$inputValues <- renderPrint({
    input$d
  })
  
  output$select <- renderPrint({
    input$select
})
  output$choice <- renderUI({
    c <- colnames(spotify)
    selectInput("select", h3("Category"), 
                choices = c)
  })
  
}



# Return a Shiny app object
  shinyApp(ui = ui, server = server)
