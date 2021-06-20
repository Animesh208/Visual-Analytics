data <- read.csv(file = 'crime_data.csv')


data$date <- as.Date(data$date, "%m/%d/%Y")

library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(ggmap)
bbox = c(left=-95.8, bottom=29.4, right=-95.0, top=30.0)
map <- get_stamenmap(bbox, zoom = 10, source="stamen")
#ggmap(map) + geom_point(data = data , aes(x = lon, y = lat))

# *******************************************************
# Create the user interface (ui)
# *******************************************************
ui <- fluidPage(
  
  titlePanel("Task 4"),
  sidebarLayout(
    sidebarPanel("Chossing the offense",
                 selectInput(inputId = "offense", 
                              label = "Please the offense to display",
                              choices = list("aggravated assault" = 'aggravated assault',
                                             "auto theft" = 'auto theft',
                                             "burglary" = 'burglary',
                                             "murder" = 'murder',
                                             "rape" = 'rape',
                                             "robbery" = 'robbery'), multiple = TRUE,
                              selected = 'aggravated assault'),
                 
                 
                 dateRangeInput(inputId = "date","Date Range: ", start = "2010-01-01", end = "2010-08-31"),
                 submitButton()
                 
                
    ),
    mainPanel('Spatial Density',
              plotOutput('myplot')
       )
  )
  
)


# *******************************************************
# Create the server function
# *******************************************************
server <- function(input, output) {
  
  data1 <- reactive({
    req(input$offense)
    req(input$date)
    data %>% subset(offense == input$offense & date >= input$date[1] & date <= input$date[2])
    
    
  })
  
  output$myplot <- renderPlot({
    ggmap(map) + 
    geom_point(data = data1(),mapping = aes(x = lon, y = lat, color = offense), size = 2)
  })
 
      
}


# *******************************************************
# Run the App
# *******************************************************
shinyApp(ui, server)