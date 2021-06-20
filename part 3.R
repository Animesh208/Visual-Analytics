library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(ggmap)
data <- read.csv(file = "big_stock_data.csv")
data$date <- as.Date(data$date, "%d/%m/%Y")
data$volume <- data$volume/100000

# *******************************************************
# SHINY APP TEMPLATE
# *******************************************************


# *******************************************************
# Load the required packages
# *******************************************************
library(shiny)


# *******************************************************
# Load the required data
# *******************************************************



# *******************************************************
# Create the user interface (ui)
# *******************************************************
ui <- fluidPage(
  
  titlePanel("Task 3"),
  sidebarLayout(
    sidebarPanel('Please select the companies and the variable',
                 selectInput(inputId =  "var_selection", label = "Variable to measure share performance",
                             choices = list(
                               "closing price" = 'close_price',
                               "share volume" = 'volume'),
                             selected = 'close_price'),
                 
                 selectInput(inputId = "company_1", label = "Companies to display and compare",
                              choices = list("Apple" = 'Apple',
                                             "Alibaba" = 'Alibaba',
                                             "Amazon" = 'Amazon',
                                             "Facebook" = 'Facebook',
                                             "Google" = 'Google',
                                             "Intel" = 'Intel',
                                             "Microsoft" = 'Microsoft',
                                             "SAP" = 'SAP'), multiple = TRUE,
                              selected = 'Apple'),
                 submitButton()
                 
    ),
    
    mainPanel(
              plotOutput('myplot'),
              ),
    position = 'left'
  )

  )
  

# *******************************************************
# Create the server function
# *******************************************************
server <- function(input, output) {
  
  output$myplot <- renderPlot(
    
    ggplot(data = data, aes_string(x = 'date', y = input$var_selection)) +
      geom_line(data = subset(data, company == input$company_1), aes(color = company)) +
      ggtitle(ifelse(input$var_selection == 'volume', 'Volume over time for selected companies (in 100,000s)', 'Closing Price over time for selected companies')) +
      labs(color = '')
      )
}


# *******************************************************
# Run the App
# *******************************************************
shinyApp(ui, server)




                 
                 
                 
                 
              