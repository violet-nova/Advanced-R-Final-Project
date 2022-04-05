#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
HighSchool2 <- read.csv("HighSchool2.csv")

# Define UI for application that draws barplot
ui <- fluidPage(

    # Application title
    titlePanel("Educational Attainment By State"),

    # sidebar with dropdown
  
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "Choose A State:", 
                        choices=colnames(HighSchool2[2:51])),
            helpText("Data from National Center of Education Statistics.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("StatePlot")
        )
    )
)

# Define server logic required to draw a barplot
server <- function(input, output) {

    hs <- reactive({
        HighSchool2[,c("Race",input$state)] 
    })
    
    output$StatePlot <- renderPlot({
        # Render a barplot
        ggplot(hs(), aes(x=Race,y=hs()[,input$state])) +
            geom_col() +
            ggtitle("Educational Attainment Level By Race") +
            xlab("Race") +
            ylab("Percent Attainment") 
    })
}

#"Alabama", "Alaska", "Arizona",
# "Arkansas", "California",
# "Colorado", "Connecticut",
# "Delaware", "Florida", "Georgia",
# "Hawaii", "Idaho", "Illinois",
# "Indiana", "Iowa", "Kansas",
# "Kentucky", "Louisana", "Maine",
# "Maryland", "Massachusetts",
# "Michigan", "Minnesota",
# "Mississippi", "Missouri", "Montana",
# "Nebraska", "Nevada", "New Hampshire",
# "New Jersey", "New Mexico","New York",
# "North Carolina", "North Dakota",
# "Ohio","Oklahoma","Oregon",
# "Pennsylvania", "Rhode Island",
# "South Carolina", "South Dakota",
# "Tennessee", "Texas", "Utah",
# "Vermont", "Virginia", "Washington",
# "West Virginia", "Wisconsin", "Wyoming"

# Run the application 
shinyApp(ui = ui, server = server)
