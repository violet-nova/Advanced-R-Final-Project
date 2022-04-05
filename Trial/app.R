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
HighSchool <- read.csv("HighSchool2.csv")
Bachelor <- read.csv("Bachelor1.csv")

# Define UI for application that draws barplot
ui <- fluidPage(

    # Application title
    titlePanel("Educational Attainment By State"),

    # sidebar with dropdown
  
    sidebarLayout(
        sidebarPanel(
          radioButtons("level", "Choose Education Level to View",
                       c("Highschool and Over", "Bachelor's and Over")),
          selectInput("state", "Choose A State:", 
                      choices=colnames(HighSchool[2:51])),
        helpText("Data from National Center of Education Statistics.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("StatePlot")
        )
    )
)


server <- function(input, output, session) {
    observe({
      updateSelectInput(session, "state", "Choose A State:", 
                  choices=colnames(HighSchool[2:51]))
    })
  
  
    hs <- reactive({
      if(input$level=="Highschool and Over"){HighSchool[,c("Race",input$state)]
        }
      else(Bachelor[,c("Race",input$state)])   
    })
    
    output$StatePlot <- renderPlot({
        # Render a barplot
        ggplot(hs(), aes(x=Race,y=hs()[,input$state],fill=Race)) +
            geom_col() +
            ggtitle("Educational Attainment Level By Race") +
            xlab("Race") +
            ylab("Percent Attainment") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
