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
HSerror <- read.csv("HighSchoolError.csv")
Berror <- read.csv("BachelorError.csv")

# Define UI for application that draws barplot
ui <- fluidPage(

    # Application title
    titlePanel("Educational Attainment By State and Race/Ethnicity"),

    # sidebar with dropdown
  
    sidebarLayout(
        sidebarPanel(
          radioButtons("level", "Choose Education Level to View:",
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
                  choices=colnames(HighSchool[2:53]))
    })
  
  
    edu <- reactive({
      if(input$level=="Highschool and Over"){
        df=data.frame(Race=HighSchool[,"Race"], 
                      state=HighSchool[,input$state],
                      error=HSerror[,input$state])
        return(df)
        }
      else({
        df=data.frame(Race=Bachelor[,"Race"], 
                         state=Bachelor[,input$state],
                         error=Berror[,input$state])
        return(df)
        })   
    })
  

    output$StatePlot <- renderPlot({
        # Render a barplot
        ggplot(edu(), aes(x=Race,y=state,fill=Race)) +
            geom_col() +
            ggtitle("Educational Attainment Level By Race") +
            xlab("Race") +
            ylab("Percent of Population") +
            geom_errorbar(aes(x=Race,
                          ymin=state-error,
                          ymax=state+error))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
