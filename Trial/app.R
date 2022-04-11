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
library(ggiraph)
library(sf)
HighSchool <- as.data.frame(read_csv("HighSchool2.csv"))
Bachelor <- as.data.frame(read_csv("Bachelor1.csv"))
HSerror <- as.data.frame(read_csv("HighSchoolError.csv"))
Berror <- as.data.frame(read_csv("BachelorError.csv"))

bach_adj_map <- read_csv("bach_adj.csv")
hs_adj_map <- read_csv("hs_adj.csv")
us <- st_read("states_map.shp")


# Define UI for application that draws barplot
ui <- fluidPage(

    # Application title
    titlePanel("Educational Attainment By State and Race/Ethnicity"),

    # sidebar with dropdown
  
    sidebarLayout(
        sidebarPanel(
          radioButtons("level", "Choose Education Level to View:",
                       c("Highschool and Over", "Bachelor's and Over")),
          varSelectInput("race", "Choose A Race/Ethnicity:",
                       data = select(bach_adj_map, -c(1,state_name))),
          selectInput("state", "Choose A State:", 
                      choices=colnames(HighSchool[2:51])),
        helpText("Data from National Center of Education Statistics.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           girafeOutput("MapPlot"),
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
  
    edu_map <- reactive({
      if(input$level=="Highschool and Over"){
        df=data.frame(hs_adj_map)
      }
      else({
        df=data.frame(bach_adj_map)
      })
      map_df <- inner_join(us, df, by = "state_name")
      return(map_df)
    })
    
    output$MapPlot <- renderGirafe({
    
      map_interactive <- ggplot(edu_map()) + 
        geom_sf_interactive(aes(fill = !!input$race, tooltip = !!input$race, data_id = state_name)) + 
        theme_void()
      
      x <- girafe(ggobj = map_interactive)
      
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
