#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggiraph)
library(sf)
#library(crayon)
library(shinyBS)

HighSchool <- as.data.frame(read_csv("Trial/HighSchool2.csv"))
Bachelor <- as.data.frame(read_csv("Trial/Bachelor1.csv"))
HSerror <- as.data.frame(read_csv("Trial/HighSchoolError.csv"))
Berror <- as.data.frame(read_csv("Trial/BachelorError.csv"))

bach_adj_map <- read_csv("Trial/bach_adj.csv")
hs_adj_map <- read_csv("Trial/hs_adj.csv")
us <- st_read("Trial/states_map.shp")
names(bach_adj_map)[8] <-"Multiracial"
names(hs_adj_map)[8] <- "Multiracial"


# Define UI for application that draws barplot
ui <- fluidPage(

    # Application title
    titlePanel("Educational Attainment By State and Race/Ethnicity"),

    # sidebar with dropdown
  
    sidebarLayout(
        sidebarPanel(
          radioButtons("level", "Choose Education Level to View:",
                       c("Highschool and Over", "Bachelor's and Over")),
          helpText("Choose between either high school or secondary educational attainment rates to display them in the 
                   map and bar chart."),
          varSelectInput("race", "Choose A Race/Ethnicity:",
                       data = select(bach_adj_map, -c(1,state_name))),
          helpText("Choose a race/ethnicity to display their attainment rates on the map."),
          selectInput("state", "Choose A State:", 
                      choices=colnames(HighSchool[2:51])),
          helpText("Select a state to compare their racial education gaps against the national average in the bar chart."),
          helpText("You can also select a state directly in the map to view it's metrics in the bar chart."),
        helpText("Data from National Center of Education Statistics.")),

        # Show a plot of the generated distribution
        mainPanel(
          girafeOutput("MapPlot",
                        width = "100%",
                        height = "500px"),
           plotOutput(outputId = "StatePlot", 
                      width = "100%",
                      height = "300px")
           
        )
    )
)

server <- function(input, output, session) {
    observe({
      updateSelectInput(session, "state", "Choose A State:", 
                  choices=colnames(HighSchool[2:53]))
    })
  
    total <- reactive({
      if(input$level == "Highschool and Over"){
        df = data.frame(Race = HighSchool[,"Race"],
                        state = HighSchool[,"United States Average"],
                        error = HSerror[,"United States Average"])
        return(df)
        }
      else{
        df = data.frame(Race = Bachelor[,"Race"],
                        state = Bachelor[,"United States Average"],
                        error = Berror[,"United States Average"])
        return(df)
        }
    })
  
    edu <- reactive({
      if(input$level=="Highschool and Over"){
        df=data.frame(Race=HighSchool[,"Race"], 
                      state=HighSchool[,stateSelection()],
                      error=HSerror[,stateSelection()])
        return(df)
        }
      else({
        df=data.frame(Race=Bachelor[,"Race"], 
                         state=Bachelor[,stateSelection()],
                         error=Berror[,stateSelection()])
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
    
    measure <- reactive({
      if(input$level=="Highschool and Over"){
        return("High School Attainment")
      }
      else{
        return("Bachelor's Attainment")}
      })
    
    stateClick <- reactive({
      if(is.null(input$MapPlot_selected)){
      return("United States Average")}
      else{
        return(input$MapPlot_selected)
      }
        })
    
    stateSelection <- reactive({
      return(input$state)
    })
    
    raceSel <- reactive({
      return(input$race)
    })
    
    plot_df <- reactive({
      state_attain <- edu() %>% 
        select(Race, state) %>% 
        rename(state = state)
      state_error <- edu() %>% 
        select(Race, error) %>% 
        rename(state = error)
      total_attain <- total() %>% 
        select(Race, state) %>%
        rename('US Average' = state)
      total_error <- total() %>% 
        select(Race, error) %>% 
        rename('US Average' = error)
      full_attain <- full_join(state_attain, total_attain, by = "Race") %>% 
        pivot_longer(c('state', "US Average"), 
                     names_to = "state",
                     values_to = "attainment")
      full_error <- full_join(state_error, total_error, by = "Race") %>%
        pivot_longer(c('state', 'US Average'), 
                     names_to = "state",
                     values_to = "error")
      full_df <- full_join(full_attain, full_error, by = c("Race", "state"))
      return(full_df)
    })
    
    # Map Plot
    
    output$MapPlot <- renderGirafe({
    
      map_interactive <- ggplot(edu_map()) +
        geom_sf_interactive(aes(fill = !!input$race, 
                                tooltip = paste(state_name, 
                                                sprintf("%+g",!!input$race), 
                                                sep=": "), 
                                data_id = state_name)) + 
        theme_void() + 
        labs(fill = "Margin") + 
        ggtitle(paste("State-by-State Comparison to U.S. Average: \n",
                      raceSel(),
                      measure())) +
        scale_fill_gradient2(low = "#CB4335", 
                             mid = "white", 
                             high = "#2E86C1",
                             na.value = "grey") + 
        theme(panel.spacing = unit(0, "cm"),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = margin(0, 0, 0, 0, "cm"))
      
      x <- girafe(ggobj = map_interactive, 
                  options = list(opts_selection(selected = stateSelection(),
                                                type = "single", 
                                                only_shiny = FALSE,
                                                css = girafe_css(area = "stroke-width:2.5px",
                                                                  css = "fill:yellow;stroke:gray;"))))
      
    })
    output$StatePlot <- renderPlot({
      # Render a barplot
      ggplot(plot_df(), aes(x=Race,y=attainment,fill=state)) +
        geom_col(width = 0.8, position = position_dodge(0.9), aes(color = state)) + 
        labs(title = str_wrap(paste(measure(),"by Race in",stateSelection()), 30)) +
        scale_fill_discrete(name = "Region", 
                            labels = c(stateSelection(), "US Average")) + 
        scale_fill_manual(values = c("#CB4335", "#2E86C1")) + 
        scale_color_manual(values = c("#B03A2E", "#2874A6")) + 
        labs(x = "Race", y = "Percent with Attainment", fill = "Region") +
        geom_errorbar(aes(x=Race,
                          ymin=attainment-error,
                          ymax=attainment+error, 
                          group = state), 
                      size = 0.7,
                      width = 0.65,
                      position = position_dodge(0.9)) + 
        guides(color = "none") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))
    })
    observeEvent(stateClick(), {
      updateSelectInput(session, "state", 
                        label = "Choose A State",
                        selected = stateClick())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

