# DATA-413: Data Science
# Final Project: Shiny App
# Abby Sommers

# datasets::USArrests
#   Format
#       A data frame with 50 observations on 4 variables.
#           [,1]	Murder	numeric	Murder arrests (per 100,000)
#           [,2]	Assault	numeric	Assault arrests (per 100,000)
#           [,3]	UrbanPop	numeric	Percent urban population
#           [,4]	Rape	numeric	Rape arrests (per 100,000)


#Recode Observations to be 'State' name column
arrests_data <- datasets::USArrests
arrests_data <- tibble::rownames_to_column(USArrests, var = "State")
library(tidyverse)
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(dplyr)

# Define UI
ui <- fluidPage(
  #add theme
  theme = shinytheme("cerulean"),
  setBackgroundColor(color ="ghostwhite"),
  # App Title
  fluidRow(
    column(width = 12, align = "center",
           titlePanel("Exploration of US Arrests Data"))
  ),
  
  # Contributor Name and App Description 
  fluidRow(
    column(width = 12, align = "center",
           h4("DATA-413 Final Project by Abby Sommers")),
    column(width = 12, align = "left",
           p("This ShinyApp uses the built-in data set USArrests. The app allows the user to explore arrest rates both nationally and by individual state. The first tab displays a scatterplot of the crime type selected by user on the x-axis vs. percent urban population and table which displays the data from US Arrests. The Second tab allows the user to select a state and displays a bar chart of the arrest rates by crime. The third tab allows the user to select 2 states to compare by bar chart of their arrests per 100,000."))
  ),
  
  # Tabset for individual panels 
  tabsetPanel(type = "tabs", 
              
              # Tab 1: Scatter Plot of National Data
              tabPanel("National Overview",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("x_var", 
                                       "Select Crime Category",
                                       choices = c("Murder","Assault","Rape"))),
                         mainPanel(
                           plotOutput("scatter_plot"),
                           dataTableOutput("table")
                         )
                       )),
              
              # Tab 2: Individual State
              tabPanel("Individual State Data",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("state_ind", "Select A State:",
                                       choices = unique(arrests_data$State))
                         ),
                         mainPanel(
                           plotOutput("state_plot")
                         )
                         
                       )),
              
              # Tab 3: Compare 2 selected states
              tabPanel("Comparing States",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("state_1","Select A State:",
                                       choices =  unique(arrests_data$State)),
                           selectInput("state_2","Select A State:",
                                       choices = unique(arrests_data$State))
                         ),
                         mainPanel(
                           plotOutput("comparison_plot")
                         )
                       ))
  )
)

# Define server logic 
server <- function(input, output) {
  # Part 1: Scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(arrests_data, aes_string(x = input$x_var, y = "UrbanPop")) +
      geom_point() +
      geom_smooth(method = 'lm')+
      labs(x = input$x_var, y = "% Urban Population")+ 
      ggtitle("Scatter Plot of Crime Type vs % Urban")
  })
  # Part 2: Data Table
  output$table <- renderDataTable({
    arrests_data
  })
  # Part 3: Individual state data
  output$state_plot <- renderPlot({
    state_data <- USArrests[input$state_ind, c("Murder", "Assault", "Rape")]
    df <- data.frame(
      State = c(input$state_ind),
      Murder = c(state_data$Murder),
      Assault = c(state_data$Assault),
      Rape = c(state_data$Rape)
    )
    df_long <- pivot_longer(df, -State)
    ggplot(df_long, aes(x = State, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
      labs(title = paste("Crime rates in", input$state_ind),
           x = "Crime Type", y = "Arrests per 100,000", fill = "Crime Type")
  })
  # Part 4: Compare 2 states
  output$comparison_plot <- renderPlot({
    state1_data <- USArrests[input$state_1, c("Murder", "Assault", "Rape")]
    state2_data <- USArrests[input$state_2, c("Murder", "Assault", "Rape")]
    df <- data.frame(
      State = c(input$state_1, input$state_2),
      Murder = c(state1_data$Murder, state2_data$Murder),
      Assault = c(state1_data$Assault, state2_data$Assault),
      Rape = c(state1_data$Rape, state2_data$Rape)
    )
    df_long <- pivot_longer(df, -State)
    ggplot(df_long, aes(x = State, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5)+
      labs(title = "Comparison of Arrest Rates Between States",
           x = "State", y = "Arrests per 100,000", fill = "Crime Type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)


