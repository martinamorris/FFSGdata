library(shiny)

#source("Analysis/Graphics/permillgraphfunc.R")

ui <- navbarPage("FFSG",
                 tabPanel("About"),
                 
                 
                 tabPanel("Per Capita",
                          fluidPage(
                            titlePanel("Victims per Capita")
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("state", "State", c(sort(c(state.name, "District of Columbia")), "United States")),
                              checkboxInput("all", "With Other States", FALSE)
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                tabPanel("plot",plotOutput("permillplot")),
                                tabPanel("table", dataTableOutput("permillDT"))
                              )
                            )
                          )
                  ),
          
                 
                 tabPanel("Mapping Demographics",
                          fluidPage(
                            titlePanel("Mapping Demographics"),
                            h3("Chloropleth mapping based on selected demographics"),
                            sidebarPanel(
                              h4("Demographics"),
                              checkboxGroupInput("gender", "Gender", c("Female", "Male", "Transgender", "Unspecified"), selected = c("Female", "Male", "Transgender", "Unspecified")),
                              checkboxGroupInput("race", "Race", c("African-American/Black", "Asian/Pacific Islander", "European-American/White", "Hispanic/Latino", "Middle Eastern","Native-American/Alaskan", "Unspecified"), selected = c("African-American/Black", "Asian/Pacific Islander", "European-American/White", "Hispanic/Latino", "Middle Eastern","Native-American/Alaskan", "Unspecified")),
                              checkboxGroupInput("mental", "Mental Illness Symptoms", c("Yes", "No", "Drug or alcohol use","Unknown"), selected = c("Yes", "No", "Drug or alcohol use","Unknown"))
                            )
                          )
                  ),
                 
                 
                 tabPanel("Mapping Individuals"),
                 
                 tabPanel("Data Compiling"),
                 inverse = TRUE
                 
  
)

server <- function(input, output, session) {
    output$permillplot <- renderPlot({permillgraph(input$state, input$all)})
    output$permillDT <- renderDataTable({permilltable(input$state, input$all)})
}

shinyApp(ui, server)