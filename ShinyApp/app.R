library(shiny)
library(here)
#require(devtools)
#devtools::install_github("saurfang/shinyCartogram")
#library(shinyCartogram)

#source(here::here("Analysis/Tables/permillcalculation.R"))
#source(here::here("Analysis/Graphics/permillgraphfunc.R"))
#source(here::here("Analysis/Graphics/choroplethmapfunc.R"))


ui <- navbarPage(
  "FFSG",
  tabPanel("About"),
  
  
  tabPanel(
    "Tables and Graphs",
    fluidPage(titlePanel("Victims per Capita")),
    sidebarLayout(
      sidebarPanel(
        selectInput("state", "State", c(sort(
          c(state.name, "District of Columbia")
        ), "United States")),
        checkboxInput("all", "With Other States", FALSE),
        checkboxInput("capita", "Graph per capita (in millions)", TRUE)
      ),
      mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel("plot", plotOutput("permillplot")),
        tabPanel("table", dataTableOutput("permillDT"))
      ))
    )
  ),
  
  
  navbarMenu(
    "Maps",
      tabPanel("Choropleth",
        fluidPage(
          titlePanel("Choropleth Map of Deaths per Capita"),
          h5("")
        ),
        sidebarLayout(
          sidebarPanel(
            h5("Map is shown for mean values, to select a year choose Select Year"),
            checkboxInput("yearselect", "Select Year", FALSE),
            conditionalPanel("input.yearselect",
              sliderInput("year", "Year", 2000, 2017, value = 2000, animate = animationOptions(1500, TRUE))
            )
          ),
          mainPanel(plotlyOutput("choropleth"))
        )    
      ),
      tabPanel("Cartogram",
        fluidPage(
          titlePanel("Cartogram of Deaths per Capita"),
          h5("")
        ),
        sidebarLayout(
          sidebarPanel(
            sliderInput("yearcart", "Year", 2000, 2017, value = 2000, animate = animationOptions(1500, TRUE))
          ),
          mainPanel(h6(""))
        )
      ),
      tabPanel("Interactive",
               fluidPage(
                 leafletOutput("intmap")
               )
      )
  ),
  
  
  tabPanel("Data Analytics"),
  
  tabPanel("Data Compiling"),
  inverse = TRUE
  
  
)

server <- function(input, output, session) {
  output$permillplot <-
    renderPlot({
      permillgraph(input$state, input$all, input$capita)
    })
  output$permillDT <-
    renderDataTable({
      permilltable(input$state, input$all, input$capita)
    })
  output$choropleth <-
    renderPlotly({
      if(input$yearselect){
        choroplethmap(paste("p", input$year, sep=""))
      }else{
        choroplethmap()
      }
    })
  output$intmap <-
    renderLeaflet({
      interactivemap
    })
  
  #output$cartogram <- renderPlot({
  #  shinyCartogram::createCartogram({
  #    ffcartogram(paste("p", input$year, sep = ""))
  #  })})
}

shinyApp(ui, server)