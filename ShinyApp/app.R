# BM: it's nice for other users if we auto-install missing packages
# for them 
# load_libraries <- function(x){
#   for( i in x ){
#     #  require returns TRUE invisibly if it was able to load package
#     if( ! require( i , character.only = TRUE ) ){
#       #  If package was not able to be loaded then re-install
#       install.packages( i , dependencies = TRUE )
#       #  Load package after installing
#       require( i , character.only = TRUE )
#     }
#   }
# }
# 
# 
# load_libraries( c("shiny" , "here" , "plotly", "leaflet",  
#                   "rio", "devtools", "maps", "sp", "maptools",
#                   "tmap", "cartogram", "DT", "dplyr") )

# seems like we need to have library calls so that shinyapps.io can detect what pkgs to 
# install
library("shiny")
library("here")
library("plotly")
library("leaflet")
library("rio")
library("devtools")
library("maps")
library("sp")
library("maptools")
library("tmap")
library("cartogram")
library("DT")
library("dplyr")


# BM: docs say that "the directory that you save server.R in 
# will become the working directory of your Shiny app.
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
# so we need to source from here
source("permillcalculation.R")
source("permilltablefunc.R")
source("permillgraphfunc.R")
source("choroplethmapfunc.R")
source("descstatfuncs.R")
source("choroplethmapfunc.R")
source("cartogramfunc.R")
source("runcart.R")
source("interactivemap.R")



ui <- navbarPage(
  "FFSG",
  tabPanel("About",
           fluidPage(titlePanel("Fatal Force Study Group")),
           sidebarLayout(
             sidebarPanel(
               h3("Resources"),
               h6("")
             ),
             mainPanel(
               h2("About"),
               h4("UW Fatal Force Research Group (FFRG) was brought together at the University of Washington by Professor Martina Morris and Ben Marwick. Morris' background in sociology and statistics led her to creating this research group to fight injustice in police using fatal force. Ben Marwick, an Archeology professor, with a background in statistics and social science joined Morris as a side project. This research group started about a year and half ago with two students of Morris. The group has now expanded to seven undergraduate students, two from Western Washington University, with the addition of the two UW Professors. UW FFRG's mission is to bring justice and peace to communities most impacted by police brutality through a comprehensive data analysis combined with the comparisons of respective laws and policies.")
                       )
           )
           ),
  
  
  navbarMenu(
    "Tables and Graphs",
      tabPanel("Counts",
        fluidPage(titlePanel("Counting Fatal Encounters")),
        sidebarLayout(
          sidebarPanel(
            selectInput("state", "State", c(sort(
              c(state.name, "District of Columbia")
            ), "United States"), selected = "Washington"),
            checkboxInput("all", "Display with other states", FALSE),
            checkboxInput("capita", "Calculate per capita (in millions)", TRUE)
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("plot", plotOutput("permillplot")),
            tabPanel("table", dataTableOutput("permillDT"))
          ))
        )
      ),
      tabPanel("Descriptive Statistics",
        fluidPage(titlePanel("Descriptive Statistics"),
          sidebarLayout(
            sidebarPanel(
              selectInput("dem", "Demographic", c("Race", "Gender", "Age")),
              h6("Disclaimer: Please take note that the data we are currently using is still a work in progress so some of the data is missing. This means that there is a possibility that the trends displayed aren't the true trends for the data.")
            ),
            mainPanel(dataTableOutput("dstbl"), plotOutput("dsplt"))
          )
        )
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
              sliderInput("year", "Year", 2000, 2017, 
                          value = 2000, 
                          animate = animationOptions(1500, TRUE),
                          sep = "") # BM: make the numbers look like years
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
            sliderInput("yearcart", "Year", 2000, 2017, value = 2010, animate = animationOptions(1500, TRUE),  sep = "") # BM: make the numbers look like years
          ),
          mainPanel(plotOutput("cartogram"))
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
  
  output$cartogram <- renderPlot({
    whichcart(input$yearcart) # BM: we capture the input value for the cartogram with the yearcart variable. 
  })
  
  output$dstbl <- renderDataTable({
    dstable(input$dem)
  }) 
  
  output$dsplt <- renderPlot({
    dsplot(input$dem)
  })
}

shinyApp(ui, server)