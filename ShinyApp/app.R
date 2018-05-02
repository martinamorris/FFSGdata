
load_libraries <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


load_libraries( c("shiny" , "here" , "plotly", "leaflet",  
                  "rio", "devtools", "maps", "sp", "maptools",
                  "tmap", "cartogram", "DT", "dplyr") )

#source(here::here("Analysis/Tables/permillcalculation.R"))
#source(here::here("Analysis/Graphics/permillgraphfunc.R"))
#source(here::here("Analysis/Graphics/choroplethmapfunc.R"))


ui <- navbarPage(
  "FFSG",
  tabPanel("About"),
  
  
  navbarMenu(
    "Tables and Graphs",
      tabPanel("Counts",
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
      tabPanel("Descriptive Statistics",
        fluidPage(titlePanel("Descriptive Statistics"),
          sidebarLayout(
            sidebarPanel(
              selectInput("dem", "Demographic", c("Race", "Gender", "Age")),
              h6("Please take note that the data we have right now is still a work in progress so some of the data is missing. This means that there is a possibility that the trends displayed aren't the true trends for the data.")
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
    ffcartogram(input$yearcart) # BM: we capture the input value for the cartogram with the yearcart variable. 
  })
  
  output$dstbl <- renderDataTable({
    dstable(input$dem)
  }) 
  
  output$dsplt <- renderPlot({
    dsplot(input$dem)
  })
}

shinyApp(ui, server)