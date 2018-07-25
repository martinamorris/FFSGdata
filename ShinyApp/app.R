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
#source("runcart.R")
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
             
             fluidPage(
               fluidRow(
                 column(10,
                        h1("Counting Fatal Encounters")),
                 column(2,
                        actionButton(inputId = "helpbutton1", label = icon(name = "question-circle")),
                        uiOutput("HelpBox1")
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("state", "State", c(sort(
                   c(state.name, "District of Columbia")
                 ), "United States"), selected = "Washington"),
                 fluidRow(
                   column(9,
                          checkboxInput("all", "Display with other states", FALSE)
                   ),
                   column(3,
                          actionButton(inputId = "helpbuttonsmall1", label = icon(name = "question-circle")),
                          uiOutput("HelpBoxsmall1")
                   )
                 ),
                 fluidRow(
                   column(9,
                          checkboxInput("capita", "Calculate per capita (in millions)", TRUE)
                   ),
                   column(3,
                          actionButton(inputId = "helpbuttonsmall2", label = icon(name = "question-circle")),
                          uiOutput("HelpBoxsmall2")
                   )
                 )
                 
               ),
               
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("plot", plotOutput("permillplot")),
                 tabPanel("table", dataTableOutput("permillDT"))
               ))
             )
    ),
    
    
    tabPanel("Descriptive Statistics",
             
             fluidPage(
               fluidRow(
                 column(10,
                        h1("Descriptive Statistics")),
                 column(2,
                        actionButton(inputId = "helpbutton2", label = icon(name = "question-circle")),
                        uiOutput("HelpBox2")
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("dem", "Demographic", c("Race", "Gender", "Age")),
                 h6("Disclaimer: Please take note that the data we are currently using is still a work in progress so some of the data is missing. This means that there is a possibility that the trends displayed aren't the true trends for the data."),
                 actionButton(inputId = "helpbuttonsmall3", label = icon(name = "question-circle")),
                 uiOutput("HelpBoxsmall3")
               ),
               mainPanel(dataTableOutput("dstbl"), plotOutput("dsplt"))
             )
    )
  ),
  
  
  navbarMenu("Maps",
             
             tabPanel("Choropleth",
                      
                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Choropleth Map of Deaths per Capita")),
                          column(2,
                                 actionButton(inputId = "helpbutton3", label = icon(name = "question-circle")),
                                 uiOutput("HelpBox3")
                          )
                        )
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
                        fluidRow(
                          column(10,
                                 h1("Cartogram of Deaths per Capita")),
                          column(2,
                                 actionButton(inputId = "helpbutton4", label = icon(name = "question-circle")),
                                 uiOutput("HelpBox4")
                          )
                        )
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
                        fluidRow(
                          column(10),
                          column(2,
                                 actionButton(inputId = "helpbutton5", label = icon(name = "question-circle")),
                                 uiOutput("HelpBox5")
                          )
                        ),
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
  
  output$HelpBox1 = renderUI({
    if (input$helpbutton1 %% 2 == 1){
      helpText("View trends by state over the years
               2000 to 2017. Plot and table tabs 
               allow you to switch between viewing
               the data in a line plot or in a table.")
    }else{
      return()
    }
  })
  
  output$HelpBox2 = renderUI({
    if (input$helpbutton2 %% 2 == 1){
      helpText("Displays total counts of fatal 
               encounters in the US by demographic
               (race, age, or gender).")
    }else{
      return()
    }
  })
  
  output$HelpBox3 = renderUI({
    if (input$helpbutton3 %% 2 == 1){
      helpText("Map displays distribution of fatal
               events by state. States that are 
               darker have more deaths per capita.
               Hovering over a state displays
               the state's name and number of
               fatal events per capita.")
    }else{
      return()
    }
  })
  
  output$HelpBox4 = renderUI({
    if (input$helpbutton4 %% 2 == 1){
      helpText("Map displays distribution of fatal
               events by state. States that are 
               darker and bigger have more deaths 
               per capita.")
    }else{
      return()
    }
  })
  
  output$HelpBox5 = renderUI({
    if (input$helpbutton5 %% 2 == 1){
      helpText("Map displays counts based on region 
               clicking on bubbles or zooming in
               breaks bubbles into smaller areas.
               At lowest level individual cases
               are showed and can be clicked on to
               display more info in a pop-up.")
    }else{
      return()
    }
  })
  
  output$HelpBoxsmall1 = renderUI({
    if (input$helpbuttonsmall1 %% 2 == 1){
      helpText("Selected state is colored red
               with the other states displayed
               in gray and the US average in 
               black. (US average is only 
               shown for per capita values)")
    }else{
      return()
    }
  })
  
  output$HelpBoxsmall2 = renderUI({
    if (input$helpbuttonsmall2 %% 2 == 1){
      helpText("When selected values are 
               calculated as a number of
               fatal events per million
               people in the population.
               Otherwise displays total
               number of fatal events.")
    }else{
      return()
    }
  })
  
  output$HelpBoxsmall3 = renderUI({
    if (input$helpbuttonsmall3 %% 2 == 1){
      helpText("Trends will differ based on
               if the data is missing at 
               random (meaning that each group
               is just as likely to have been 
               marked as unspecified) or not. 
               If not the trends will change.")
    }else{
      return()
    }
  })
  }

shinyApp(ui, server)