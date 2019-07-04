#BM: it's nice for other users if we auto-install missing packages
#for them
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
                   "devtools", "maps", "sp", "maptools",
                  "tmap", "cartogram", "DT", "dplyr") )

# seems like we need to have library calls so that shinyapps.io can detect what pkgs to
# install
library("shiny")
library("here")
library("plotly")
library("leaflet")
#library("devtools")
library("maps")
library("sp")
library("maptools")
#library("tmap")
#library("cartogram")
library("DT")
library("dplyr")
#library("tidyverse")
#library("shinyWidgets")


# BM: docs say that "the directory that you save server.R in
# will become the working directory of your Shiny app.
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
# so we need to source from here
source("R/permillcalculation.R")
source("R/permilltablefunc.R")
source("R/permillgraphfunc.R")
source("R/choroplethmapfunc.R")
source("R/descstatfuncs.R")
source("R/choroplethmapfunc.R")
source("R/interactivemap.R")


#-------------------------------------------------------UI-----------------------------------------------------------------#

ui <- navbarPage(title = "FFSG", id = "navbar",

  #About Page
  tabPanel(title = "About", value = "tab1",
           fluidPage(fluidRow(
             column(10,
                    h1("Fatal Force Study Group")),
             column(2,
                    icon('question-circle', class='fa-2x helper-btn'),
                    tags$div(class="helper-box", style="display:none",
                             p('Upload a file of observed network data (must be of a supported type).',
                               'Add custom attributes or symmetrize on the "Edit Network" tab.')),
                    actionLink('abtleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                    actionLink('abtright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

             )
             )),
           sidebarLayout(
             sidebarPanel(

               h3("Data Resources"),
               uiOutput("felink"),
               #h6("This data source has been collected since 2000 and is active until present day. As of the month of April there have been a total of 19,856 number of cases that are recorded. This database allows you to go in and download any data needed and also includes visualizations."),
               uiOutput("kbplink"),
               actionLink("moredata", "More"),

               h3("Aditional Information")
             ),
             mainPanel(
               h2("About"),
               h4("UW Fatal Force Research Group (FFRG) was brought together at the University of Washington by Professor Martina Morris and Ben Marwick. Morris' background in sociology and statistics led her to creating this research group to fight injustice in police using fatal force. Ben Marwick, an Archeology professor, with a background in statistics and social science joined Morris as a side project. This research group started about a year and half ago with two students of Morris. The group has now expanded to seven undergraduate students, two from Western Washington University, with the addition of the two UW Professors. UW FFRG's mission is to bring justice and peace to communities most impacted by police brutality through a comprehensive data analysis combined with the comparisons of respective laws and policies."),
               h2("Washington Policies"),
               h4("In the state of Washington, De-Escalate Washington Initiative 940 was introduced to initiate officer training and community safety. Because of the amount of deaths by police that happened in the state of Washington action was called. I-940 required training on mental illness, violence de-escalation, and first aid. It also required that the communities stakeholders be involved in any policy making. Community stakeholders include persons with disabilities; members of the lesbian, gay, bisexual, transgender, and queer community; persons of color; immigrants; non-citizens; native Americans; youth; and formerly incarcerated persons."),
               h4("On March 8, 2018, Washington state legislature voted on I-940 with the inclusion of ESHB 3003 to come to an agreement on how to further build trust back into the communities. With ESHB 3003, both sides agreed that there needs to be a clearer meaning of good faith. Together with I-940 and ESHB 3003 resulted in requiring violence de-escalation and mental health training. Require first aid training for all officers and require that police render first aid at the earliest safe opportunity. Removes the de facto immunity and adopts a reasonable officer standard. Requires completely independent investigations of use of deadly force. Requires notification of Tribal governments where a tribal person was injured or killed. Brings diverse community stakeholders into the process for input on policy."),
               h4("On April 20, 2018, Judge Christine Schaller of Thurston County, WA ordered state legislature to put I-940 back on the November 2018 ballot. Time Eyman argued that the passing of ESHB 3003 was rush and,  disrespect[ed] initiative signers and prevent[ed] voters from exercising their right to vote. Since wording and phrases were changed from the original initiative it went against Washington state's constitution stated that it must be passed with such wording or it should be sent to the ballot.")
             )
           )
  ),

  #Tab for Tables and Graphs
  navbarMenu(
    "Tables and Graphs",

    #Page for counts and per capita values of Fatal Encounters
    tabPanel(title = "Counts", value = "tab2",

             fluidPage(
               fluidRow(
                 column(10,
                        h1("Counting Fatal Encounters")),
                 column(2,
                          icon('question-circle', class='fa-2x helper-btn'),
                          tags$div(class="helper-box", style="display:none",
                                 p("View trends by state over the years
                                    2000 to 2017. Plot and table tabs
                                    allow you to switch between viewing
                                    the data in a line plot or in a table.")),
                        actionLink('cntleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                        actionLink('cntright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                 )
               )
             ),

             sidebarLayout(
               sidebarPanel(
                 selectInput("state", "State", c(sort(c(state.name, "District of Columbia")), "United States"), selected = "Washington"),
                 checkboxInput("all", "Display with other states", FALSE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                                   p("Selected state is colored red
                                    with the other states displayed
                                     in gray and the US average in
                                     black. (US average is only
                                     shown for per capita values)")),

                 checkboxInput("capita", "Calculate per capita (in millions)", TRUE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                                   p("When selected values are
                                     calculated as a number of
                                     fatal events per million
                                     people in the population.
                                     Otherwise displays total
                                     number of fatal events."))
               ),
               #Creates plot and table tabs so user can view data in either form
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("plot", plotOutput("permillplot")),
                 tabPanel("table", dataTableOutput("permillDT"))
               ))
             )
    ),

    #Page for stats based on demographics: Race, Gender, Age
    tabPanel(title= "Descriptive Statistics", value="tab3",

             fluidPage(
               fluidRow(
                 column(10,
                        h1("Descriptive Statistics")),
                 column(2,
                        icon('question-circle', class='fa-2x helper-btn'),
                        tags$div(class="helper-box", style="display:none",
                            p("Displays total counts of fatal
                              encounters in the US by demographic
                              (race, age, or gender).")),
                        actionLink('dsleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                        actionLink('dsright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                 )
               )
             ),

             sidebarLayout(
               sidebarPanel(
                 selectInput("dem", "Demographic", c("Race", "Gender", "Age")),
                 h6("Disclaimer: Please take note that the data we are currently using is still a work in progress so some of the data is missing. This means that there is a possibility that the trends displayed aren't the true trends for the data."),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                          p("Trends will differ based on
                            if the data is missing at
                            random (meaning that each group
                            is just as likely to have been
                            marked as unspecified) or not.
                            If not the trends will change."))
               ),
               mainPanel(dataTableOutput("dstbl"), plotOutput("dsplt"))
             )
    )
  ),

  #Tab for Maps
  navbarMenu("Maps",

             #Choropleth Page
             tabPanel(title = "Choropleth", value = "tab4",

                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Choropleth Map of Deaths per Capita")),
                          column(2,
                                 icon('question-circle', class='fa-2x helper-btn'),
                                 tags$div(class="helper-box", style="display:none",
                                          p("Map displays distribution of fatal
                                            events by state. States that are
                                            darker have more deaths per capita.
                                            Hovering over a state displays
                                            the state's name and number of
                                            fatal events per capita.")),
                                 actionLink('cpleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                 actionLink('cpright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                          )
                        )
                      ),

                      sidebarLayout(
                        sidebarPanel(
                          #displays mean and when selected shows values by year
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

             #Cartogram Page
             tabPanel(title = "Cartogram", value = "tab5",

                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Cartogram of Deaths per Capita")),
                          column(2,
                                 icon('question-circle', class='fa-2x helper-btn'),
                                 tags$div(class="helper-box", style="display:none",
                                          p("Map displays distribution of fatal
                                            events by state. States that are
                                            darker and bigger have more deaths
                                            per capita.")),
                                 actionLink('crgleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                 actionLink('crgright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                          )
                        )
                      ),

                      sidebarLayout(
                        sidebarPanel(
                          #displays by year and allows for animation when play button is hit
                          sliderInput("yearcart", "Year", 2000, 2017, value = 2010, animate = animationOptions(1500, TRUE),  sep = "") # BM: make the numbers look like years
                        ),
                        mainPanel(plotOutput("cartogram"))
                      )
             ),


             #Interactive Map Page
             tabPanel(title = "Interactive", value = "tab6",
                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Interactive Map")),
                          column(2,
                                 icon('question-circle', class='fa-2x helper-btn'),
                                 tags$div(class="helper-box", style="display:none",
                                          p("Map displays counts based on region
                                            clicking on bubbles or zooming in
                                            breaks bubbles into smaller areas.
                                            At lowest level individual cases
                                            are showed and can be clicked on to
                                            display more info in a pop-up.")),
                                 actionLink('intleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                 actionLink('intright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                          )
                        ),
                        leafletOutput("intmap")
                      )
             )
  ),

  #THIS IS TAB PANEL PURGATORY
  #tabPanel("Data Analytics"),
  #
  #tabPanel("Data Compiling"),
  inverse = TRUE,
  tags$head(
    tags$script(type="text/javascript", src="alert.js"),
    tags$link(rel="stylesheet", type="text/css",href="style.css")
  )


)

#-------------------------------------------------------Server----------------------------------------------------------#

server <- function(input, output, session) {

  #update active tab in navbar when arrows are clicked
  leftarrowclicks <- reactive({
    input$abtleft+input$cntleft+input$dsleft+input$cpleft+input$crgleft+input$intleft
  })
  rightarrowclicks <- reactive({
    input$abtright+input$cntright+input$dsright+input$cpright+input$crgright+input$intright
  })
  observe({
    if(leftarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
  })
  observe({
    if(rightarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
  })

  #Outputs for tables, plots, and maps
  #Plot for fatal encounter total or capita values by state
  output$permillplot <-
    renderPlot({
      permillgraph(input$state, input$all, input$capita) #From: "permillgraphfunc.R"
    })
  #Data table for fatal encounter total or capita values by state
  output$permillDT <-
    renderDataTable({
      permilltable(input$state, input$all, input$capita) #From: "permilltablefunc.R"
    })
  #Choropleth Map
  output$choropleth <-
    renderPlotly({
      if(input$yearselect){
        choroplethmap(paste("p", input$year, sep="")) #From: "choroplethmapfunc.R"
      }else{
        choroplethmap()
      }
    })
  #Interactive Leaflet Map
  output$intmap <-
    renderLeaflet({
      interactivemap #From: "interactivemap.R"
    })
  #Cartogram Map
  output$cartogram <- renderImage({
    filename <- normalizePath(file.path(paste('data/cartogramplots/cart', input$yearcart, '.jpg', sep="")))
    list(src = filename)
  }, deleteFile = FALSE) #Static Jpegs from "data/cartogramplots" folder. Update using "updatecart.R" in folder
  #Data Table for demographics (Race, Gender, Age)
  output$dstbl <- renderDataTable({
    dstable(input$dem) #From: "descstatfuncs.R"
  })
  #Plot for demographics (Race, Gender, Age)
  output$dsplt <- renderPlot({
    dsplot(input$dem) #From: "descstatfuncs.R"
  })

  output$felink <- renderUI({
    tagList(a("Fatal Encounters", href="http://www.fatalencounters.org"), "- This data source has been collected since 2000 and is active until present day. As of the month of April there have been a total of 19,856 number of cases that are recorded. This database allows you to go in and download any data needed and also includes visualizations.")
  })

  output$kbplink <- renderUI({
    tagList(a("Killed by Police", href="http://killedbypolice.net"), "- An open sourced data collection from an online anonymous source that dates back to May 1, 2013. The data set is still in continuation and the legitimacy of each data point is confirmed through actual online news articles of each fatality. Killed By Police has a number of 4,629 cases recorded.")
  })
}

shinyApp(ui, server)
