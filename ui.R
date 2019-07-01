library(leaflet) # to render the leaflet map
library(shinythemes) # to chamg the color of shiny dashboard
library(shinydashboard) # inbuit dashboard
library(plotly) # to render interactive maps
library(dygraphs)# time series graph
# drop down values for time stamp

time <- c(
    "None" =5,
    "6:00 AM-12:00 PM(Morning)" = 1,
    "12:00 PM-6:00 PM(Afternoon)" = 2,
    "6:00 PM-12:00AM(Evening)" = 3,
    "12:00AM- 6:00AM(Midnight)" =4
    
)
# value fro parameter selection in fare prediction
para <- c(
    "Distance"=1 ,
    "Travel time"=2
)
# type of smoother
smooth <- c(
    
  
    "Linear Model"="lm" , 
    "Genralized Linear Model"="glm", 
    "Genralized Additive Model"="gam"
    
)
# to analyse trend in various factors
polar<-c(
    
    "Average Travel Time per trip" =1,
    "Average Fare per trip"=2,
    "Average Number Of Trips in 2 months"=3,
    "Average Distance in (miles) per day"=4
)
dashboardPage(
    skin = "green",
    dashboardHeader(title = 'NYC Green Taxi', titleWidth = 450),
    dashboardSidebar(
        width = 250
        # side bar menu
        ,sidebarMenu(
            menuItem("Queens", tabName = "Queens", icon = icon("car")),
            menuItem("Brooklyn", tabName = "Brooklyn", icon = icon("car")),
            menuItem("Manhattan", tabName = "Manhattan", icon = icon("car")),
            menuItem("Bronx", tabName = "Bronx", icon = icon("car")),
            menuItem("Overview", tabName = "Overview")
        )
    ),
    dashboardBody(
        # for each tab list the sub-tab elements
        tabItems( 
            ####################
            tabItem(tabName = "Queens",
                    tags$style(HTML(".box.box-solid.box-danger>.box-header {
                            background:#006400;
                            }
                            .box.box-solid.box-danger{
                            border-bottom-color:#006400;
                            border-left-color:#006400;
                            border-right-color:#006400;
                            border-top-color:#006400;
                            }"))
                    
                    ,fluidRow( # title for each tab
                        tabBox(tabPanel(title = "Average Taxi-Trips per year", 
                                fluidRow(
                                    column(7,leafletOutput("map",height="700px" )), # leaflet plot
                                    column(5,box(width=12,background = "blue",selectInput("TimeStamp", "TimeStamp", time, selected = "None"),
                                    tableOutput("table")), # to hold the dataframe value
                                    box(width=12,title="Trend Of taxi Trips Over the years",
                                    plotlyOutput("barchart") # to render the bar plot
                                    ))))
                               ,tabPanel(title = "Traffic Analysis on day", 
                                         fluidRow(dygraphOutput("timeseries",height="500px"), # time series grpah
                                                  box(width=6,valueBoxOutput("box2Q",width = 12)),
                                                  box(width=6,valueBoxOutput("box2q2",width = 12))
                                                  )
                                         )
                               ,tabPanel(title="Fare Prediction",
                                fluidRow(
                                    column(7,plotOutput("farepredictionq")),
                                    column(5, # input slector for time stamp,paramters and smoothers
                                           box(width=12,background = "blue",selectInput("TimeStamp_q", "TimeStamp", time, selected = "None"),
                                               selectInput("Parameters", "Parameters", para, selected = 1), 
                                               selectInput("Smoothers", "Smoothers",smooth, selected = "lm")
                                               
                                           ))
                                ),
                                fluidRow(
                                    box(width=6,sliderInput("integer", "FARE STIMATION BASED ON DISTANCE(Distance in Miles):",
                                                min = 1, max = 80,
                                                value = 10)),
                                    box(width=6,valueBoxOutput("boxfairpred",width = 12)) # show the predicted fare
                                    
                                )
                               )
                               , width=12)))
            
        
          ,#end of tab one  
          tabItem(tabName = "Brooklyn",
                  # define style tag
                  tags$style(HTML(".box.box-solid.box-danger>.box-header {
                            background:#006400;
                            }
                            .box.box-solid.box-danger{
                            border-bottom-color:#006400;
                            border-left-color:#006400;
                            border-right-color:#006400;
                            border-top-color:#006400;
                            }"))
                  ,fluidRow(
                      tabBox(tabPanel(title = "Average Taxi-Trips per year", 
                                      fluidRow(
                                          column(7,leafletOutput("mapb",height="700px" )), # render leaflet
                                          column(5,box(width=12,background = "blue",selectInput("TimeStampb", "TimeStamp", time, selected = "None"),
                                                       tableOutput("tableb")), # render datframe output
                                                 box(width=12,title="Trend Of taxi Trips Over the years",
                                                     plotlyOutput("barchartb") # plot the interactive bar plot
                                                 ))))
                             ,tabPanel(title = "Traffic Analysis on day", 
                                       fluidRow(dygraphOutput("timeseriesb",height="500px"), # time serie sgraph
                                                box(width=6,valueBoxOutput("box2Qb",width = 12)), # hold time formaximum trip count
                                                box(width=6,valueBoxOutput("box2q2b",width = 12)) # hold time for mimum trip count
                                       )
                             )
                             ,tabPanel(title="Fare Prediction",
                                       fluidRow(
                                           column(7,plotOutput("farepredictionqb")), # scatter plot
                                           column(5, # filters
                                                  box(width=12,background = "blue",selectInput("TimeStamp_qb", "TimeStamp", time, selected = "None"),
                                                      selectInput("Parametersb", "Parameters", para, selected = 1),
                                                      selectInput("Smoothersb", "Smoothers",smooth, selected = "lm")
                                                      
                                                  ))
                                       ),
                                       fluidRow( # fare prediction using the value selected on silder
                                           box(width=6,sliderInput("integerb", "FARE STIMATION BASED ON DISTANCE(Distance in Miles):",
                                                                   min = 1, max = 80,
                                                                   value = 10)),
                                           box(width=6,valueBoxOutput("boxfairpredb",width = 12))
                                           
                                       )
                             )
                             , width=12))),
          ####Tab for bronx        
          tabItem(tabName = "Bronx",
                  tags$style(HTML(".box.box-solid.box-danger>.box-header {
                            background:#006400;
                            }
                            .box.box-solid.box-danger{
                            border-bottom-color:#006400;
                            border-left-color:#006400;
                            border-right-color:#006400;
                            border-top-color:#006400;
                            }"))
                  ,fluidRow(
                      tabBox(tabPanel(title = "Average Taxi-Trips per year", 
                                      fluidRow(
                                          column(7,leafletOutput("mapbro",height="700px" )), #leaflet map
                                          column(5,box(width=12,background = "blue",selectInput("TimeStampbro", "TimeStamp", time, selected = "None"),
                                                       tableOutput("tablebro")), # datframe holder
                                                 box(width=12,title="Trend Of taxi Trips Over the years",
                                                     plotlyOutput("barchartbro") # interactive bar plot
                                                 ))))
                             ,tabPanel(title = "Traffic Analysis on day",  
                                       # time series graph
                                       fluidRow(dygraphOutput("timeseriesbro",height="500px"),
                                                box(width=6,valueBoxOutput("box2Qbro",width = 12)),
                                                box(width=6,valueBoxOutput("box2q2bro",width = 12))
                                       )
                             )
                             ,tabPanel(title="Fare Prediction",
                                       fluidRow(
                                           column(7,plotOutput("farepredictionqbro")),
                                           column(5, # slect smoother type and other paramter against which scatter plot must be done
                                                  box(width=12,background = "blue",selectInput("TimeStamp_qbro", "TimeStamp", time, selected = "None"),
                                                      selectInput("Parametersbro", "Parameters", para, selected = 1),
                                                      selectInput("Smoothersbro", "Smoothers",smooth, selected = "lm")
                                                      
                                                  ))
                                       ),
                                       fluidRow(
                                           box(width=6,sliderInput("integerbro", "FARE STIMATION BASED ON DISTANCE(Distance in Miles):",
                                                                   min = 1, max = 80,
                                                                   value = 10)),
                                           box(width=6,valueBoxOutput("boxfairpredbro",width = 12)) # predicted fare value in valuebox based  on the above slider input
                                           
                                       )
                             )
                             , width=12))),
          ############# Tab for Manhattan
          tabItem(tabName = "Manhattan",
                  tags$style(HTML(".box.box-solid.box-danger>.box-header {
                            background:#006400;
                            }
                            .box.box-solid.box-danger{
                            border-bottom-color:#006400;
                            border-left-color:#006400;
                            border-right-color:#006400;
                            border-top-color:#006400;
                            }"))
                  ,fluidRow(
                      tabBox(tabPanel(title = "Average Taxi-Trips per year", 
                                      fluidRow(
                                          column(7,leafletOutput("mapmah",height="700px" )), # leaflet plot
                                          column(5,box(width=12,background = "blue",selectInput("TimeStampman", "TimeStamp", time, selected = "None"),
                                                       tableOutput("tableman")),
                                                 box(width=12,title="Trend Of taxi Trips Over the years",
                                                     plotlyOutput("barchartman") # interactive bar plot
                                                 ))))
                             ,tabPanel(title = "Traffic Analysis on day", 
                                       # value box to hold tme at which taxi traffic is mimimum and maximum time 
                                       fluidRow(dygraphOutput("timeseriesman",height="500px"),
                                                box(width=6,valueBoxOutput("box2Qman",width = 12)),
                                                box(width=6,valueBoxOutput("box2q2man",width = 12))
                                       )
                             )
                             ,tabPanel(title="Fare Prediction",
                                       fluidRow(
                                           column(7,plotOutput("farepredictionqman")),
                                           column(5, # filter 
                                                  box(width=12,background = "blue",selectInput("TimeStamp_qman", "TimeStamp", time, selected = "None"),
                                                      selectInput("Parametersman", "Parameters", para, selected = 1),
                                                      selectInput("Smoothersman", "Smoothers",smooth, selected = "lm")
                                                      
                                                  ))
                                       ),
                                       fluidRow( # slider to select distance
                                           box(width=6,sliderInput("integerman", "FARE STIMATION BASED ON DISTANCE(Distance in Miles):",
                                                                   min = 1, max = 80,
                                                                   value = 10)), # corresponding predicted value of fare for distnace
                                           box(width=6,valueBoxOutput("boxfairpredman",width = 12))
                                           
                                       )
                             )
                             , width=12)))  
          # Overview Tab
          , tabItem(tabName = "Overview",
                    # to get the circukar bar plot
                    fluidRow(h2("Trend across boroughs throughout years"),column(8,plotOutput("circular",height="700px")),
                column(4,box(background ="blue",width=12,selectInput("polar", "Apply Filters To check Trend", polar, selected = 3))))
                
               
          )
          )
        )
        

        
        
        
    )
        
        
        
        
