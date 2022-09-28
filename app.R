#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# Sys.setenv(scdb_readPass="dbread")
#    http://shiny.rstudio.com/

library(shiny)
library(RPostgres)
library(DBI)
library(ggplot2)
library(stringr)
library(leaflet)
library(shinyWidgets)
source('functions.r')

conn=scdbConnect()
# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    
    # Landing Page
    tabPanel("Big Wood River Streamflow Tools",
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
               tags$meta(name="viewport", content="initial-scale=1")
             ),
             setBackgroundColor("lightgrey"), 
             tags$img(src="silvercreekPano.jpg", align = "right"), #class="bg", 
             tags$div(class = "text-block", # load CSS .text-block (style and positioning)
                      tags$h1("Big Wood River"), # Title
                      tags$h1("Streamflow and Water Quality Tools")),
             tags$div(class="landing-block",
                      p(class="lp_text","The Big Wood River Dashboard is an interactive set of tool to visualize 
                                observational data and modeling ouput in the Big Wood River Basin and Silver Creek"),
                      p(class='lp_text', "This integrates data from a range of sources and provides timely information that may be used
                              to inform water management in the basin"),
                      p(class="lp_text","Use the toolbar at the top of the page to select the data or information category of interest 
                              and you’ll be directed to a dynamic graph for visualization.The Big Wood Streamflow Tools provide 
                              real-time forecasts of the irrigation season streamflow volumes on the Big Wood, Camas Creek, and Silver Creek. 
                              The Water Quality Tools are focused on stream health in Silver Creek as it pertains to the trout fishery. 
                              The data explorer allows you to dig into the datasets behind these models, and explore changes over time.")
                      ))),
    # Streamflow Tools
    tabPanel("Big Wood River Streamflow Tools",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("year",
                                "Date Range",
                                start = "2020-10-01",
                                end = "2021-10-01",
                                min = "1990-10-01",
                                max = "2022-10-01"),
                 
                 selectInput(
                   "variable",
                   "Select Variable:",
                   choices= dbGetQuery(conn,"SELECT name FROM metrics;"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = TRUE,
                   width = NULL,
                   size = NULL
                 )),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("predPlot"),
                 plotOutput("varPlot"),
               ))),
    
    tabPanel("Water Quality Data Explorer",
             
             sidebarLayout(
               sidebarPanel(
                 h4("Select Variable(s):"),
                 selectInput(
                   "wqVars",
                   NULL,
                   choices= dbGetQuery(conn,"SELECT name FROM metrics;"),
                   selected = NULL,
                   multiple = FALSE,
                   selectize = TRUE,
                   width = NULL,
                   size = NULL
                 ),
                 h4("Select Data Extent:"),
                 leafletOutput("dataExtentMap",width="auto",height="250px")
               ),
               
               mainPanel( 
                 
                 
                 
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #this will all go in the ui side for user to select timescale (stream flow model output will be static though)
  useLocations=dbGetQuery(conn, "SELECT locationid, name FROM locations WHERE locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID' );")
  useMetrics=dbGetQuery(conn, "SELECT metricid, name, isprediction FROM metrics WHERE metrics.name IN ('irrigation season volume (april 1 - september 31)', 'simulated irrigation season volume (april 1 - september 31)');")
  
  query=paste0("SELECT metric, value, locationid, simnumber FROM data WHERE data.metricid IN ('",
               paste0(useMetrics$metricid,collapse="', '"),
               "') AND data.locationid IN ('",
               paste0(useLocations$locationid,collapse="', '"),"');")
  
  useData=dbGetQuery(conn,query)
  useData=merge(useData,useMetrics,by.x="metric",by.y="name")
  
  
  output$predPlot <- renderPlot({
    
    # is there a better way to do this? 
    useData$site<- "sitename"
    useData$site[which(useData$locationid == 140 & useData$isprediction == "FALSE")]<- c("Big Wood Hailey Hist")
    useData$site[which(useData$locationid == 140 & useData$isprediction == "TRUE")]<- c("Big Wood Hailey")
    useData$site[which(useData$locationid == 141 & useData$isprediction == "FALSE")]<- c("Big Wood Stanton Hist")
    useData$site[which(useData$locationid == 141 & useData$isprediction == "TRUE")]<- c("Big Wood Stanton")
    useData$site[which(useData$locationid == 167 & useData$isprediction == "FALSE")]<- c("Camas Creek Hist")
    useData$site[which(useData$locationid == 167 & useData$isprediction == "TRUE")]<- c("Camas Creek")
    useData$site<- factor(useData$site, levels = c("Big Wood Hailey Hist","Big Wood Hailey", "Big Wood Stanton Hist", "Big Wood Stanton", "Camas Creek Hist", "Camas Creek" ), ordered = TRUE)
    
    #plot the figure; we're going to have so many figures, need to consider how to make the app.R script not too cumbersome, for static plots you could save elsewhere (like here) and call the name
    ggplot(useData, aes(x=site, y=value/1000, fill=isprediction), alpha=0.6) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_fill_manual(values=c("royalblue3", "grey90"), labels=c('Historic', 'Modeled'), name="") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      scale_y_continuous(breaks = round(seq(0, max(useData$value, na.rm=TRUE), by = 50),1))+
      ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
      xlab("")+
      ylab("Irrigation Season Volume (KAF)") +
      theme_bw()
  })
  
  # generate plot from the input variable and date range
  output$varPlot<- renderPlot({
    usemetric = dbGetQuery(conn,"SELECT name FROM metrics WHERE name = 'Dissolved Oxygen';") #input$variable - make sure this output works in the query
    #input$year
    #query=paste0("SELECT metric, value, locationid, simnumber FROM data WHERE data.metricid IN ('",
    #            paste0(usemetric$metricid,collapse="', '"),
    #           "') AND data.locationid IN ('",
    #          paste0(useLocations$locationid,collapse="', '"),"');")
    
    #useData=dbGetQuery(conn,query)
    
    #ggplot(useData)
  })
  
  dataExtentMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  
  dataExtentMap = setView(map=dataExtentMap,lng=-114.15,lat=43.33,zoom=12)
  
  dataExtentMap = addWMSTiles(map=dataExtentMap,baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
                              layers=0,
                              attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
                              tileOptions(zIndex=1))
  output$dataExtentMap = renderLeaflet(dataExtentMap)
  
  
  observeEvent(input$dataExtentMap_bounds, {
    print(input$dataExtentMap_bounds)
    print(names(input))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
