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
end_date <<-as.Date("2022-08-01") # this will be changed to using the sys.date after testing
ww<-getwd()
# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    
    # Landing Page
    tabPanel("Big Wood River Streamflow and Water Quality Dashboard",
             tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
               tags$meta(name="viewport", content="initial-scale=1")),
                setBackgroundColor("white"), 
             splitLayout(cellWidths = c("45%", "55%"), cellArgs = list(style='white-space: normal;'),
             tags$img(class = 'image', height="98%", width="98%", src="silvercreekSquare.jpg", align = "left", style="border:30px solid white"), 
             tags$div(class = "text-block", # load CSS .text-block (style and positioning)
                      tags$h1("Big Wood River Streamflow and Water Quality Tools")), # Title
             tags$div(class="landing-block",
                      p(class="lp_text","The Big Wood River Dashboard is an interactive set of tool to visualize 
                                observational data and modeling ouput in the Big Wood River Basin and Silver Creek."),
                      p(class='lp_text', "This integrates data from a range of sources and provides timely information that may be used
                              to inform water management in the basin. Use the toolbar at the top of the page to select the data or information category of interest 
                              and you’ll be directed to a dynamic graph for visualization."),
                      p(class="lp_text","The Big Wood Streamflow Tools provide real-time forecasts of the irrigation season streamflow volumes on the Big Wood, Camas Creek, and Silver Creek. 
                              The Water Quality Tools are focused on stream health in Silver Creek as it pertains to the trout fishery. 
                              The data explorer allows you to dig into the datasets behind these models, and explore changes over time."))
             )),

    # Streamflow Tools
    tabPanel("Streamflow Forecasts",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 width = 4, style = 'height:80vh; font-size:1.2vh',
                 strong('Streamflow Forecasting Output', style = 'font-size:2.5vh'), 
                 br(), br(),
                 p('The streamflow forecasting suite predicts total irrigation season runoff volume, "center of mass", and timing of delivery calls
                   in the Big Wood River Basin (above Magic), Camas Creek and Silver Creek.', style = "font-size:1.5vh"),
                 p('', style = "font-size:1.5vh"),
                 br(), div(class = "intro-divider3"), br(),
                 ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 br(), br(),
                 plotOutput("big_vols", width = "80%"),
                 p('Figure 1: These box plots show the historic range of irrigation season volume (blue) and the predicted range of volumes (grey) that were calculated for each gage. 
                   The boxes represent the 25th - 75th percentiles, the median is the solid line in the middle, and circles are outliers.', style = "font-size:1.5vh"),
                 br(), br(),
                 plotOutput("sc_vols", width = "30%"),
                 p('Figure 2: Box plots of Silver Creek historic and forecasted streamflow')
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
               mainPanel()
               ))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$big_vols <- renderPlot({readRDS("www/sampled_volumes.rds")})
  output$sc_vols <- renderPlot({readRDS(file.path(paste0("www/sampled_sc_vol-", end_date, ".rds")))})
  
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
