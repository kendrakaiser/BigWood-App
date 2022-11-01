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
<<<<<<< HEAD
    tabPanel("Big Wood River Streamflow Tools",
             
             #tags$head(
            #   tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
            #   tags$meta(name="viewport", content="initial-scale=1")
            # ),
             setBackgroundColor("lightgrey"), 
             tags$img(class="bg", src="silvercreekSquare.jpg", align = "left"), #class="bg", 
             tags$div(class = "text-block", # load CSS .text-block (style and positioning)
                      tags$h1("Big Wood River"), # Title
                      tags$h1("Streamflow and Water Quality Tools")),
             tags$div(class="landing-block",
                      p(class="lp_text","The Big Wood River Dashboard is an interactive set of tool to visualize 
                                observational data and modeling ouput in the Big Wood River Basin and Silver Creek"),
                      p(class='lp_text', "This integrates data from a range of sources and provides timely information that may be used
=======
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
>>>>>>> a89396f67bd8adf3f3b28b9aa64abad72a4f56f5
                              to inform water management in the basin. Use the toolbar at the top of the page to select the data or information category of interest 
                              and you’ll be directed to a dynamic graph for visualization."),
                                  p(class="lp_text","The Big Wood Streamflow Tools provide real-time forecasts of the irrigation season streamflow volumes on the Big Wood, Camas Creek, and Silver Creek. 
                              The Water Quality Tools are focused on stream health in Silver Creek as it pertains to the trout fishery. 
<<<<<<< HEAD
                              The data explorer allows you to dig into the datasets behind these models, and explore changes over time.")
             )
             
    ),
=======
                              The data explorer allows you to dig into the datasets behind these models, and explore changes over time."))
             )),
    
>>>>>>> a89396f67bd8adf3f3b28b9aa64abad72a4f56f5
    # Streamflow Tools
    tabPanel("Streamflow Forecasts",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
<<<<<<< HEAD
                 dateRangeInput("year",
                                "Date Range",
                                start = "2020-10-01",
                                end = "2021-10-01",
                                min = "1990-10-01",
                                max = "2022-10-01"),
=======
                 width = 4, style = 'height:80vh; font-size:1.2vh',
                 strong('Streamflow Forecasting Output', style = 'font-size:2.5vh'), 
                 br(), br(),
                 p('The streamflow forecasting suite predicts total irrigation season runoff volume, "center of mass", and timing of delivery calls
                   in the Big Wood River Basin (above Magic), Camas Creek and Silver Creek.', style = "font-size:1.5vh"),
                 p('', style = "font-size:1.5vh"),
                 br(), div(class = "intro-divider3"), br(),
>>>>>>> a89396f67bd8adf3f3b28b9aa64abad72a4f56f5
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 br(), br(),
                 plotOutput("predPlot"),
                 p('Figure 1: These box plots show the historic range of irrigation season volume (blue) and the predicted range of volumes (grey) that were calculated for each gage. 
                   The boxes represent the 25th - 75th percentiles, the median is the solid line in the middle, and circles are outliers.', style = "font-size:1.5vh")
               ))),
    
    tabPanel("Water Quality Data Explorer",
             
             sidebarLayout(
               sidebarPanel(
                 h4("Select Variable(s):"),
                 selectInput(
                   "plotVars",
                   NULL,
                   choices= dbGetQuery(conn,"SELECT name FROM metrics;"),
                   selected = NULL,
                   multiple = T,
                   selectize = TRUE,
                   width = NULL,
                   size = NULL
                 ),
                 h4("Select Date Range"),
                 dateRangeInput(
                   inputId="plotDateRange",
                   label=NULL,
                   start=as.Date("2021-03-01"),
                   end=as.Date("2021-11-01"),
                   min=minDateTime,
                   max=maxDateTime,
                   format="M-yyyy",
                   startview = "year"
                   #minview="months",
                   #maxview = "decades"
                 ),
                 #use updateSliderInput to limit range?
                 h4("Select Data Extent:"),
                 leafletOutput("plotExtent",width="auto",height="300px"),
                 actionButton("makePlot", "Make Plot")
               ),
               
<<<<<<< HEAD
               mainPanel())
=======
               
               
               mainPanel( 
                 plotOutput("dataPlot")
                 
                 
               )
             )
>>>>>>> a89396f67bd8adf3f3b28b9aa64abad72a4f56f5
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
    useData$site[which(useData$locationid == 140 & useData$isprediction == "TRUE")]<- c("Big Wood Hailey Pred")
    useData$site[which(useData$locationid == 141 & useData$isprediction == "FALSE")]<- c("Big Wood Stanton Hist")
    useData$site[which(useData$locationid == 141 & useData$isprediction == "TRUE")]<- c("Big Wood Stanton Pred")
    useData$site[which(useData$locationid == 167 & useData$isprediction == "FALSE")]<- c("Camas Creek Hist")
    useData$site[which(useData$locationid == 167 & useData$isprediction == "TRUE")]<- c("Camas Creek Pred")
    useData$site<- factor(useData$site, levels = c("Big Wood Hailey Hist","Big Wood Hailey Pred", "Big Wood Stanton Hist", "Big Wood Stanton Pred", "Camas Creek Hist", "Camas Creek Pred" ), ordered = TRUE)
    
    #plot the figure; we're going to have so many figures, need to consider how to make the app.R script not too cumbersome, for static plots you could save elsewhere (like here) and call the name
    ggplot(useData, aes(x=site, y=value/1000, fill=isprediction), alpha=0.6) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_fill_manual(values=c("royalblue3", "grey90"), labels=c('Historic', 'Predicted'), name="") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
      scale_y_continuous(breaks = round(seq(0, max(useData$value, na.rm=TRUE), by = 50),1))+
      ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
      xlab("")+
      ylab("Irrigation Season Volume (KAF)") +
      theme_bw()+
      theme(axis.text=element_text(size=15), axis.title = element_text(size = 20))
  })
  

  #map for defining data viz extent
  
  dataExtentMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  
  dataExtentMap = setView(map=dataExtentMap,lng=-114.15,lat=43.33,zoom=12)
  
  dataExtentMap = addWMSTiles(map=dataExtentMap,baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
                              layers=0,
                              attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
                              tileOptions(zIndex=1))

  output$plotExtent = renderLeaflet(dataExtentMap)
  
  
  
  
  observeEvent(input$makePlot,{
    plotData=getDataByVarTimeExtent(useVars=input$plotVars,
                                    startDateTime=input$plotDateRange[1],
                                    endDateTime = input$plotDateRange[2],
                                    extent=input$plotExtent_bounds)
    #print(head(plotData))
    
    output$dataPlot=renderPlot(
      if(nrow(plotData)>1){
        plot(plotData$value~plotData$datetime)
      }
    )
    
  })
  
  observe({
    
    
    locationPoints=getLocationsForVariables(useVars=input$plotVars,
                                            startDate=input$plotDateRange[1],
                                            endDate=input$plotDateRange[2])
    
    if(!is.null(locationPoints)){
 
      
      allIcons=getAllIcons(locationDF=locationPoints)
      leafletProxy("plotExtent") %>% 
        clearGroup("dataLocationPoints") %>%
        addAwesomeMarkers(
          data=locationPoints,
          group="dataLocationPoints",
          icon=allIcons[locationPoints$iconName],
          popup=~sitenote)
      
    } else {
      leafletProxy("plotExtent") %>% 
        clearGroup("dataLocationPoints")
    }
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
