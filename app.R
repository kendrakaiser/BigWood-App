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
library(sf)

library(shinyWidgets)

source('functions.r')

conn=scdbConnect()

end_date <<-as.Date("2022-08-01") # this will be changed to using the sys.date after testing

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    
    # Landing Page
    tabPanel("Overview",
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
                 img(class = 'image', height="75%", width="75%", src="hist_explanation.eps", align = "center", style="border:10px solid white"),
                 p('Streamflow forecasts are shown as boxplots in omparison to the range of historical conditions. The exceedance probabilities align with the Northwest River Forecasting Center probabilities for comparison. The median forecasted streamflow volume for each gage and the exceednace probabilities are shown in the table.')
               ),
               
               
               # Show a plot of the generated distribution
               mainPanel(
                 br(), br(),
                 p('Streamflow Forecast for', print(Sys.Date())), #check to make sure this works, or alt way??
                 tableOutput("forecasted_vols"), 
                 p('Forecasted irrigation season streamflow volumes with exceedance probabilities, these probabilities are aligned with the Northwest River Forecasting Center for comparison purposes.'),
                 br(), br(),
                 plotOutput("big_vols", width = "80%"),
                 p('Figure 1: These box plots show the historic range of irrigation season volume (blue) and the predicted range of volumes (grey) that were calculated for each gage. 
                   The boxes represent the 25th - 75th percentiles, the median is the solid line in the middle, and circles are outliers.', style = "font-size:1.5vh"),
                 br(), br(),
                 plotOutput("sc_vols", width = "30%"),
                 p('Figure 2: Box plots of Silver Creek historic and forecasted streamflow'),
               ))
    ),
    
    tabPanel("Stream Temperature Forecasts",
             sidebarLayout(
               sidebarPanel(
                 h4("Input Parameters"),
                 h5("Time of Year"),
                 h6("Time of year is used to identify historical distributions of air temperature and streamflow values, and to determine day length for use in the forecasting process.  Set the time of year below:"),
                 sliderInput(inputId = "tf_date",
                             label=NULL,
                             value=as.Date("2000-07-01"),
                             min=as.Date("2000-05-01"),
                             max=as.Date("2000-10-30"),
                             timeFormat="%m/%d"),
                 
                 
                 h5("Air Temperature"),
                 h6(textOutput('tf_airTempDisplay')),
                 fluidRow(align="center",
                          plotOutput('airTempHighsHist', height="80px",width="100%"),
                          # uiOutput("tf_maxAirTempSlider")
                          sliderInput(inputId = "tf_maxAirTemp",
                                      label=NULL,
                                      value=80,
                                      min=40,
                                      max=110,
                                      ticks=FALSE,
                                      width="99%"
                          )
                 ),
                 
                 h5("Streamflow"),
                 h6(textOutput("tf_flowDisplay")),
                 
                 fluidRow(align="center",
                          plotOutput('flowForecastHist', height="80px",width="100%"),   
                          sliderInput(inputId = "tf_indexFlow",
                                      label=NULL,
                                      value=100,
                                      min=20,
                                      max=200,
                                      ticks=FALSE,
                                      width="100%"
                          )
                          
                          
                 )
               ),
               
               mainPanel(
                 
                 
               )
               
             )
    ),
    
    tabPanel("Data Explorer",
             
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
                   start=as.Date("2021-06-01"),
                   end=as.Date("2021-07-01"),
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
               
               
               
               mainPanel( 
                 plotOutput("dataPlot",height="500px")
                 
                 
               )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$big_vols <- renderPlot({readRDS("www/sampled_volumes.rds")})
  output$sc_vols <- renderPlot({readRDS(file.path(paste0("www/sampled_sc_vol-", end_date, ".rds")))})
  output$forecasted_vols <- renderTable(readRDS("www/ex.vols.rds"), digits = 0)
  
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
        #plot(plotData$value~plotData$datetime)
        plotAll(plotData)
      }
    )
    
  })
  
  observe({
    locationPoints=getLocationsForVariables(useVars=input$plotVars,
                                            startDate=input$plotDateRange[1],
                                            endDate=input$plotDateRange[2])
    if(!is.null(locationPoints)){
      allIcons=getAllIcons(locationDF=locationPoints)
      #print(allIcons)
      #print(head(locationPoints))
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
  })# end of observe for locationPoints
  
  
  tf_airTemps=reactive({getAirTempsByDate(input$tf_date)})
  
  output$tf_airTempDisplay=renderText({paste0("The average high air temperature at the Picabo AgriMet station for ",
                                              format.Date(input$tf_date,"%B "), as.numeric(format.Date(input$tf_date, "%d"))," is ",round(mean(tf_airTemps()),1),
                                              "°F, and the distribution of observed daily high temperatures for this date are shown in the plot below.  Use the slider beneath this plot to set the air temperature for the simulation:")
  })
  
  output$airTempHighsHist=renderPlot({
    
    tf_airT_DisplayMin=round(min(tf_airTemps()-10),digits=0)
    tf_airT_DisplayMax=round(max(tf_airTemps()+10),digits=0)
    #side effects on sliderInput
    updateSliderInput(inputId = "tf_maxAirTemp",min=tf_airT_DisplayMin,max=tf_airT_DisplayMax)
    
    par(bg="transparent")
    par(oma=c(0,0,0,0))
    par(mar=c(2,0,0,0))
    hist(x=tf_airTemps(),
         xlim=c(tf_airT_DisplayMin, tf_airT_DisplayMax),
         xlab=NULL,ylab=NULL,
         axes=F,
         breaks=seq(from=0,to=120,by=2.5),
         main=NULL
    )
    axis(side=1)
  })
  
  tf_indexFlows=reactive({getIndexFlowsByDate(input$tf_date)})
  
  output$tf_flowDisplay=renderText({paste0("The state of streamflow in Silver Creek is described in terms of the flow at the Sportsmans Gauge.  The average streamflow at Sportsmans for ",
                                           format.Date(input$tf_date,"%B "), as.numeric(format.Date(input$tf_date, "%d"))," is ",round(mean(tf_indexFlows()),1)," cfs, and the distribution of observed streamflows for this date are shown in the plot below.  Use the slider beneath this plot to set the streamflow for the simulation:")
  })
  
  output$flowForecastHist=renderPlot({
    minForecastFlow=round(min(tf_indexFlows())*.5,digits=0)
    maxForecastFlow=round(max(tf_indexFlows())*1.25,digits=-1)
    
    updateSliderInput(inputId = "tf_indexFlow",min=minForecastFlow,max=maxForecastFlow)
    
    par(bg="transparent")
    par(oma=c(0,0,0,0))
    par(mar=c(2,0,0,0))
    hist(x=tf_indexFlows(),
         xlim=c(minForecastFlow, maxForecastFlow),
         xlab=NULL,ylab=NULL,
         axes=F,
         breaks=seq(from=0,to=500,by=10),
         main=NULL
    )
    axis(side=1)
  })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
