#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
# Sys.setenv(scdb_readPass="")

library(shinythemes)
library(shiny)
library(RPostgres)
library(DBI)
library(ggplot2)
library(stringr)
library(leaflet)
library(sf)
library(shinyWidgets)

source('functions.r')
source('plotter.R') 
conn=scdbConnect()

base_path <- normalizePath(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "."))
setwd(base_path)

end_date <<-as.Date("2022-08-01") # this will be changed to using the sys.date after testing

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  tabsetPanel(
    
    # Landing Page
    tabPanel("Overview",
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
               tags$meta(name = "viewport", content = "initial-scale=1")
             ),
             setBackgroundColor("white"),
             fluidPage(
               splitLayout(
                 cellWidths = c("50%", "50%"),
                 cellArgs = list(style = 'white-space: normal;'),
                 tags$img(
                   class = 'image',
                   height = "98%",
                   width = "98%",
                   src = "silvercreekSquare.jpg",
                   align = "left",
                   style = "border: 10px solid #ECF0F1; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);"
                 ),
                 tags$div(
                   # class = "text-block",
                   # style = "margin-top: -20px; background-color: transparent; box-shadow: 2px 4px 10px black; border-radius: 5px; padding: 5%; box-sizing: border-box;",
                   tags$h1(
                     "Big Wood River Streamflow and Water Quality Tools",
                     style = "color: #2980B9; font-family: 'Helvetica Neue', Arial, sans-serif; font-size: 3.5vw; font-weight: bold; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.2); background-color: transparent; border-radius: 5px; word-wrap: break-word; max-width: 100%;"
                   )
                 )
               ),
               tags$div(
                 class = "landing-block",
                 style = "background-color: white; font-size: 2.5vh; text-align: center; margin-top: -20px; margin-left: 35px; box-shadow: 2px 4px 10px black;",
                 p(class = "lp_text", "The Big Wood River Dashboard is an interactive set of tools to visualize observational data and modeling output in the Big Wood River Basin and Silver Creek."),
                 p(
                   class = "lp_text",
                   "This integrates data from a range of sources and provides timely information that may be used to inform water management in the basin. Use the toolbar at the top of the page to select the data or information category of interest, and you'll be directed to a dynamic graph for visualization."
                 ),
                 p(
                   class = "lp_text",
                   "The Big Wood Streamflow Tools provide real-time forecasts of the irrigation season streamflow volumes on the Big Wood, Camas Creek, and Silver Creek. The Water Quality Tools are focused on stream health in Silver Creek as it pertains to the trout fishery. The data explorer allows you to dig into the datasets behind these models and explore changes over time."
                 )
               )
             )
    ),
    
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

                 img(
                   class = 'image',
                   style = "max-width: 100%; height: auto; border: 10px solid light grey; margin-right: 30px;",
                   src = "hist_explanation.png",
                   align = "center"
                 ),
                 p(
                   'Streamflow forecasts are shown as boxplots in comparison to the range of historical conditions. The exceedance probabilities align with the Northwest River Forecasting Center probabilities for comparison. The median forecasted streamflow volume for each gage and the exceedance probabilities are shown in the table.',
                   style = "background-color: #ECF0F1; box-shadow: 2px 4px 10px black; border: 1px solid black; padding: 5px"
                 )
               ),
               
               
               # Show a plot of the generated distribution
               mainPanel(
                 br(),
                 p('Streamflow Forecast for', print(Sys.Date()), style = "font-weight: bolder; font-size: 20px;"), #check to make sure this works, or alt way??
                 tableOutput("forecasted_vols"), 
                 p('Forecasted irrigation season streamflow volumes with exceedance probabilities, these probabilities are aligned with the Northwest River Forecasting Center for comparison purposes.'),
                 br(),
                 #plotOutput("big_vols", width = "80%"),
                 div(

                   img(class = 'image', height = '75%', width = '75%', src = 'sampled_vol_bw.png', 
                     align = 'center', style="border:10px solid white", alt="historical streamflow volume"),
                     style = "display: flex; justify-content: space-between;"
                 ),
                 p('Figure 1: These box plots show the historic range of irrigation season volume (blue) and the predicted range of volumes (grey) that were calculated for each gage. 
                   The boxes represent the 25th - 75th percentiles, the median is the solid line in the middle, and circles are outliers.', style = "font-size:1.5vh"),
                 br(),
                 div(
                   img(class = 'image', height = '75%', width = '75%', src = 'sampled_vol_cc.png', 
                       align = 'center', style="border:10px solid white", alt="historical streamflow volume"),
                   img(class = 'image', height = '75%', width = '75%', src = 'sampled_vol_sc.png', 
                       align = 'center', style="border:10px solid white", alt="historical streamflow volume"),
                   style = "display: flex; justify-content: space-between;"
                 ),
                 #plotOutput("sc_vols", width = "30%"),
                 p('Figure 2: Box plots of Silver Creek and Camas Creek historic and forecasted streamflow'),
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
                             value=as.Date("2000-06-01"),
                             min=as.Date("2000-05-01"),
                             max=as.Date("2000-09-30"),
                             timeFormat="%m/%d"),
                 
                 
                 h5("Air Temperature"),
                 h6(textOutput('tf_airTempDisplay')),
                 # fluidRow(align="center",
                 #          plotOutput('airTempHighsHist', height="80px",width="100%"),
                 #          # uiOutput("tf_maxAirTempSlider")
                 #          sliderInput(inputId = "tf_maxAirTemp",
                 #                      label=NULL,
                 #                      value=80,
                 #                      min=40,
                 #                      max=110,
                 #                      ticks=FALSE,
                 #                      width="99%"
                 #          )
                 # ),
                 
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
                          
                          
                 ),
                 actionButton("mapTemperature","Forecast Stream Temperatures")
               ),
               
               mainPanel(
                 leafletOutput("temperatureMap",height="500px"),
                 plotOutput("tf_summaryPlot")
                 
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


########---------Server side.  Note that the server side instanc3e is shared between multiple applications (UIs), so avoid use of UI-specific variables here.
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
  
  ############### data explorer ---------
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
  
  
  ############# temperature forecasting tool -----------
  
  temperatureModel=readRDS(paste0(getwd(),"/www/temperatureModel.rds"))
  
  tf_airTemps=reactive({getAirTempsByDate(input$tf_date)})
  tf_maxAirTemps=reactive({getAirTempsByDate(input$tf_date,high=T)})
  
  output$tf_airTempDisplay=renderText({paste0("Daily average average air temperatures at the Picabo AgriMet station for ",
                                              format.Date(input$tf_date,"%B "), as.numeric(format.Date(input$tf_date, "%d"))," range from ",
                                              round(min(tf_airTemps())),"°F to ", round(max(tf_airTemps())),"°F.  Highs for this day range from ",
                                              round(min(tf_maxAirTemps())),"°F to ",round(max(tf_maxAirTemps())),"°F.  This forecast simulates a hot but not unusual day with an average temperature of ",
                                              round(quantile(tf_airTemps(),.9)),"°F and a high temperature of ",round(quantile(tf_maxAirTemps(),.9)),"°F.")

  })
  
  # output$airTempHighsHist=renderPlot({
  #   
  #   tf_airT_DisplayMin=round(min(tf_airTemps()-10),digits=0)
  #   tf_airT_DisplayMax=round(max(tf_airTemps()+10),digits=0)
  #   #side effects on sliderInput
  #   updateSliderInput(inputId = "tf_maxAirTemp",min=tf_airT_DisplayMin,max=tf_airT_DisplayMax)
  #   
  #   par(bg="transparent")
  #   par(oma=c(0,0,0,0))
  #   par(mar=c(2,0,0,0))
  #   hist(x=tf_airTemps(),
  #        xlim=c(tf_airT_DisplayMin, tf_airT_DisplayMax),
  #        xlab=NULL,ylab=NULL,
  #        axes=F,
  #        breaks=seq(from=0,to=120,by=2.5),
  #        main=NULL
  #   )
  #   axis(side=1)
  # })
  
  tf_indexFlows=reactive({getIndexFlowsByDate(input$tf_date)})
  tf_predFlows=reactive({getPredictedFlowsByDate(input$tf_date)})
  
  tf_stats=reactive({getTemperatureStats(tf_indexFlows = tf_indexFlows(), tf_predFlows = tf_predFlows(), airTemp = quantile(tf_airTemps(),.9), forecastDate=tf_date(), streamSegs = streamSegs, temperatureModel = temperatureModel)})
  
  output$tf_flowDisplay=renderText({paste0("Streamflow in Silver Creek is described in terms of the flow at the Sportsmans Gauge.  The predicted streamflow at Sportsmans for ",
                                           tf_predFlows()$date," is ",round(tf_predFlows()$sc.med)," cfs (90% confidence interval: ",
                                           round(tf_predFlows()$sc.low)," - ",round(tf_predFlows()$sc.hi)," cfs ), and the historical distribution of streamflow for this day is shown in the plot below.  Use the slider beneath this plot to set the streamflow for the simulation:")
  })
  
  # output$tf_flowDisplay=renderText({paste0("Streamflow in Silver Creek is described in terms of the flow at the Sportsmans Gauge.  The average streamflow at Sportsmans for ",
  #                                          format.Date(input$tf_date,"%B "), as.numeric(format.Date(input$tf_date, "%d"))," is ",round(mean(tf_indexFlows()),1)," cfs, and the historical distribution of streamflow for this day is shown in the plot below.  Use the slider beneath this plot to set the streamflow for the simulation:")
  # })
  
  output$flowForecastHist=renderPlot({
    minForecastFlow=round(min(tf_indexFlows())*.8,digits=0)
    maxForecastFlow=round(max(tf_indexFlows())*1.2,digits=-1)
    maxForecastFlow=min(maxForecastFlow,300)
    
    updateSliderInput(inputId = "tf_indexFlow",value=round(tf_predFlows()$sc.med),min=minForecastFlow,max=maxForecastFlow)
    
    
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
  
  #temperatures map
  tf_indexFlow=reactive(input$tf_indexFlow)
  tf_maxAirTemp=reactive(input$tf_maxAirTemp)
  tf_date=reactive(input$tf_date)
  
  g=1
  streamSegs= st_transform(st_read(conn,query="SELECT segid, geometry, length FROM streamsegments;"),4326)
  streamSegs=streamSegs[!streamSegs$segid %in% c(1308, 1629),] #two short segs with inflated uaa (and therefore eronious flow) due to raster->vector issues
  streamSegs$color=("darkblue")
  
  #segTemperatures=reactive(predictSegTemperatures(indexFlow = tf_indexFlow(), meanAirTemp_F = round(quantile(tf_airTemps(),.9)), forecastDate=tf_date(), streamSegs=streamSegs, tempModel=temperatureModel) )
  
  
  temperatureMap = leaflet( leafletOptions(leafletCRS(crsClass="L.CRS.EPSG4326")) )
  
  temperatureMap = setView(map=temperatureMap,lng=-114.13,lat=43.31,zoom=11)
  
  temperatureMap = addWMSTiles(map=temperatureMap,baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSImageryOnly/MapServer/WmsServer",
                               layers=0,
                               attribution = 'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>',
                               tileOptions(zIndex=1))
  
  temperatureMap=addPolylines(map=temperatureMap,data=streamSegs,color="darkgray", label="",opacity=1,group=paste0("g",g), weight=3)
  
  
  temperatureMap=addLegend(map=temperatureMap,position="topright",pal=getTemperatureColor,values=50:72,opacity=1)
  
  output$temperatureMap = renderLeaflet(temperatureMap)
  
  ##update temperature map and plot on click:
  observeEvent(input$mapTemperature,{
    
    segTemperatures=predictSegTemperatures(indexFlow = isolate(tf_indexFlow()), meanAirTemp_F = isolate(round(quantile(tf_airTemps(),.9))), forecastDate=isolate(tf_date()), streamSegs=streamSegs, tempModel=temperatureModel)
    
    leafletProxy("temperatureMap", data=segTemperatures) %>% 
      addPolylines(label=round(segTemperatures$temperature_F,1),color=segTemperatures$color,opacity=1,group = paste0("g",g+1))
    #clearGroup(paste0("g",g))  #add new layer first, then remove old one.  g is inherited from parent environment and never sent back
    
    
    output$tf_summaryPlot=renderPlot({
      old_mar=par("mar")
      
      par(mar=old_mar+c(0,0,0,2))
      plot(isolate(tf_stats()$meanSegTemp)~isolate(tf_stats()$indexFlow), type="l",col="red4", lwd=2,axes=F,ylab="", xlab = "Index Flow", main=paste0("Stream Temperatures vs. Flow for ", isolate(tf_predFlows()$date)), ylim=c(55,70))
      axis(side=1,at=seq(from=0,to=300,by=50))
      axis(side=2,col="red4", lwd=2)
      mtext("Average Stream Temperature", side=2, line=3, col="red4")
      par(new=T)
      plot(isolate(tf_stats()$pctAbove65)~isolate(tf_stats()$indexFlow), type="l", axes=F, ylab="",xlab="",col="seashell4",lwd=2,lty=2,ylim=c(0,100))
      axis(side=4,col="seashell4",lwd=2,lty=2)
      mtext("Percent of network above 65°F", side = 4, line=3, col="seashell4")
      abline(v=isolate(tf_indexFlow()))
      
      par(mar = old_mar)
      
    }, width=500, height=350)
    
  })
  

  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
