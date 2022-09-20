#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# Sys.setenv(scdb_readPass="dbread")
#    http://shiny.rstudio.com/
#

library(shiny)
library(RPostgres)
library(DBI)
library(ggplot2)
library(stringr)
source('functions.r')

conn=scdbConnect()
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Big Wood River Streamflow Tools"),
  
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
      print(dbGetQuery(conn,"SELECT name FROM metrics WHERE metricid = '1'"))
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
    #input$year -- this is actually a date range right now
    #uselocations - I would hard code this for a given DO site for now or just plot data from all DO sensors-- we prob wont want the full list of locations in a drop down?
    
    #query=paste0("SELECT metric, value, locationid, simnumber FROM data WHERE data.metricid IN ('",
     #            paste0(usemetric$metricid,collapse="', '"),
      #           "') AND data.locationid IN ('",
       #          paste0(useLocations$locationid,collapse="', '"),"');")
    
    #useData=dbGetQuery(conn,query)
    
    #ggplot(useData)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
