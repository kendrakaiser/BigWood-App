conn=scdbConnect()
#dbGetQuery(conn,"SELECT * FROM metrics;")

useLocations=dbGetQuery(conn, "SELECT locationid, name FROM locations WHERE locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID' );")
useMetrics=dbGetQuery(conn, "SELECT metricid, name, isprediction FROM metrics WHERE metrics.name IN ('irrigation season volume (april 1 - september 31)', 'simulated irrigation season volume (april 1 - september 31)');")

query=paste0("SELECT metric, value, locationid, simnumber FROM data WHERE data.metricid IN ('",
             paste0(useMetrics$metricid,collapse="', '"),
             "') AND data.locationid IN ('",
             paste0(useLocations$locationid,collapse="', '"),"');")

useData=dbGetQuery(conn,query)
useData=merge(useData,useMetrics,by.x="metric",by.y="name")

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
p<-ggplot(useData, aes(x=site, y=value/1000, fill=isprediction), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90"), labels=c('Historic', 'Modeled'), name="") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(useData$value, na.rm=TRUE), by = 50),1))+
  ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
  xlab("")+
  ylab("Irrigation Season Volume (KAF)") +
  theme_bw()


#list icons:
file_text <- readr::read_file(
  paste0(.libPaths()[1], 
         "/leaflet/htmlwidgets/plugins/Leaflet.awesome-markers/font-awesome.min.css")
)

iconList=data.frame(iconName <- stringr::str_extract_all(file_text, "(fa-)([^:]+)")[[1]])



plotAll=function(plotData=dbGetQuery(conn,"SELECT data.metric, data.value, data.datetime, data.locationid FROM data INNER JOIN locations ON data.locationid = locations.locationid WHERE data.metric IN ('Water Temperature', 'Dissolved Oxygen', 'flow') AND ST_Within(locations.geometry, ST_Polygon('LINESTRING (734457.9 4797348, 727968.2 4797127, 727687.9 4805448, 734169.6 4805669, 734457.9 4797348)'::geometry, 26911) ) AND data.datetime > '2021-03-01' AND data.datetime < '2021-11-01';")){
  
  plotData=addRepFromLocationIDs(plotData)
  
  xmin=min(plotData$datetime)
  xmax=max(plotData$datetime)
  metrics=unique(plotData$metric)
  #set up plot area:
  firstLine=plotData[plotData$metric==metrics[1],]
  firstLine=firstLine[firstLine$locationid==firstLine$locationid[1],]
  leftMargin=3*length(metrics)
  
  par(mar=c(5.1,leftMargin,4.1,2.1))
  plot(firstLine$value~firstLine$datetime,type="n",xlim=c(xmin,xmax),yaxt="n",ylab="",xlab="Date")
  
  for(metric in metrics){
  
    
    #sort by date  
  }
}