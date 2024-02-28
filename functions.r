scdbConnect=function(){
  conn=dbConnect(RPostgres::Postgres(),
                 # host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                 # port="25060",
                 # dbname="silvercreekdb" ,
                 # user="dbread",
                 # password="dbread"
                 
                 host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                 port="25061",
                 dbname="shinyPool" ,
                 user="dbread",
                 password="dbread"
                 
  )
  return(conn)
}

conn=scdbConnect()

#minDateTime=as.Date(dbGetQuery(conn,"SELECT MIN (datetime) FROM data;")$min)
#maxDateTime=as.Date(dbGetQuery(conn,"SELECT MAX (datetime) FROM data;")$max)

currentModelDate=dbGetQuery(conn,"SELECT MAX(rundate) FROM summarystatistics;")$max

minDateTime=as.Date("1911-01-01")
maxDateTime=Sys.Date()

tf_predFlowData=dbGetQuery(conn,'SELECT date, "sc.low", "sc.med", "sc.mean", "sc.hi" FROM predictionintervals;')
tf_predFlowData$doy=as.numeric(format.Date(tf_predFlowData$date, "%j"))


#dbGetQuery(conn,"SELECT DISTINCT ST_SRID(geometry) FROM locations;")$st_srid
#dbGetQuery(conn,"SELECT DISTINCT name FROM metrics;")

#dbGetQuery(conn,"SELECT * FROM locations LIMIT 1;")

getDataByVarTimeExtent=function(useVars,startDateTime,endDateTime,extent){
  #convert to proper epsg, make postgis extent object, return from data join metrics where IN vars and WITHIN extent
  #avoid writing to db to avoid permission issues?
  
  
  extBox = st_bbox(c(xmin=extent$east,xmax=extent$west,ymax=extent$north,ymin=extent$south),crs=st_crs(4326))
  extBox=st_as_sfc(extBox)
  extBox=st_transform(extBox,st_crs(26911))
  #this is stupid, but...
  extBox=st_cast(extBox,'LINESTRING',flatten=T)
  
  
  
  query=paste0("SELECT data.metric, data.value, data.datetime, data.locationid FROM data ",
               "INNER JOIN locations ON data.locationid = locations.locationid ",
               "WHERE data.metric IN ('",paste0(useVars,collapse="', '"),"') ",
               "AND ST_Within(locations.geometry, ST_Polygon('",
               print(extBox[[1]]),"'::geometry, 26911) ) ",
               "AND data.datetime > '",startDateTime,
               "' AND data.datetime < '",endDateTime,"'
               AND data.qcstatus='true';")
  
  #print(query)
  
  thisData=dbGetQuery(conn,query)
  
  #print(format(object.size(thisData),units="MB"))
  
  return(thisData)
}


getLocationsForVariables=function(useVars,startDate=as.Date("2021-03-01"),endDate=as.Date("2021-11-01")){
  if(length(useVars)>=1){
    
    locationQuery=paste0("SELECT DISTINCT locations.locationid, locations.name, locations.sitenote, locations.geometry, data.metric ",
                         "FROM locations LEFT JOIN data ON locations.locationid = data.locationid WHERE data.metric IN ('",
                         paste0(useVars,collapse="', '"),"')",
                         " AND data.datetime > '",startDate,
                         "' AND data.datetime < '",endDate,"'
                         AND data.qcstatus='true';")
    
    #print(locationQuery)
    locations=st_read(conn,query = locationQuery)
    
    if(nrow(locations)>=1){
      
      locations=addRepFromLocationIDs(locations)
      
      locations$iconName=paste0(locations$metric," ",locations$rep)
      
      return(st_transform(locations,st_crs(4326)))
    } else return(NULL)
  } else return(NULL)
  
}


addRepFromLocationIDs=function(df,maxReps=length(globalColorOrder)){
  if(!"rep" %in% names(df)){
    
    df$rep=(df$locationid %% maxReps) +1
    #this (below) might be better because it considers only the locations that are 
    #in the current dataset, but it breaks when the map and plot have a different dataset
    # locationIDs=unique(df$locationid)
    # locationsToReps=data.frame(locations=locationIDs[order(locationIDs)])
    # locationsToReps$rep=1:nrow(locationsToReps)
    # locationsToReps$rep=(locationsToReps$rep %% maxReps) +1
    # df=merge(df,locationsToReps,by.x="locationid",by.y="locations",all.x=T)
  }
  return(df)
}


buildIcon=function(metric="",rep=1,colorOrder=globalColorOrder){
  thisIcon="beer"
  if(metric == "Water Temperature"){
    thisIcon="thermometer"
  }
  if(metric == "Dissolved Oxygen"){
    thisIcon="cloud"
  }
  if(metric == "flow"){
    thisIcon="bathtub"
  }
  thisColor=colorOrder[rep]
  #print(paste0("icon:",thisIcon,", rep:",rep,", color:",thisColor))
  
  return( makeAwesomeIcon(icon=thisIcon,markerColor = thisColor,library = "fa",iconColor = "black") )
}


getAllIcons=function(locationDF){
  locationDF=addRepFromLocationIDs(locationDF)
  allIcons=eval(
    parse(
      text=paste0("awesomeIconList(",
                  paste0("'",locationDF$iconName,"' = buildIcon('",locationDF$metric,"', ",locationDF$rep,")",collapse=", "),
                  ")")
    )
  ) 
  return(allIcons)
}

globalColorOrder=c("red", "orange", "green", "blue", "purple", "pink", "gray", "cadetblue", "lightgreen", "lightblue")

#defaults are for easy development and debug
plotAll=function(plotData=dbGetQuery(conn,"SELECT data.metric, data.value, data.datetime, data.locationid FROM data INNER JOIN locations ON data.locationid = locations.locationid WHERE data.metric IN ('flow', 'Water Temperature', 'Dissolved Oxygen') AND ST_Within(locations.geometry, ST_Polygon('LINESTRING (749004.1 4786269, 733100.8 4785705, 732814.7 4794040, 748698.5 4794604, 749004.1 4786269)'::geometry, 26911) ) AND data.datetime > '2021-06-01' AND data.datetime < '2021-07-01' AND data.qcstatus='T';")){
  
  plotData=addRepFromLocationIDs(plotData)
  
  xmin=min(plotData$datetime)
  xmax=max(plotData$datetime)
  metrics=unique(plotData$metric)
  #set up plot area:
  firstLine=plotData[plotData$metric==metrics[1],]
  firstLine=firstLine[firstLine$locationid==firstLine$locationid[1],]
  
  leftMargin=4*length(metrics)
  bottomMargin=3+length(metrics)
  par(mar=c(bottomMargin,leftMargin,2,0))
  
  plot(firstLine$value~firstLine$datetime,type="n",xlim=c(xmin,xmax),yaxt="n",ylab="",xlab="")
  
  metricLty=1:length(metrics)
  
  axisLine=0
  
  for(metric in metrics){
    thisPlotData=plotData[plotData$metric==metric,]
    
    par(new=T)
    plot(firstLine$value~firstLine$datetime,type="n",xlim=c(xmin,xmax),ylim=c(min(thisPlotData$value),max(thisPlotData$value)),axes=F,ylab="",xlab="")
    
    
    for(location in unique(thisPlotData$locationid)){
      thisLineData=thisPlotData[thisPlotData$locationid==location,]
      thisLineData=thisLineData[order(thisLineData$datetime),]
      globalColorOrder[thisLineData$rep[1]]
      lines(thisLineData$value~thisLineData$datetime,col=globalColorOrder[thisLineData$rep[1]],lty=metricLty[metrics==metric],lwd=2)
      
    }
    
    axis(side=2,line=axisLine)
    mtext(text=metric, side=2, line=axisLine+2,font=2)
    axisLine=axisLine+4
    
  }
  par(xpd=T)
  par(new=T)
  par(mar=c(.1,2,2,2))
  plot.new()
  
  legend(x="bottom",legend=rev(metrics),lty=rev(metricLty),lwd=2,ncol=2,bty="n")
}

getAirTempsByDate=function(tf_date,high=F, airTempReferenceLocation=180){
  doy=format.Date(tf_date,format="%j")
  if(high){
    airTemps=dbGetQuery(conn, paste0("SELECT max(value) AS high FROM data WHERE locationid = '",airTempReferenceLocation,"'
                          AND metric = 'air temperature'
                          AND DATE_PART('doy',datetime) = '",doy,"'
                          AND qcstatus='true'
                             GROUP BY datetime::date;"))$high
  } else {
    airTemps=dbGetQuery(conn, paste0("SELECT avg(value) AS avg FROM data WHERE locationid = '",airTempReferenceLocation,"' 
                          AND metric = 'air temperature' 
                          AND DATE_PART('doy',datetime) = '",doy,"'
                          AND qcstatus='true'
                             GROUP BY datetime::date;"))$avg
  }
  
  #exclude occasional unreasonable values:
  airTemps=airTemps[airTemps>0]
  airTemps=airTemps[airTemps<120] #climate change compatable????
  return(airTemps)
}

getIndexFlowsByDate=function(tf_date, indexFlowLocation=144){
  doy=format.Date(tf_date,format="%j")
  indexFlows=dbGetQuery(conn, paste0("SELECT avg(value) AS flow FROM data WHERE locationid = '",indexFlowLocation,"' 
                          AND metric = 'flow' 
                          AND DATE_PART('doy',datetime) = '",doy,"'
                          AND qcstatus = 'true'
                             GROUP BY datetime::date;"))$flow
  return(indexFlows)
}

getPredictedFlowsByDate=function(tf_date, predictedFlows=tf_predFlowData){
  doy=as.numeric(format.Date(tf_date,"%j"))
  return(predictedFlows[predictedFlows$doy==doy,])
}

# getTemperatureModel=function(){
#   temperatureModel=readRDS("/home/sam/Documents/R Workspace/SilverCreekQualityModel/temperatureModel.rds")
#   return(temperatureModel)
# }
#saveRDS(getTemperatureModel(),paste0(getwd(),"/www/temperatureModel.rds"))


getTemperatureColor=colorNumeric( palette=viridis::turbo(60), domain = c(49, 72), na.color = "#7A0403")


predictSegTemperatures=function(indexFlow, meanAirTemp_F, forecastDate, streamSegs, tempModel=temperatureModel){
  maxSunAngleFun=function(doy){
    43+23.45*sin((2*pi/360)*(360/365)*(doy-81))
  }
  
  
  
  
  meanAirTemp = 5/9*(meanAirTemp_F - 32)

  forecastDate=as.Date(forecastDate)
  forecastDOY=as.numeric(format.Date(forecastDate, "%j"))
  
  segTemperatures=dbGetQuery(conn,paste0("SELECT specificresidencetoseg AS sr, meanresidencetoseg AS residence, flow, segid FROM residences WHERE indexflow = '",indexFlow,"';"))
  names(segTemperatures)[names(segTemperatures)=="sr"] = "sR"
  
  segTemperatures=merge(streamSegs, segTemperatures, by="segid")
  segTemperatures$maxSunElevation=maxSunAngleFun(forecastDOY)
  #segTemperatures$maxAirTemp=maxAirTemp
  segTemperatures$meanAirTemp=meanAirTemp
  segTemperatures$indexFlow=indexFlow
  #  print(names(segTemperatures))
  
  segTemperatures$temperature_F=( predict(tempModel,newdata=segTemperatures) * (9/5) ) + 32 
  
  
  #segTemperatures$temperature_F=  as.numeric( ( predict(tempModel,newdata=segTemperatures) * (9/5) ) + 32 )
  
  #print(max(segTemperatures$temperature_F))
  
  
  segTemperatures$color=getTemperatureColor(c(segTemperatures$temperature_F))
  
  segTemperatures=st_transform(segTemperatures,4326)
  
  return(segTemperatures)
  
}

getTemperatureStats=function(tf_indexFlows,tf_predFlows, airTemp, forecastDate, streamSegs, temperatureModel){
  
  getSegTempStats=function(indexFlow, airTemp, forecastDate, streamSegs, tempModel){
    segTemps=predictSegTemperatures(indexFlow,airTemp,forecastDate,streamSegs=streamSegs, tempModel = tempModel)
    return(data.frame(date=forecastDate,airTemp=airTemp,indexFlow=indexFlow,meanSegTemp=mean(segTemps$temperature_F),lengthAbove65=sum((segTemps$temperature_F>65)*segTemps$length)))
  }
  
  allFlows=seq(from=round(min(c(tf_indexFlows*.8,tf_predFlows$sc.low))), to=round(max(c(tf_indexFlows*1.2,tf_predFlows$sc.hi))), by=10)
  tf_stats=do.call(rbind,lapply(allFlows,getSegTempStats,airTemp=airTemp,forecastDate=forecastDate,streamSegs=streamSegs,tempModel=temperatureModel))
  tf_stats$pctAbove65=100 * (tf_stats$lengthAbove65 / 101221.3)
  return(tf_stats)
  
}





#the following should add minview and maxview args to daterange, but has a broken dependency
# dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
#   d <- shiny::dateRangeInput(inputId, label, ...)
#   d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
#   d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
#   d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
#   d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
#   d
# }