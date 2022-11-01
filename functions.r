scdbConnect=function(){
  conn=dbConnect(RPostgres::Postgres(),
                 host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                 port="25060",
                 dbname="silvercreekdb" ,
                 user="dbread",
                 password="dbread"
  )
  return(conn)
}

extent=list(north=43.32,east=-114.1,south=43.29,west=-114.15)
useVars=c("Dissolved Oxygen", "Water Temperature", "flow")
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
  
  #p=print(extBox[[1]])
  
  
  query=paste0("SELECT data.metric, data.value, data.datetime, data.locationid FROM data ",
               "INNER JOIN locations ON data.locationid = locations.locationid ",
               "WHERE data.metric IN ('",paste0(useVars,collapse="', '"),"') ",
               "AND ST_Within(locations.geometry, ST_Polygon('",
               print(extBox[[1]]),"'::geometry, 26911) );")
  
  returnData=dbGetQuery(conn,query)
  
  format(object.size(returnData),units="MB")
  
}


