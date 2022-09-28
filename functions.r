library(sf)

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

#extent=list(north=43.36113,east=-114.1098,south=43.2987,west=-114.1898)
#useVars=c("Dissolved Oxygen", "Water Temperature", "flow")
#dbGetQuery(conn,"SELECT DISTINCT ST_SRID(geometry) FROM locations;")$st_srid
#dbGetQuery(conn,"SELECT DISTINCT name FROM metrics;")

#dbGetQuery(conn,"SELECT * FROM locations LIMIT 1;")

getDataByVarAndExtent=function(useVars,extent){
  #convert to proper epsg, make postgis extent object, return from data join metrics where IN vars and WITHIN extent
 
  extBox = st_bbox(c(xmin=extent$east,xmax=extent$west,ymax=extent$north,ymin=extent$south),crs=st_crs(4326))
  extBox=st_as_sfc(extBox)
  extBox=st_transform(extBox,st_crs(26911))
  
  
  query=paste0("SELECT data.metric, data.value, data.datetime, data.locationid FROM data ",
               "INNER JOIN locations ON data.locationid = locations.locationid ",
               "WHERE data.metric IN ('",paste0(useVars,collapse="', '"),"')",
               "AND ST_Within(locations.geometry, ",extBox,");")
  
}
