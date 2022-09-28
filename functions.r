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

getDataByVarAndExtent=function(vars,extent){
  extPoly=st_polygon( x=list(matrix(c( extent$west,extent$north,
                                       extent$east,extent$north,
                                       extent$east,extent$south,
                                       extent$west,extent$south,
                                       extent$west,extent$north), #closed
                                    ncol=2,byrow=T)) )
  
  
  #st_as_sfc(extPoly,crs=st_crs(4326))
  #st_crs(extPoly)=st_crs(4326)
  #convert to proper epsg, make postgis extent object, return from data join metrics where IN vars and WITHIN extent
  
}