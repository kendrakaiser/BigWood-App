scdbConnect=function(readOnly=T){
  if(readOnly){
    conn=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbread",
                   password=Sys.getenv("scdb_readPass")
    )
  } else {
    conn=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbwrite",
                   password=Sys.getenv("scdb_pass")
    )
  }
  return(conn)
}
