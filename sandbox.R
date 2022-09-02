conn=scdbConnect()
dbGetQuery(conn,"SELECT * FROM metrics;")

useLocations=dbGetQuery(conn, "SELECT locationid, name FROM locations WHERE locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID' );")
useMetrics=dbGetQuery(conn, "SELECT metricid, name, isprediction FROM metrics WHERE metrics.name IN ('irrigation season volume (april 1 - september 31)', 'simulated irrigation season volume (april 1 - september 31)');")

query=paste0("SELECT metric, value, locationid, simnumber FROM data WHERE data.metricid IN ('",
             paste0(useMetrics$metricid,collapse="', '"),
             "') AND data.locationid IN ('",
             paste0(useLocations$locationid,collapse="', '"),"');")

useData=dbGetQuery(conn,query)
useData=merge(useData,useMetrics,by.x="metric",by.y="name")

boxplot(useData$value~useData$isprediction)
