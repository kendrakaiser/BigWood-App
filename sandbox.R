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

# is there a better way to do this? 
useData$site<- "sitename"
useData$site[which(useData$locationid == 140 & useData$isprediction == "FALSE")]<- c("Big Wood Hailey Hist")
useData$site[which(useData$locationid == 140 & useData$isprediction == "TRUE")]<- c("Big Wood Hailey")
useData$site[which(useData$locationid == 141 & useData$isprediction == "FALSE")]<- c("Big Wood Stanton Hist")
useData$site[which(useData$locationid == 141 & useData$isprediction == "TRUE")]<- c("Big Wood Stanton")
useData$site[which(useData$locationid == 167 & useData$isprediction == "FALSE")]<- c("Camas Creek Hist")
useData$site[which(useData$locationid == 167 & useData$isprediction == "TRUE")]<- c("Camas Creek")
useData$site<- factor(useData$site, levels = c("Big Wood Hailey Hist","Big Wood Hailey", "Big Wood Stanton Hist", "Big Wood Stanton", "Camas Creek Hist", "Camas Creek" ), ordered = TRUE)

#plot the figure 
ggplot(useData, aes(x=site, y=value, fill=isprediction), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  #scale_y_continuous(breaks = round(seq(0, max(useData$value, na.rm=TRUE), by = 50),1))+
  ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
  xlab("")+
  ylab("Irrigation Season Volume (KAF)") +
  theme_bw()
