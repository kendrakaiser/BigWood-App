# Figures and tables for data analysis and model output from wood river streamflow forecasting

pred.yr <<- 2024 # loop back to this
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")

makeBoxplotData=function(dbdf=dbGetQuery(conn,"SELECT * FROM summarystatistics;")){
  groups=unique(dbdf[c("site","metric","simdate","rundate")])
  bpData=list(stats=matrix(nrow=5,ncol=nrow(groups)),n=rep(NA,nrow(groups)),out=vector(),group=vector(),names=vector())
  for(i in 1:nrow(groups)){
    thisName=paste0(groups[i,"site"],".",groups[i,"metric"],"_simDate:",groups[i,"simdate"],"_runDate:",groups[i,"rundate"])
    bpData$names[i]=thisName
    thisData=merge(groups[i,],dbdf, all=F)
    thisData$ssid=NULL#drop this column so unique works
    thisData=unique(thisData) # drop duplicate records (from multiple model runs on the same day)
    #if there are still multiple model runs on the same days(simdate and rundate), warn but proceed
    if(length(thisData$value[thisData$stat==c("min")])!=1 ){
      warning("multiple model outputs found for: \n",paste(capture.output(print(groups[i,])),collapse="\n"))
    }
    bpData$stats[,i]=c(mean(thisData$value[thisData$stat==c("min")]),
                       mean(thisData$value[thisData$stat==c("lower_hinge")]),
                       mean(thisData$value[thisData$stat==c("med")]),
                       mean(thisData$value[thisData$stat==c("upper_hinge")]),
                       mean(thisData$value[thisData$stat==c("max")]))
    bpData$stats[,i]=bpData$stats[,i]
    
    bpData$n[i]=mean(thisData$value[thisData$stat==c("n")])
    
    outliers=thisData$value[thisData$stat==c("outlier")]
    bpData$out=c(bpData$out,outliers)
    bpData$group=c(bpData$group,rep(i,length(outliers)))
    
  }
  return(bpData)
}

sites <- c("bwh", "bws", "cc", "sc")


vol_data <- data.frame()

for (site in sites) {
  query <- sprintf("SELECT * FROM summarystatistics WHERE site = '%s' AND simdate = (SELECT MAX(simdate) FROM summarystatistics);", site)
  bxpData <- makeBoxplotData(dbGetQuery(conn, query))
  bxpData$value <- bxpData$stats/1000
  bxpData$out <- bxpData$out/1000
  
  extra_rows <- data.frame(value = bxpData$value, site = rep(site, length(bxpData$value)))
  vol_data <- rbind(vol_data, extra_rows)
  vol_data <- unique(vol_data)
}
sites_mapping <- c("Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")

for (i in 1:nrow(vol_data)) {
  if (vol_data[i, "site"] == "cc") {
    vol_data[i, "site"] <- "Camas Creek"
  } else if (vol_data[i, "site"] == "bwh") {
    vol_data[i, "site"] <- "Big Wood Hailey"
  } else if (vol_data[i, "site"] == "bws") {
    vol_data[i, "site"] <- "Big Wood Stanton"
  } else if (vol_data[i, "site"] == "sc") {
    vol_data[i, "site"] <- "Silver Creek"
  }
}
vol_data$t <- 'Predicted'

vol.big <- vol_data[vol_data$site == 'Big Wood Hailey' | vol_data$site == 'Big Wood Stanton',]
vol.sc <- vol_data[vol_data$site == 'Silver Creek',]
vol.cc <- vol_data[vol_data$site == 'Camas Creek',]
#-----------------------------------------------------------------------------------#
# calculating historical volume data from database, date range 2003 - 2022
# site id:
#     bwh <- 140
#     bws <- 141
#     sc <- 163
#     cc <- 167
#-----------------------------------------------------------------------------------#
hist_irr <- dbGetQuery(conn," SELECT * FROM (SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98/1000 AS irr_vol, data.locationid, name, sitenote, COUNT(DISTINCT( dataid)) AS days_in_record
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear) as histvols WHERE days_in_record > 180;")  # complete record is 183 days



bwh_hist<- hist_irr[hist_irr$sitenote== 'bwh',]
bws_hist <- hist_irr[hist_irr$sitenote== 'bws',]
sc_hist <- hist_irr[hist_irr$sitenote== 'sc',]
cc_hist <- hist_irr[hist_irr$sitenote== 'cc',]

bwh_hist <- boxplot.stats(bwh_hist$irr_vol)
bwh_hist$t <- 'Historic'

bws_hist <- boxplot.stats(bws_hist$irr_vol)
bws_hist$t <- 'Historic'

sc_hist <- boxplot.stats(sc_hist$irr_vol)
sc_hist$t <- 'Historic'

cc_hist <- boxplot.stats(cc_hist$irr_vol)
cc_hist$t <- 'Historic'

bwh_hist$t <- factor(bwh_hist$t)
bws_hist$t <- factor(bws_hist$t)
sc_hist$t <- factor(sc_hist$t)
cc_hist$t <- factor(cc_hist$t)

vol.1big.hist <- data.frame( # reformatting for merge with modeled vols
  value = bwh_hist$stats,
  site = rep('Big Wood Hailey (Historic)', length(bwh_hist$stats)),
  t = as.character(bwh_hist$t)
)
vol.2big.hist <- data.frame( # reformatting for merge
  value = bws_hist$stats,
  site = rep('Big Wood Stanton (Historic)', length(bws_hist$stats)),
  t = as.character(bws_hist$t)
)
vol.big <- rbind(vol.1big.hist, vol.2big.hist, vol.big) # complete dataframe of big wood with historic and predicted values

vol.sc.hist <- data.frame( # reformatting for merge with modeled vols
  value = sc_hist$stats,
  site = rep('Silver Creek (Historic)', length(sc_hist$stats)),
  t = as.character(sc_hist$t)
)
vol.sc <- rbind(vol.sc.hist, vol.sc) # complete dataframe of silver creek with historic and predicted values

vol.cc.hist <- data.frame( # reformatting for merge with modeled vols
  value = cc_hist$stats,
  site = rep('Camas Creek (Historic)', length(cc_hist$stats)),
  t = as.character(cc_hist$t)
)
vol.cc <- rbind(vol.cc.hist, vol.cc) # complete dataframe of camas creek with historic and predicted values
#-------------------------------------------------------------------------------------#

exc_prob=dbGetQuery(conn,"SELECT * FROM exceednaceprobabilities;") # note table misspelling, need to fix
exc_prob <- exc_prob %>% rename( "Big Wood Hailey" = bwh.irr_vol)
exc_prob <- exc_prob %>% rename( "Big Wood Stanton" = bws.irr_vol)
exc_prob <- exc_prob %>% rename( "Camas Creek" = cc.irr_vol)
exc_prob <- exc_prob %>% rename( "Silver Creek" = sc.irr_vol)
prb<- c(0.1, 0.25, 0.5, 0.75, 0.9)
ex.vols3 <- exc_prob %>%pivot_longer(!Exceedance, names_to="site", values_to="value")
#-------------------------------------------------------------------------------------#
#colfunc<-colorRamp(c("red","darkorange","green3","deepskyblue", "blue3"))

vol.big$site=factor(vol.big$site, levels=c("Big Wood Hailey (Historic)", "Big Wood Hailey", "Big Wood Stanton (Historic)", "Big Wood Stanton")) #this sets the order for the boxplot

gen_bw <- function(vol.big, ex.vols3){
  p<-ggplot(vol.big, fill=t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = .3) +  #outliers are not actually drawn
    scale_fill_manual(values=c("royalblue3", "grey90","royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 50),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site !="Silver Creek" & ex.vols3$site !="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2.5, shape=15)+
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 14)+
    ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Season Volume (KAF)")
  
  return(p)
}



vol.sc$site=factor(vol.sc$site, levels=c("Silver Creek (Historic)", "Silver Creek"))

gen_sc <- function(vol.sc,ex.vols3){
  ps<- ggplot(vol.sc, fill =t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.sc$value, na.rm=TRUE), by = 10),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Silver Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2.5, shape=15)+
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 18) +
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Volume (KAF)")
  
  return(ps)
}

vol.cc$site=factor(vol.cc$site, levels=c("Camas Creek (Historic)", "Camas Creek"))


gen_cc <- function(vol.cc, ex.vols3){
  pc<- ggplot(vol.cc, fill = t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.cc$value, na.rm=TRUE), by = 40),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2.5, shape=15)+
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 18)+
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none")+
    xlab("")+
    ylab("")
  
  return(pc)
}



