# Figures and tables for data analysis and model output from wood river streamflow forecasting

#------------------------------------------------------------------------------
# Historic condition data
#TODO: update
library("magrittr")
library("tidyr")
library("ggplot2")
library("stringr")
library("dplyr")
source(file.path('init_db.R'))
source(file.path('dbIntakeTools.R')) 

conn=scdbConnect() 
#-----------------------------------------------------------------------------------#
steven_dir = "C:/Users/stevenschmitz/Desktop/" # local directory, removed for git push

pred.yr <<- 2024 # loop back to this
alldat <- read.csv("all_vars.csv") # fix directory potentially
wq <- alldat %>% select("wateryear", "bwh.wq", "bws.wq", "cc.wq", "sc.wq") %>% pivot_longer(!wateryear, names_to = "site", values_to = "winterFlow")

# Boxplots of Historic Conditions
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")
wq_box<- ggplot(wq %>% filter(wateryear < pred.yr), aes(x=factor(site), y=winterFlow))+
  geom_boxplot(alpha=0.8)+
  theme_bw()+
  xlab("USGS Site")+
  ylab("Average Nov-Jan Winter Flow (cfs)")+
  geom_point(data = wq %>% filter(wateryear == pred.yr),  aes(x=factor(site), y=winterFlow), color="blue", size=3, shape=15)+
  scale_x_discrete(labels= sitelabs)

png(filename = file.path("www/wq_box.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(wq_box)
dev.off()

#-----------------------------------------------------------------------------------#
x <- dbGetQuery(conn,"SELECT metric,value, datetime, locationid FROM data WHERE datetime >= '2003-01-01' AND qcstatus = 'TRUE' AND locationid = 164;")
calcVolStats=function(x,site.metric,simDate,runDate=Sys.Date()){
  
  simDate=as.Date(simDate)
  
  
  if(length(strsplit(site.metric,"\\.")[[1]])!=2){
    stop(paste0("Invalid site.metric ",site.metric))
  }
  
  site=strsplit(site.metric,"\\.")[[1]][1]
  metric=strsplit(site.metric,"\\.")[[1]][2]
  
  
  x.stats=boxplot.stats(x)
  
  statDF=data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="n",value=x.stats$n)
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="min",value=x.stats$stats[[1]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="lower_hinge",value=x.stats$stats[[2]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="med",value=x.stats$stats[[3]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="upper_hinge",value=x.stats$stats[[4]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="max",value=x.stats$stats[[5]]))
  if(length(x.stats$out)>0){
    statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="outlier",value=x.stats$out))
  }
  return(statDF)
  
  #dbWriteTable(conn,"summarystatistics",statDF,append=T)
}
makeBoxplotData=function(dbdf=dbGetQuery(conn,"SELECT * FROM summarystatistics;")){
  groups=unique(dbdf[c("site","metric","simdate","rundate")])
  bpData=list(stats=matrix(nrow=5,ncol=nrow(groups)),n=rep(NA,nrow(groups)),out=vector(),group=vector(),names=vector())
  for(i in 1:nrow(groups)){
    thisName=paste0(groups[i,"site"],".",groups[i,"metric"],"_simDate:",groups[i,"simdate"],"_runDate:",groups[i,"rundate"])
    bpData$names[i]=thisName
    thisData=merge(groups[i,],dbdf)
    bpData$stats[,i]=c(thisData$value[thisData$stat==c("min")],
                       thisData$value[thisData$stat==c("lower_hinge")],
                       thisData$value[thisData$stat==c("med")],
                       thisData$value[thisData$stat==c("upper_hinge")],
                       thisData$value[thisData$stat==c("max")])
    
    bpData$n[i]=thisData$value[thisData$stat==c("n")]
    
    outliers=thisData$value[thisData$stat==c("outlier")]
    bpData$out=c(bpData$out,outliers)
    bpData$group=c(bpData$group,rep(i,length(outliers)))
    
  }
  
  return(bpData)
}

data_list <- list()
sites <- c("bwh", "bws", "cc", "sc")

vol_data <- data.frame()

for (site in sites) {
  query <- sprintf("SELECT * FROM summarystatistics WHERE site = '%s' AND metric = 'irr_vol' AND simdate = '2023-10-01';", site)
  data <- makeBoxplotData(dbGetQuery(conn, query))
  data$value <- exp(data$stats)/1000
  data$out <- exp(data$out)/1000
  
  extra_rows <- data.frame(value = data$value, site = rep(site, length(data$value)))
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
#-----------------------------------------------------------------------------------#
var<-read.csv(file.path("all_vars.csv"))

vol.hist<- as.data.frame(var[var$wateryear < pred.yr ,] %>% dplyr::select(c(bwh.irr_vol, bws.irr_vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/1000
vol.hist$t<- "Historic"
vol.hist.sm<-as.data.frame(var[var$wateryear < pred.yr,] %>% dplyr::select(c(sc.irr_vol)) %>% `colnames<-`(c("Silver Creek Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000
vol.hist.sm$t<- "Historic"
vol.hist.cc<-as.data.frame(var[var$wateryear < pred.yr,] %>% dplyr::select(c(cc.irr_vol)) %>% `colnames<-`(c("Camas Creek Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.cc$value<-vol.hist.cc$value/1000
vol.hist.cc$t<- "Historic"

vol.pred <-as.data.frame(vol_data) 
vol.pred$t<- "Predicted"

vol.big<- rbind(vol.hist, vol.pred[vol.pred$site != "Silver Creek" & vol.pred$site != "Camas Creek",])
vol.sm<- rbind(vol.hist.sm, vol.pred[vol.pred$site == "Silver Creek",])
vol.cc<- rbind(vol.hist.cc, vol.pred[vol.pred$site == "Camas Creek",])
vol.cc$t <- factor(vol.cc$t)
vol.big$t <- factor(vol.big$t)
vol.sm$t <- factor(vol.sm$t)
vol.big$site<-factor(vol.big$site,levels = c("Big Wood Hailey Hist","Big Wood Hailey", "Big Wood Stanton Hist", "Big Wood Stanton"), ordered = TRUE)
vol.sm$site<-factor(vol.sm$site,levels = c("Silver Creek Hist","Silver Creek"), ordered = TRUE)
vol.cc$site<-factor(vol.cc$site,levels = c("Camas Creek Hist", "Camas Creek" ), ordered = TRUE)
vol.big <- vol.big[vol.big$t != "Levels: Historic Predicted", ]
#-------------------------------------------------------------------------------------#

exc_prob=dbGetQuery(conn,"SELECT * FROM exceednaceprobabilities;") # note table mispelling, need to fix
exc_prob <- exc_prob %>% rename( "Big Wood Hailey" = bwh.irr_vol)
exc_prob <- exc_prob %>% rename( "Big Wood Stanton" = bws.irr_vol)
exc_prob <- exc_prob %>% rename( "Camas Creek" = cc.irr_vol)
exc_prob <- exc_prob %>% rename( "Silver Creek" = sc.irr_vol)
prb<- c(0.1, 0.25, 0.5, 0.75, 0.9)
ex.vols3 <- exc_prob %>%pivot_longer(!Exceedance, names_to="site", values_to="value")
#-------------------------------------------------------------------------------------#
colfunc<-colorRampPalette(c("red","darkorange","green3","deepskyblue", "blue3"))

gen_bw <- function(vol.big, ex.vols3){
  p<-ggplot(vol.big, fill=t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90","royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 50),1))+
  
    geom_point(data=ex.vols3[ex.vols3$site !="Silver Creek" & ex.vols3$site !="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=21)+
    scale_color_manual(values=c("black","black","black","black","black"))+
    theme_bw(base_size = 14)+
    ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Season Volume (KAF)")

  return(p)
}

gen_sc <- function(vol.sm,ex.vols3){
  ps<- ggplot(vol.sm, fill =t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90", "royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.sm$value, na.rm=TRUE), by = 10),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Silver Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=21)+
    scale_color_manual(values=c("black","black","black","black","black"))+
    theme_bw(base_size = 18) +
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Volume (KAF)")
  
  return(ps)
}

gen_cc <- function(vol.cc, exvols3){
  pc<- ggplot(vol.cc, fill = t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90", "royalblue3", "grey90"))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.cc$value, na.rm=TRUE), by = 40),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=21)+
    scale_color_manual(values=c("black","black","black","black","black"))+
    theme_bw(base_size = 18)+
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none")+
    xlab("")+
    ylab("Irrigation Volume (KAF)")
  
  return(pc)
}
