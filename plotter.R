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

pred.yr <<- 2024 # loop back to this

# Boxplots of Historic Conditions
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")

#x <- dbGetQuery(conn,"SELECT metric,value, datetime, locationid FROM data WHERE datetime >= '2003-01-01' AND qcstatus = 'TRUE' AND locationid = 164;")
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
# calculating historical volume data from database, date range 2003 - 2022
# site id:
#     bwh <- 140
#     bws <- 141
#     sc <- 163
#     cc <- 167
#-----------------------------------------------------------------------------------#
bwh_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '140' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
bws_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '141' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
sc_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '163' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
cc_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '167' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")

bwh_hist <- boxplot.stats(bwh_hist$value)
bwh_hist <- bwh_hist$stats/1000
bwh_hist$t <- 'Historic'
bws_hist <- boxplot.stats(bws_hist$value)
bws_hist <- bws_hist$stats/1000
bws_hist$t <- 'Historic'

sc_hist <- boxplot.stats(sc_hist$value)
sc_hist <- sc_hist$stats/1000
sc_hist$t <- 'Historic'
cc_hist <- boxplot.stats(cc_hist$value)
cc_hist <- cc_hist$stats/1000
cc_hist$t <- 'Historic'

bwh_hist$t <- factor(bwh_hist$t)
bws_hist$t <- factor(bws_hist$t)
sc_hist$t <- factor(sc_hist$t)
cc_hist$t <- factor(cc_hist$t)
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
