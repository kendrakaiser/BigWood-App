# Figures and tables for data analysis and model output from wood river streamflow forecasting
pred.yr <<- 2024 # loop back to this
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")

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

sites <- c("bwh", "bws", "cc", "sc")

vol_data <- data.frame()

for (site in sites) {
  query <- sprintf("SELECT * FROM summarystatistics WHERE site = '%s' AND simdate = '2023-10-01';", site)
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
bwh_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '140' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
bws_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '141' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
sc_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '144' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")
cc_hist <- dbGetQuery(conn,"SELECT * FROM data WHERE locationid = '167' 
                AND qcstatus = 'TRUE' 
                AND metricid = '14'
                AND datetime >= '2003-01-01'")

bwh_hist <- boxplot.stats(bwh_hist$value)
bwh_hist$stats <- bwh_hist$stats/1000
bwh_hist$t <- 'Historic'
bws_hist <- boxplot.stats(bws_hist$value)
bws_hist$stats <- bws_hist$stats/1000
bws_hist$t <- 'Historic'

sc_hist <- boxplot.stats(sc_hist$value)
sc_hist$stats <- sc_hist$stats/1000
sc_hist$t <- 'Historic'
cc_hist <- boxplot.stats(cc_hist$value)
cc_hist$stats <- cc_hist$stats/1000
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
vol.big <- rbind(vol.big, vol.1big.hist, vol.2big.hist) # complete dataframe of big wood with historic and predicted values

vol.sc.hist <- data.frame( # reformatting for merge with modeled vols
  value = sc_hist$stats,  
  site = rep('Silver Creek (Historic)', length(sc_hist$stats)),
  t = as.character(sc_hist$t)
)
vol.sc <- rbind(vol.sc, vol.sc.hist) # complete dataframe of silver creek with historic and predicted values

vol.cc.hist <- data.frame( # reformatting for merge with modeled vols
  value = cc_hist$stats,  
  site = rep('Camas Creek (Historic)', length(cc_hist$stats)),
  t = as.character(cc_hist$t)
)
vol.cc <- rbind(vol.cc, vol.cc.hist) # complete dataframe of camas creek with historic and predicted values
#-------------------------------------------------------------------------------------#

exc_prob=dbGetQuery(conn,"SELECT * FROM exceednaceprobabilities;") # note table misspelling, need to fix
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
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 14)+
    ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Season Volume (KAF)")

  return(p)
}

gen_sc <- function(vol.sc,ex.vols3){
  ps<- ggplot(vol.sc, fill =t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.sc$value, na.rm=TRUE), by = 10),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Silver Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=21)+
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 18) +
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none") +
    xlab("")+
    ylab("Irrigation Volume (KAF)")
  
  return(ps)
}

gen_cc <- function(vol.cc, exvols3){
  pc<- ggplot(vol1.cc, fill = t, aes(x=site, y=value, fill=site), alpha=0.6) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_fill_manual(values=c("royalblue3", "grey90")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    scale_y_continuous(breaks = round(seq(0, max(vol.cc$value, na.rm=TRUE), by = 40),1))+
    
    geom_point(data=ex.vols3[ex.vols3$site =="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=21)+
    scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
    theme_bw(base_size = 18)+
    ggtitle("") +
    labs(fill="", color="Exceedance")+
    guides(fill = "none", color = "none")+
    xlab("")+
    ylab("")
  
  return(pc)
}

#--------------------#
# making historical boxplots
#--------------------#

query <- sprintf("SELECT site,simdate,rundate,stat,value FROM summarystatistics WHERE site = 'bwh' AND stat != 'n' AND stat != 'outlier'")
bwh <- dbGetQuery(conn,query)
bwh$rundate <- as.Date(bwh$rundate)
bwh$t <- 'Modeled'
bwh$t <- factor(bwh$t)
bwh$value <- bwh$value/1000

bwh <- bwh %>% # removing duplicates where multiple model outputs exist for the same rundate
  group_by(rundate, stat) %>%
  arrange(rundate) %>%
  distinct(stat, .keep_all = TRUE) %>%
  ungroup()

histbwh <- function(){
  ggplot(bwh, aes(x = rundate, y = value, fill = t)) +  # Add closing parenthesis here
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=t),position = "identity", width = 0.2,fill='yellow') +
    facet_wrap(~ rundate, scales = "free_x", nrow = 1) + 
    labs(x = "", y = "Modeled Streamflow Volume (KAF)") +
    ggtitle("Big Wood at Hailey") +
    theme_minimal() +
    guides(fill = 'none') +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 
}

query <- sprintf("SELECT site,simdate,rundate,stat,value FROM summarystatistics WHERE site = 'bws' AND stat != 'n' AND stat != 'outlier'")
bws <- dbGetQuery(conn,query)
bws$rundate <- as.Date(bws$rundate)
bws$t <- 'Modeled'
bws$t <- factor(bws$t)
bws$value <- bws$value/1000

bws <- bws %>% # removing duplicates where multiple model outputs exist for the same rundate
  group_by(rundate, stat) %>%
  arrange(rundate) %>%
  distinct(stat, .keep_all = TRUE) %>%
  ungroup()

histbws <- function(){
  ggplot(bws, aes(x = rundate, y = value, fill = t)) +  # Add closing parenthesis here
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=t),position = "identity", width = 0.2,fill='yellow') +
    facet_wrap(~ rundate, scales = "free_x", nrow = 1) + 
    labs(x = "", y = "Modeled Streamflow Volume (KAF)") +
    ggtitle("Big Wood at Stanton") +
    theme_minimal() +
    guides(fill = 'none') +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 
}
# bws

query <- sprintf("SELECT site,simdate,rundate,stat,value FROM summarystatistics WHERE site = 'bwh' AND stat != 'n' AND stat != 'outlier'")
bwh <- dbGetQuery(conn,query)
bwh$rundate <- as.Date(bwh$rundate)
bwh$t <- 'Modeled'
bwh$t <- factor(bwh$t)
bwh$value <- bwh$value/1000

bwh <- bwh %>% # removing duplicates where multiple model outputs exist for the same rundate
  group_by(rundate, stat) %>%
  arrange(rundate) %>%
  distinct(stat, .keep_all = TRUE) %>%
  ungroup()

histbwh <- function(){
  ggplot(bwh, aes(x = rundate, y = value, fill = t)) +  # Add closing parenthesis here
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=t),position = "identity", width = 0.2,fill='yellow') +
    facet_wrap(~ rundate, scales = "free_x", nrow = 1) + 
    labs(x = "", y = "Modeled Streamflow Volume (KAF)") +
    ggtitle("Big Wood at Hailey") +
    theme_minimal() +
    guides(fill = 'none') +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 
}

# cc

query <- sprintf("SELECT site,simdate,rundate,stat,value FROM summarystatistics WHERE site = 'cc' AND stat != 'n' AND stat != 'outlier'")
cc <- dbGetQuery(conn,query)
cc$rundate <- as.Date(cc$rundate)
cc$t <- 'Modeled'
cc$t <- factor(cc$t)
cc$value <- cc$value/1000

cc <- cc %>% # removing duplicates where multiple model outputs exist for the same rundate
  group_by(rundate, stat) %>%
  arrange(rundate) %>%
  distinct(stat, .keep_all = TRUE) %>%
  ungroup()

histcc <- function(){
  ggplot(cc, aes(x = rundate, y = value, fill = t)) +  # Add closing parenthesis here
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=t),position = "identity", width = 0.2,fill='yellow') +
    facet_wrap(~ rundate, scales = "free_x", nrow = 1) + 
    labs(x = "", y = "Modeled Streamflow Volume (KAF)") +
    ggtitle("Camas Creek") +
    theme_minimal() +
    guides(fill = 'none') +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 
}

# sc

query <- sprintf("SELECT site,simdate,rundate,stat,value FROM summarystatistics WHERE site = 'sc' AND stat != 'n' AND stat != 'outlier'")
sc <- dbGetQuery(conn,query)
sc$rundate <- as.Date(sc$rundate)
sc$t <- 'Modeled'
sc$t <- factor(sc$t)
sc$value <- sc$value/1000

sc <- sc %>% # removing duplicates where multiple model outputs exist for the same rundate
  group_by(rundate, stat) %>%
  arrange(rundate) %>%
  distinct(stat, .keep_all = TRUE) %>%
  ungroup()

histsc <- function(){
  ggplot(sc, aes(x = rundate, y = value, fill = t)) +  # Add closing parenthesis here
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=t),position = "identity", width = 0.2,fill='yellow') +
    facet_wrap(~ rundate, scales = "free_x", nrow = 1) + 
    labs(x = "", y = "Modeled Streamflow Volume (KAF)") +
    ggtitle("Silver Creek") +
    theme_minimal() +
    guides(fill = 'none') +
    theme(strip.text.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 
}

