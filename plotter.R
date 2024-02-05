#------------------------------------------------------------------#
# This script accesses Big Wood app server and retrieves all data
# necessary for generating app figures (box plots, others), saves figures
# in /www/ for app.R to access.
#
# Steven Schmitz
# 2.1.24
#------------------------------------------------------------------#
#setwd("C:/Users/stevenschmitz/Desktop/BigWood-App-main")

library("ggplot2")
library("stringr")
library("dplyr")
source(file.path(git_dir,'code/init_db.R'))
source(file.path(git_dir,'code/dbIntakeTools.R')) 

conn=scdbConnect() 

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
data_list <- list()

# query data for selected sites, log transformation of stats and outliers
for (site in sites) {
  query <- sprintf("SELECT * FROM summarystatistics WHERE site = '%s' AND metric = 'irr_vol' AND simdate = '2023-10-01';", site)
  data <- makeBoxplotData(dbGetQuery(conn, query))
  data_list[[site]] <- data
  data_list[[site]]$stats <- exp(data_list[[site]]$stats)/1000
  data_list[[site]]$out <- exp(data_list[[site]]$out)/1000
}
#------------------------------------------------------------------------------------#
# organizing data into boxplot format - name|stats for box, name|out for outlier points
#------------------------------------------------------------------------------------#

data_list[['bwh']]$name <- "Big Wood at Haley"
data_list[['bws']]$name <- "Big Wood at Stanton"
bw_out <- rbind(
  data.frame(name = data_list[['bwh']]$name, out = data_list[['bwh']]$out),
  data.frame(name = data_list[['bws']]$name, out = data_list[['bws']]$out)
)
bw <- rbind(
  data.frame(stats = data_list[['bwh']]$stats, name = data_list[['bwh']]$name),
  data.frame(stats = data_list[['bws']]$stats, name = data_list[['bws']]$name)
)
max_values <- bw %>%
  group_by(name) %>%
  summarize(max_stat = max(stats),min_stat = min(stats))

data_list[['cc']]$name <- "Camas Creek"
cc_out <- rbind(
  data.frame(name = data_list[['cc']]$name, out = data_list[['cc']]$out)
)
cc <- rbind(
  data.frame(stats = data_list[['cc']]$stats, name = data_list[['cc']]$name)
)

data_list[['sc']]$name <- "Silver Creek"
sc_out <- rbind(
  data.frame(name = data_list[['sc']]$name, out = data_list[['sc']]$out)
)
sc <- rbind(
  data.frame(stats = data_list[['sc']]$stats, name = data_list[['sc']]$name)
)
#--------------------------------------------------------------------------------------#
# generate box plots for big wood sites, camas, and silver creek, save to /www/
#--------------------------------------------------------------------------------------#

bwbox <-ggplot(bw, aes(x=name, y=stats, fill=name), alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("deepskyblue", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(bw$stats, na.rm=TRUE), by = 25),1))+
  
  geom_point(data=bw_out, aes(x=name, y=out),position = position_jitter(width = 0.08), size=2, shape=21, color = "black")+
  #scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  geom_segment(aes(x = name, xend = name, y = min_stat, yend = max_stat),
               data = max_values, color = "black", linetype = "solid", linewidth = 0.5) +
  theme_bw(base_size = 14)+
  ggtitle("Irrigation Season Volumes (April-Sept.)") +
  guides(fill = "none")+
  xlab("")+
  ylab("Irrigation Season Volume (KAF)")
print(bwbox)
ggsave(file.path("www/sampled_vol_bw.png"), bwbox,
       width = 6.5, height = 5.5, units = "in", dpi = 600)
dev.off()

ccbox <-ggplot(cc, aes(x=name, y=stats, fill=name), alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("deepskyblue", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(cc$stats, na.rm=TRUE), by = 25),1))+
  
  geom_point(data=cc_out, aes(x=name, y=out),position = position_jitter(width = 0.08), size=2, shape=21, color = "black")+
  #scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  geom_segment(aes(x = name, xend = name, y = min(stats), yend = max(stats)),
               data = cc, color = "black", linetype = "solid", linewidth = 0.5) +
  theme_bw(base_size = 14)+
  ggtitle("Irrigation Season Volumes (April-Sept.)") +
  guides(fill = "none")+
  xlab("")+
  ylab("Irrigation Season Volume (KAF)")
print(ccbox)
ggsave(file.path("www/sampled_vol_cc.png"), ccbox,
       width = 6.5, height = 5.5, units = "in", dpi = 600)
dev.off()

scbox <-ggplot(sc, aes(x=name, y=stats, fill=name), alpha = 0.6) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("deepskyblue", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(sc$stats, na.rm=TRUE), by = 25),1))+
  
  geom_point(data=sc_out, aes(x=name, y=out),position = position_jitter(width = 0.08), size=2, shape=21, color = "black")+
  #scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  geom_segment(aes(x = name, xend = name, y = min(stats), yend = max(stats)),
               data = sc, color = "black", linetype = "solid", linewidth = 0.5) +
  theme_bw(base_size = 14)+
  ggtitle("Irrigation Season Volumes (April-Sept.)") +
  guides(fill = "none")+
  xlab("")+
  ylab("Irrigation Season Volume (KAF)")
print(scbox)
ggsave(file.path("www/sampled_vol_sc.png"), scbox,
       width = 6.5, height = 5.5, units = "in", dpi = 600)
dev.off()
