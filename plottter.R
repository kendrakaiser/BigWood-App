#------------------------------------------------------------------#
# This script accesses Big Wood app server and retrieves all data
# necessary for generating app figures (box plots, others).
#
# Steven Schmitz
# 2.1.24
#------------------------------------------------------------------#
library("ggplot2")
source(file.path(git_dir, 'code/init_db.R'))
source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) 

conn=scdbConnect() 
shiny_path <- "C:/Users/stevenschmitz/Desktop/BigWood-App-main"

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

bwh_dat=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'bwh' AND metric = 'irr_vol' AND simdate='2022-10-01';"))
bws_dat=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'bws' AND metric = 'irr_vol' AND simdate='2022-10-01';"))
cc_dat=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'cc' AND metric = 'irr_vol' AND simdate='2022-10-01';"))
sc_dat=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'sc' AND metric = 'irr_vol' AND simdate='2022-10-01';"))

combined_data_bw <- data.frame(
  Group = rep(c("Big Wood at Hailey", "Big Wood at Stanton"), 
              each = nrow(bwh_dat$stats)),
  Value = c(bwh_dat$stats, bws_dat$stats)
)
combined_data_sccc <- data.frame(
  Group = rep(c("Silver Creek", "Camas Creek"), 
              each = nrow(sc_dat$stats)),
  Value = c(sc_dat$stats, cc_dat$stats)
)

png(paste0(git_dir,"www/boxplot_big_wood.png"), width = 800, height = 600)
boxplot(Value ~ Group, data = combined_data_bw, # big wood stanton/haley box plot
        col = c("royalblue3", "grey90", "blue3", "deepskyblue", "green3", "darkorange", "red"),
        main = "Historic & Modeled Irrigation Season Volumes (April-Sept.)",
        xlab = "",
        ylab = "Irrigation Season Volume (KAF)"
        )
grid(lty = "dotted", col = "gray")
dev.off()

png(paste0(git_dir,"www/boxplot_sccc.png"), width = 800, height = 600)
boxplot(Value ~ Group, data = combined_data_sccc, # silver creek, camas creek box plot
        col = c("royalblue3", "grey90", "blue3", "deepskyblue", "green3", "darkorange", "red"),
        main = "Historic & Modeled Irrigation Season Volumes (April-Sept.)",
        xlab = "",
        ylab = "Irrigation Season Volume (KAF)")
grid(lty = "dotted", col = "gray")
dev.off()
