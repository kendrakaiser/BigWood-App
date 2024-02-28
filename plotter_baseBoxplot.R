#gen_bw_bxp=function()

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
    bpData$stats[,i]=bpData$stats[,i]/1000 #convert to KAF
    
    bpData$n[i]=mean(thisData$value[thisData$stat==c("n")])
    
    outliers=thisData$value[thisData$stat==c("outlier")]/1000 #convert to KAF
    bpData$out=c(bpData$out,outliers)
    bpData$group=c(bpData$group,rep(i,length(outliers)))
    
  }
  return(bpData)
}

mergeBxpLists=function(...){
  allLists=list(...)
  baseList=allLists[[1]]
  baseList$conf=NULL
  baseList$group=rep(1,length(baseList$out))
  if(is.null(baseList$names)){
    baseList$names="group1"
  }
  
  for (i in 2:length(allLists)){
    thisStats=matrix(allLists[[i]]$stats,ncol=1)
    baseList$stats=cbind(baseList$stats,allLists[[i]]$stats)
    baseList$n=c(baseList$n,allLists[[i]]$n)
    baseList$out=c(baseList$out,allLists[[i]]$out)
    baseList$group=c(baseList$group,rep(i,length(allLists[[i]]$out)) )
    if(is.null(allLists[[i]]$names)){
      allLists[[i]]$names=paste0("group",i)
    }
    baseList$names=c(baseList$names,allLists[[i]]$names)
  }
  return(baseList)
}



hist_irr <- dbGetQuery(conn," SELECT * FROM (SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98/1000 AS irr_vol, data.locationid, name, sitenote, COUNT(DISTINCT( dataid)) AS days_in_record
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear) as histvols WHERE days_in_record > 180;")  # complete record is 183 days



bw_bxpList=mergeBxpLists(boxplot.stats(hist_irr$irr_vol[hist_irr$sitenote== 'bwh']),
                         makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site = 'bwh' AND simdate = (SELECT MAX(simdate) FROM summarystatistics);")),
                         boxplot.stats(hist_irr$irr_vol[hist_irr$sitenote== 'bws']),
                         makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site = 'bws' AND simdate = (SELECT MAX(simdate) FROM summarystatistics);"))
)

bxp(bw_bxpList,names=c("BWH","BWH_P","BWS","BWS_P"),show.names = F, border=c("royalblue3", "grey60"))
axis(1, at=1:4, labels=c("BWH","BWH_Predict","BWS","BWS_Predict"))
