## Find stores that reported sales last week

activeStores <- function(dataset, numberofperiods, lastweek){
  lwlist <- subset(dataset,dataset[[2]]== lastweek) ## Subsets to last week only
  activestorelist <- subset(lwlist,lwlist[[3]]>0)
  activestorelist <- activestorelist[[1]]
  
 ps <- list()
  for (i in activestorelist){
  ps[[i]] <- storepredict(dataset, i, numberofperiods)
 }
 predictedSales <- do.call("rbind",ps)
 write.csv(predictedSales,paste(c("predicted sales for week ",lastweek)))
}