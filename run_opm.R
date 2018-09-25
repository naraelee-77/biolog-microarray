 # library(opm)

# convert kinetic file to opm format
run_opm <- function(inFile, outFile) {
  New <- read_opm(inFile[["datapath"]])
  op <- set_spline_options(type = "smooth.spline")
  New_agg<-do_aggr(New, boot=0, method = "spline", options = op)
  
  L<-round(aggregated(New_agg, "lambda")/10,3)
  S<- round(aggregated(New_agg,"mu")/100,2)
  Max<- round(aggregated(New_agg,"A")/1000,3)
  Auc<- round(aggregated(New_agg,"AUC")/10000,3)
  
  cute <-rbind(L[1,], S[1,], Max[1,],Auc[1,])
  well.number<- c("lambda", "slope", "maximum", "AUC")
  
  TM<-t(cbind(well.number, cute))
  
  write.table(TM, outFile, sep="\t" ,row.names=T, col.names=F)
}

