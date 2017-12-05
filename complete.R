# This program works correctly, no updates needed

complete<-function( directory = "specdata"
                          ,id = 1:2){
  
  data_return<-data.frame( id = id, result = vector(mode = "numeric", length = length(id)) )
  names(data_return)<-c("ID","nobs")
  
  for( i in 1:length(id)){
    path <- paste("./",directory,"/", sprintf("%03d",id[i]), ".csv",sep="",collapse = "")
    dt <- read.csv( path )
    
    data_return[i,]<-c(id[i], sum(!is.na( dt[,"nitrate"]) & !is.na( dt[ ,"sulfate"] )) )
  }
  return( data_return )

}
complete("specdata", 1)