# This program works correctly, no updates needed

corr<-function(directory = "./specdata/"
              ,treshhold = 0 ){

  max_val <- length(list.files( directory  ))
  ds_compl <- complete(id = 1:max_val)  
  
  id_ds <- subset(ds_compl, ds_compl[ ,"nobs" ] > treshhold )
  
  data_corr <- vector(mode = "numeric", length = nrow( id_ds ) )

  if( nrow(id_ds) == 0 ){
    message("No Datasets Selected due to High treshhold!") 
    return( vector(mode = "numeric") )
  }

  for( i in 1:nrow(id_ds)){
    path <- paste("./",directory,"/", sprintf("%03d",id_ds$ID[i]), ".csv",sep="",collapse = "")

    dt <- read.csv( path )
    dt_subset<-subset(dt,subset = !is.na(dt$sulfate) & !is.na(dt$nitrate))
    
    data_corr[ i ] <- cor( x = dt_subset[,"nitrate"], y = dt_subset[,"sulfate"] )
  } 
  
  return( data_corr )
}