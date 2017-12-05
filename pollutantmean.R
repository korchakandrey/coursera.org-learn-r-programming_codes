# This program works correctly, no updates needed

pollutantmean<-function( directory = "specdata"
                         ,pollutant = "sulfate"
                         ,id = 1:332 
                        ){
    res <-vector(mode="numeric")
    for( i in 1:length(id) ){
         path <- paste("./",directory,"/", sprintf("%03d",id[i]), ".csv",sep="",collapse = "")
         #print(path)
         
         data_csv <- read.csv( path )
         #print(head(data_csv))
         
         # This calculates MEAN across all monitors
         #res[i]<-mean( data_csv[,pollutant], na.rm = TRUE)
         
         # This calculates MEAN across all data
         res<-c(res,data_csv[[pollutant]])
         }
    message("Mean is ");
    return( mean(res, na.rm = TRUE) )
}
#pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)

