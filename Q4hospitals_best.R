# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state.
best <- function(  state_name   = "TX"
                 , outcome_name = "heart attack"
                 , rank_to_return = 1) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #dim(outcome)
      
      
  names(outcome)<-tolower(names(outcome))
  
  #Looking for needed column
  column<-paste("hospital.30.day.death..mortality..rates.from."
                ,sub(pattern = " ", replacement = ".",x = outcome_name)
                ,sep = "" ,collapse = ""
               )
  #print(paste("Column is",column))
  
  ## Check that state and outcome are valid
  if ( nrow( outcome[outcome$state == state_name,] )==0 ){
        message("Invalid State")
      stop()
  }else if(!( column %in% colnames(outcome)) ){
       message("Invalid Outcome")
       stop()
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  outcome[,column]<-as.numeric(outcome[,column])
  #head(outcome)
  nrow(outcome)
  
  outcome_non_miss<-outcome[!is.na(outcome[,column])
                              & outcome$state == state_name
                            , c(column, "hospital.name")]
  #head(outcome_non_miss)
  #str(outcome_non_miss)
  dim(outcome_non_miss)
  
  best_hosp<-outcome_non_miss[order(  outcome_non_miss[ ,column]
                           ,outcome_non_miss$hospital.name),]
  #head(best_hosp)
  #print(best_hosp[1,])
  #print(best_hosp[nrow(best_hosp),])
  return(best_hosp[rank_to_return,2])
}


# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. 

rankhospital<- function(  state_name     = "TX"
                        , outcome_name   = "heart attack"
                        , num            = "best"
                        , rank_to_return = 1
                        ){
  
    # args_call<-match.call()
    # as.character(args_call[3])
    # num_of_args = length(args_call)-1
    # if (num_of_args %in% 3:4){
    #   if(as.character(args_call[4]))    
    # }
    # #Checking if Third and forth argument are spcified correctlY:
    # if ( class( num ) == "numeric"  & class( rank_to_return ) == "character" ){
    #   temp <- num
    #   num  <- rank_to_return
    #   rank_to_return <- temp
    # }
    # 
    # 
  
    #Check 'num' argument
    if ( !num %in% c("best", "worst") ) {
      message(paste("Invalid 'num' vulue:", num))
      stop()
    }
  
    ## Read outcome data
    #if ( !exists(x="outcome"))
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #dim(outcome)
    
    names(outcome)<-tolower(names(outcome))
    
    #Looking for needed column
    column<-paste("hospital.30.day.death..mortality..rates.from."
                  ,sub(pattern = " ", replacement = ".",x = outcome_name)
                  ,sep = "" ,collapse = ""
    )
    #print(paste("Column is",column))
    
    ## Check that state and outcome are valid
    if ( nrow( outcome[outcome$state == state_name,] )==0 ){
      message("Invalid State")
      stop()
    }else if(!( column %in% colnames(outcome)) ){
      message("Invalid Outcome")
      stop()
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    outcome[,column]<-as.numeric(outcome[,column])
    #head(outcome)
    nrow(outcome)
    
    outcome_non_miss<-outcome[!is.na(outcome[,column])
                              & outcome$state == state_name
                              , c(column, "hospital.name")]
    # head(outcome_non_miss)
    #str(outcome_non_miss)
    dim(outcome_non_miss)
    
    # Changing Sorting Order according to value of 'NUM' argument
    if ( num == "best"  ){ desc_order = 1 
    }else { desc_order = -1
    }
    
    best_hosp<-outcome_non_miss[order( desc_order * outcome_non_miss[ ,column]
                                      ,outcome_non_miss$hospital.name)
                                ,]
    
    
    head(best_hosp[,1])
    # print(best_hosp[1,])
    # print(best_hosp[nrow(best_hosp),])
    
    #Check the value of 'rank_to_return'
    if ( !rank_to_return %in% 1:nrow(best_hosp) ) {
      message("Invalid rank_to_return vulue:",rank_to_return)
      stop()
    }
    #return(best_hosp[,])
    return(best_hosp[rank_to_return,2])
}


# a<-function(a= 1, b ="a"){
#   
#  return( list(sys.call(), match.call()) )
#   
#   return(paste("a=",a,",b=",b))  
#   
# }
# a(a= 1, b ="a")
