loadoutcomefile <- function(){vdata <- read.csv("~/DataScience/outcome-of-care-measures.csv",colClasses = "character") #load into a data.frame object
}

#Plot histogram
#outcome[,11] <- as.numeric(outcome[,11]) # coerce to numeric
#hist(outcome[,11]) #plot a histogram
#End Plot Histogram

rankall <- function(outcome , num="best") {
  
  outcomeFile <- loadoutcomefile()  
  
   #check for accuracy
  if(is.null(outcome)  | !is.character(outcome))
    {stop("invalid outcome")} 
  
  # assign column numbers
  
  if(outcome == "heart attack")
                                    {outcomedata <- outcomeFile[,c(2,7,11)]
                                     outcomedata[,3] <- as.numeric(outcomedata[,3]) 
                                    }
  else if(outcome == "heart failure"){outcomedata <- outcomeFile[,c(2,7,17)]
                                      outcomedata[,3] <- as.numeric(outcomedata[,3])
                                     }
  else if(outcome == "pneumonia"){outcomedata <- outcomeFile[,c(2,7,23)]
                                    outcomedata[,3] <- as.numeric(outcomedata[,3]) 
  }
  else {stop("invalid outcome")} # invalid outcome
  
  outcomedata <- outcomedata[complete.cases(outcomedata[,3]),] # get rid of NAs in the 2nd column
  
  #outcomedata <- outcomedata[outcomedata[,2] == min(outcomedata[,2]),] -- This code is not required
  names(outcomedata) <- c("name","state","rate") # change the names to simplify
  #outcomedata[1,1]
  
  mydataframe <- data.frame("name","state",stringsAsFactors = FALSE) #empty data frame
  
  if(is.numeric(num)) { 
                        
                                outcomedata <- outcomedata[order(outcomedata$state, outcomedata$rate, outcomedata$name),]
                                y <- split(outcomedata,outcomedata$state)
                                for(i in 1:length(y)){
                                                    if(num > nrow(outcomedata[outcomedata$state == y[[i]]$state[[1]],]))
                                                      {
                                                      #print(y[[i]]$state[[1]])  
                                                      mydataframe[i,] <- c(NA,y[[i]]$state[[1]])
                                                      #return(NA) -- was stopping the function from executing further
                                                      } # compare state in list to dataframe
                                                    else{
                                                      
                                                      #print(y[[i]]$name[num])
                                                      #print(y[[i]]$state[num])
                                                      #print(num)
                                                      #print(i)
                                                      mydataframe[i,] <- c(y[[i]]$name[num],y[[i]]$state[num])}    
                                                    }
                               
                      }
    # order will rank and return the value back
  else if(num == "best")   {
                              outcomedata <- outcomedata[order(outcomedata$state, outcomedata$rate, outcomedata$name),]
                              y <- split(outcomedata,outcomedata$state)
                              for(i in 1:length(y)){
                                # given rank
                                mydataframe[i,] <- c(y[[i]]$name[1],y[[i]]$state[1])
                              }                    
                          }
  else                {
                        outcomedata <- outcomedata[order(outcomedata$state,-outcomedata$rate, outcomedata$name),]
                        y <- split(outcomedata,outcomedata$state)
                        for(i in 1:length(y)){
                          # given rank
                          mydataframe[i,] <- c(y[[i]]$name[1],y[[i]]$state[1])
    }
  }
  
  colnames(mydataframe)<- c("name","state")
  mydataframe
  
# filter data frame by state
# x <- tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),outcome$State ="AL",max,na.rm=TRUE, simplify = TRUE) 
#voutcome <- tapply(as.numeric(outcomeFile[,voutcomecol]),outcomeFile[,State],max,na.rm=TRUE, simplify = TRUE)

}