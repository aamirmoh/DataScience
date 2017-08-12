loadoutcomefile <- function(){vdata <- read.csv("~/DataScience/outcome-of-care-measures.csv",colClasses = "character") #load into a data.frame object
}

#Plot histogram
#outcome[,11] <- as.numeric(outcome[,11]) # coerce to numeric
#hist(outcome[,11]) #plot a histogram
#End Plot Histogram

best <- function(state, outcome) {
  
  outcomeFile <- loadoutcomefile()  
  
   #check for accuracy
 
  if(is.null(state)  | !is.character(state) | nchar(state)!=2 | sum(outcomeFile$State==state)== 0)
    {stop("invalid state")} 
  if(is.null(outcome)  | !is.character(outcome))
    {stop("invalid outcome")} 
  #!identical('heart attack',outcome) | !identical('heart failure',outcome) | !identical('pneumonia',outcome)
  #load outcome file
  
  
  # assign column numbers
  
  if(outcome == "heart attack")
                                    {outcomedata <- outcomeFile[outcomeFile$State == state,c(2,11)]
                                     outcomedata[,2] <- as.numeric(outcomedata[,2]) 
                                    }
  else if(outcome == "heart failure"){outcomedata <- outcomeFile[outcomeFile$State == state,c(2,17)]
                                      outcomedata[,2] <- as.numeric(outcomedata[,2])
                                     }
  else if(outcome == "pneumonia"){outcomedata <- outcomeFile[outcomeFile$State == state,c(2,23)]
                                    outcomedata[,2] <- as.numeric(outcomedata[,2]) 
  }
  else {stop("invalid outcome")}
  
  outcomedata <- outcomedata[complete.cases(outcomedata[,2]),] # get rid of NAs
  outcomedata <- outcomedata[outcomedata[,2] == min(outcomedata[,2]),]
  names(outcomedata) <- c("name","outcome")
  outcomedata <- outcomedata[order(outcomedata$name),]
  outcomedata[1,1]
  
# filter data frame by state
# x <- tapply(as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),outcome$State ="AL",max,na.rm=TRUE, simplify = TRUE) 
#voutcome <- tapply(as.numeric(outcomeFile[,voutcomecol]),outcomeFile[,State],max,na.rm=TRUE, simplify = TRUE)

}