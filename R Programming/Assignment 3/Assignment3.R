## Assignment 3

setwd("C:/users/julia.teng/Desktop/Julia/Training/Data Science Specialization/datasciencecoursera/R Programming/Assignment 3")

## Part 1 - Plot the 30 day mortality rates for heart attack
## read file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11],main = "Death Rates from Heart Attack (30 days)",xlab="Death Rates (30 days)")

## Part 2 - Finding the best hospital in state
best <- function(state,outcome){
  
  ## Read outcome data
  outtable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- as.data.frame(cbind(outtable[,2], # Hospital
                              outtable[,7], # State
                              outtable[,11], # Heart Attack
                              outtable[,17], # Heart Failure
                              outtable[,23]), # Pneumonia
                              stringsAsFactors = FALSE
                              )
  colnames(data)<-c("Hospital","State","heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if(state %in% data$State & outcome %in%c("heart attack","heart failure","pneumonia")){
    # check state return id
    isState<-which(data[,"State"]==state)
    StateData<-data[isState,]
    # get outcome value
    StateOutcome<-as.numeric(StateData[,eval(outcome)])
    min_val<-min(StateOutcome,na.rm=TRUE)
    result<-StateData[,"Hospital"][which(StateOutcome==min_val)]
    rank <- result[order(result)]
  }else if (!state %in% data$State){
    stop('invalid state')
  }else if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop('invalid outcome')
  }
  ## retunr hospital name in that state with lowest 30 day death rate
  rank
}

## Part 3 Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outtable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- as.data.frame(cbind(outtable[,2], # Hospital
                              outtable[,7], # State
                              outtable[,11], # Heart Attack
                              outtable[,17], # Heart Failure
                              outtable[,23]), # Pneumonia
                        stringsAsFactors = FALSE
  )
  colnames(data)<-c("Hospital","State","heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State){
    stop('invalid state')
  }else if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop('invalid outcome')
  }else if(is.numeric(num)==FALSE){
     if(num=="best"){
       rank<-best(state,outcome)
    }else if(num=="worst"){
      isState<-which(data[,"State"]==state)
      StateData<-data[isState,]
      # get outcome value
      StateData[,eval(outcome)]<-as.numeric(StateData[,eval(outcome)])
      result<-StateData[order(StateData[,eval(outcome)],StateData[,"Hospital"],decreasing = TRUE),]
      rank <- StateData[, "Hospital"][1]
    }else{
     stop('invalid rank')
    }
  }else if (is.numeric(num)==TRUE){
     isState<-which(data[,"State"]==state)
     StateData<-data[isState,]
     # get outcome value
     StateData[,eval(outcome)]<-as.numeric(StateData[,eval(outcome)])
     result<-StateData[order(StateData[,eval(outcome)],StateData[,"Hospital"],decreasing = FALSE),]
     rank <- StateData[, "Hospital"][num]
  }
  rank
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
    