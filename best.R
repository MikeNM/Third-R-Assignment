best <- function(state, outcome){
  fullData <- read.csv("outcome-of-care-measures.csv", colClasses= 'character')
  if(!any(state == fullData$State)) {
    stop('invalid state')
  }
  if(outcome == 'heart attack'){
    i <- 11
  }
  else if(outcome == 'heart failure'){
    i <- 17
  }
  else if(outcome == 'pneumonia'){
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  StateData <- fullData[fullData$State == state,]
  StateData[,i] <- as.numeric(StateData[,i])
  StateData <- StateData[complete.cases(StateData),]
  NameBest <- StateData[(StateData[,i] == min(StateData[,i])),]$Hospital.Name
  NameBest
}