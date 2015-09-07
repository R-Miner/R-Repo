best <- function(state, outcome) {

  ## Read outcome data
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!state %in% unique(hospital_data[,7])) {stop("Invalid State")}
  
  if(outcome == "heart attack"){
    col = 11
  } else if(outcome == "heart failure"){
    col = 17
  } else if (outcome == "pneumonia"){
    col = 23
  } else{ 
    stop("Invalid outcome")
    }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  req_data<- subset(hospital_data,State  == state, select =c(2,col))
  req_data[which.min(req_data[,2]), 1]

}