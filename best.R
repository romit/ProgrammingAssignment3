## Finding the best hospital in the state

best <- function(state, outcome="pneumonia") {
  
  ## Step 1: Read the outcome data
  outcare <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  
  ## Step 2: Check validity of input 
  
  # See if state and outcomes are %in% the vectors of existing values
  # Else return the errors
  
  # For state we look for state within the states mentioned. We first create a unique vector
  # of states
  listofstates <- unique(outcare$State)
  
  if (!state %in% listofstates) {
    stop("invalid state")
  }
  
  # For checking outcomes we look for passed paratemeter and compare it with a prepareed
  # vector. In this case, there are only three possible outcomes
  listofoutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!outcome %in% listofoutcomes) {
    stop("invalid outcome")
  }
  
  
  ## Step 3 
  
  # Define a generic function that returns the best hospital
  bestHospital <- function(df,colnum,st){
    d2 <- subset(df,df$state==st) # filter and keep rows with the state
    d2 <- d2[order(d2[,colnum]),] # sort the column associated with colnum
    return(d2$name[1]) 
  }
  
  # take a subset of the data with the mortality data (using dplyr)
  library(dplyr)
  d1 <- select(outcare, Hospital.Name, State,  starts_with("Hospital.30.Day.Death."))
  
  # Simplify column names
  names(d1) <- c("name", "state", "heart.attack", "heart.failure", "pneumonia")

  # Coerce the columns in question into numeric, needed for sorting
  d1$heart.attack <- suppressWarnings(as.numeric(d1$heart.attack))
  d1$heart.failure <- suppressWarnings(as.numeric(d1$heart.failure))
  d1$pneumonia <- suppressWarnings(as.numeric(d1$pneumonia))
  
  
  # If statements to assign colnum
  if (outcome=="heart attack"){
    colnum <- 3 # 3rd column
  }
    
  if (outcome=="heart failure"){
      colnum <- 4 # 4th column
  } else {
    colnum <- 5 # 5th column
  }
  
  # Call the bestHospital function. The function will return the name 
  bestHospital(d1,colnum,state)

  
}