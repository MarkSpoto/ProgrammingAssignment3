#setwd("d:/GitHub/ProgrammingAssignment3")
source("best.R")

#
# rankhospitalState returns the matrix containg the
# state, hospital name and ranking.  The repository containing 
# the data must be passed in
#
rankhospitalState <- function(data, state, outcome, num) {
  ## Get the outcome which checks the state and outcome are valid
  #result <- repository$getOutcome(state, outcome)
  result <- data
  
  ## Stop the program if the result is NULL or NA
  if (is.null(result) || is.na(result)) {
    stop("Result Set Is Null")
  }
  
  ## Return the hospital name in that state with the given rank
  maxRows <- nrow(result)
  rankNbr <- 1
  if (tolower(num) == "best") {
    rankNbr <- 1
  } else if (tolower(num) == "worst") {
    rankNbr <- maxRows
  } else if (num > maxRows) {
    return(NA)
  } else {
    rankNbr <- num
  }
  result[rankNbr,]
}

#
# rankhospital returns the hospital based on the state, outcome and ranking number
#
rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  repository <- hospitalRepository()
  data <- repository$getOutcome(state, outcome)
  
  # Get matrix containing the hospital rank, which validate the state and outcome
  result <- rankhospitalState(data, state, outcome, num)
  
  # Return the hospital name
  result[1,1]
}



