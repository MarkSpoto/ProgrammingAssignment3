#setwd("d:/GitHub/ProgrammingAssignment3")
source("best.R")

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  repository <- hospitalRepository()
  
  ## Get the outcome which checks the state and outcome are valid
  result <- repository$getOutcome(state, outcome)
  
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
    message(paste("maxRows=", maxRows))
    rankNbr <- maxRows
    message(paste("rankNbr=", rankNbr))
  } else if (num > maxRows) {
    return(NA)
  } else {
    rankNbr <- num
  }
  result[rankNbr, 1]
}


