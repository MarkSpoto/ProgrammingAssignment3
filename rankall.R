source("rankhospital.R")


#
# Rank All States for the best hospital based on 
# outcome and specified ranking number.  If no
# ranking number is passed in use the best value
#
rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  repository <- hospitalRepository()
  
  statedata <- repository$getAll()
  if (is.null(statedata)) {
    stop("Data Load is NULL")
  }
  
  ## Get the outcome which checks the state and outcome are valid
  if (repository$validOutcome(outcome) <= 0) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  stateList <- unique(statedata$State)
  bestStateHospitals <- data.frame(hospital = character(), state = character(), rank = numeric(), stringsAsFactors = FALSE)
  
  i <- 1
  for (state in stateList) {
    data <- repository$getOutcome(state, outcome)
    result <- rankhospitalState(data, state, outcome, num)
    hospital <- NA
    rank <- NA
    if (!is.null(result) && !is.na(result)) {
      hospital <- result$Hospital.Name
      rank <- result[1,2]
    }
    bestStateHospitals[i,] <- list(hospital, state, rank)
    i <- i + 1
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## Sort the data by State
  bestStateHospitals[order(bestStateHospitals$state),1:2]
}
