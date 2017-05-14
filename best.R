# Set the working directory
#setwd("P:/gitprojects/ProgrammingAssignment3")
setwd("d:/GitHub/ProgrammingAssignment3")
packages <- c("dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(dplyr)

#
# Function returns data based on passed in stateCode and data matrix
# Columns in Matrix
#   State, (column 7)
#   Hospital.Name (column 2)
#   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack (column 11) 
#
deathByHeartAttack <- function(stateCode, data) {
  filters <- c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
  dataSelected <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  filteredData <- dataSelected[dataSelected$State==stateCode & dataSelected$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' , filters]
  
  sortedByHospital <- arrange(filteredData, Hospital.Name)
  
  # Convert Death by Heart Attack from character to numeric 
  sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  
  # Now sort by number of heart attackes
  sortedBycondition <- arrange(sortedByHospital, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
}

#
# Function returns data based on passed in stateCode and data matrix
# Columns in Matrix
#   State, (column 7)
#   Hospital.Name (column 2)
#   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure (column 17) 
#
deathByHeartFailure <- function(stateCode, data) {
  filters <- c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
  dataSelected <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  filteredData <- dataSelected[dataSelected$State==stateCode & dataSelected$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' , filters]
  
  sortedByHospital <- arrange(filteredData, Hospital.Name)
  
  # Convert Death by Heart Failure from character to numeric 
  sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  
  # Now sort by number of heart attackes
  sortedByCondition <- arrange(sortedByHospital, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
}

#
# Function returns data based on passed in stateCode and data matrix
# Columns in Matrix
#   State, (column 7)
#   Hospital.Name (column 2)
#   Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure (column 23) 
#
deathByPneumonia <- function(stateCode, data) {
  filters <- c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  dataSelected <- select(data, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  filteredData <- dataSelected[dataSelected$State==stateCode & dataSelected$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!= 'Not Available' , filters]
  
  sortedByHospital <- arrange(filteredData, Hospital.Name)
  
  # Convert Death by Pneumonia from character to numeric 
  sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedByHospital$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  # Now sort by number of heart attackes
  sortedByCondition <- arrange(sortedByHospital, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
}

#
# Hospital Repository
#
hospitalRepository <- function(x = matrix()) {
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  data <- NULL
  
  # Load hospital data
  load <- function() {
    if (is.null(data)) {
      #message("Load Data")
      data <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    }
    data
  }
  
  getOutcome <- function(stateCode, outcome) {
    if (is.null(data)) {
      load()  
    }
    
    ## Check state abbbreviation is valid
    if (!toupper(stateCode) %in% state.abb) {
      stop("invalid state")
      return(NULL)
    }
    
    ## Check the outcome is valid
    if (!tolower(outcome) %in% possibleOutcomes) {
      #stop ("invalid outcome - Must be 'heart attack', 'heart failure' or 'pneumonia'")
      stop ("invalid outcome")
      return(NULL)
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    result <- NULL
    if (tolower(outcome) == "heart attack") {
      result <- deathByHeartAttack(toupper(stateCode), data)
    } else if (tolower(outcome) == "heart failure") {
      result <- deathByHeartFailure(toupper(stateCode), data)
    } else if (tolower(outcome) == "pneumonia") {
      result <- deathByPneumonia(toupper(stateCode), data)
    } else {
      message("Unknown outcome")
    }
    
    # Return the result
    result
  }
  
  # Store the function operations
  list(load = load,
       getOutcome = getOutcome)
  
}

## Function
best <- function(state, outcome) {
  ## Read out data
  #data <- NULL
  repository <- hospitalRepository()
  #data <- repository$load()
  
  result <- repository$getOutcome(state, outcome)
  result[1,1]
}


