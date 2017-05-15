setwd("d:/GitHub/ProgrammingAssignment3")
source("best.R")
source("rankhospital.R")
source("rankall.R")
best("ma", "heart attack")
best("TX", "heart failure")
best("MD", "Pneumonia")
best("bb", "heart attack")
best("NY", "hert attack")


best("MA", "Heart Attack")

rankhospital("MA", "Heart Attack", 53)

rankhospitalState("MA", "Heart Attack", 53)
rankhospital("TX", "Heart Failure", 53)
rankhospital("NH", "Heart Atack", "best")
rankhospital("NH", "Heart Attack", "worst")

repository <- hospitalRepository()
x <- repository$getOutcome("TX", "Heart Failure")
x
nrow(x)

repository$getStateList()

rankall("Heart Attack",20)
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

repo <- hospitalRepository()
x <- repo$getAll()
sl <- unique(x$State)
sl
nrow(sl)
length(sl)
names(x)

names(x)
dataSelected <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
dataSelected

badHospital <- data.frame(x$State, x$Hospital.Name)
badHospital[badHospital$Hospital.Name == "CRESTWOOD MEDICAL CENTER"]
stateList <- unique(x$State)
stateList

myha <- data.frame(x$State, x$Hospital.Name, x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
myhf <- data.frame(x$State, x$Hospital.Name, x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
mypn <- data.frame(x$State, x$Hospital.Name, x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
myha <- myha[myha$x.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
myha

cat("\014");

head()
state.abb
state.x77


