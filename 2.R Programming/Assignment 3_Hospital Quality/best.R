
## Part 1 : Finding the best hospital in a state

## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specised outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state, outcome) {    
    
    library(dplyr)
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    valid_outcome <- c("heart attack", "heart failure", "pneumonia") 
    
    if(!is.element(state,mydata[,c("State")])) stop("invalid state")
    if(!is.element(outcome, valid_outcome)) stop("invalid outcome")
    
    column <- NULL 
    if (outcome == "heart attack") { 
        column <- names(mydata)[11] 
    }
    else if (outcome == "heart failure") { 
        column <- names(mydata)[17]
    } 
    else if (outcome == "pneumonia") {
        column <- names(mydata)[23] 
    } 
    else {
        stop("invalid outcome")
    }
    
    State <- subset(mydata, state == mydata[,"State"])
    State[,column] <- as.numeric(State[,column])
    
    State$Rank <- rank(State[,column])
    
    Result <- State %>%
        filter(!is.na(Rank)) %>%
        arrange(Rank) %>%
        select(Hospital.Name)
    
    Result[1,]
} 


## Test

best("TX", "heart attack")
#"CYPRESS FAIRBANKS MEDICAL CENTER"

best("TX", "heart failure")
#"FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack")
#"JOHNS HOPKINS HOSPITAL, THE"

best("MD", "pneumonia")
#"GREATER BALTIMORE MEDICAL CENTER"

best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state

best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome






