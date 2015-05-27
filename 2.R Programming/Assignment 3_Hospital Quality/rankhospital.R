
## Part 2 : Ranking hospitals by outcome in a state

## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).

## The function reads the outcome-of-care-measures.csv and returns a character vector with the name
## of the hospital that has the ranking specised by the num argument. 

## For example, the call rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values \best", \worst", or an integer indicating the ranking
## (smaller numbers are better).


rankhospital <- function(state, outcome, num = "best") {    
    
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
    
    State.new <- State[!is.na(State[,column]),]
    State.new$Rank <- rank(State.new[,column])
    
    Result <- State.new %>%
        arrange(Rank) %>%
        select(Hospital.Name)   
    
    if (num == "best") {
        rank_result <- 1
    }
    else if (num == "worst") {
        rank_result <- nrow(Result)
    }
    else {
        rank_result <- num
    }
    
    Result[rank_result,]
    
}



## Test

rankhospital("TX", "heart failure", 4)
# "DETAR HOSPITAL NAVARRO"

rankhospital("MD", "heart attack", "worst")
# "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
# NA
