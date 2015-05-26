

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


















