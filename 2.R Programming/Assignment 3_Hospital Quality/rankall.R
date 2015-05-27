
## Part 3 : Ranking hospitals in all states

## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
## (num). 

## The function reads the outcome-of-care-measures.csv and returns a 2-column data frame
## containing the hospital in each state that has the ranking specised in num. 

## For example the function call rankall("heart attack", "best") would return a data frame containing 
## the names of the hospitals that are the best in their respective states for 30-day heart attack death rates. 
## The function should return a value for every state (some may be NA). The first column in the data frame is named hospital, 
## which contains the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.




rankall <- function(outcome, num = "best") {    
    
    library(dplyr)
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    valid_outcome <- c("heart attack", "heart failure", "pneumonia") 
    
    if(!is.element(outcome, valid_outcome)) stop("invalid outcome")
    
    column <- NULL 
    if (outcome == "heart attack") { 
        data <- mydata[,c(2,7,11)] 
    }
    else if (outcome == "heart failure") { 
        data <- mydata[,c(2,7,17)]
    } 
    else if (outcome == "pneumonia") {
        data <- mydata[,c(2,7,23)] 
    } 
    else {
        stop("invalid outcome")
    }
    
    data[,3] <- as.numeric(data[,3])
    
    data.new<- data[!is.na(data[,3]),]
    names(data.new)[3] <- "column"
    
    Result <- data.new %>%
        group_by(State) %>%
        arrange(column,State,Hospital.Name) %>%
        mutate(Num = 1,
               rank = cumsum(Num)) %>%
        select(Hospital.Name, State, rank) %>%
        rename(hospital = Hospital.Name, state = State) 
    
    all <- expand.grid(state = unique(Result$state), rank = c(1:227)) %>%
           mutate(state = as.character(state)) %>%
           arrange(state) %>%
           left_join(Result, by = c("state","rank"))
    
    if (num == "best") {
        rank_result <- 1
        A <- all %>% 
             filter(rank == rank_result) %>%
             select(hospital, state)
        
    }
    else if (num == "worst") {
        A <- all %>%
             filter(!is.na(hospital)) %>%
             group_by(state) %>%
             filter(rank == n_distinct(hospital)) %>%
             select(hospital, state)
    }
    else {
        rank_result <- num
        A <- all %>% 
             filter(rank == rank_result) %>%
             select(hospital, state)
    }
    A
    
}


## Test

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)


