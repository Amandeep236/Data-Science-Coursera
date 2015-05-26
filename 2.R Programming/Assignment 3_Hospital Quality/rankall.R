

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