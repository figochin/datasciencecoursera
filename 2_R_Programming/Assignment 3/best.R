## This function takes two arguments:
##      state   : the 2-character abbreviated name of a state
##      outcome : an outcome name
## The function reads the outcome-of-care-measures.csv file and returns a character 
## vector with the name of the hospital that has the best (i.e. lowest) 30-day 
## mortality for the specified outcome in that state.


best <- function(state, outcome) {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## List of possible outcomes
    outcome <- tolower(outcome)
    conditions <- c('heart attack','heart failure','pneumonia')
    
    ## Check that state and outcome are valid
    if(!outcome %in% conditions) {
        stop("invalid outcome")
    }
    
    if(!state %in% unique(dat$State)) {
        stop("invalid state")
    }
    
    ## Create a subset of outcome data with following columns:
    ## [1] Hospital Name
    ## [2] State
    ## [3] 30-day Death Mortality Rates from Heart Attack
    ## [4] 30-day Death Mortality Rates from Heart Failure
    ## [5] 30-day Death Mortality Rates from Pneumonia
    sub_dat <- dat[c(2, 7, 11, 17, 23)]
    names(sub_dat)[3] <- 'heart attack'
    names(sub_dat)[4] <- 'heart failure'
    names(sub_dat)[5] <- 'pneumonia'
    
    ## Extract the data with respect to State & Outcome arguments
    sub_dat <- subset(sub_dat, State == state & sub_dat[outcome] != 'Not Available')

    ## Check for the row index of the minimum death rate
    row_min <- which.min(sub_dat[outcome][, 1])
    
    ## Return hospital name in that state with lowest 30-day death rate
    best_hosp <- sub_dat[row_min, 1]
    
    ## If there are more than one best hospital, select the first hospital
    ## sorted in alphabetical order
    if(length(best_hosp) > 1) {
        best_hosp <- sort(best_hosp)[1]
    }
    
    best_hosp
}
