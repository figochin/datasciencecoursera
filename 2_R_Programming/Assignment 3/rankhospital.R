## This function takes three arguments:
##      state   : the 2-character abbreviated name of a state
##      outcome : an outcome name
##      num     : ranking of a hospital in that state for the outcome
## The function reads the outcome-of-care-measures.csv file and returns a character 
## vector with the name of the hospital that has ranking specified by 'num' argument


rankhospital <- function(state, outcome, num) {
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
    
    ## Order by ascending order of hospital name, then by death rate of outcome
    sub_dat <- sub_dat[order(as.numeric(sub_dat[outcome][,1]), sub_dat$Hospital.Name), ]
    
    ## Return hospital name in that state with the given rank of 30-day death rate
    if(num == 'best') {
        rank_hosp <- sub_dat[1, 1]
    } else if(num == 'worst') {
        rank_hosp <- sub_dat[nrow(sub_dat), 1]
    } else {
        rank_hosp <- sub_dat[num, 1]
    }

    rank_hosp
}