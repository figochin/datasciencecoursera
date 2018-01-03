## This function takes two arguments:
##      outcome : an outcome name
##      num     : ranking of a hospital in that state for the outcome
## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in 'num' argument.
## The first column in the data frame is named hospital, which contains the hospital name, 
## and the second column is named state, the 2-character abbreviation for the state name.


rankall <- function(outcome, num = 'best') {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## List of possible outcomes
    outcome <- tolower(outcome)
    conditions <- c('heart attack','heart failure','pneumonia')
    
    ## Check that outcome is valid
    if(!outcome %in% conditions) {
        stop("invalid outcome")
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
    sub_dat <- subset(sub_dat, sub_dat[outcome] != 'Not Available')    

    ## Order by ascending order of hospital name, then by death rate, and lastly by state
    sub_dat <- sub_dat[order(sub_dat$State, as.numeric(sub_dat[outcome][,1]),
                             sub_dat$Hospital.Name), ]
    
    ## Create a helper function to return hospital name with the given rank
    rankHosp <- function(df, st, n) {
        df <- df[df$State == st, ]
        
        if(n == 'best') {
            rank_hosp <- df[1, 1]
        } else if(n == 'worst') {
            rank_hosp <- df[nrow(df), 1]
        } else {
            rank_hosp <- df[n, 1]
        }
        
        rank_hosp
    }
    
    ## Create an empty data frame to store the output values
    new_df <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
    
    ## Loop through each state and obtain the hospital with the given rank
    for(st in unique(sub_dat$State)) {
        rank_hosp_st = rankHosp(sub_dat, st, num)
        new_df <- rbind(new_df, data.frame(hospital = rank_hosp_st, state = st))
    }
    
    new_df

}