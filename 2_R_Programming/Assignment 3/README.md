# Introduction

The zip file containing the data for this assignment can be downloaded here: [Programming Assignment 3 Data](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip) [832K]

The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and Human Services. 
The purpose of the web site is to provide data and information about the quality of care at over 4,000 Medicare-certified
hospitals in the U.S. This dataset essentiallycovers all major U.S. hospitals. This dataset is used for a variety 
of purposes, including determining whether hospitals should be fined for not providing high quality care to patients
(see http://goo.gl/jAXFX for some background on this particular topic).

## Part 1: Plot the 30-day mortality rates for heart attack

```R
## Read outcome data in R
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

## Extract information on the 30-day death rates from heart attack (column 11)
outcome[, 11] <- as.numeric(outcome[, 11])

## Create a histogram of the data
hist(outcome[, 11],
     xlab = 'Death Rates',
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     col="lightblue",
)
```

## Part 2: Finding the best hospital in a state

```R
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
```

## Part 3: Ranking hospitals by outcome in a state

```R
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
```

## Part 4: Ranking hospitals in all states

```R
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
```
