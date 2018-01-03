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