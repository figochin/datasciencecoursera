# Programming Assignment 1: Air Pollution

For this first programming assignment you will write three functions that are meant to interact with dataset that accompanies this assignment. The dataset is contained in a zip file specdata.zip that you can download from the Coursera web site.

The zip file containing the data can be downloaded here: [specdata.zip](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip) [2.4MB]
The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
- Date    : the date of the observation in YYYY-MM-DD format (year-month-day)
- sulfate : the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
- nitrate : the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

## Part 1
Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.

```R
install.packages("data.table")
library("data.table")

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    # Obtain the list of .csv files to be read
    temp <- list.files(directory)[id]
    
    # Enter directory, read data into table, and exit to previous directory
    setwd(directory)
    DT = do.call(rbind, lapply(temp, data.table::fread))
    setwd('..')
    
    # Extract columns based on type of pollutant
    newDT <- DT[[match(pollutant, names(DT))]]
    
    # Calculate and return the mean
    mean(newDT, na.rm = TRUE)
}
```

## Part 2
Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.

```R
complete <- function(directory, id = 1:332) {
    
    # Create empty vectors to store ID's and Row number
    idlist <- c()
    rowlist <- c()
    idx <- 1
    
    # Obtain the list of .csv files to be read
    temp <- list.files(directory)[id]
    
    # Enter directory with .csv files
    setwd(directory)
    
    for (i in id) {
        
        # Read .csv files
        DF <- read.csv(temp[idx], header = TRUE)

        # Extract rows that does not have missing values
        DFnotna <- DF[complete.cases(DF), ]
        
        # Concatenate the ID's and Row number vectors
        idlist <- c(idlist, i)
        rowlist <- c(rowlist, nrow(DFnotna))
        
        # Increment of index by 1
        idx <- idx + 1
        
    }
    # Exit to previous directory
    setwd('..')
    
    # Return data frame of file name and number of complete cases
    data.frame(id = idlist, nobs = rowlist)   
}
```

## Part 3
Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 

```R
source("complete.R")

corr <- function(directory, threshold = 0) {
    
    # Create a numeric vector of correlations
    correlation <- vector()
    
    # Determine the file ID's where number of complete cases above threshold
    completes <- complete( directory, 1:length(list.files(directory)) )
    completes_threshold <- subset( completes, nobs > threshold )
    
    if ( all(is.na(completes_threshold)) == FALSE ) {
    
        readID <- completes_threshold$id
        
        # Obtain the list of .csv files to be read
        temp <- list.files(directory)[readID]
        setwd(directory)
        
        for (i in 1:length(temp)) {
            
            # Read .csv files
            DF <- read.csv(temp[i], header = TRUE)
    
            # Extract rows that does not have missing values
            DFnotna <- DF[complete.cases(DF), ]
            
            # Calculate correlations between sulfate and nitrate columns
            correlation <- c(correlation, cor(DFnotna$sulfate, DFnotna$nitrate))

        }
        # Exit to previous directory
        setwd('..')
    }
    # Return the correlations vector
    correlation
}
```
