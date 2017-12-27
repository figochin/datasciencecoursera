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