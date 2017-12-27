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
