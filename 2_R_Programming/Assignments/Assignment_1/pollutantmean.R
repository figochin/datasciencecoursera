install.packages("data.table")
library("data.table")

pollutantmean <- function(directory, pollutant, id=1:332) {
    
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
