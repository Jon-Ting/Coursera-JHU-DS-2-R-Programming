pollutantmean <- function(directory, pollutant, id=1:332) {
  pollutantsum <- 0
  numentry <- 0
  for (i in id) {
    
    # Open csv file and extract the subset of interest
    if (i < 10) { csvname <- paste("00", i, ".csv", sep="") }
    else if (i < 100) { csvname <- paste("0", i, ".csv", sep="") }
    else { csvname <- paste(i, ".csv", sep="") }
    csv <- read.csv(paste(directory, csvname, sep="/"))
    data <- subset(csv, !is.na(csv[, pollutant]))
    
    # Compute cumulative sum and keep track of number of entries
    pollutantsum <- pollutantsum + sum(data[, pollutant])
    numentry <- numentry + dim(data)[1]
  }
  return(pollutantsum / numentry)
}