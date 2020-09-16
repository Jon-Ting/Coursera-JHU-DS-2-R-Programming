corr <- function(directory, threshold=0) {
  correlations <- numeric(0)
  for (i in 1:332) {
    
    # Open csv file and extract the subset of interest
    if (i < 10) { csvname <- paste("00", i, ".csv", sep="") }
    else if (i < 100) { csvname <- paste("0", i, ".csv", sep="") }
    else { csvname <- paste(i, ".csv", sep="") }
    csv <- read.csv(paste(directory, csvname, sep="/"))
    data <- na.omit(csv)
    
    # Compute correlation based on conditions
    if (dim(data)[1] > threshold) {
      correlation <- cor(data["sulfate"], data["nitrate"])
      correlations <- c(correlations, correlation)
    }
  }
  return(correlations)
}