complete <- function(directory, id=1:332) {
  nobs <- numeric(0)
  for (i in id) {
    
    # Open csv file and extract the subset of interest
    if (i < 10) { csvname <- paste("00", i, ".csv", sep="") }
    else if (i < 100) { csvname <- paste("0", i, ".csv", sep="") }
    else { csvname <- paste(i, ".csv", sep="") }
    csv <- read.csv(paste(directory, csvname, sep="/"))
    data <- subset(csv, (!is.na(csv[, "sulfate"])) & (!is.na(csv[, "nitrate"])))
    
    # Count the number of rows and store it in vector
    nobs <- c(nobs, dim(data)[1])
  }
  df <- data.frame("id"=id, "nobs"=nobs)
  return(df)
}