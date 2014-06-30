complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  len <- length(id)
  m <- matrix(ncol=2, nrow=len)
  j <- 1
  for (i in id) {
    monitor = sprintf("%03i", i)
    path <- paste(directory, "/", monitor, ".csv", sep="")
    data <- read.csv(path)
    nobs <- nrow(data[complete.cases(data),])
    m[j,1] <- i
    m[j,2] <- nobs
    j <- j + 1
  }
  d <- data.frame(m)
  colnames(d) <- c("id", "nobs")
  return(d)
}