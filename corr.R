corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ret <- vector()
  i <- 1
  
  files <- list.files(directory, pattern="*.csv")
  for (f in files) {
    data <- read.csv(paste(directory, "/", f, sep=""))
    data_c <- data[complete.cases(data),]
    nobs <- nrow(data_c)
    if (nobs > threshold) {
      ret[i] <- cor(data_c$sulfate, data_c$nitrate)
      i <- i + 1
    }
  }
  return (ret)
}