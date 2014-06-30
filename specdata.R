source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  total <- 0
  count <- 0
  
  for (i in id) {
    monitor = sprintf("%03i", i)
    path <- paste(directory, "/", monitor, ".csv", sep="")
    data <- read.csv(path)
    data_p <- data[[pollutant]]
    data_p <- data_p[complete.cases(data_p)]
    total <- total + sum(data_p)
    count <- count + length(data_p)
  }
  
  return (total / count)
}

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