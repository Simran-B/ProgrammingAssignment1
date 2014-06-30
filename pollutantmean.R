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