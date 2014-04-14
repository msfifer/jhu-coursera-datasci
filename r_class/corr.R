corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # get all the filenames
  filenames = list.files(directory, pattern=".csv")
  
  # get all the data first
  correlations = vector()
  for (filename in filenames) {
    newData <- read.csv(paste(directory, "/", sprintf("%03d", oneID), ".csv", sep=""))
    completeCases = complete.cases(newData)
    if(sum(completeCases) > threshold) {
      correlations = c(correlations, cor(newData["sulfate"][completeCases], newData["nitrate"][completeCases]))
    }
    allData <- rbind(allData, newData)
  }
  
  # then, get the pollutant in question
  if(length(correlations) < 1) {
    return(0)
  }
  else {
    return(correlations) 
  }
  
}