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
  
  # get all the data first
  allData <- read.csv(paste(directory, "/", sprintf("%03d", id[1]), ".csv", sep=""))
  for (oneID in id[-1]) {
    newData <- read.csv(paste(directory, "/", sprintf("%03d", oneID), ".csv", sep=""))
    allData <- rbind(allData, newData)
  }
  
  # then, get the pollutant in question
  allData <- allData[pollutant]
  allData <- allData[!is.na(allData)]
  return(mean(allData))
  
}