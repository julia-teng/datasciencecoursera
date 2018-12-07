pollutantmean <- function(directory,pollutant,id=1:332){
  
  ## directory is a character vector of length 1 
  ## indicating the location og the CSV files
  
  ## pollutant is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate
  ## the mean; either sulfate or nitrate
  
  ## id is an integer vector indicating the monitor id 
  ## numbers to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the is vector 
  
  directory <- paste(getwd(),"/",directory,"/",sep="")
  directory

  filelist <-list.files(directory,pattern=".csv",full.names = TRUE)
  ## Empty for now
  data <- NA

  for(i in id ){
    file_data <- read.csv(filelist[i])

    data <- rbind(data,file_data)
  }
  mean(data[[pollutant]],na.rm = TRUE)
  
}
