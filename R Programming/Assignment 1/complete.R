complete <- function (directory, id=1:332){
  ## directory is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## id is an integer vector indicating the monitor id numbers
  ## to be used
  
  ## return a data frame of the form:
  ## id nobs
  ## 1  117
  ## id is the monitor is number and nobs is the number of complete cases
  
  directory <- paste(getwd(),"/",directory,"/",sep="")
  
  filelist <-list.files(directory,pattern=".csv",full.names = TRUE)
  ids <- vector()
  nobs <- vector()
  
  for (i in id){
    ids = c(ids,i)
    nobs = c(nobs,sum(complete.cases(read.csv(filelist[i])))) 
    
    }
  
  data.frame(ids,nobs)
}