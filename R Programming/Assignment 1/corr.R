corr <- function(directory, threshold =0){
  ## director is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## threshold is a numeric vector of length 1 indicating the 
  ## number of completely observed observations required to
  ## compute the correlation between nitrate and sulfate
  
  ## return a numeric vector of correlations

  
  completeness <- complete(directory)
  ## getting nonbs greater than threshold
  filter_com = subset(completeness,completeness$nobs>threshold)
  filter_com
  
  filelist <- list.files(directory,pattern=".csv",full.names = TRUE)
  cr <- vector()
  
  for (i in filter_com$ids){
    
    data <- read.csv(filelist[i])
    ## remove NA
    data <- subset(data,complete.cases(data))
    cr <-c(cr,cor(data$nitrate,data$sulfate))
  }
  cr
  
}