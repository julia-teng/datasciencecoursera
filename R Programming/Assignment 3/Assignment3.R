## Assignment 3

setwd("C:/users/julia.teng/Desktop/Julia/Training/Data Science Specialization/datasciencecoursera/R Programming/Assignment 3")

## Part 1 - Plot the 30 day mortality rates for heart attack
## read file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])