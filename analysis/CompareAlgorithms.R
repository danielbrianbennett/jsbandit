# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v2point2" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# load list of filtered IDs
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

# retain only participants with an ID in the white-list
sorted.data <- sorted.data[sorted.data$ID %in% filtered.IDs,]

nParticipants <- length(filtered.IDs)

# calculate cumulative winnings
nSims <- 1000
cumWinnings <- matrix(data = NA, nrow = nParticipants, ncol = 90)
cumWinnings_random <- matrix(data = NA, nrow = nParticipants, ncol = 90)
cumWinnings_perfect <- matrix(data = NA, nrow = nParticipants, ncol = 90)

for (i in 1:nParticipants){
  
  cumWinnings[i,] <- cumsum(sorted.data[sorted.data$ID == filtered.IDs[i],]$pointsWon)
  payoffs <- unsorted.data[unsorted.data$ID == filtered.IDs[i],]$payoffs
  payoffs <- payoffs[which(nchar(payoffs) == max(nchar(payoffs)))]
  payoffs <- scan(text = gsub("[^0-9,]", "", payoffs),sep=",")
  payoffs <- aperm(array(data = payoffs, dim = c(30,3,4)), perm = c(3,1,2))
  
  # perfect knowledge
  counter <- 0
  bestOption <- vector(mode = "logical",length = 90)
  for (j in 1:3){
    for (k in 1:30){
      counter <- counter + 1
      bestOption[counter] <- max(payoffs[,k,j])
    }
  }
  cumWinnings_perfect[i,] <- cumsum(bestOption)
  
  # random choices
  randomTotal <- matrix(data = NA,nrow = nSims, ncol = 90)
  for (sim in 1:nSims){
    counter <- 0
    randomOption <- vector(mode = "logical", length = 90)
    for (j in 1:3){
      for (k in 1:30){
        counter <- counter + 1
        randomOption[counter] <- (payoffs[sample(1:4,1),k,j])
      }
    }
    randomTotal[sim,] <- cumsum(randomOption)
  }
  cumWinnings_random[i,] <- colMeans(randomTotal)
}