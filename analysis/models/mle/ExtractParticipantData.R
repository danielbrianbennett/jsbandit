nBlocks <- length(unique(sorted.data$block))
nTrials <- max(sorted.data$trial)
nBandits <- 4
participantData <- subset(sorted.data,ID == participantID)

# extract choice and points data
choices <- matrix(participantData$choice,nBlocks,nTrials,byrow = T)
choices[choices == "top"] <- 1
choices[choices == "right"] <- 2
choices[choices == "bottom"] <- 3
choices[choices == "left"] <- 4
choices <- matrix(as.numeric(choices),nBlocks,nTrials)
points <- matrix(participantData$pointsWon,nBlocks,nTrials,byrow = T)
changepoint <- vector(mode = "integer",length = nBlocks)
changeID <- vector(mode = "character",length = nBlocks)

for (i in 1:nBlocks){
  blockData <- subset(participantData,block == i)
  changepoint[i] <- match(unique(blockData$whichFilled),blockData$whichFilled)[2]
  changeID[i] <- blockData[changepoint[i],]$whichFilled
}

changeID[changeID == "top"] <- 1
changeID[changeID == "right"] <- 2
changeID[changeID == "bottom"] <- 3
changeID[changeID == "left"] <- 4
changeID <- as.numeric(changeID)