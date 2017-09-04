nBlocks <- length(unique(sorted.data$block))
nTrials <- max(sorted.data$trial)
nBandits <- 4
participantData <- subset(sorted.data,ID %in% participantID)
nParticipants <- length(participantID)

choices <- array(0,dim = c(nBlocks,nTrials,nParticipants))
points <- array(0,dim = c(nBlocks,nTrials,nParticipants))
changepoint <- matrix(0,nrow = nParticipants, ncol = nBlocks)
changeID <- matrix(0,nrow = nParticipants, ncol = nBlocks)

# extract choice and points data
for (i in 1:nParticipants){
  temp <- subset(participantData,ID == participantID[i])
  choices[,,i] <- matrix(temp$choice,nBlocks,nTrials,byrow = T)
  points[,,i] <- matrix(temp$pointsWon,nBlocks,nTrials,byrow = T)
  for (ii in 1:nBlocks){
    blockData <- subset(temp,block == ii)
    changepoint[i,ii] <- match(unique(blockData$whichFilled),blockData$whichFilled)[2]
    changeID[i,ii] <- blockData[changepoint[i,ii],]$whichFilled
  }
}
choices[choices == "top"] <- 1
choices[choices == "right"] <- 2
choices[choices == "bottom"] <- 3
choices[choices == "left"] <- 4
choices <- array(as.numeric(choices),dim = c(nBlocks,nTrials,nParticipants))
changeID[changeID == "top"] <- 1
changeID[changeID == "right"] <- 2
changeID[changeID == "bottom"] <- 3
changeID[changeID == "left"] <- 4
changeID <- matrix(as.numeric(changeID), nrow = nParticipants, ncol = nBlocks)