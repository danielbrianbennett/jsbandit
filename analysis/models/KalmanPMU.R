KalmanPMU <- function(participantData,gamma,epsilon){

  # hard code number of bandits
  nBandits <- 4
  
  # derive number of blocks and trials
  nBlocks <- length(unique(participantData$block))
  nTrials <- max(participantData$trial)
  
  # create containers
  banditMean <- array(data = 0, dim = c(nBandits,nTrials+1,nBlocks))
  banditVariance <- array(data = 1000, dim = c(nBandits,nTrials+1,nBlocks))
  kalmanGain <- array(dim = c(nBandits,nTrials,nBlocks))
  
  # get choice and winnings data
  choices <- matrix(participantData$choice,nBlocks,nTrials,byrow = T)
  choices[choices == "top"] <- 1
  choices[choices == "right"] <- 2
  choices[choices == "bottom"] <- 3
  choices[choices == "left"] <- 4
  choices <- matrix(as.numeric(choices),nBlocks,nTrials)
  
  points <- matrix(participantData$pointsWon,nBlocks,nTrials,byrow = T)

}