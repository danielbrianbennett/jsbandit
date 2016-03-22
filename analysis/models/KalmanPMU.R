A <- array(data = 0, dim = c(3,4,4))
A[,,1] <- matrix(c(1,1,1,-1,0,0,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
A[,,2] <- matrix(c(-1,0,0,1,1,1,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
A[,,3] <- matrix(c(-1,0,0,0,-1,0,1,1,1,0,0,-1),nrow = 3, ncol = 4)
A[,,4] <- matrix(c(-1,0,0,0,-1,0,0,0,-1,1,1,1),nrow = 3, ncol = 4)

library('mvtnorm')

KalmanPMU <- function(choices,points,zeta,epsilon){

  # hard code number of bandits
  nBandits <- 4
  
  # extract n blocks and n trials
  nBlocks <- dim(choices)[1]
  nTrials <- dim(choices)[2]
    
  # create containers
  banditMean <- array(data = 0, dim = c(nBandits,nTrials+1,nBlocks))
  banditVariance <- array(data = 1000, dim = c(nBandits,nTrials+1,nBlocks))
  kalmanGain <- array(dim = c(nBandits,nTrials,nBlocks))
  choiceProb <- array(dim = c(nBandits,nTrials,nBlocks))
  
  for (iBlock in 1:nBlocks){
    
    for (iTrial in 2:(nTrials+1)){
      
      deltaFunction <- vector(mode = "integer",length = nBandits)
      deltaFunction[choices[iBlock,iTrial-1]] = 1
      
      for (iBandit in 1:nBandits){
        
        kalmanGain[iBandit,iTrial-1,iBlock] <- (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2)) / (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2) + (epsilon ^ 2))
        banditMean[iBandit,iTrial,iBlock] <- banditMean[iBandit,iTrial-1,iBlock]  + deltaFunction[iBandit] * kalmanGain[iBandit,iTrial-1,iBlock] * (points[iBlock,iTrial-1] - banditMean[iBandit,iTrial-1,iBlock])
        banditVariance[iBandit,iTrial,iBlock] <- (1 - (deltaFunction[iBandit] * kalmanGain[iBandit,iTrial-1,iBlock])) * (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2))
        M = A[,,iBandit] %*% banditMean[,iTrial-1,iBlock]
        H = A[,,iBandit] %*% diag(banditVariance[,iTrial-1,iBlock] + (epsilon^2)) %*% t(A[,,iBandit])
        choiceProb[iBandit,iTrial-1,iBlock] <- pmvnorm(lower= 0,mean = as.vector(M), sigma = H)[1]
      }
  }
}
}