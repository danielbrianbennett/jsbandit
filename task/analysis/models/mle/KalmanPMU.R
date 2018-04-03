A <- array(data = 0, dim = c(3,4,4))
A[,,1] <- matrix(c(1,1,1,-1,0,0,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
A[,,2] <- matrix(c(-1,0,0,1,1,1,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
A[,,3] <- matrix(c(-1,0,0,0,-1,0,1,1,1,0,0,-1),nrow = 3, ncol = 4)
A[,,4] <- matrix(c(-1,0,0,0,-1,0,0,0,-1,1,1,1),nrow = 3, ncol = 4)

ChangeBonus <- function(nTrials, nBlocks, changeBonus, trialDecay, blockDecay){  
  bonus <- matrix(data = 0, nrow = nBlocks, ncol = nTrials)  
  for (i in 1:nBlocks){
    bonus[i,] <- changeBonus * ( i ^ -blockDecay ) * ( c(1:nTrials) ^ -trialDecay ) 
  }  
  return(bonus)
}


KalmanPMU <- function(choices,points,zeta,epsilon,B,k,j){
  
  if (zeta >= 0 & epsilon >= 0){
  
  bonus <- ChangeBonus(nTrials,nBlocks,B,k,j)
  
  # create containers
  banditMean <- array(data = 0, dim = c(nBandits,nTrials+1,nBlocks))
  banditVariance <- array(data = 1000, dim = c(nBandits,nTrials+1,nBlocks))
  kalmanGain <- array(dim = c(nBandits,nTrials,nBlocks))
  allChoiceProb <- array(dim = c(nBandits,nTrials,nBlocks))
  choiceProb <- matrix(0,nrow = nBlocks, ncol = nTrials)
  
  for (iBlock in 1:nBlocks){
    
    for (iTrial in 2:(nTrials+1)){
      
      deltaFunction <- vector(mode = "integer",length = nBandits)
      deltaFunction[choices[iBlock,iTrial-1]] = 1
      
      for (iBandit in 1:nBandits){
        
        kalmanGain[iBandit,iTrial-1,iBlock] <- (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2)) / (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2) + (epsilon ^ 2))
        banditMean[iBandit,iTrial,iBlock] <- banditMean[iBandit,iTrial-1,iBlock]  + deltaFunction[iBandit] * kalmanGain[iBandit,iTrial-1,iBlock] * (points[iBlock,iTrial-1] - banditMean[iBandit,iTrial-1,iBlock])
        if ((iTrial-1) >= changepoint[iBlock] & iBandit == changeID[iBlock]){
          banditMean[iBandit,iTrial-1,iBlock] <- banditMean[iBandit,iTrial-1,iBlock] + bonus[iBlock,iTrial-changepoint[iBlock]]
        }
         
        banditVariance[iBandit,iTrial,iBlock] <- (1 - (deltaFunction[iBandit] * kalmanGain[iBandit,iTrial-1,iBlock])) * (banditVariance[iBandit,iTrial-1,iBlock] + (zeta ^ 2))
        M = A[,,iBandit] %*% banditMean[,iTrial-1,iBlock]
        H = A[,,iBandit] %*% diag(banditVariance[,iTrial-1,iBlock] + (epsilon^2)) %*% t(A[,,iBandit])
        allChoiceProb[iBandit,iTrial-1,iBlock] <- pmvnorm(lower= 0,mean = as.vector(M), sigma = H)[1]
        choiceProb[iBlock,iTrial-1] <- allChoiceProb[choices[iBlock,iTrial-1],iTrial-1,iBlock]
      }
  }
}
  
  temp <- log(choiceProb)
  temp[temp == -Inf] <- -100
  negativeLL <- -sum(temp)
  
  output <- list("banditMean" = banditMean,
                 "banditVariance" = banditVariance,
                 "kalmanGain" = kalmanGain,
                 "choices" = choices,
                 "points" = points,
                 "bonus" = bonus,
                 "changepoint" = changepoint,
                 "changeID" = changeID,
                 "allChoiceProb" = allChoiceProb,
                 "choiceProb" = choiceProb,
                 "negativeLL" = negativeLL)
  } else{
    output <- list("negativeLL" = NA)
  }
  
}