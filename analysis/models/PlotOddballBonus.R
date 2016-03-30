# parameters
A <- 16.79 # size of change bonus
k <- 14.74 # trial decay parameter
j <- 1.2 # block decay parameter

# constants
nTrials <- 30 # number of trials to calculate bonus size for
nTrialsToPlot <- 15 # number of trials to plot bonus size for
nBlocks <- 3

# function
bonusFunction <- function(nTrials, blockNumber, changeBonus, trialDecay, blockDecay){
  
  return( changeBonus * ( blockNumber ^ -blockDecay ) * ( c(1:nTrials) ^ -trialDecay ) )
}

# calculate and plot bo nuses as a function of block and trial number
bonuses <- matrix(nrow = nBlocks, ncol = nTrials)
par(mfrow = c(1,nBlocks))

for (i in 1:nBlocks){
  
  bonuses[i,1:nTrials] <- bonusFunction(nTrials, i, A, k, j)
  plot(1:nTrialsToPlot, bonuses[i,1:nTrialsToPlot], type = "b", ylim = c(1,min(100,max(30, A * 2.5))),main = sprintf("Block %.0f", i), xlab = "N Trials Post-Change", ylab = "Oddball Bonus")
  
}