# Model for a restless bandit task:
# Kalman filter with Probability of Maximum Utility choice rule
# as per Konstantinidis & Speekenbrink (2015)
#
# inputs are
#   choices:          matrix of participant's selections
#   whichFilled:      matrix governing which bandit was filled on a given trial
#   points:           matrix of observed payouts
#   nBandits:         number of bandits
#   nBlocks:          number of blocks per participant
#   nTrials:          number of trials per block
#   mean0:            presumed initial mean value of bandit payoff
#   variance0:        presumed initial variance of bandit payoff
#
# parameters to be estimated are
#   sigma_epsilon:    bandit payout SD
#   b:                bonus size
#   p:                trial-wise bonus decay
#   q:                block-wise bonus decay
#   beta:             softmax inverse temperature

data {
  
  int<lower=1,upper=4> choices;
  int<lower=0,upper=4> whichFilled;
  int<lower=0,upper=100> points;
  int nBandits;
  int nBlocks;
  int nTrials;
  real mean0;
  real variance0;
  
}

parameters {
  
  real<lower=0> sigma_epsilon;
  real b;
  real p;
  real q;
  real<lower=0> beta;
  
}

model{
  
  real sigma_zeta;
  
  #  fix sigma_zeta
  sigma_zeta = 5;
  
  # sigma parameters distributed as uniform
  sigma_epsilon ~ uniform(0,100);
  b ~ normal(0,10);
  p ~ normal(0,5);
  q ~ normal(0,5);
  
  # initialise bandit mean and variance
  for (iBandit in 1:nBandits){
    for (iBlock in 1:nBlocks){
      banditMean[iBlock,1,iBandit] = mean0;
      banditVariance[iBlock,1,iBandit] = variance0;
    }
  }
  
  # Loop through blocks
  for (iBlock in 1:nBlocks){
  
    # Loop through trials
    for (iTrial in 2:(nTrials+1)){
    
      # Loop through bandits a first time
      for (iBandit in 1:nBandits){
      
        # retrieve whether chosen for each bandit
        deltaFunction[iBlock,iTrial,iBandit] = ifelse(iBandit == indicatorChoices[iBlock,iTrial-1],1,0) # deltaFunction = 1 if bandit chosen, 0 otherwise
        bonus[iBlock,iTrial,iBandit] = ifelse(whichFilled[iBlock,iTrial-1] == iBandit && changeLag[iBlock,iTrial-1] > 0, b * pow(changeLag[iBlock,iTrial-1],p) * pow(iBlock - 1,q), 0)
      
        # calculate kalman gain, mean and variance of each bandit
        kalmanGain[iBlock,iTrial,iBandit] = (banditVariance[iBlock,iTrial-1,iBandit] + pow(sigma_zeta,2)) /  (banditVariance[iBlock,iTrial-1,iBandit] + pow(sigma_zeta,2) + pow(sigma_epsilon,2)) 
        banditVariance[iBlock,iTrial,iBandit] = (1 - deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit]) * (banditVariance[iBlock,iTrial-1,iBandit] + pow(sigma_zeta,2)) # K&S, eq. 5
        banditMean[iBlock,iTrial,iBandit] = banditMean[iBlock,iTrial-1,iBandit] + (deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit] * (points[iBlock,iTrial-1] - banditMean[iBlock,iTrial-1,iBandit])) + bonus[iBlock,iTrial,iBandit] # K&S, eq. 4
        

        # each bandit's momentary utility is distributed as a gaussian
        u[iBlock,iTrial,iBandit] ~ dnorm(banditMean[iBlock,iTrial-1,iBandit],1/banditVariance[iBlock,iTrial-1,iBandit]) # parameterised by mean and precision (1/variance)
        
      } # bandit loop 1
      
      maxBandit[iBlock,iTrial] = max(u[iBlock,iTrial,])
      
      # Loop through bandits a second time
      for (iBandit in 1:nBandits){
      
        pi[iBlock,iTrial,iBandit] = ifelse(u[iBlock,iTrial,iBandit] == maxBandit[iBlock,iTrial], 1 - fudgeFactor, fudgeFactor / (nBandits - 1)) # set choice probability to 1 if is max util, 0 otherwise
        
      } # bandit loop 2
      
      # choices distributed as categorical distribution with probability vector as per the above
      choices[iBlock,iTrial-1] ~ dcat(pi[iBlock,iTrial,]) 

    } # trial
  } # block
}