// Model for a restless bandit task:
// Kalman filter with Probability of Maximum Utility choice rule as per Konstantinidis & Speekenbrink (2015)
//
// inputs are
//   choices:          matrix of participant's selections
//   whichFilled:      matrix governing which bandit was filled on a given trial
//   points:           matrix of observed payouts
//   nBandits:         number of bandits
//   nBlocks:          number of blocks per participant
//   nTrials:          number of trials per block
//   mean0:            presumed initial mean value of bandit payoff
//   variance0:        presumed initial variance of bandit payoff
//   changeLag:        n trials after change
//   sigma_zeta
//
// parameters to be estimated are
//   sigma_epsilon:    bandit payout SD
//   b:                bonus size
//   p:                trial-wise bonus decay
//   q:                block-wise bonus decay
//   beta:             softmax inverse temperature

data {
  
  int nBandits;
  int nBlocks;
  int nTrials;
  real mean0;
  real variance0;
  real<lower=0> sigma_zeta;
  
  int<lower=1,upper=4> choices[nBlocks,nTrials];
  int<lower=0,upper=4> whichFilled[nBlocks,nTrials];
  int<lower=0,upper=100> points[nBlocks,nTrials];
  int changeLag[nBlocks,nTrials];
  
}

parameters {
  
  real<lower=0> sigma_epsilon;
  real b;
  real p;
  real q;
  real<lower=0> beta;
  
}

model{
  
  // initialise containers
  real banditMean[nBlocks,nTrials,nBandits];
  real banditVariance[nBlocks,nTrials,nBandits];
  int deltaFunction[nBlocks,nTrials,nBandits];
  real bonus[nBlocks,nTrials,nBandits];
  real kalmanGain[nBlocks,nTrials,nBandits];
  real u[nBlocks,nTrials,nBandits];
  real choiceProb[nBlocks,nTrials,nBandits];
  real indicatorChoices[nBlocks,nTrials];
  
  // assign indicator choices
  indicatorChoices = choices;
  
  // sigma parameters distributed as uniform
  sigma_epsilon ~ normal(0,10);
  b ~ normal(0,10);
  p ~ normal(0,5);
  q ~ normal(0,5);
  beta ~ normal(0,5);
  
  // Loop through blocks
  for (iBlock in 1:nBlocks){
  
    // Loop through trials
    for (iTrial in 1:nTrials){
    
      // Loop through bandits a first time
      for (iBandit in 1:nBandits){
      
        // retrieve whether chosen for each bandit
        deltaFunction[iBlock,iTrial,iBandit] = iBandit == indicatorChoices[iBlock,iTrial] ? 1 : 0 ; // deltaFunction = 1 if bandit chosen, 0 otherwise
        bonus[iBlock,iTrial,iBandit] = whichFilled[iBlock,iTrial] == iBandit && changeLag[iBlock,iTrial] > 0 ? (b * changeLag[iBlock,iTrial]^p * (iBlock - 1)^q) : 0;
      
        if (iTrial == 1){

          // calculate kalman gain, mean and variance of each bandit
          kalmanGain[iBlock,iTrial,iBandit] = (variance0 + sigma_zeta^2) /  (variance0 + sigma_zeta^2 + sigma_epsilon^2); 
          banditVariance[iBlock,iTrial,iBandit] = (1 - deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit]) * (variance0 + sigma_zeta^2); // K&S, eq. 5
          banditMean[iBlock,iTrial,iBandit] = mean0 + (deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit] * (points[iBlock,iTrial] - mean0)) + bonus[iBlock,iTrial,iBandit] ;// K&S, eq. 4

        } else {
          
          // calculate kalman gain, mean and variance of each bandit
          kalmanGain[iBlock,iTrial,iBandit] = (banditVariance[iBlock,iTrial-1,iBandit] + sigma_zeta^2) /  (banditVariance[iBlock,iTrial-1,iBandit] + sigma_zeta^2 + sigma_epsilon^2); 
          banditVariance[iBlock,iTrial,iBandit] = (1 - deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit]) * (banditVariance[iBlock,iTrial-1,iBandit] + sigma_zeta^2); // K&S, eq. 5
          banditMean[iBlock,iTrial,iBandit] = banditMean[iBlock,iTrial-1,iBandit] + (deltaFunction[iBlock,iTrial,iBandit] * kalmanGain[iBlock,iTrial,iBandit] * (points[iBlock,iTrial] - banditMean[iBlock,iTrial-1,iBandit])) + bonus[iBlock,iTrial,iBandit] ;// K&S, eq. 4
          
        }
        
        // each bandit's momentary utility is distributed as a gaussian
        //u[iBlock,iTrial,iBandit] ~ dnorm(banditMean[iBlock,iTrial-1,iBandit],1/banditVariance[iBlock,iTrial-1,iBandit]) // parameterised by mean and precision (1/variance)
        
      } // bandit loop 1
      
      //maxBandit[iBlock,iTrial] = max(u[iBlock,iTrial,])
      
      // Loop through bandits a second time

      for (iBandit in 1:nBandits){

        choiceProb[iBlock,iTrial,iBandit] = exp(beta * banditMean[iBlock,iTrial,iBandit]) / sum(exp(beta * to_vector(banditMean[iBlock,iTrial,])));

      } // bandit loop 2
      
      // choices distributed as categorical distribution with probability vector as per the above
      choices[iBlock,iTrial] ~ categorical(to_vector(choiceProb[iBlock,iTrial,]));

    } // trial
  } // block
}
