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
//   nSubjects:        number of participants
//   mean0:            presumed initial mean value of bandit payoff
//   variance0:        presumed initial variance of bandit payoff
//   changeLag:        n trials after change
//   sigma_zeta
//
// parameters to be estimated for each participant are
//   sigma_epsilon:    bandit payout SD
//   b:                bonus size
//   p:                trial-wise bonus decay
//   q:                block-wise bonus decay
//   beta:             softmax inverse temperature

data {

int nSubjects;
int nBandits;
int nBlocks;
int nTrials;
real mean0;
real variance0;
real<lower=0> sigma_zeta;

int<lower=1,upper=4> choices[nBlocks,nTrials,nSubjects];
int<lower=0,upper=4> whichFilled[nBlocks,nTrials,nSubjects];
int<lower=0,upper=100> points[nBlocks,nTrials,nSubjects];
int changeLag[nBlocks,nTrials,nSubjects];

}

parameters {

// group-level
real<lower=0> mu_sigma_epsilon;
real<lower=0> sigma_sigma_epsilon;
real mu_b;
real<lower=0> sigma_b;
real mu_p;
real<lower=0> sigma_p;
real mu_q;
real<lower=0> sigma_q;
real<lower=0> mu_beta;
real<lower=0> sigma_beta;

// participant-level
real<lower=0> sigma_epsilon[nSubjects];
real b[nSubjects];
real p[nSubjects];
real q[nSubjects];
real<lower=0> beta[nSubjects];

}

model{

// initialise containers
real banditMean[nBlocks,nTrials,nBandits,nSubjects];
real banditVariance[nBlocks,nTrials,nBandits,nSubjects];
int deltaFunction[nBlocks,nTrials,nBandits,nSubjects];
real bonus[nBlocks,nTrials,nBandits,nSubjects];
real kalmanGain[nBlocks,nTrials,nBandits,nSubjects];
real u[nBlocks,nTrials,nBandits,nSubjects];
real choiceProb[nBlocks,nTrials,nBandits,nSubjects];
real indicatorChoices[nBlocks,nTrials,nSubjects];

// assign indicator choices
indicatorChoices = choices;

// Loop through blocks
for (iSubject in 1:nSubjects){

    // sigma parameters distributed as uniform
    sigma_epsilon[iSubject] ~ normal(mu_sigma_epsilon,sigma_sigma_epsilon);
    b[iSubject] ~ normal(mu_b,sigma_b);
    p[iSubject] ~ normal(mu_p,sigma_p);
    q[iSubject] ~ normal(mu_q,sigma_q);
    beta[iSubject] ~ normal(mu_beta,sigma_beta);
    print(beta[iSubject])
    print(mu_beta)
    print(sigma_beta)
    


  // Loop through blocks
  for (iBlock in 1:nBlocks){
  
    // Loop through trials
    for (iTrial in 1:nTrials){
    
      // Loop through bandits a first time
      for (iBandit in 1:nBandits){
      
        // retrieve whether chosen for each bandit
        deltaFunction[iBlock,iTrial,iBandit,iSubject] = iBandit == indicatorChoices[iBlock,iTrial,iSubject] ? 1 : 0 ; // deltaFunction = 1 if bandit chosen, 0 otherwise
        bonus[iBlock,iTrial,iBandit,iSubject] = whichFilled[iBlock,iTrial,iSubject] == iBandit && changeLag[iBlock,iTrial,iSubject] > 0 ? (b[iSubject] * changeLag[iBlock,iTrial,iSubject]^p[iSubject] * (iBlock)^q[iSubject]) : 0;


        if (iTrial == 1){
        
          // calculate kalman gain, mean and variance of each bandit
          kalmanGain[iBlock,iTrial,iBandit,iSubject] = (variance0 + sigma_zeta^2) /  (variance0 + sigma_zeta^2 + sigma_epsilon[iSubject]^2); 
          banditVariance[iBlock,iTrial,iBandit,iSubject] = (1 - deltaFunction[iBlock,iTrial,iBandit,iSubject] * kalmanGain[iBlock,iTrial,iBandit,iSubject]) * (variance0 + sigma_zeta^2); // K&S, eq. 5
          banditMean[iBlock,iTrial,iBandit,iSubject] = mean0 + (deltaFunction[iBlock,iTrial,iBandit,iSubject] * kalmanGain[iBlock,iTrial,iBandit,iSubject] * (points[iBlock,iTrial,iSubject] - mean0)) + bonus[iBlock,iTrial,iBandit,iSubject] ;// K&S, eq. 4
        
        } else {
        
          // calculate kalman gain, mean and variance of each bandit
          kalmanGain[iBlock,iTrial,iBandit,iSubject] = (banditVariance[iBlock,iTrial-1,iBandit,iSubject] + sigma_zeta^2) /  (banditVariance[iBlock,iTrial-1,iBandit,iSubject] + sigma_zeta^2 + sigma_epsilon[iSubject]^2); 
          banditVariance[iBlock,iTrial,iBandit,iSubject] = (1 - deltaFunction[iBlock,iTrial,iBandit,iSubject] * kalmanGain[iBlock,iTrial,iBandit,iSubject]) * (banditVariance[iBlock,iTrial-1,iBandit,iSubject] + sigma_zeta^2); // K&S, eq. 5
          banditMean[iBlock,iTrial,iBandit,iSubject] = banditMean[iBlock,iTrial-1,iBandit,iSubject] + (deltaFunction[iBlock,iTrial,iBandit,iSubject] * kalmanGain[iBlock,iTrial,iBandit,iSubject] * (points[iBlock,iTrial,iSubject] - banditMean[iBlock,iTrial-1,iBandit,iSubject])) + bonus[iBlock,iTrial,iBandit,iSubject] ;// K&S, eq. 4
        
        }
        
      } // bandit loop 1
      // Loop through bandits a second time
  
      for (iBandit in 1:nBandits){
        choiceProb[iBlock,iTrial,iBandit,iSubject] = exp(beta[iSubject] * banditMean[iBlock,iTrial,iBandit,iSubject] - max(beta[iSubject] * to_vector(banditMean[iBlock,iTrial,,iSubject]))) / sum(exp(beta[iSubject] * to_vector(banditMean[iBlock,iTrial,,iSubject]) - max(beta[iSubject] * to_vector(banditMean[iBlock,iTrial,,iSubject]))));
        // choiceProb[iBlock,iTrial,iBandit,iSubject] = exp(beta[iSubject] * banditMean[iBlock,iTrial,iBandit,iSubject]) / sum(exp(beta[iSubject] * to_vector(banditMean[iBlock,iTrial,,iSubject])));

      } // bandit loop 2
      // choices distributed as categorical distribution with probability vector as per the above
      print(choiceProb[iBlock,iTrial,,iSubject])


      choices[iBlock,iTrial,iSubject] ~ categorical(to_vector(choiceProb[iBlock,iTrial,,iSubject]));
    
    } // trial
  } // block
} //subject
}
