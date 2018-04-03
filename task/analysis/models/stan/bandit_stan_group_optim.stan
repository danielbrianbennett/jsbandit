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
real<lower=0> sigma_zeta;

int<lower=1,upper=4> choices[nSubjects,nBlocks,nTrials];
int<lower=0,upper=4> whichFilled[nSubjects,nBlocks,nTrials];
int<lower=0,upper=100> points[nSubjects,nBlocks,nTrials];
int changeLag[nSubjects,nBlocks,nTrials];

}

parameters {

// group-level
real mu_b;
real<lower=0> sigma_b;
real mu_p;
real<lower=0> sigma_p;
real mu_q;
real<lower=0> sigma_q;
real<lower=0> mu_beta;
real<lower=0> sigma_beta;
real<lower=0,upper=100> mean0;
real<lower=0> variance0;
real<lower=0> sigma_epsilon;

// participant-level
real b[nSubjects];
real p[nSubjects];
real q[nSubjects];
real<lower=0> beta[nSubjects];

}

model{

// initialise containers
real banditMean[nSubjects,nBlocks,nTrials,nBandits];
real banditVariance[nSubjects,nBlocks,nTrials,nBandits];
int deltaFunction[nSubjects,nBlocks,nTrials,nBandits];
real bonus[nSubjects,nBlocks,nTrials,nBandits];
real kalmanGain[nSubjects,nBlocks,nTrials,nBandits];
real choiceProb[nSubjects,nBlocks,nTrials,nBandits];
real indicatorChoices[nSubjects,nBlocks,nTrials];

// group-level parameters
mu_b ~ normal(0,50);
sigma_b ~ normal(0,50);
mu_p ~ normal(0,10);
sigma_p ~ normal(0,10);
mu_q ~ normal(0,10);
sigma_q ~ normal(0,10);
mu_beta ~ normal(0,20);
sigma_beta ~ normal(0,20);
mean0 ~ normal(50,20);
variance0 ~ normal(0,1000);
sigma_epsilon ~ normal(0,10);

// assign indicator choices
indicatorChoices = choices;

// sigma parameters distributed as uniform
b ~ normal(mu_b,sigma_b);
p ~ normal(mu_p,sigma_p);
q ~ normal(mu_q,sigma_q);
beta ~ normal(mu_beta,sigma_beta);

// Loop through blocks
for (iSubject in 1:nSubjects){
  
  // Loop through blocks
  for (iBlock in 1:nBlocks){
  
    // Loop through trials
    for (iTrial in 1:nTrials){
    
      // Loop through bandits a first time
      for (iBandit in 1:nBandits){
      
      // retrieve whether chosen for each bandit
      deltaFunction[iSubject,iBlock,iTrial,iBandit] = iBandit == indicatorChoices[iSubject,iBlock,iTrial] ? 1 : 0 ; // deltaFunction = 1 if bandit chosen, 0 otherwise
      bonus[iSubject,iBlock,iTrial,iBandit] = whichFilled[iSubject,iBlock,iTrial] == iBandit && changeLag[iSubject,iBlock,iTrial] > 0 ? (b[iSubject] * changeLag[iSubject,iBlock,iTrial]^p[iSubject] * (iBlock)^q[iSubject]) : 0;
      
      
      if (iTrial == 1){
      
        // calculate kalman gain, mean and variance of each bandit
        kalmanGain[iSubject,iBlock,iTrial,iBandit] = (variance0 + sigma_zeta^2) /  (variance0 + sigma_zeta^2 + sigma_epsilon^2); 
        banditVariance[iSubject,iBlock,iTrial,iBandit] = (1 - deltaFunction[iSubject,iBlock,iTrial,iBandit] * kalmanGain[iSubject,iBlock,iTrial,iBandit]) * (variance0 + sigma_zeta^2); // K&S, eq. 5
        banditMean[iSubject,iBlock,iTrial,iBandit] = mean0 + (deltaFunction[iSubject,iBlock,iTrial,iBandit] * kalmanGain[iSubject,iBlock,iTrial,iBandit] * (points[iSubject,iBlock,iTrial] - mean0)) + bonus[iSubject,iBlock,iTrial,iBandit] ;// K&S, eq. 4
      
      } else {
      
        // calculate kalman gain, mean and variance of each bandit
        kalmanGain[iSubject,iBlock,iTrial,iBandit] = (banditVariance[iSubject,iBlock,iTrial-1,iBandit] + sigma_zeta^2) /  (banditVariance[iSubject,iBlock,iTrial-1,iBandit] + sigma_zeta^2 + sigma_epsilon^2); 
        banditVariance[iSubject,iBlock,iTrial,iBandit] = (1 - deltaFunction[iSubject,iBlock,iTrial,iBandit] * kalmanGain[iSubject,iBlock,iTrial,iBandit]) * (banditVariance[iSubject,iBlock,iTrial-1,iBandit] + sigma_zeta^2); // K&S, eq. 5
        banditMean[iSubject,iBlock,iTrial,iBandit] = banditMean[iSubject,iBlock,iTrial-1,iBandit] + (deltaFunction[iSubject,iBlock,iTrial,iBandit] * kalmanGain[iSubject,iBlock,iTrial,iBandit] * (points[iSubject,iBlock,iTrial] - banditMean[iSubject,iBlock,iTrial-1,iBandit])) + bonus[iSubject,iBlock,iTrial,iBandit] ;// K&S, eq. 4
      
      }
      
    } // bandit loop 1
    
    // Loop through bandits a second time
    for (iBandit in 1:nBandits){
      choiceProb[iSubject,iBlock,iTrial,iBandit] = exp(beta[iSubject] * banditMean[iSubject,iBlock,iTrial,iBandit] - max(beta[iSubject] * to_vector(banditMean[iSubject,iBlock,iTrial,]))) / sum(exp(beta[iSubject] * to_vector(banditMean[iSubject,iBlock,iTrial,]) - max(beta[iSubject] * to_vector(banditMean[iSubject,iBlock,iTrial,]))));
    
    } // bandit loop 2
    // choices distributed as categorical distribution with probability vector as per the above
    choices[iSubject,iBlock,iTrial] ~ categorical(to_vector(choiceProb[iSubject,iBlock,iTrial,]));
    
    } // trial
  } // block
} //subject
}
