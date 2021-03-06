data {
    
    // Metadata
    int  N;                     // number of subjects
    int  T;                     // number of trials
    int participant_ix[T];      // index of subject for trial i
    int block_ix[T];               // index of block for trial i
    int trial[T];               // index of trial number
    real V_init;                // initial value of options
    real var_init;              // initial variance of options
    real sigma_epsilon;         // payout sd of options
    
    // Data
    int choice[T];              // choice data, range [0, 1]
    int outcome[T];             // outcome data, range [0, 1]
    int whichFilled[T];         // option fill data
    int changeLag[T];           // time from change data
    
}
parameters {
    
    // Group-level parameters
    vector[5] mu_pr;
    vector<lower=0>[5] sigma; 
    
    // Subject-level parameters
    vector[N] beta_pr;          // Inverse temperature
    vector[N] B_pr;             // bonus size
    vector[N] j_pr;             // block-wise decay
    vector[N] k_pr;             // trial-wise decay
    vector[N] sigma_zeta_pr;    // SD of bandit walk
    
}
transformed parameters {
    
    // Subject-level parameters
    vector<lower=0,upper=10>[N]         beta;
    vector[N]                           B;
    vector[N]                           j;
    vector[N]                           k;
    vector<lower = 0>[N]                sigma_zeta;
    
    for (i in 1:N) {
        beta[i]  =  Phi_approx(mu_pr[1] + sigma[1] * beta_pr[i]) * 10;
        B[i] =  (mu_pr[2] + sigma[3] * B_pr[2]) * 100;
        j[i] = (mu_pr[3] + sigma[4] * j_pr[3]) * 10;
        k[i] = (mu_pr[4] + sigma[5] * k_pr[4]) * 10;
        sigma_zeta[i] = Phi_approx(mu_pr[5] + sigma[5] * sigma_zeta_pr[i]) * 40;
    }
    
}
model {
    
    // Generated data
    vector[4]           V;            // Q-values
    vector[4]           optVar;       // estimate of variance
    vector[4]           kalmanGain;   // kalman gain
    
    // Group-level priors
    mu_pr ~ normal(0, 1);
    sigma ~ gamma(1, 0.5);
    
    // Subject-level priors
    beta_pr  ~ normal(0, 1);
    B_pr ~ normal(0, 1);
    j_pr ~ normal(0, 1);
    k_pr ~ normal(0, 1);
    sigma_zeta_pr ~ normal(0, 1);
    
    // Compute values.
    for (i in 1:T) {
        
        // Initialize/update value container at the beginning of each block 
        if (trial[i] == 1) {
            V = rep_vector(V_init, 4);
            optVar = rep_vector(var_init, 4);
            kalmanGain = rep_vector( ( var_init + sigma_zeta[participant_ix[i]]^2 ) / ( var_init + sigma_zeta[participant_ix[i]]^2 + sigma_epsilon^2 ), 4);
        }
        
        // update values based on perceptual change
        if (whichFilled[i] != -1){
            V[whichFilled[i]] = V[whichFilled[i]] + ( B[participant_ix[i]] * ( block_ix[i] ^ j[participant_ix[i]] ) * ( changeLag[i] ^ k[participant_ix[i]] ) );
        }
        
        // likelihood with softmax
        choice[i] ~ categorical(softmax(beta[participant_ix[i]] * V));
        
        // calculate kalman gain for this trial
        for (ii in 1:4){
            kalmanGain[ii] = ( optVar[ii] + sigma_zeta[participant_ix[i]]^2 ) / ( optVar[ii] + sigma_zeta[participant_ix[i]]^2 + sigma_epsilon^2 );
        }  
        
        // update variance based on observed outcome
        for (ii in 1:4){
            if(ii == choice[i]) { optVar[ii] = ( 1 - kalmanGain[ii] ) * ( optVar[ii] + sigma_zeta[participant_ix[i]] ^ 2 ); }
            else { optVar[ii] = optVar[ii] + sigma_zeta[participant_ix[i]] ^ 2; }
        }        
        
        // update values based on observed outcome
        V[choice[i]] = V[choice[i]] + kalmanGain[choice[i]] * ( outcome[i] - V[choice[i]] );
        
    }
    
}

generated quantities {
    
    // Transformed group-level parameters.
    real mu_beta;           // Inverse temperature
    real mu_B;              // Bonus size
    real mu_j;              // Block-wise bonus decay
    real mu_k;              // Trial-wise bonus decay
    real mu_sigma_zeta;     // SD of bandit walk
    real sigma_B;
    real sigma_j;
    real sigma_k;
    
    // Transform parameters.
    mu_beta = Phi_approx(mu_pr[1]) * 10;
    mu_B = mu_pr[2] * 100;
    mu_j = mu_pr[3] * 10;
    mu_k = mu_pr[4] * 10;
    mu_sigma_zeta = Phi_approx(mu_pr[5]) * 40;
    sigma_B = sigma[2] * 100;
    sigma_j = sigma[3] * 10;
    sigma_k = sigma[4] * 10;
}
