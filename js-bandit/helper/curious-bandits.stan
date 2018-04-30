data {
    
    // Metadata
    int  N;                     // number of subjects
    int  T;                     // number of trials
    int participant_ix[T];      // index of subject for trial i
    int block_ix[T];               // index of block for trial i
    int trial[T];               // index of trial number
    real V_init;                // initial value of options
    
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
    vector[N] eta_pr;           // Learning rate
    vector[N] B_pr;             // bonus size
    vector[N] j_pr;             // block-wise decay
    vector[N] k_pr;             // trial-wise decay

}
transformed parameters {
    
    // Subject-level parameters
    vector<lower=0,upper=30>[N]         beta;
    vector<lower=0,upper=1>[N]          eta;
    vector<lower=-100,upper=100>[N]     B;
    vector<lower=-10,upper=10>[N]       j;
    vector<lower=-10,upper=10>[N]       k;

    for (i in 1:N) {
        beta[i]  = Phi_approx( mu_pr[1] + sigma[1] * beta_pr[i] ) * 30;
        eta[i] = Phi_approx( mu_pr[2] + sigma[2] * eta_pr[i] );
        B[i] = Phi_approx( mu_pr[3] + sigma[3] * B_pr[i] ) * 100;
        j[i] = Phi_approx( mu_pr[4] + sigma[4] * j_pr[i] ) * 10;
        k[i] = Phi_approx( mu_pr[5] + sigma[5] * k_pr[i] ) * 10;
    }

}
model {
    
    // Generated data
    vector[4]           V;            // Q-values
    vector[4]  choiceProb;        // choice probabilty container

    // Group-level priors
    mu_pr ~ normal(0, 1);
    sigma ~ gamma(1, 0.5);

    // Subject-level priors
    beta_pr  ~ normal(0, 1);
    eta_pr ~ normal(0, 1);
    B_pr ~ normal(0, 1);
    j_pr ~ normal(0, 1);
    k_pr ~ normal(0, 1);

    // Precompute values.
    for (i in 1:T) {
        
        // Initialize/update value container at the beginning of each block 
        if (trial[i] == 1) {
            V = rep_vector(V_init, 4);
        }
        
        // update values based on perceptual change
        if (whichFilled[i] != -1){
            V[whichFilled[i]] = V[whichFilled[i]] + ( B[participant_ix[i]] * ( block_ix[i] ^ j[participant_ix[i]] ) * ( changeLag[i] ^ k[participant_ix[i]] ) );
        }
       
        // update values based on observed outcome
        V[choice[i]] = V[choice[i]] + eta[participant_ix[i]] * ( outcome[i] - V[choice[i]]);
        
        // likelihood with stabilised softmax
        choiceProb = exp((beta[participant_ix[i]] * V) - max(beta[participant_ix[i]] * V)) / sum(exp((beta[participant_ix[i]] * V) - max(beta[participant_ix[i]] * V)));
        choice[i] ~ categorical(choiceProb);
        
    }

}
