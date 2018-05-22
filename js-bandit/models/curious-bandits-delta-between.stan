data {
    
    // Metadata
    int  N;                     // number of subjects
    int  N_g;                   // number of subjets with good tag
    int  N_b;                   // number of subjects with bad tag
    int  N_q;                   // number of subjects with ?? tag
    int  T;                     // number of trials
    int participant_ix[T];      // index of subject for trial i
    int block_ix[T];            // index of block for trial i
    int trial[T];               // index of trial number
    int group_ix[N];            // index of tag condition for each participant
    real V_init;                // initial value of options
    
    // Data
    int choice[T];              // choice data, range [0, 1]
    int outcome[T];             // outcome data, range [0, 1]
    int whichFilled[T];         // option fill data
    int changeLag[T];           // time from change data

}
parameters {
    
    // Group-level parameters
    vector[11] mu_pr;
    vector<lower=0>[11] sigma; 

    // Subject-level parameters
    vector[N] beta_pr;              // Inverse temperature
    vector[N] eta_pr;               // Learning rate
    vector[N] B_pr;             // bonus size
    vector[N] j_pr;             // block-wise decay
    vector[N] k_pr;             // trial-wise decay
}
transformed parameters {
    
    // Subject-level parameters
    vector<lower=0,upper=10>[N]         beta;
    vector<lower=0,upper=1>[N]          eta;
    vector[N]                           B;
    vector[N]                           j;
    vector[N]                           k;

    // assign parameters depending on group
    for (i in 1:N) {
        beta[i]  =  Phi_approx(mu_pr[1] + sigma[1] * beta_pr[i]) * 10;
        eta[i] = Phi_approx( mu_pr[2] + sigma[2] * eta_pr[i] );
        if (group_ix[i] == 1) {
            B[i] =  (mu_pr[3] + sigma[3] * B_pr[i]) * 100;
            j[i] = (mu_pr[4] + sigma[4] * j_pr[i]) * 10;
            k[i] = (mu_pr[5] + sigma[5] * k_pr[i]) * 10;
        } else if (group_ix[i] == 2){
            B[i] =  (mu_pr[6] + sigma[6] * B_pr[i]) * 200;
            j[i] = (mu_pr[7] + sigma[7] * j_pr[i]) * 1;
            k[i] = (mu_pr[8] + sigma[8] * k_pr[i]) * 1;
        }  else if (group_ix[i] == 3){
            B[i] =  (mu_pr[9] + sigma[9] * B_pr[i]) * 100;
            j[i] = (mu_pr[10] + sigma[10] * j_pr[i]) * 10;
            k[i] = (mu_pr[11] + sigma[11] * k_pr[i]) * 10;
        }

    }

}
model {
    
    // Generated data
    vector[4]           V;            // Q-values
    vector[4]           V_choice;     // copy of Q-values to be augmented with value bonus

    // Group-level priors
    mu_pr ~ normal(0, 1);
    sigma ~ gamma(1, 0.5);

    // Subject-level priors
    beta_pr  ~ normal(0, 1);
    eta_pr ~ normal(0, 1);
    B_pr ~ normal(0, 1);
    j_pr ~ normal(0, 1);
    k_pr ~ normal(0, 1);

    // Compute values.
    for (i in 1:T) {
        
        // Initialize/update value container at the beginning of each block 
        if (trial[i] == 1) {
            V = rep_vector(V_init, 4);
        }
        
        // update values based on perceptual change
        V_choice = V;
        if (whichFilled[i] != -1){
            V_choice[whichFilled[i]] = V_choice[whichFilled[i]] + ( B[participant_ix[i]] * ( block_ix[i] ^ j[participant_ix[i]] ) * ( changeLag[i] ^ k[participant_ix[i]] ) );
        }
 
 
         // likelihood with softmax
        choice[i] ~ categorical(softmax(beta[participant_ix[i]] * V_choice));
        
        // update values based on observed outcome
        V[choice[i]] = V[choice[i]] + eta[participant_ix[i]] * ( outcome[i] - V[choice[i]]);
        
    }

}

generated quantities {

    // Transformed group-level parameters.
    real mu_beta;             // Inverse temperature
    real mu_eta;              // Learning rate
    real mu_B_g;              // Mean of Bonus size for good tag
    real mu_j_g;              // Mean of block-wise bonus decay for good tag
    real mu_k_g;              // Mean of Trial-wise bonus decay for good tag
    real mu_B_b;              // Mean of Bonus size for bad tag
    real mu_j_b;              // Mean of Block-wise bonus decay for bad tag
    real mu_k_b;              // Mean of Trial-wise bonus decay for bad tag
    real mu_B_q;              // Mean of Bonus size for ?? tag
    real mu_j_q;              // Mean of Block-wise bonus decay for ?? tag
    real mu_k_q;              // Mean of Trial-wise bonus decay for ?? tag
    real sigma_B_g;           // SD of Bonus size for good tag
    real sigma_j_g;           // SD of block-wise bonus decay for good tag
    real sigma_k_g;           // SD of Trial-wise bonus decay for good tag
    real sigma_B_b;           // SD of Bonus size for bad tag
    real sigma_j_b;           // SD of block-wise bonus decay for based tag
    real sigma_k_b;           // SD of Trial-wise bonus decay for bad tag
    real sigma_B_q;           // SD of Bonus size for ?? tag
    real sigma_j_q;           // SD of block-wise bonus decay for ?? tag
    real sigma_k_q;           // SD of Trial-wise bonus decay for ?? tag
    
    // Transform parameters.
    mu_beta = Phi_approx(mu_pr[1]) * 10;
    mu_eta = Phi_approx(mu_pr[2]);
    mu_B_g = mu_pr[3] * 100;
    mu_j_g = mu_pr[4] * 10;
    mu_k_g = mu_pr[5] * 10;
    mu_B_b = mu_pr[6] * 200;
    mu_j_b = mu_pr[7] * 1;
    mu_k_b = mu_pr[8] * 1;
    mu_B_q = mu_pr[9] * 100;
    mu_j_q = mu_pr[10] * 10;
    mu_k_q = mu_pr[11] * 10;
    sigma_B_g = sigma[3] * 100;
    sigma_j_g = sigma[4] * 10;
    sigma_k_g = sigma[5] * 10;
    sigma_B_b = sigma[6] * 200;
    sigma_j_b = sigma[7] * 1;
    sigma_k_b = sigma[8] * 1;
    sigma_B_q = sigma[9] * 100;
    sigma_j_q = sigma[10] * 10;
    sigma_k_q = sigma[11] * 10;
}
