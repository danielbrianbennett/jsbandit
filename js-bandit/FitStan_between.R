rm(list = ls())
require(rstan)
require(here)
require(tictoc)

source(here("helper","StanHelperFunctions.R"))

stanData <- ExtractBetweenDataForStan(here("raw_data","banditData_v4.Rdata"))
stanData$V_init <- 50

# list parameters to estimate in stan
parameters <- c("mu_beta",
                "mu_eta",
                "mu_B_g",
                "mu_j_g",
                "mu_k_g",
                "mu_B_b",
                "mu_j_b",
                "mu_k_b",
                "mu_B_q",
                "mu_j_q",
                "mu_k_q",
                "mu_pr",
                "sigma",
                "beta",
                "eta",
                "sigma_B_g",
                "sigma_j_g",
                "sigma_k_g",
                "sigma_B_b",
                "sigma_j_b",
                "sigma_k_b",
                "sigma_B_q",
                "sigma_j_q",
                "sigma_k_q"
) 

initVals = list(list(mu_pr = rep(0,11),
                     sigma = rep(1,11),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,11),
                     sigma = rep(1,11),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,11),
                     sigma = rep(1,11),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,11),
                     sigma = rep(1,11),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N)))

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanFile = here("models","curious-bandits-delta-between.stan")
# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = stanData,
                init = initVals,
                pars=parameters,
                iter=1500, 
                chains=4, 
                thin=1,
                control = list("adapt_delta" = 0.95),
                warmup = 1000  # Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()

save(samples, file = here("models","samples", "delta_v4.Rdata"))