require(rstan)
require(here)
require(tictoc)

source(here("helper","StanHelperFunctions.R"))
source(here("models","ModelLikelihood.R"))

stanData <- ExtractDataForStan(here("raw_data","banditData_v2point2.Rdata"))
stanData$V_init <- 50
stanData$var_init <- 1000
stanData$sigma_epsilon <- 4


# list parameters to estimate in stan
parameters <- c("mu_beta",
                "mu_B",
                "mu_j",
                "mu_k",
                "mu_sigma_zeta",
                "mu_pr",
                "sigma",
                "beta",
                "B",
                "j",
                "k",
                "beta_pr",
                "B_pr",
                "j_pr",
                "k_pr",
                "sigma_zeta_pr"
) 

initVals = list(list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N),
                     sigma_zeta_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N),
                     sigma_zeta_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N),
                     sigma_zeta_pr = rep(0,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     B_pr = rep(0,stanData$N),
                     j_pr = rep(0,stanData$N),
                     k_pr = rep(0,stanData$N),
                     sigma_zeta_pr = rep(0,stanData$N)))

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanFile = here("helper","curious-bandits-kalman.stan")
# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = stanData,
                init = initVals,
                pars=parameters,
                iter=1250, 
                chains=4, 
                thin=1,
                warmup = 750  # Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()