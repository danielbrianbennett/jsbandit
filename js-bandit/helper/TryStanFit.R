require(rstan)
require(here)
require(tictoc)

source(here("helper","StanHelperFunctions.R"))
source(here("models","ModelLikelihood.R"))

stanData <- ExtractDataForStan(here("raw_data","banditData_v2point2.Rdata"))
stanData$V_init <- 100

# list parameters to estimate in stan
parameters <- c("mu_pr",
                "sigma",
                "beta",
                "eta",
                "B",
                "j",
                "k"
) 

initVals = list(list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(-10,stanData$N),
                     j_pr = rep(-10,stanData$N),
                     k_pr = rep(-10,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(-10,stanData$N),
                     j_pr = rep(-10,stanData$N),
                     k_pr = rep(-10,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(-10,stanData$N),
                     j_pr = rep(-10,stanData$N),
                     k_pr = rep(-10,stanData$N)),
                list(mu_pr = rep(0,5),
                     sigma = rep(1,5),
                     beta_pr =rep(0,stanData$N),
                     eta_pr = rep(0,stanData$N),
                     B_pr = rep(-10,stanData$N),
                     j_pr = rep(-10,stanData$N),
                     k_pr = rep(-10,stanData$N)))

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanFile = here("helper","curious-bandits.stan")
# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = stanData,
                init = initVals,
                pars=parameters,
                iter=1000, 
                chains=4, 
                thin=1,
                warmup = 500  # Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()