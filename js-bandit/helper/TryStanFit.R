require(rstan)
require(here)
require(tictoc)

source(here("helper","StanHelperFunctions.R"))
source(here("models","ModelLikelihood.R"))

stanData <- ExtractDataForStan(here("raw_data","banditData_v2point2.Rdata"))
stanData$V_init <- 100

# list parameters to estimate in stan
parameters <- c("beta",
                "eta",
                "B",
                "j",
                "k"
) 

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stanFile = here("helper","curious-bandits.stan")
# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = stanData, 
                pars=parameters,
                iter=100, 
                chains=2, 
                thin=1,
                warmup = 50  # Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()