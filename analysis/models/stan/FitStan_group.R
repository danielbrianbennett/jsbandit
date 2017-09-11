# clear workspace:  
rm(list=ls())

# load rjags package
library(rstan)
library(tictoc)

# set a couple of helpful variables
participantNumbers <- 1:149 # which participants' data to test?
nTrials <- 30 # per block
nBlocks <- 3
nBandits <- 4
sigma_zeta <- 8

# define working directory and datafile
dataDir <- "~/Documents/Git/jsbandit/data/"
dataFile <- "banditData_v2point2.RData"
stanFile <- "/Users/danielbennett/Documents/Git/jsbandit/analysis/models/stan/bandit_stan_group_vector.stan"

# set working directory and load file
setwd(dataDir)
load(dataFile)

# recode choices from string to numeric 
try(sorted.data[sorted.data$choice == "top",]$choice <- 1)
try(sorted.data[sorted.data$choice == "right",]$choice <- 2)
try(sorted.data[sorted.data$choice == "bottom",]$choice <- 3)
try(sorted.data[sorted.data$choice == "left",]$choice <- 4)
sorted.data$choice <- as.numeric(sorted.data$choice)

try(sorted.data[sorted.data$whichFilled == "none",]$whichFilled <- 0)
try(sorted.data[sorted.data$whichFilled == "top",]$whichFilled <- 1)
try(sorted.data[sorted.data$whichFilled == "right",]$whichFilled <- 2)
try(sorted.data[sorted.data$whichFilled == "bottom",]$whichFilled <- 3)
try(sorted.data[sorted.data$whichFilled == "left",]$whichFilled <- 4)
sorted.data$whichFilled <- as.numeric(sorted.data$whichFilled)

# extract the specified participant
IDs <- unique(sorted.data$ID)
subsetID <- IDs[participantNumbers]
extract <- subset(sorted.data, ID %in% subsetID)
nSubjects = length(participantNumbers)

# create data containers to pass to stan, and fill them
choices <- array(data = NA, dim = c(nSubjects,nBlocks,nTrials)) # choice data
points <- array(data = NA, dim = c(nSubjects,nBlocks,nTrials)) # points data
changeLag <- array(data = NA, dim = c(nSubjects,nBlocks,nTrials)) # change lag data
whichFilled <- array(data = NA, dim = c(nSubjects,nBlocks,nTrials)) # which changed data

# deal data to the arrays built above
for (p in 1:nSubjects){
  choices[p,,] <- matrix(extract[extract$ID == subsetID[p],]$choice,nBlocks,nTrials,byrow = T)
  points[p,,] <- matrix(extract[extract$ID == subsetID[p],]$pointsWon,nBlocks,nTrials,byrow = T)
  changeLag[p,,] <- matrix(extract[extract$ID == subsetID[p],]$changeLag,nBlocks,nTrials,byrow = T)
  whichFilled[p,,] <- matrix(extract[extract$ID == subsetID[p],]$whichFilled,nBlocks,nTrials,byrow = T)
}

# list data to be passed on to stan
data <- list(choices = choices,
             whichFilled = whichFilled,
             points = points,
             nBandits = nBandits, 
             nBlocks = nBlocks,
             nTrials = nTrials,
             nSubjects = nSubjects,
             changeLag = changeLag,
             sigma_zeta = sigma_zeta
) 

# list parameters to estimate in stan
parameters <- c("sigma_epsilon",
                "mu_b",
                "mu_p",
                "mu_q",
                "mu_beta",
                "sigma_b",
                "sigma_p",
                "sigma_q",
                "sigma_beta",
                "sigma_epsilon",
                "mean0",
                "variance0",
                "b",
                "p",
                "q",
                "beta"
) 

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# set inits
load("~/Documents/Git/jsbandit/analysis/models/stan/savedInitVals.RData")
initVals <- list(
  sigma_epsilon = savedInitVals$sigma_epsilon,
  mu_b = savedInitVals$mu_b,
  mu_p = savedInitVals$mu_p,
  mu_q = savedInitVals$mu_q,
  mu_beta = savedInitVals$mu_beta,
  sigma_b = savedInitVals$sigma_b,
  sigma_p = savedInitVals$sigma_p,
  sigma_q = savedInitVals$sigma_q,
  sigma_beta = savedInitVals$sigma_beta,
  mean0 = savedInitVals$mean0,
  variance0 = savedInitVals$variance0,
  b = savedInitVals$b,
  p = savedInitVals$p,
  q = savedInitVals$q,
  beta = savedInitVals$beta
)

myInits <- list(initVals,initVals)


# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = data, 
                init = myInits,  # If not specified, gives random inits
                pars=parameters,
                iter=1000, 
                chains=2, 
                thin=1
                # warmup = 0  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()