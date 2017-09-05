# clear workspace:  
rm(list=ls())

# load rjags package
library(rstan)
library(tictoc)

# set a couple of helpful variables
participantNumbers <- 1:10 # which participant's data to test?
nTrials <- 30 # per block
nBlocks <- 3
nBandits <- 4
mean0 <- 50
variance0 <- 1000
sigma_zeta <- 5

# define working directory and datafile
dataDir <- "~/Documents/Git/jsbandit/data/"
dataFile <- "banditData_v2point2.RData"
stanFile <- "/Users/danielbennett/Documents/Git/jsbandit/analysis/models/stan/bandit_stan_group.stan"

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
choices <- array(data = NA, dim = c(nBlocks,nTrials,nSubjects)) # choice data
points <- array(data = NA, dim = c(nBlocks,nTrials,nSubjects)) # points data
changeLag <- array(data = NA, dim = c(nBlocks,nTrials,nSubjects)) # change lag data
whichFilled <- array(data = NA, dim = c(nBlocks,nTrials,nSubjects)) # which changed data

# deal data to the arrays built above
for (p in 1:nSubjects){
  choices[,,p] <- matrix(extract[extract$ID == subsetID[p],]$choice,nBlocks,nTrials,byrow = T)
  points[,,p] <- matrix(extract[extract$ID == subsetID[p],]$pointsWon,nBlocks,nTrials,byrow = T)
  changeLag[,,p] <- matrix(extract[extract$ID == subsetID[p],]$changeLag,nBlocks,nTrials,byrow = T)
  whichFilled[,,p] <- matrix(extract[extract$ID == subsetID[p],]$whichFilled,nBlocks,nTrials,byrow = T)
}

# list data to be passed on to stan
data <- list(choices = choices,
             whichFilled = whichFilled,
             points = points,
             nBandits = nBandits, 
             nBlocks = nBlocks,
             nTrials = nTrials,
             nSubjects = nSubjects,
             mean0 = mean0,
             variance0 = variance0,
             changeLag = changeLag,
             sigma_zeta = sigma_zeta
) 

# list parameters to estimate in stan
parameters <- c("mu_sigma_epsilon",
                "mu_b",
                "mu_p",
                "mu_q",
                "mu_beta",
                "sigma_sigma_epsilon",
                "sigma_b",
                "sigma_p",
                "sigma_q",
                "sigma_beta",
                "sigma_epsilon",
                "b",
                "p",
                "q",
                "beta"
) 

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# set inits
myInits <- list(list(
  mu_sigma_epsilon = 5,
  mu_b = 5,
  mu_p = 5,
  mu_q = 5,
  mu_beta = .5,
  sigma_sigma_epsilon = 10,
  sigma_b = 10,
  sigma_p = 10,
  sigma_q = 10,
  sigma_beta = 1
  ))

# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = data, 
                #init = myInits,  # If not specified, gives random inits
                pars=parameters,
                iter=1000, 
                chains=3, 
                thin=1
                # warmup = 100  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()