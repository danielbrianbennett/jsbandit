# clear workspace:  
rm(list=ls())

# load rjags package
library(rstan)
library(tictoc)

# set a couple of helpful variables
participantID <- 45348020 # which participant's data to test?
nTrials <- 30 # per block
nBlocks <- 3
nBandits <- 4
mean0 <- 50
variance0 <- 1000
sigma_zeta <- 5

# define working directory and datafile
dataDir <- "~/Documents/Git/jsbandit/data/"
dataFile <- "banditData_v2point2.RData"
stanFile <- "/Users/danielbennett/Documents/Git/jsbandit/analysis/models/stan/bandit_stan_single.stan"

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
subsetID <- participantID
extract <- subset(sorted.data, ID %in% subsetID)

# create data containers to pass to stan, and fill them
choices <- array(data = NA, dim = c(nBlocks,nTrials)) # choice data
points <- array(data = NA, dim = c(nBlocks,nTrials)) # points data
changeLag <- matrix(extract[extract$ID == subsetID,]$changeLag,nBlocks,nTrials,byrow = T) # record how long since change
choices[,] <- matrix(extract[extract$ID == subsetID,]$choice,nBlocks,nTrials,byrow = T)
points[,] <- matrix(extract[extract$ID == subsetID,]$pointsWon,nBlocks,nTrials,byrow = T)
whichFilled <- matrix(extract[extract$ID == subsetID,]$whichFilled,nBlocks,nTrials,byrow = T) 


# list data to be passed on to stan
data <- list(choices = choices,
             whichFilled = whichFilled,
             points = points,
             nBandits = nBandits, 
             nBlocks = nBlocks,
             nTrials = nTrials,
             mean0 = mean0,
             variance0 = variance0,
             changeLag = changeLag,
             sigma_zeta = sigma_zeta
) 

# list parameters to estimate in stan
parameters <- c("sigma_epsilon",
                "b",
                "p",
                "q",
                "beta"
) 

# set parallel options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Call stan 
tic()
samples <- stan(file = stanFile,   
                data = data, 
                #init = myInits,  # If not specified, gives random inits
                pars=parameters,
                iter=10000, 
                chains=3, 
                thin=1
                # warmup = 100  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed
)
toc()