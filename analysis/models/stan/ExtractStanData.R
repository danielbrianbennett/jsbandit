# clear workspace:  
rm(list=ls())

# load rjags package
library(rstan)

# set a couple of helpful variables
participantID <- 10206771 # which participant's data to test?
nTrials <- 30 # per block
nBlocks <- 3
nBandits <- 4
mean0 <- 0
variance0 <- 1000

# define working directory and datafile
dataDir <- "~/Documents/Git/jsbandit/data/"
dataFile <- "banditData_v2point2.RData"
stanFile <- "~/Documents/Git/bandit-jags/analysis/models/stan/bandit_stan_single.txt"

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


# list data to be passed on to JAGS
data <- list("choices",
             "changeLag",
             "nBandits", 
             "nBlocks",
             "nTrials",
             "points",
             "whichFilled",
             "mean0",
             "variance0",
             "fudgeFactor"
) 

# list parameters to estimate in JAGS
parameters <- c("sigma_zeta",
                "sigma_epsilon",
                "b",
                "p",
                "q"
) 

# initial values of parameters
initVals <-	list(list(
  sigma_zeta = 10,
  sigma_epsilon = 10,
  b = 10,
  p = .1,
  q = .1
))

# call stan
samples <- jags(data, inits=initVals, parameters, model.file = bugsFile, 
                n.chains=1, n.iter=5000, n.burnin=2000, n.thin=1)