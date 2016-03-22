# install packages
library('stats4')

# load data
#load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")
load("/Users/Daniel/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# constants
nBlocks <- length(unique(sorted.data$block))
nTrials <- max(sorted.data$trial)

# get input parameters
varianceZeta_start <- 4
varianceEpsilon_start <- 4
participantID <- 10868001

#source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/KalmanPMU.R")

participantData <- subset(sorted.data,ID == participantID)

# extract choice and points data
choices <- matrix(participantData$choice,nBlocks,nTrials,byrow = T)
choices[choices == "top"] <- 1
choices[choices == "right"] <- 2
choices[choices == "bottom"] <- 3
choices[choices == "left"] <- 4
choices <- matrix(as.numeric(choices),nBlocks,nTrials)
points <- matrix(participantData$pointsWon,nBlocks,nTrials,byrow = T)
changepoint <- vector(mode = "integer",length = nBlocks)
changeID <- vector(mode = "character",length = nBlocks)

for (i in 1:nBlocks){
  blockData <- subset(participantData,block == i)
  changepoint[i] <- match(unique(blockData$whichFilled),blockData$whichFilled)[2]
  changeID[i] <- blockData[changepoint[i],]$whichFilled
}

changeID[changeID == "top"] <- 1
changeID[changeID == "right"] <- 2
changeID[changeID == "bottom"] <- 3
changeID[changeID == "left"] <- 4
changeID <- as.numeric(changeID)

LL <- function(zeta,epsilon) {
  output <- KalmanPMU(choices,points,zeta,epsilon)
  return(output$negativeLL)
}

t1 <- Sys.time()
mle.fit <- mle(LL, 
               start = list(zeta = varianceZeta_start),
               fixed = list(epsilon = 4))
t2 <- Sys.time()

difftime(t2,t1)

output <- KalmanPMU(sorted.data,participantID,mle.fit@coef[1],4)
# plot bandit means
for (i in 1:nBlocks) {
  matplot(t(output$banditMeans[,,i]), type = "b")
}