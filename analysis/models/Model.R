# install packages
library('mvtnorm')
library('stats4')

# load data
load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# get input parameters
varianceGamma_start <- 4
varianceEpsilon_start <- 4
participantID <- 10868001

source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/KalmanPMU.R")

participantData <- subset(sorted.data,ID == participantID)


LL <- function(gamma,epsilon) {
  output <- KalmanPMU(particpantData,gamma,epsilon)
  return(output$negativeLL)
}

t1 <- Sys.time()
mle.fit <- mle(LL, 
               start = list(gamma = varianceGamma_start),
               fixed = list(epsilon = 4))
t2 <- Sys.time()

difftime(t2,t1)

output <- KalmanPMU(sorted.data,participantID,mle.fit@coef[1],4)
# plot bandit means
for (i in 1:nBlocks) {
  matplot(t(output$banditMeans[,,i]), type = "b")
}