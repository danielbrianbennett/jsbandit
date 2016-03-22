# install packages
library('stats4')
library('mvtnorm')

# load data
#load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")
load("/Users/Daniel/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# get input parameters
varianceZeta_start <- 4
varianceEpsilon_start <- 4
participantID <- 10868001

#source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/ExtractParticipantData.R")
#source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/KalmanPMU.R")
source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/ExtractParticipantData.R")
source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/KalmanPMU.R")

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

output <- KalmanPMU(choices,points,mle.fit@coef[1],4)
# plot bandit means
for (i in 1:nBlocks) {
  matplot(t(output$banditMean[,,i]), type = "b")
}