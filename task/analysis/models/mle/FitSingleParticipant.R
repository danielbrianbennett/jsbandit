# install packages
library('stats4')
library('mvtnorm')

# load data
#load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")
load("~/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# get input parameters
varianceZeta_start <- 5
varianceEpsilon_start <- 5
B_start <- 10
j_start <- 0
k_start <- 0
participantID <- 46280863

#source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/ExtractParticipantData.R")
#source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/KalmanPMU.R")
source("~/Documents/Git/jsbandit/analysis/models/ExtractParticipantData.R")
source("~/Documents/Git/jsbandit/analysis/models//KalmanPMU.R")

LL <- function(zeta,epsilon,B,j,k) {
  output <- KalmanPMU(choices,points,zeta,epsilon,B,j,k)
  if (length(output$negativeLL) == 0){}
  return(output$negativeLL)
}

system.time(
  mle.fit <- mle(LL, 
                 start = list(zeta = varianceZeta_start, 
                              epsilon = varianceEpsilon_start,
                              B = B_start,
                              j = j_start,
                              k = k_start)
  )
)

t1 <- Sys.time()
output <- KalmanPMU(choices,points,mle.fit@fullcoef[1],mle.fit@fullcoef[2],mle.fit@fullcoef[3],mle.fit@fullcoef[4],mle.fit@fullcoef[5])
t2 <- Sys.time()
difftime(t2,t1)

# plot bandit means
for (i in 1:nBlocks) {
  matplot(t(output$banditMean[,,i]), type = "b")
}