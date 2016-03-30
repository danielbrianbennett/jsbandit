# install packages
library('stats4')
library('mvtnorm')

# load data
load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")
#load("/Users/Daniel/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# get input parameters
varianceZeta_start <- 7.014663
varianceEpsilon_start <- 20.234
B_start <- 16.796
j_start <- 14.744
k_start <- 1.19
participantID <- unique(sorted.data$ID)

source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/ExtractMultipleParticipantData.R")
source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/KalmanPMU.R")
#source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/ExtractParticipantData.R")
#source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/KalmanPMU.R")

LL <- function(zeta,epsilon,B,j,k) {
  negativeLL <- 0
  for (i in 1:length(participantID)){
    output <- KalmanPMU(choices[,,i],points[,,i],zeta,epsilon,B,j,k)
    negativeLL <- negativeLL + output$negativeLL
  }
  return(negativeLL)
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
allVary <- mle.fit

#t1 <- Sys.time()
#output <- KalmanPMU(choices,points,mle.fit@fullcoef[1],mle.fit@fullcoef[2],mle.fit@fullcoef[3],mle.fit@fullcoef[4],mle.fit@fullcoef[5])
#t2 <- Sys.time()
#difftime(t2,t1)
#
# plot bandit means
#for (i in 1:nBlocks) {
#  matplot(t(output$banditMean[,,i]), type = "b")
#}