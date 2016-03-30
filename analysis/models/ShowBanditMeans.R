#ShowBanditMeans

# install packages
library('stats4')

# load data
load("C:/Users/dbennett1/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")
#load("/Users/Daniel/Google Drive/Works in Progress/JSBANDIT/Bandit/data/v2.2/banditData_v2point2.RData")

# get input parameters
varianceZeta <- 7.014663
varianceEpsilon <- 20.234
B <- 16.796
j <- 14.744
k <- 1.19
participantID <- 25933761

source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/ExtractParticipantData.R")
source("C:/Users/dbennett1/Documents/GitHub/jsbandit/analysis/models/KalmanPMU.R")
#source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/ExtractParticipantData.R")
#source("/Users/Daniel/Git Repositories//jsbandit/analysis/models/KalmanPMU.R")

t1 <- Sys.time()
output <- KalmanPMU(choices,points,varianceZeta,varianceEpsilon,B,j,k)
t2 <- Sys.time()
difftime(t2,t1)

# plot bandit means
for (i in 1:nBlocks) {
  matplot(t(output$banditMean[,,i]), type = "b")
}