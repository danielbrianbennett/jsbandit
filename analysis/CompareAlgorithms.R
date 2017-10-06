# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v2point2" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# load list of filtered IDs
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

# retain only participants with an ID in the white-list
sorted.data <- sorted.data[sorted.data$ID %in% filtered.IDs,]

nParticipants <- length(filtered.IDs)

# calculate cumulative winnings
nSims <- 1000
winnings <- array(data = NA, dim = c(3,30,nParticipants))
winnings_random <- array(data = NA, dim = c(3,30,nParticipants))
winnings_perfect <- array(data = NA, dim = c(3,30,nParticipants))

for (i in 1:nParticipants){
  
  winnings[,,i] <- t(matrix(sorted.data[sorted.data$ID == filtered.IDs[i],]$pointsWon, nrow = 30, ncol = 3))
  payoffs <- unsorted.data[unsorted.data$ID == filtered.IDs[i],]$payoffs
  payoffs <- payoffs[which(nchar(payoffs) == max(nchar(payoffs)))]
  payoffs <- scan(text = gsub("[^0-9,]", "", payoffs),sep=",")
  payoffs <- aperm(array(data = payoffs, dim = c(30,3,4)), perm = c(3,1,2))
  
  # perfect knowledge
  bestOption <- matrix(data = NA,nrow = 3, ncol = 30)
  for (j in 1:3){
    for (k in 1:30){
      bestOption[j,k] <- max(payoffs[,k,j])
    }
  }
  winnings_perfect[,,i] <- bestOption
  
  # random choices
  randomTotal <- array(data = NA,dim = c(3,30,nSims))
  for (sim in 1:nSims){
    randomOption <- matrix(data = NA,nrow = 3, ncol = 30)
    for (j in 1:3){
      for (k in 1:30){
        randomOption[j,k] <- (payoffs[sample(1:4,1),k,j])
      }
    }
    randomTotal[,,sim] <- randomOption
  }
  winnings_random[,,i] <- apply(randomTotal, c(1,2), mean)
}
 
meanWinnings_random <- rowMeans(colMeans(winnings_random))
stdErrWinnings_random <- 1.96 * apply(colMeans(winnings_random),c(1), sd) / sqrt(nParticipants)
meanWinnings_perfect <- rowMeans(colMeans(winnings_perfect))
stdErrWinnings_perfect <- 1.96 *apply(colMeans(winnings_perfect),c(1), sd) / sqrt(nParticipants)
meanWinnings <- rowMeans(colMeans(winnings))
stdErrWinnings <- 1.96 *apply(colMeans(winnings),c(1), sd) / sqrt(nParticipants)

## make plot

df <- data.frame(meanPoints = c(meanWinnings_random, meanWinnings, meanWinnings_perfect), 
                 stdErrPoints = c(stdErrWinnings_random, stdErrWinnings, stdErrWinnings_perfect),
                 algorithm = c(rep("random",30), rep("behav", 30), rep("perfect", 30)),
                 trialNo = rep(1:30,3))

cols <- c("random" = "#bfbfbf", "behav" = "#235ce1", "perfect" = "#000000")

p <- ggplot(data = df, aes(x = trialNo, y = meanPoints,  group = algorithm)) +
  geom_ribbon(aes(ymin = meanPoints - stdErrPoints, ymax = meanPoints + stdErrPoints), 
              fill = "grey70", alpha = 0.3)  +
  geom_line(aes(color = algorithm, linetype = algorithm)) +
  geom_point(aes(color = algorithm, shape = algorithm))
     
p + scale_color_manual(values = cols) + ylim(0,90) + xlim(0.5, 30.5) +
  scale_linetype_manual(values = c("solid","dashed","dashed")) + 
  scale_shape_manual(values = c(1,0,0)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), axis.line = element_line(color = "black", size = 0.3)) +
  scale_x_continuous(breaks = c(5,10,15,20,25,30))
