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

winningsAsPercentage <- colMeans(colMeans(winnings))/colMeans(colMeans(winnings_random)) * 100
  
meanWinnings_random <- rowMeans(colMeans(winnings_random))
ciWinnings_random <- 1.96 * apply(colMeans(winnings_random),c(1), sd) / sqrt(nParticipants)
meanWinnings_perfect <- rowMeans(colMeans(winnings_perfect))
ciWinnings_perfect <- 1.96 *apply(colMeans(winnings_perfect),c(1), sd) / sqrt(nParticipants)
meanWinnings <- rowMeans(colMeans(winnings))
ciWinnings <- 1.96 *apply(colMeans(winnings),c(1), sd) / sqrt(nParticipants)

## make plot 1

df <- data.frame(meanPoints = c(meanWinnings_random, meanWinnings, meanWinnings_perfect), 
                 ciPoints = c(ciWinnings_random, ciWinnings, ciWinnings_perfect),
                 algorithm = c(rep("Random",30), rep("Behaviour", 30), rep("Maximum", 30)),
                 trialNo = rep(1:30,3))

cols <- c("Random" = "#000000", "Behaviour" = "#235ce1", "Maximum" = "#000000")

p <- ggplot(data = df, aes(x = trialNo, y = meanPoints,  group = algorithm)) +
  geom_ribbon(aes(ymin = meanPoints - ciPoints, ymax = meanPoints + ciPoints), 
              fill = "grey", alpha = 0.6)  +
  geom_line(aes(color = algorithm, linetype = algorithm), size = 1) +
  geom_point(aes(color = algorithm, shape = algorithm),size = 3, fill = "#235ce1") +
  labs(x = "\nTrial number", y = "Points per trial\n")
     
p + scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("solid","solid","dotted")) + 
  scale_shape_manual(values = c(21,32,32)) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text (size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks = c(5,10,15,20,25,30), expand = c(0,0), limits = c(0,30)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80), expand = c(0,0), limits = c(30,90))


## make plot 2

blockMeans <- apply(winnings,c(1,3),mean)
overallBlockMean <- apply(blockMeans,1,mean)
overallBlockCI <- 1.96 * apply(blockMeans,1,sd) / sqrt(nParticipants)

df <- data.frame(meanPoints = overallBlockMean, 
                 ciPoints = overallBlockCI,
                 block = c("Block 1","Block 2","Block 3"))

p <- ggplot(data = df, aes(x = block, y = meanPoints)) +
  geom_col(width = 0.6, fill = "white",colour = "black", size = 1.5) +
  geom_errorbar(aes(y = meanPoints, ymin = meanPoints - ciPoints, ymax = meanPoints + ciPoints, width = 0.2)) +
  labs(x = "\nBlock", y = "Points per trial\n")


p + theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text (size = 16)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80), expand = c(0,0)) +
  coord_cartesian(ylim = c(30,90))

# make plot 3
p <- ggplot(data.frame(winningsAsPercentage),aes(y = winningsAsPercentage, x = 1))
p + geom_violin(adjust = 1, color = "#235ce1",trim = FALSE, size = 1.5) + 
  geom_jitter(height = .02, width = .02,color = "#235ce1") + 
  scale_y_continuous(breaks = c(40,60,80,100,120,140,160,180)) +
  ylim(35,200) + 
  labs(x = "", y = "Total points (% of random)\n") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(face = "bold", size = 18),
        axis.text.y = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  geom_hline(yintercept = 100,linetype = "dotted",size = 1)

