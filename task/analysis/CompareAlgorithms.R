# load relevant packages
library(ggplot2)
library(boot)

# clear workspace
rm(list = ls())

# set version
version <- "v2point2" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/task/data/"

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

winningsAsProportion <- colMeans(colMeans(winnings))/colMeans(colMeans(winnings_random))
  
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

cols <- c("Random" = "#000000", "Behaviour" = "#0059b3", "Maximum" = "#000000")

p1 <- ggplot(data = df, aes(x = trialNo, y = meanPoints,  group = algorithm)) +
  geom_ribbon(aes(ymin = meanPoints - ciPoints, ymax = meanPoints + ciPoints), 
              fill = "grey", alpha = 0.6)  +
  geom_line(aes(color = algorithm, linetype = algorithm), size = 1) +
  geom_point(aes(color = algorithm, shape = algorithm),size = 3, fill = "#235ce1") +
  labs(x = "\nTrial number", y = "Points per trial\n")
     
p1 + scale_color_manual(values = cols, labels = c("Behaviour","Omniscient choices", "Random choices") ) +
  scale_linetype_manual(values = c("solid","solid","dotted"), labels = c("Behaviour","Omniscient choices", "Random choices") ) + 
  scale_shape_manual(values = c(21,32,32), labels = c("Behaviour","Omniscient choices", "Random choices") ) +
    guides(color = guide_legend(nrow = 3)) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(size = 28),
        axis.text = element_text (size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 24),
        legend.position = c(.5, .15),
        legend.key.size = unit(2, 'lines'),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_continuous(breaks = c(5,10,15,20,25,30), expand = c(0,0), limits = c(0,31)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80), expand = c(0,0), limits = c(30,90))


## make plot 2

blockMeans <- apply(winnings,c(1,3),mean)
overallBlockMean <- apply(blockMeans,1,mean)
overallBlockCI <- 1.96 * apply(blockMeans,1,sd) / sqrt(nParticipants)

df <- data.frame(meanPoints = overallBlockMean, 
                 ciPoints = overallBlockCI,
                 block = c("Block 1","Block 2","Block 3"))

p2 <- ggplot(data = df, aes(x = block, y = meanPoints)) +
  geom_col(width = 0.6, fill = "white",colour = "black", size = 1.5) +
  geom_errorbar(aes(y = meanPoints, ymin = meanPoints - ciPoints, ymax = meanPoints + ciPoints, width = 0.2)) +
  labs(x = "\nBlock", y = "Points per trial\n")


p2 + theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(size = 28),
        axis.text = element_text (size = 20)) +
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80), expand = c(0,0)) +
  coord_cartesian(ylim = c(30,90))

# make plot 3
p3 <- ggplot(data.frame(winningsAsProportion),aes(y = winningsAsProportion, x = 1))
p3 + geom_violin(adjust = 1, color = "#0059b3",trim = TRUE, size = 1.5, width = 1.2) + 
  geom_jitter(height = .02, width = .03,color = "#0059b3") + 
  ylim(.4,2) + 
  labs(x = "\n", y = "Total points ratio\n") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(size = 28),
        axis.text.y = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  geom_hline(yintercept = 1,linetype = "dotted",size = 1)

# retrieve only data from 5 trials before to 7 trials after a change
proximal.data <- subset(sorted.data, sorted.data$changeLag < 11 & sorted.data$changeLag > -11)

# aggregate choice proportions by lag number across participants
choice.by.lag.short <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$ID), FUN = mean)

# get mean and sd choice proportion by lag number
mean.choice.prop.short <- tapply(choice.by.lag.short$x, choice.by.lag.short$Group.1, FUN = mean)
lower.ci.choice.prop.short <- vector(mode = "logical", length = dim(mean.choice.prop.short))
upper.ci.choice.prop.short <- vector(mode = "logical", length = dim(mean.choice.prop.short))
boot.fun <- function(data, indices){
  return(mean(d[indices]))
}
indices <- c(-10:-2, 1:10)
for (i in 1:length(indices)){
  print(i)
  d <- choice.by.lag.short[choice.by.lag.short$Group.1 == indices[i],]$x
  results <-  boot(data = d,statistic = boot.fun, R = 1000)
  results.95.ci <- boot.ci(results)
  if (indices[i] > -2){
    putLoc <- i + 1
  } else{
    putLoc <- i
  }
    
  lower.ci.choice.prop.short[putLoc] = results.95.ci$normal[2]
  upper.ci.choice.prop.short[putLoc] = results.95.ci$normal[3]
}
choice.prop.short <- data.frame(mean.choice.prop.short, lower.ci.choice.prop.short,upper.ci.choice.prop.short)
plotLabs <- as.numeric(rownames(choice.prop.short))
plotLocs <- plotLabs
plotLocs[plotLocs < 0] <- plotLocs[plotLocs < 0] + 1
choice.prop.short <- data.frame(plotLocs,plotLabs,mean.choice.prop.short, lower.ci.choice.prop.short,upper.ci.choice.prop.short)


# create short plot
p4 <- ggplot(choice.prop.short,
                            aes(x = plotLocs, y = mean.choice.prop.short)) +
  geom_ribbon(aes(ymin = lower.ci.choice.prop.short, ymax = upper.ci.choice.prop.short),
              colour = "white",fill = "gray", alpha = 0.7) +
  geom_line(size = 2) +
  geom_point(size = 2.5, shape = 21, fill = "white") +
  labs(x = "\nChange lag", y = "Novel choice proportion\n") +
  scale_x_continuous(breaks = c(-9, -4, 0.5, 5, 10), labels = c(-10, -5, 0, 5, 10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5)) +
    geom_vline(xintercept = 0.5, lty = "dotted", size = 1) +
    theme(panel.grid = element_blank(), 
          panel.background = element_rect(fill = "white"), 
          axis.line = element_line(color = "black", size = 0.3),
          axis.title = element_text(size = 28),
          axis.text = element_text (size = 20),
          legend.title = element_blank(),
          legend.text = element_text(size = 24),
          plot.margin = margin(1, 1, 1, 1, "cm"))



# build short plot
p4

# get mean and sd choice proportion by lag number and block number
choice.by.lag.short <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$block, proximal.data$ID), FUN = mean)
mean.choice.prop.short <- matrix(data = 0, nrow = 3, ncol = 20)
lower.ci.choice.prop.short <- matrix(data = 0, nrow = 3, ncol = 20)
upper.ci.choice.prop.short <- matrix(data = 0, nrow = 3, ncol = 20)

boot.fun <- function(data, indices){
  return(mean(d[indices]))
}
indices <- c(-10:-2, 1:10)

for (j in 1:3){
  temp.data <- subset(choice.by.lag.short, choice.by.lag.short$Group.2 == j)
  mean.choice.prop.short[j,] <- tapply(temp.data$x, temp.data$Group.1, FUN = mean)
  
  for (i in 1:length(indices)){
    print(i)
    d <- temp.data[temp.data$Group.1 == indices[i],]$x
    results <-  boot(data = d,statistic = boot.fun, R = 1000)
    results.95.ci <- boot.ci(results)
    if (indices[i] > -2){
      putLoc <- i + 1
    } else{
      putLoc <- i
    }
    
    lower.ci.choice.prop.short[j,putLoc] = results.95.ci$normal[2]
    upper.ci.choice.prop.short[j,putLoc] = results.95.ci$normal[3]
  }
}

mean.choice.prop.short <- c(mean.choice.prop.short[1,],mean.choice.prop.short[2,],mean.choice.prop.short[3,])
lower.ci.choice.prop.short <-  c(lower.ci.choice.prop.short[1,],lower.ci.choice.prop.short[2,],lower.ci.choice.prop.short[3,])
upper.ci.choice.prop.short <-  c(upper.ci.choice.prop.short[1,],upper.ci.choice.prop.short[2,],upper.ci.choice.prop.short[3,])
block.number <- c(rep(1,20), rep(2,20),rep(3,20))

choice.prop.short <- data.frame(mean.choice.prop.short, lower.ci.choice.prop.short,upper.ci.choice.prop.short,block.number)
plotLabs <- rep(c(-10:-1,1:10),3)
plotLocs <- plotLabs
plotLocs[plotLocs < 0] <- plotLocs[plotLocs < 0] + 1

# create short plot
cols <- c("1" = "#003e7d", "2" = "#0059b3", "3" = "#b2cde8")

p5 <- ggplot(choice.prop.short,
             aes(x = plotLocs, y = mean.choice.prop.short, colour = factor(block.number))) +
  # geom_ribbon(aes(ymin = lower.ci.choice.prop.short, ymax = upper.ci.choice.prop.short, group = block.number),
  #             colour = "gray",fill = "gray", alpha = 0.3) +
  geom_line(size = 2) +
  geom_point(size = 2.5, shape = 21, fill = "white") +
  labs(x = "\nChange lag", y = "Novel choice proportion\n") +
  scale_x_continuous(breaks = c(-9, -4, 0.5, 5, 10), labels = c(-10, -5, 0, 5, 10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5)) +
    geom_vline(xintercept = 0.5, lty = "dotted", size = 1) +
    theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        axis.line = element_line(color = "black", size = 0.3),
        axis.title = element_text(size = 28),
        axis.text = element_text (size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 24),
        legend.key = element_rect(fill = 'white'),
        legend.key.size = unit(2, 'lines'),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = c(.2, .9))


# build short plot
p5 + scale_color_manual(name = "Block",values = cols, labels = c("Block 1","Block 2", "Block 3") )
