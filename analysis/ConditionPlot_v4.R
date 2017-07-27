# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v4" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# aggregate choice proportions by lag number across participants
choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block, sorted.data$tagText), FUN = mean)
sd.choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block, sorted.data$tagText), FUN = sd)
choice.by.lag.cond <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$tagText), FUN = mean)
choice.by.lag.cond.sd <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$tagText), FUN = mean)
choice.by.lag[,5] <- sd.choice.by.lag$x
choice.by.lag.cond[,4] <- choice.by.lag.cond.sd$x
colnames(choice.by.lag) <- c("lag","block","tag", "meanProp","sdProp")
colnames(choice.by.lag.cond) <- c("lag","tag", "meanProp","sdProp")
choice.by.lag <- as.data.frame(choice.by.lag)
choice.by.lag.cond <- as.data.frame(choice.by.lag.cond)

# create short plot for ???? tag
plot.data <- subset(choice.by.lag, tag == "???")
choice.plot <- ggplot(plot.data,
                      aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
  # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
  #   colour = "gray",fill = "gray", alpha = 0.4) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (??? tag condition)") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-10:-1, 1:10)) +
  xlim(-10,10) +
  ylim(0,1)

# build short plot
choice.plot

# create short plot for good tag
plot.data <- subset(choice.by.lag, tag == "good")
choice.plot <- ggplot(plot.data,
                      aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
  # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
  #   colour = "gray",fill = "gray", alpha = 0.4) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (good tag condition)") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-10:-1, 1:10)) +
  xlim(-10,10) +
  ylim(0,1)

# build short plot
choice.plot

# create short plot for bad tag
plot.data <- subset(choice.by.lag, tag == "bad")
choice.plot <- ggplot(plot.data,
                      aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
  # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
  #   colour = "gray",fill = "gray", alpha = 0.4) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (bad tag condition)") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-10:-1, 1:10)) +
  xlim(-10,10) +
  ylim(0,1)

# build short plot
choice.plot

# create short plot comparing tags
choice.plot <- ggplot(choice.by.lag.cond,
                      aes(x = lag, y = meanProp, group = tag, colour = as.factor(tag))) +
  # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
  #   colour = "gray",fill = "gray", alpha = 0.4) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-10:-1, 1:10)) +
  xlim(-10,10) +
  ylim(0,1)

# build short plot
choice.plot