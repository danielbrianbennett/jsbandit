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
choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block), FUN = mean)
sd.choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block), FUN = sd)
choice.by.lag[,4] <- sd.choice.by.lag$x
colnames(choice.by.lag) <- c("lag","block","meanProp","sdProp")
choice.by.lag <- as.data.frame(choice.by.lag)

# create short plot
choice.plot <- ggplot(choice.by.lag,
                            aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
  # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
  #   colour = "gray",fill = "gray", alpha = 0.4) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-10:-1, 1:10)) +
  xlim(-10,10) +
  ylim(0,.6)


# build short plot
choice.plot

