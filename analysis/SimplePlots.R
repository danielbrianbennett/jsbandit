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

# retrieve only data from 5 trials before to 7 trials after a change
proximal.data <- subset(sorted.data, sorted.data$changeLag < 8 & sorted.data$changeLag > -6)

# aggregate choice proportions by lag number across participants
choice.by.lag.short <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$ID), FUN = mean)
choice.by.lag.long <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$ID), FUN = mean)

# get mean and sd choice proportion by lag number
mean.choice.prop.short <- tapply(choice.by.lag.short$x, choice.by.lag.short$Group.1, FUN = mean)
sd.choice.prop.short <- tapply(choice.by.lag.short$x, choice.by.lag.short$Group.1, FUN = sd)
choice.prop.short <- data.frame(mean.choice.prop.short, sd.choice.prop.short)
mean.choice.prop.long <- tapply(choice.by.lag.long$x, choice.by.lag.long$Group.1, FUN = mean)
sd.choice.prop.long <- tapply(choice.by.lag.long$x, choice.by.lag.long$Group.1, FUN = sd)
choice.prop.long <- data.frame(mean.choice.prop.long, sd.choice.prop.long)

# create short plot
choice.plot.short <- ggplot(choice.prop.short,
                            aes(x = as.numeric(rownames(choice.prop.short)), y = mean.choice.prop.short)) +
  geom_ribbon(aes(ymin = pmax(0, mean.choice.prop.short - sd.choice.prop.short), ymax = pmin(mean.choice.prop.short + sd.choice.prop.short,1)),
              colour = "gray",fill = "gray", alpha = 0.7) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  xlim(-5,7) +
  ylim(0,1) +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-5:-1, 1:7))


# build short plot
choice.plot.short

# create long plot
choice.plot.long <- ggplot(choice.prop.long,
                              aes(x = as.numeric(rownames(choice.prop.long)), y = mean.choice.prop.long)) +
  geom_ribbon(aes(ymin = pmax(0, mean.choice.prop.long - sd.choice.prop.long), ymax = pmin(mean.choice.prop.long + sd.choice.prop.long,1)),
              colour = "gray",fill = "gray", alpha = 0.7) +
  geom_line(size = 2) +
  geom_point(size = 4, shape = 21, fill = "white") +
  xlim(-5,7) +
  ylim(0,1) +
  labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
  scale_x_continuous(breaks = c(-5:-1, 1:7))


# build long plot
choice.plot.long

