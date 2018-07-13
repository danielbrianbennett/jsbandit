# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v3"
fileDir <- "~/Documents/Git/jsbandit/task/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# load participant whitelist
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

# aggregate choice proportions by lag number across participants
eligible.data <- subset(sorted.data, sorted.data$changeLag >= -10 & sorted.data$changeLag <= 10)
choice.by.lag <- aggregate(eligible.data$filledChosen, by = list(eligible.data$changeLag, eligible.data$block), FUN = mean)
sd.choice.by.lag <- aggregate(eligible.data$filledChosen, by = list(eligible.data$changeLag, eligible.data$block), FUN = sd)
choice.by.lag[,4] <- sd.choice.by.lag$x
colnames(choice.by.lag) <- c("lag","block", "meanProp","sdProp")
choice.by.lag <- as.data.frame(choice.by.lag)
choice.by.lag[choice.by.lag$lag < 0,]$lag <- choice.by.lag[choice.by.lag$lag < 0,]$lag + 1
cols <- c("1" = "#003e7d", "2" = "#0059b3", "3" = "#b2cde8")

condPlot <- ggplot(choice.by.lag,
             aes(x = lag, y = meanProp, colour = factor(block))) +
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
condPlot + scale_color_manual(name = "Block",values = cols, labels = c("Block 1","Block 2", "Block 3") )
# 
# # create short plot comparing tags
# cbPalette <- c("#0000CD", "#2F4F4F", "#FFFF00")
# choice.plot <- ggplot(choice.by.lag,
#                       aes(x = lag, y = meanProp, group = colour, colour = colour)) +
#     # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#     #   colour = "gray",fill = "gray", alpha = 0.4) +
#     geom_line(size = 2) +
#     geom_point(size = 4, shape = 21, fill = "white") +
#     labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
#     theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#     scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#     xlim(-10,10) +
#     ylim(0,1) +
#     scale_colour_manual(values=cbPalette)
# 
# 
# 
# 
# # aggregate choice proportions by lag number across participants
# choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$fillColour), FUN = mean)
# sd.choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$fillColour), FUN = sd)
# choice.by.lag[,4] <- sd.choice.by.lag$x
# colnames(choice.by.lag) <- c("lag","colour", "meanProp","sdProp")
# choice.by.lag <- as.data.frame(choice.by.lag)
# 
# # create short plot comparing tags
# cbPalette <- c("#0000CD", "#2F4F4F", "#FFFF00")
# choice.plot <- ggplot(choice.by.lag,
#                       aes(x = lag, y = meanProp, group = colour, colour = colour)) +
#   # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#   #   colour = "gray",fill = "gray", alpha = 0.4) +
#   geom_line(size = 2) +
#   geom_point(size = 4, shape = 21, fill = "white") +
#   labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
#   theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#   scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#   xlim(-10,10) +
#   ylim(0,1) +
#   scale_colour_manual(values=cbPalette)
# 
# # build short plot
# choice.plot
# 
# # aggregate choice proportions by lag number across participants
# choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block, sorted.data$fillColour), FUN = mean)
# sd.choice.by.lag <- aggregate(sorted.data$filledChosen, by = list(sorted.data$changeLag, sorted.data$block, sorted.data$fillColour), FUN = sd)
# choice.by.lag[,5] <- sd.choice.by.lag$x
# colnames(choice.by.lag) <- c("lag", "block", "colour", "meanProp","sdProp")
# choice.by.lag <- as.data.frame(choice.by.lag)
# 
# # create short plot comparing tags blockwise
# plot.data <- subset(choice.by.lag, colour == "#0000CD")
# choice.plot <- ggplot(plot.data,
#                       aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
#   # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#   #   colour = "gray",fill = "gray", alpha = 0.4) +
#   geom_line(size = 2) +
#   geom_point(size = 4, shape = 21, fill = "white") +
#   labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (blue condition)") +
#   theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#   scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#   xlim(-10,10) +
#   ylim(0,1)
# 
# # build short plot
# choice.plot
# 
# # create short plot comparing tags blockwise
# plot.data <- subset(choice.by.lag, colour == "#2F4F4F")
# choice.plot <- ggplot(plot.data,
#                       aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
#   # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#   #   colour = "gray",fill = "gray", alpha = 0.4) +
#   geom_line(size = 2) +
#   geom_point(size = 4, shape = 21, fill = "white") +
#   labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (black condition)") +
#   theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#   scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#   xlim(-10,10) +
#   ylim(0,1)
# 
# # build short plot
# choice.plot
# 
# # create short plot comparing tags blockwise
# plot.data <- subset(choice.by.lag, colour == "#FFFF00")
# choice.plot <- ggplot(plot.data,
#                       aes(x = lag, y = meanProp, group = block, colour = as.factor(block))) +
#   # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#   #   colour = "gray",fill = "gray", alpha = 0.4) +
#   geom_line(size = 2) +
#   geom_point(size = 4, shape = 21, fill = "white") +
#   labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (yellow condition)") +
#   theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#   scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#   xlim(-10,10) +
#   ylim(0,1)
# 
# # build short plot
# choice.plot
# 
# # create short plot comparing tags blockwise
# plot.data <- subset(choice.by.lag, block == 1)
# choice.plot <- ggplot(plot.data,
#                       aes(x = lag, y = meanProp, group = colour, colour = colour)) +
#   # geom_ribbon(aes(ymin = pmax(0, meanProp - sdProp), ymax = pmin(meanProp + sdProp,1)),
#   #   colour = "gray",fill = "gray", alpha = 0.4) +
#   geom_line(size = 2) +
#   geom_point(size = 4, shape = 21, fill = "white") +
#   labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change (block 1 only)") +
#   theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
#   scale_x_continuous(breaks = c(-10:-1, 1:10)) +
#   xlim(-10,10) +
#   ylim(0,1) +
#   scale_colour_manual(values=cbPalette)
# 
# # build short plot
# choice.plot