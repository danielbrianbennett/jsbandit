# set file name and directory
base.directory <- "/Users/Daniel/Google Drive/Works in Progress/JSBANDIT/Bandit/data/Bandit project shared data"
directory.separator <- "/"
file.name <- "banditData_v2point2.RData"

# load data
load(paste(base.directory, file.name, sep = directory.separator))

# calculate variability of choices across participants and plot in a histogram
switch.prob <- aggregate(sorted.data$switchChoice, by = list(sorted.data$ID), FUN = "mean", na.rm = TRUE)
hist(switch.prob$x, breaks = 40, xlim = c(0,1), xlab = "Choice Stochasticity Index", ylab = "", main= "", col = "gray")

# divide choices into top, middle, and bottom tertiles
tertile.boundaries <- quantile(switch.prob$x, probs = c(1/3, 2/3))
bottom.tertile <- subset(switch.prob$Group.1, switch.prob$x <= tertile.boundaries[1])
middle.tertile <- subset(switch.prob$Group.1, switch.prob$x > tertile.boundaries[1] & switch.prob$x <= tertile.boundaries[2])
top.tertile <- subset(switch.prob$Group.1, switch.prob$x > tertile.boundaries[2])

sorted.data["switchTertile"] <- NA

for (i in 1:nrow(sorted.data)) {
  
  if (sorted.data[i,]$ID %in% bottom.tertile) {
    sorted.data[i,]$switchTertile <- "bottom"
  } else if (sorted.data[i,]$ID %in% middle.tertile){
    sorted.data[i,]$switchTertile <- "middle"
  } else if (sorted.data[i,]$ID %in% top.tertile){
    sorted.data[i,]$switchTertile <- "top"
  }
}

# retrieve only data from 5 trials before to 7 trials after a change
proximal.data <- subset(sorted.data, sorted.data$changeLag < 8 & sorted.data$changeLag > -6)

# aggregate choice proportions by lag number across participants
choice.by.lag <- aggregate(proximal.data$filledChosen, by = list(proximal.data$switchTertile, proximal.data$changeLag, proximal.data$ID), FUN = mean)

# get mean and sd choice proportion by lag number
mean.choice.prop <- aggregate(choice.by.lag$x, list(choice.by.lag$Group.1, choice.by.lag$Group.2), FUN = mean)

# create plot
choice.prop.chart <- ggplot(mean.choice.prop,
                            aes(x = as.numeric(mean.choice.prop$Group.2), y = mean.choice.prop$x, group = mean.choice.prop$Group.1, color = mean.choice.prop$Group.1)) +
                            geom_line(size = 2) +
                            geom_point(size = 4, shape = 21, fill = "white") +
                            xlim(-5,7) +
                            ylim(0,1) +
                            labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
                            theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
                            scale_x_continuous(breaks = c(-5:-1, 1:7))


# build plot
choice.prop.chart
