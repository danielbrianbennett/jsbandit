# retrieve only data within 7 trials either side of a change
proximal.data <- subset(sorted.data, sorted.data$changeLag < 8 & sorted.data$changeLag > -6)

# aggregate choice proportions by lag number across participants
choice.by.lag <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$ID), FUN = mean)

# get mean and sd choice proportion by lag number
mean.choice.prop <- tapply(choice.by.lag$x, choice.by.lag$Group.1, FUN = mean)
sd.choice.prop <- tapply(choice.by.lag$x, choice.by.lag$Group.1, FUN = sd)
choice.prop <- data.frame(mean.choice.prop, sd.choice.prop)

# create plot
choice.prop.chart <- ggplot(choice.prop,
                            aes(x = as.numeric(rownames(choice.prop)), y = mean.choice.prop)) +
                            geom_ribbon(aes(ymin = pmax(0, mean.choice.prop - sd.choice.prop), ymax = pmin(mean.choice.prop + sd.choice.prop,1)),
                                        colour = "gray",fill = "gray", alpha = 0.7) +
                            geom_line(size = 2) +
                            geom_point(size = 4, shape = 21, fill = "white") +
                            xlim(-5,7) +
                            ylim(0,1) +
                            labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
                            theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
                            scale_x_continuous(breaks = c(-5:-1, 1:7)) +
                            geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), linetype = "dashed", size = 1)
            
# build plot
choice.prop.chart
                    

# choice prop by block
choice.by.lag <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$block), FUN = mean)
sd.by.lag <- aggregate(proximal.data$filledChosen, by = list(proximal.data$changeLag, proximal.data$block), FUN = sd)
choice.by.lag$sd <- sd.by.lag$x
colnames(choice.by.lag) <- list("Lag", "Block", "Mean", "SD")

choice.prop.chart <- ggplot(data = choice.by.lag, 
                            aes(x = Lag, y = Mean, group = Block)) + 
                            geom_line(aes(colour = as.factor(Block))) + 
                            geom_ribbon(aes(ymin = pmax(0, Mean - SD), ymax = pmin(Mean, SD,1), colour = as.factor(Block), fill = as.factor(Block)),
                            alpha = 0.2)  +
                            geom_point(size = 4, shape = 21, fill = "white") +
                            xlim(-5,7) +
                            ylim(0,1) +
                            labs(x = "Change Lag", y = "Oddball Choice Proportion", title = "Oddball Choice Proportion Pre/Post Change") +
                            theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold")) + 
                            scale_x_continuous(breaks = c(-5:-1, 1:7)) +
                            geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), linetype = "dashed", size = 1)

# build plot
choice.prop.chart

