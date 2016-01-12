library(ggplot2)

sorted.data["changeLag"] <- NA
sorted.data["filledChosen"] <- NA
sorted.data["switchChoice"] <- NA

lag.counter <- 1
for (i in 1:nrow(sorted.data)) {
  
  if (sorted.data[i,]$trial > 1 && sorted.data[i,]$choice == sorted.data[i-1,]$choice){
    sorted.data[i,]$switchChoice <- 0
  } else if (sorted.data[i,]$trial > 1 && sorted.data[i,]$choice != sorted.data[i-1,]$choice){
    sorted.data[i,]$switchChoice <- 1
  }
  
  if (sorted.data[i,]$whichFilled == "none") {
    lag.counter <- 1
    next
  }
  sorted.data[i,]$changeLag <- lag.counter
  lag.counter <- lag.counter + 1
}

# sorted.data <- sorted.data[sorted.data$ID != 48241651 & sorted.data$ID != 64207138,]

filled <-  sorted.data$whichFilled != "none" & sorted.data$choice != sorted.data$whichFilled
sorted.data[filled,]$filledChosen <- 0
filled.chosen <-  sorted.data$whichFilled != "none" & sorted.data$choice == sorted.data$whichFilled
sorted.data[filled.chosen,]$filledChosen <- 1

switch.filled.data <- sorted.data[sorted.data$switchChoice == 1 & sorted.data$whichFilled != "none" & sorted.data$changeLag <= 2,]


plot.choice.data <- data.frame(lag = 1:19, choiceProp = tapply(sorted.data$filledChosen,sorted.data$changeLag,mean))
p <- ggplot(plot.choice.data, aes(x = lag, y = choiceProp, group = 1)) + geom_line() + ylim(0, 0.5) + xlab("Trial Number After Change") + ylab("Proportion of Choices of Changed Option")
p + theme(axis.title = element_text(size=20), axis.text = element_text(size = 16))


