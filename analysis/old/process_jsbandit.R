library(ggplot2)

sorted.data["changeLag"] <- NA
sorted.data["filledChosen"] <- NA
sorted.data["switchChoice"] <- NA

nBlocks <- 3
nTrials <- 30

IDs <- unique(sorted.data$ID)
weirdParticipants <- c()

for ( p in 1:length(IDs) ){
  for ( block in 1:nBlocks ) {
    
    # get participant/block data
    temp.data <- sorted.data[sorted.data$ID == IDs[p] & sorted.data$block == block,]
    
    # check for weird data
    if (length(temp.data$trial) != nTrials) {
      weirdParticipants <- c(weirdParticipants, unique(temp.data$ID))
    }
    
    # assign changeLag
    changeNumber <- match(unique(temp.data$whichFilled),temp.data$whichFilled)[2]
    temp.data[,"changeLag"] <- temp.data$trial - changeNumber
    temp.data[temp.data$changeLag >= 0,]$changeLag = temp.data[temp.data$changeLag >= 0,]$changeLag + 1
    
    # work out whether filled option was chosen
    whichFilled <- unique(temp.data$whichFilled)[2]
    test <- temp.data$choice == as.character(whichFilled)
    if (any(test)){
      temp.data[test,]$filledChosen <- 1
    }
    if (!all(test)){
      temp.data[!test,]$filledChosen <- 0
    }
    
    # reassign to sorted.data
    sorted.data[sorted.data$ID == IDs[p] & sorted.data$block == block,] <- temp.data
  }
}

# define as either switch or stay
for (i in 1:nrow(sorted.data)) {
  if (sorted.data[i,]$trial > 1 && sorted.data[i,]$choice == sorted.data[i-1,]$choice){
    sorted.data[i,]$switchChoice <- 0
  } else if (sorted.data[i,]$trial > 1 && sorted.data[i,]$choice != sorted.data[i-1,]$choice){
    sorted.data[i,]$switchChoice <- 1
  }
}

# work out which are the weird participants and exclude them
weirdParticipants <- unique(weirdParticipants)
weird.data <- subset(sorted.data, (sorted.data$ID %in% weirdParticipants))
sorted.data <- subset(sorted.data, !(sorted.data$ID %in% weirdParticipants))