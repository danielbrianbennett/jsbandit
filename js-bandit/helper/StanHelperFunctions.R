ExtractDataForStan <- function(file){
    
    #### load data ####
    load(file)
    
    # exclude incomplete participants
    IDs <- unique(sorted.data$ID)
    nTrials <-  as.vector(by(sorted.data$trial, sorted.data$ID, FUN = function(x){length(x)}))
    incompleteParticipants <- which(nTrials != (max(sorted.data$trial) * max(sorted.data$block)))
    sorted.data <- subset(sorted.data, !(ID %in% IDs[incompleteParticipants]))
    IDs <- unique(sorted.data$ID)
    
    # calculate important variables
    nParticipants <- length(unique(sorted.data$ID))
    nBlocks <- max(sorted.data$block)
    nTrials <- max(sorted.data$trial)

    #### extract relevant variables ####
    block_ix <- sorted.data$block
    trial <- sorted.data$trial
    whichFilled <- sorted.data$whichFilled
    outcome <- sorted.data$pointsWon
    choice <- sorted.data$choice
    changeLag <- sorted.data$changeLag

    # remove 0 lags
    changeLag[changeLag < 0] <- 0
    
    # convert string choice values to numbers, 1-4, clockwise from top
    choice[choice == "top"] <- 1
    choice[choice == "right"] <- 2
    choice[choice == "bottom"] <- 3
    choice[choice == "left"] <- 4
    choice <- as.numeric(choice)
    whichFilled[whichFilled == "none"] <- -1
    whichFilled[whichFilled == "top"] <- 1
    whichFilled[whichFilled == "right"] <- 2
    whichFilled[whichFilled == "bottom"] <- 3
    whichFilled[whichFilled == "left"] <- 4
    whichFilled <- as.numeric(whichFilled)
    
    # make participant index for parameter indexing
    participant_ix <- NULL
    for(i in 1:nParticipants){
        participant_ix <- c(participant_ix, rep(i, times = nBlocks * nTrials))
    }

    
    return(list("N" = nParticipants,
                "T" = length(choice),
                "participant_ix" = participant_ix,
                "block_ix" = block_ix,
                "trial" = trial,
                "choice" = choice,
                "outcome" = outcome,
                "whichFilled" = whichFilled,
                "changeLag" = changeLag))
}