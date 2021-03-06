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
    whichFilled <- as.character(sorted.data$whichFilled)
    outcome <- sorted.data$pointsWon
    choice <- as.character(sorted.data$choice)
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

ExtractBetweenDataForStan <- function(file){
    
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
    whichFilled <- as.character(sorted.data$whichFilled)
    outcome <- sorted.data$pointsWon
    choice <- as.character(sorted.data$choice)
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
    group_ix <- rep(NA, times = nParticipants)
    for(i in 1:nParticipants){
        participant_ix <- c(participant_ix, rep(i, times = nBlocks * nTrials))
        pData <- subset(sorted.data, sorted.data$ID == IDs[i])
        if (pData[1,]$tagText == "good"){
            group_ix[i] <- 1
        } else if (pData[1,]$tagText == "bad"){
            group_ix[i] <- 2
        } else if (pData[1,]$tagText == "???"){
            group_ix[i] <- 3
        }
    }
    
    N_g <- sum(group_ix == 1)
    N_b <- sum()
    
    
    return(list("N" = nParticipants,
                "N_g" = sum(group_ix == 1),
                "N_b" = sum(group_ix == 2),
                "N_q" = sum(group_ix == 3),
                "T" = length(choice),
                "participant_ix" = participant_ix,
                "block_ix" = block_ix,
                "group_ix" = group_ix,
                "trial" = trial,
                "choice" = choice,
                "outcome" = outcome,
                "whichFilled" = whichFilled,
                "changeLag" = changeLag))
}