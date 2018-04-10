ExtractData <- function(file){

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
    nOptions <- length(unique(sorted.data$choice))
    
    #### extract relevant variables ####
    block <- matrix(sorted.data$block, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    trial <- matrix(sorted.data$trial, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    whichFilled <- matrix(sorted.data$whichFilled, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    outcome <- matrix(sorted.data$pointsWon, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    choice <- matrix(sorted.data$choice, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    changeLag <- matrix(sorted.data$changeLag, nrow = nParticipants, ncol = nBlocks * nTrials, byrow = TRUE)
    
    # remove 0 lags
    changeLag[changeLag < 0] <- NA
    
    # convert string choice values to numbers, 1-4, clockwise from top
    choice[choice == "top"] <- 1
    choice[choice == "right"] <- 2
    choice[choice == "bottom"] <- 3
    choice[choice == "left"] <- 4
    choice <- matrix(sapply(choice,FUN = as.numeric),nrow = dim(choice)[1] ,ncol = dim(choice)[2])
    whichFilled[whichFilled == "none"] <- 0
    whichFilled[whichFilled == "top"] <- 1
    whichFilled[whichFilled == "right"] <- 2
    whichFilled[whichFilled == "bottom"] <- 3
    whichFilled[whichFilled == "left"] <- 4    
    whichFilled <- matrix(sapply(whichFilled,FUN = as.numeric),nrow = dim(whichFilled)[1] ,ncol = dim(whichFilled)[2])
    
    return(list("block" = block,
                "trial" = trial,
                "whichFilled" = whichFilled,
                "outcome" = outcome,
                "choice" = choice,
                "changeLag" = changeLag))
}

CalculateA <- function(){
    A <- array(data = 0, dim = c(3,4,4))
    A[,,1] <- matrix(c(1,1,1,-1,0,0,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
    A[,,2] <- matrix(c(-1,0,0,1,1,1,0,-1,0,0,0,-1),nrow = 3, ncol = 4)
    A[,,3] <- matrix(c(-1,0,0,0,-1,0,1,1,1,0,0,-1),nrow = 3, ncol = 4)
    A[,,4] <- matrix(c(-1,0,0,0,-1,0,0,0,-1,1,1,1),nrow = 3, ncol = 4)
    
    return(A)
}

CalculateBonus <- function(block, changeLag, bonusAmount, blockDecay, trialDecay){
    return(bonusAmount * (block ^ blockDecay) * (changeLag ^ trialDecay))
}