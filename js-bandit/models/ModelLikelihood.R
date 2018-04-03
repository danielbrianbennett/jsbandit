likelihood <- function(data,model){
    
    # set constants
    nBandits <- 4
    
    # create containers
    allChoiceProb <- matrix(data = NA, nrow = nBandits, ncol = length(data$block))
    choiceProb <- rep(NA, times = length(data$block))        
    
    # calculate bonus amount
    bonus <- CalculateBonus(data$block,data$changeLag,model$pars$B,model$pars$k,model$pars$j)
    
    for (i in 1:length(data$trial)){
        
        # re-initialise means on first trial of block
        if (data$trial[i] == 1){
            banditMean <- rep(model$pars$V_init, times = nBandits)
            banditVariance <- rep(model$pars$var_init, times = nBandits)
        }
        
        # calculate choice probabilities
        for (j in 1:nBandits){
            M = A[,,j] %*% banditMean
            H = A[,,j] %*% diag(banditVariance + (model$pars$epsilon^2)) %*% t(A[,,j])
            allChoiceProb[j,i] <- pmvnorm(lower= 0,mean = as.vector(M), sigma = H, algorithm = TVPACK())[1]
        }
        choiceProb[i] <- allChoiceProb[data$choice[i],i]
        
        # index chosen option and changed option
        chosenIndicator <- 1 * (1:nBandits == data$choice[i])
        changedIndicator <- 1 * (1:nBandits == data$whichFilled[i])
        
        # update kalman gain
        kalmanGain <- (banditVariance + model$pars$zeta^2) / (banditVariance + model$pars$zeta^2 + model$pars$epsilon^2)
        
        # update bandit means
        banditMean <- banditMean + (chosenIndicator * kalmanGain * (data$outcome[i] - banditMean))
        
        # append bonus to bandit mean
        if (!(is.na(bonus[i]))){
            banditMean <- banditMean + bonus[i]
        }
        
        # update bandit variance
        banditVariance <- (1 - (chosenIndicator * kalmanGain)) * (banditVariance + (model$pars$zeta^2))
        
    }
    
    return(list("negLL" = sum(-log(choiceProb)),
                "choiceProb" = choiceProb))
}