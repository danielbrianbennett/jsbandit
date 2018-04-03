library(mvtnorm)

source(here::here("helper","HelperFunctions.R"))
A <- CalculateA()
allData <- ExtractData(here::here("raw_data","banditData_v2point2.Rdata"))
pID <- 1

data <- list("block" = allData$block[1,],
               "trial" = allData$trial[1,],
               "whichFilled" = allData$whichFilled[1,],
               "outcome" = allData$outcome[1,],
               "choice" = allData$choice[1,],
               "changeLag" = allData$changeLag[1,])

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
            allChoiceProb[j,i] <- pmvnorm(lower= 0,mean = as.vector(M), sigma = H)[1]
        }
        choiceProb[i] <- allChoiceProb[data$choice[i],i]
        
        # index chosen option and changed option
        chosenIndicator <- 1 * (1:nBandits == data$choice[i])
        changedIndicator <- 1 * (1:nBandits == data$whichFilled[i])
        
        # update kalman gain
        kalmanGain <- (banditVariance + model$pars$zeta^2) / (banditVariance + model$pars$zeta^2 + model$pars$epsilon^2)
        
        # update bandit means
        banditMean <- banditMean + (chosenIndicator * kalmanGain * (data$outcome[i] - banditMean))
        
        # update bandit variance
        banditVariance <- (1 - (deltaFunction * kalmanGain)) * (banditVariance + (model$pars$zeta^2))
        
    }
    
    return(sum(-log(choiceProb)))
}