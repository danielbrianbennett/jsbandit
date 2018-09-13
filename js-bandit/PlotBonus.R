library(here)
library(rstan)

##### EXPERIMENT 1 #####

# load samples for experiment 1
load(here("models","samples","delta_v2.Rdata"))

# extract samples of interest
bonusSamples <- extract(samples, pars = c("mu_B","mu_j","mu_k","sigma_B","sigma_j","sigma_k"))

# calculate posterior mean for each of the parameters of interest
postMean <- lapply(bonusSamples, FUN = mean)

# specify block and trial indices
nTimePoints <- 10
blockIndices <- c(rep(1, times = nTimePoints), rep(2, times = nTimePoints), rep(3, times = nTimePoints))
trialIndices <- rep(1:nTimePoints, times = 3)

# plot bonus size over time
bonusSize <- mapply(FUN = function(B,j,k,blockIndex,trialIndex){return(B * (blockIndex ^ j) * (trialIndex ^ k))}, rep(postMean$mu_B, times = 3 * nTimePoints), rep(postMean$mu_j, times = 3 * nTimePoints), rep(postMean$mu_k,times = 3 * nTimePoints), blockIndices, trialIndices)
bonusSize <- matrix(data = bonusSize, nrow = 3, ncol = nTimePoints, byrow = TRUE)
plot(1:nTimePoints, bonusSize[1,],  type = "b", lwd = 2, xlab = "Post-change lag", ylab = "Bonus size", main = "Bonus size over time")
lines(1:nTimePoints, bonusSize[2,], type = "b", lwd = 2, col = "#979797")
lines(1:nTimePoints, bonusSize[3,], type = "b", lwd = 2, col = "#D3D3D3")
legend(x = "topright", col = c("black","#979797","#D3D3D3"), lwd = c(2,2,2), legend = c("Block 1","Block 2", "Block 3"), bty = "n")

##### EXPERIMENT 2 #####

# load samples for experiment 2
load(here("models","samples","delta_v3.Rdata"))

# extract samples of interest
bonusSamples <- extract(samples, pars = c("mu_B","mu_j","mu_k","sigma_B","sigma_j","sigma_k"))

# calculate posterior mean for each of the parameters of interest
postMean <- lapply(bonusSamples, FUN = mean)

# specify block and trial indices
nTimePoints <- 10
blockIndices <- c(rep(1, times = nTimePoints), rep(2, times = nTimePoints), rep(3, times = nTimePoints))
trialIndices <- rep(1:nTimePoints, times = 3)

# plot bonus size over time
bonusSize <- mapply(FUN = function(B,j,k,blockIndex,trialIndex){return(B * (blockIndex ^ j) * (trialIndex ^ k))}, rep(postMean$mu_B, times = 3 * nTimePoints), rep(postMean$mu_j, times = 3 * nTimePoints), rep(postMean$mu_k,times = 3 * nTimePoints), blockIndices, trialIndices)
bonusSize <- matrix(data = bonusSize, nrow = 3, ncol = nTimePoints, byrow = TRUE)
plot(1:nTimePoints, bonusSize[1,],  type = "b", lwd = 2, xlab = "Post-change lag", ylab = "Bonus size", main = "Bonus size over time", col = "#6f5884")
lines(1:nTimePoints, bonusSize[2,], type = "b", lwd = 2, col = "#9f7ebd")
lines(1:nTimePoints, bonusSize[3,], type = "b", lwd = 2, col = "#cfbede")
legend(x = "topright", col = c("black","#979797","#D3D3D3"), lwd = c(2,2,2), legend = c("Block 1","Block 2", "Block 3"), bty = "n")

##### EXPERIMENT 3 #####

# load samples for experiment 3
load(here("models","samples","delta_v4.Rdata"))

# extract samples of interest
bonusSamples <- extract(samples, pars = c("mu_B_g","mu_j_g","mu_k_g","sigma_B_g","sigma_j_g","sigma_k_g","mu_B_b","mu_j_b","mu_k_b","sigma_B_b","sigma_j_b","sigma_k_b","mu_B_q","mu_j_q","mu_k_q","sigma_B_q","sigma_j_q","sigma_k_q"))

# calculate posterior mean for each of the parameters of interest
postMean <- lapply(bonusSamples, FUN = mean)

Bs <- c(postMean$mu_B_g, postMean$mu_B_b, postMean$mu_B_q)
js <- c(postMean$mu_j_g, postMean$mu_j_b, postMean$mu_j_q)
ks <- c(postMean$mu_k_g, postMean$mu_k_b, postMean$mu_k_q)
cols <- list(list("#00b200","#66d066","#b2e7b2"),
             list("#ff3232","#ff7f7f","#ffcccc"),
             list("#0000cc","#6666e0","#b2b2ef"))
titles <- c("Good tag","Bad tag","???? tag")

nTimePoints <- 20
for (i in 1:3){
    
    # extract loop-dependent variables
    B <- Bs[i]
    j <- js[i]
    k <- ks[i]
    col <- unlist(cols[[i]])
    title <- titles[i]
    
    # specify block and trial indices
    blockIndices <- c(rep(1, times = nTimePoints), rep(2, times = nTimePoints), rep(3, times = nTimePoints))
    trialIndices <- rep(1:nTimePoints, times = 3)
    
    # plot bonus size over time
    bonusSize <- mapply(FUN = function(B,j,k,blockIndex,trialIndex){return(B * (blockIndex ^ j) * (trialIndex ^ k))}, rep(B, times = 3 * nTimePoints), rep(j, times = 3 * nTimePoints), rep(k,times = 3 * nTimePoints), blockIndices, trialIndices)
    bonusSize <- matrix(data = bonusSize, nrow = 3, ncol = nTimePoints, byrow = TRUE)
    plot(1:nTimePoints, bonusSize[1,],  type = "b", lwd = 2, xlab = "Post-change lag", ylab = "Bonus size", main = title, col = col[1])
    lines(1:nTimePoints, bonusSize[2,], type = "b", lwd = 2, col = col[2])
    lines(1:nTimePoints, bonusSize[3,], type = "b", lwd = 2, col = col[3])
    legend(x = "topright", col = col, lwd = c(2,2,2), legend = c("Block 1","Block 2", "Block 3"), bty = "n")
}