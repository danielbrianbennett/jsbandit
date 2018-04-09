#### Preliminaries ####

libLoc <- "/jukebox/niv/Dan/Rpackages"
projDir <- "/jukebox/niv/Dan/Rprojects/js-bandit"

require(mvtnorm, lib.loc = libLoc)
require(stats4, lib.loc = libLoc)
require(tictoc, lib.loc = libLoc)
require(iterators, lib.loc = libLoc)
require(foreach, lib.loc = libLoc)
require(doParallel, lib.loc = libLoc)


source(paste(projDir,"helper","HelperFunctions.R", sep = "/"))
source(paste(projDir,"models","ModelLikelihood.R", sep = "/"))


# specify truncated cauchy 
r_tcauchy <- function(nStarts=1,location,scale,bound){
    starts <- rcauchy(nStarts, location = location, scale = scale)
    while (!all(abs(starts) <= bound)){
        starts[abs(starts) > bound] <- rcauchy(sum(abs(starts) > bound),location = location, scale = scale)
    }
    return(starts)
}

# specify truncated cauchy 
d_tcauchy <- function(vals,location,scale,bound){
    normalisingConstant <- diff(pcauchy(c(-bound,bound), location = location, scale = scale))
    return(dcauchy(vals, location=location, scale=scale) / normalisingConstant)
}

A <- CalculateA()
allData <- ExtractData(paste(projDir,"raw_data","banditData_v2point2.RData", sep = "/"))

nSamples <- 1000


cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)

#### Specify model ####
V_init_constant <- 0
var_init_constant <- 1000
epsilon_constant <- 4
zeta_constant <- 8

model_KS <- list("pars" = list("zeta" = NA,
                               "epsilon" = NA,
                               "B" = NA,
                               "j" = NA,
                               "k" = NA),
                 "lowerBound" = c(0,0,-100,-Inf,-Inf),
                 "upperBound" = c(100,100,100,Inf,Inf),
                 "lambda" = function(zeta,epsilon,B,j,k,V_init,var_init){
                     model$pars$zeta <- zeta
                     model$pars$epsilon <- epsilon
                     model$pars$B <- B
                     model$pars$j <- j
                     model$pars$k <- k
                     model$pars$V_init <- V_init
                     model$pars$var_init <- var_init
                     ll <- likelihood(fitData,model)
                     return(ll$negLL)
                 },
                 "startPoints" = function(nStarts = 1){
                     return(list("zeta" = runif(nStarts, min = 0, max = 10),
                                 "epsilon" = runif(nStarts, min = 0, max = 10),
                                 "B" = rnorm(nStarts, mean = 0, sd = 20),
                                 "j" = rnorm(nStarts, mean = 0, sd = 1),
                                 "k" = rnorm(nStarts, mean = 0, sd = 1)))},
                 "fixed" = list("V_init" = V_init_constant,
                                "var_init" = var_init_constant),
                 "samplingRand" = function(nStarts = 1){
                     return(list("zeta" = rep(zeta_constant, times = nStarts),# runif(nStarts, min = 0, max = 100),
                                 "epsilon" = rep(epsilon_constant, times = nStarts),# runif(nStarts, min = 0, max = 100),
                                 "B" = r_tcauchy(nStarts,location = 0, scale = 10, bound = 100),
                                 "j" = r_tcauchy(nStarts,location = 0, scale = 1, bound = 20),
                                 "k" = r_tcauchy(nStarts,location = 0, scale = 1, bound = 20),
                                 "V_init" = rep(V_init_constant, times = nStarts),
                                 "var_init" = rep(var_init_constant, times = nStarts)))},
                 "samplingCDF" = function(rands){
                     return(
                         list("zeta" = rep(1, times = length(rands$V_init)),#dunif(rands$zeta,  min = 0, max = 100),
                              "epsilon" = rep(1, times = length(rands$V_init)),#dunif(rands$epsilon,  min = 0, max = 100),
                              "B" = d_tcauchy(rands$B, location = 0, scale = 10, bound = 100),
                              "j" = d_tcauchy(rands$j, location = 0, scale = 1, bound = 20),
                              "k" = d_tcauchy(rands$k, location = 0, scale = 1, bound = 20),
                              "V_init" = rep(1, times = length(rands$V_init)),
                              "var_init" = rep(1, times = length(rands$var_init))))}
)

model <- model_KS

LL <- matrix(nrow = dim(allData$whichFilled)[1], ncol = nSamples)
allPars <- list()
parWeights <- list()
for (pID in 1:dim(allData$whichFilled)[1]){
    data <- list("block" = allData$block[pID,],
                 "trial" = allData$trial[pID,],
                 "whichFilled" = allData$whichFilled[pID,],
                 "outcome" = allData$outcome[pID,],
                 "choice" = allData$choice[pID,],
                 "changeLag" = allData$changeLag[pID,])
    
    tic()
    # for (j in 1:nSamples){
    allPars[[pID]] <- model$samplingRand(nSamples)
    parWeights[[pID]] <- model$samplingCDF(allPars[[pID]])
    LL[pID,] <- foreach(j = 1:nSamples, .combine = c) %dopar% {
        model$pars$zeta <- allPars[[pID]]$zeta[j]
        model$pars$epsilon <- allPars[[pID]]$epsilon[j]
        model$pars$B <- allPars[[pID]]$B[j]
        model$pars$j <- allPars[[pID]]$j[j]
        model$pars$k <- allPars[[pID]]$k[j]
        model$pars$V_init <- allPars[[pID]]$V_init[j]
        model$pars$var_init <- allPars[[pID]]$var_init[j]
        ll <- likelihood(data,model)
        ll$negLL
    }
    toc()
    
    # calculate likelihood
    p_x <- (1/LL[pID])
    
    # calculate importance weights for parameter
    q_x <- parWeights[[pID]]$B
    impWeight <- (p_x/q_x) / sum((p_x/q_x),na.rm=TRUE)
    
    hist(sample(allPars[[pID]]$B, size = 100000, replace = TRUE, prob = impWeight[!is.na(impWeight)]),100,col="gray")
    print(sum(allPars[[pID]]$B * impWeight))
}

stopCluster(cl)
