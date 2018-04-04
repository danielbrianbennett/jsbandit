        #### Preliminaries ####
        
        require(mvtnorm)
        require(stats4)
        require(tictoc)
        
        source(here::here("helper","HelperFunctions.R"))
        source(here::here("models","ModelLikelihood.R"))
        
        # specify truncated cauchy 
        r_tcauchy <- function(nStarts=1,location,scale,bound){
            starts <- rcauchy(nStarts, location = location, scale = scale)
            while (!all(abs(starts) <= bound)){
                starts[abs(starts) > bound] <- rcauchy(sum(abs(starts) > bound),location = 0, scale = 1)
            }
            return(starts)
        }
        
        # specify truncated cauchy 
        d_tcauchy <- function(vals,location,scale,bound){
            normalisingConstant <- diff(pcauchy(c(-bound,bound), location = location, scale = scale))
            return(dcauchy(vals, location=location, scale=scale) / normalisingConstant)
        }
        
        A <- CalculateA()
        allData <- ExtractData(here::here("raw_data","banditData_v2point2.Rdata"))
        pID <- 9
        nSamples <- 5000
        
        data <- list("block" = allData$block[pID,],
                     "trial" = allData$trial[pID,],
                     "whichFilled" = allData$whichFilled[pID,],
                     "outcome" = allData$outcome[pID,],
                     "choice" = allData$choice[pID,],
                     "changeLag" = allData$changeLag[pID,])
        
        cl <- makeCluster(parallel::detectCores(), type = "FORK")
        registerDoParallel(cl)
        
        #### Specify model ####
        V_init_constant <- 0
        var_init_constant <- 1000
        
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
                             return(list("zeta" = rgamma(nStarts, shape = 1, rate = 0.01),
                                         "epsilon" = rgamma(nStarts, shape = 1, rate = 0.01),
                                         "B" = r_tcauchy(nStarts,location = 0, scale = 10, bound = 100),
                                         "j" = r_tcauchy(nStarts,location = 0, scale = 1, bound = 100),
                                         "k" = r_tcauchy(nStarts,location = 0, scale = 1, bound = 100),
                                         "V_init" = rep(V_init_constant, times = nStarts),
                                         "var_init" = rep(var_init_constant, times = nStarts)))},
                         "samplingCDF" = function(rands){
                                                return(
                                                    list("zeta" = dgamma(rands$zeta, shape = 1, rate = 0.01),
                                                        "epsilon" = dgamma(rands$epsilon, shape = 1, rate = 0.01),
                                                        "B" = d_tcauchy(rands$B, location = 0, scale = 10, bound = 1000),
                                                        "j" = d_tcauchy(rands$j, location = 0, scale = 1, bound = 1000),
                                                        "k" = d_tcauchy(rands$k, location = 0, scale = 1, bound = 1000),
                                                        "V_init" = rep(1, times = length(rands$V_init)),
                                                        "var_init" = rep(1, times = length(rands$var_init))))}
        )
        
        model <- model_KS

        tic()
        # for (j in 1:nSamples){
        allPars <- model$samplingRand(nSamples)
        parWeights <- model$samplingCDF(allPars)
        LL <- foreach(j = 1:nSamples, .combine = c) %dopar% {
                    model$pars$zeta <- allPars$zeta[j]
                    model$pars$epsilon <- allPars$epsilon[j]
                    model$pars$B <- allPars$B[j]
                    model$pars$j <- allPars$j[j]
                    model$pars$k <- allPars$k[j]
                    model$pars$V_init <- allPars$V_init[j]
                    model$pars$var_init <- allPars$var_init[j]
                    ll <- likelihood(data,model)
                    ll$negLL
        }
        toc()
        
        # calculate likelihood
        p_x <- (1/LL)
        
        # calculate importance weights for parameter
        q_x <- parWeights$k
        impWeight <- (p_x/q_x) / sum((p_x/q_x),na.rm=TRUE)
        hist(sample(allPars$k, size = 1000, replace = TRUE, prob = impWeight[!is.na(impWeight)]),100,col="gray")

        # B <- seq(from = -100, to = 100, length.out = 200)
        # LL <- NULL
        # for(i in 1:length(B)){
        #     model$pars$B <- B[i]
        #     LL[i] <- likelihood(data,model)
        # }
        # plot(B,exp(-unlist(LL)/90))
        
        # function(nStarts){starts <- rcauchy(nStarts, location = 0, scale = 20)
        # while (!all(abs(starts) <= 100)){
        #     starts[abs(starts) > 100] <- rcauchy(sum(abs(starts) > 100),location = 0, scale = 20)
        # }
        # return(starts)
        # },

        stopCluster(cl)
        