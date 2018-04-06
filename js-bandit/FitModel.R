#### Preliminaries ####

require(mvtnorm)
require(stats4)
require(tictoc)

source(here::here("helper","HelperFunctions.R"))
source(here::here("models","ModelLikelihood.R"))

A <- CalculateA()
allData <- ExtractData(here::here("raw_data","banditData_v2point2.Rdata"))
pID <- 15

data <- list("block" = allData$block[pID,],
             "trial" = allData$trial[pID,],
             "whichFilled" = allData$whichFilled[pID,],
             "outcome" = allData$outcome[pID,],
             "choice" = allData$choice[pID,],
             "changeLag" = allData$changeLag[pID,])



#### Specify model ####
V_init_constant <- 0
var_init_constant <- 1000

model_KS <- list("pars" = list("zeta" = NA,
                               "epsilon" = NA,
                               "B" = NA,
                               "j" = NA,
                               "k" = NA),
                 "lowerBound" =  c(-100,-Inf,-Inf),#c(0,0,-100,-Inf,-Inf),
                 "upperBound" = c(100,Inf,Inf),#c(100,100,100,Inf,Inf),
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
                     return(list(#"zeta" = runif(nStarts, min = 0, max = 10),
                                 # "epsilon" = runif(nStarts, min = 0, max = 10),
                                 "B" = rnorm(nStarts, mean = 0, sd = 20),
                                 "j" = rnorm(nStarts, mean = 0, sd = 1),
                                 "k" = rnorm(nStarts, mean = 0, sd = 1)))},
                 "fixed" = list("zeta" = 8,
                                "epsilon" = 3,
                                "V_init" = V_init_constant,
                                "var_init" = var_init_constant)
)

model <- model_KS
fitData <- data

# fit model
tic()
fit <- mle(model$lambda,
           method = "L-BFGS-B",
           start = model$startPoints(),
           lower = model$lowerBound,
           upper = model$upperBound,
           fixed = model$fixed)
toc()

# assign parameters to model for testing
for (n in 1:length(names(fit@fullcoef))){
    model$pars[names(fit@fullcoef)[n]] <- fit@fullcoef[names(fit@fullcoef)[n]]
}
fitLL <- likelihood(data,model)


B <- seq(from = -100, to = 100, length.out = 200)
LL <- NULL
for(i in 1:length(B)){
    model$pars$B <- B[i]
    LL[i] <- likelihood(data,model)
}
plot(B,exp(-unlist(LL)/90))
