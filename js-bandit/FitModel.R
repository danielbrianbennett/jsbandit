library(mvtnorm)

source(here::here("helper","HelperFunctions.R"))
source(here::here("models","ModelLikelihood.R"))

A <- CalculateA()
allData <- ExtractData(here::here("raw_data","banditData_v2point2.Rdata"))
pID <- 3

data <- list("block" = allData$block[pID,],
             "trial" = allData$trial[pID,],
             "whichFilled" = allData$whichFilled[pID,],
             "outcome" = allData$outcome[pID,],
             "choice" = allData$choice[pID,],
             "changeLag" = allData$changeLag[pID,])

model <- list()
model$pars$zeta <- 4
model$pars$epsilon <- 10
model$pars$V_init <- 0
model$pars$var_init <- 100
model$pars$B <- 10
model$pars$j <- -0.3
model$pars$k <- -0.3

LL <- likelihood(data,model)
print(LL)

LL <- NULL
for(i in 1:length(B)){
    model$pars$B <- B[i]
    LL[i] <- likelihood(data,model)
}
plot(exp(-unlist(LL)/90))
