library(lsr)
load("sortedData.RData")

# exclude the weird subjects
sorted.data <- sorted.data[ sorted.data$ID != 51460628, ]
sorted.data <- sorted.data[ sorted.data$ID != 58600059, ]

# get a (50 trials) x (20 subject) matrix indicating whether filled was chosen, for a specific block
getData <- function(blocknum){
  block1 <- sorted.data[sorted.data$block==blocknum,c("ID","trial","filledChosen")]
  data1 <- t(as.matrix(longToWide(block1, filledChosen ~ trial)[,-1]))
  rownames(data1) <- paste0("trial",1:30)
  return(data1)
}
  
# count the number of cases in which, for the data in d, a participant chose the filled circle
# "lag" trials after the change occurs
nFilledChoices <- function(lag,d){ 
  sum(apply(d,2,function(x){ x[!is.na(x)][lag] } ),na.rm=TRUE) 
}

# number of cases where there was actually data at this "lag" (e.g. largest lag if change occured 
# on trial 48 is 2)
casesFilledChoices <- function(lag,d){ 
  sum(apply(d,2,function(x){ x[!is.na(x)][lag] } ),na.rm=TRUE) +
    sum(apply(d,2,function(x){ 1-x[!is.na(x)][lag] } ),na.rm=TRUE) 
}

# proportion of cases where the filled option was chosen
propFilledChoices <- function(lag,d){ 
  mean(apply(d,2,function(x){ x[!is.na(x)][lag] } ),na.rm = TRUE) 
}


# produce matrices showing counts and proportion of choices broken down
# by block number and lag
nTrials <- 10
nBlocks <- 3
counts <- matrix(NA,nTrials,nBlocks)
ncases <- matrix(NA,nTrials,nBlocks) 
proportions <- matrix(NA,nTrials,nBlocks)
for( b in 1:nBlocks) {
  d <- getData(b)
  for( t in 1:nTrials) {
    counts[t,b] <- nFilledChoices(t,d)
    ncases[t,b] <- casesFilledChoices(t,d)
    proportions[t,b] <- propFilledChoices(t,d)
  }
}
proportions <- round(proportions*100)
dimnames(proportions) = list(lag=paste0("lag",1:nTrials),block=paste0("block",1:nBlocks))
