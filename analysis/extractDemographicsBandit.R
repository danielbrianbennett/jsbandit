######## Experiment 1 ##########

# clear workspace
rm(list = ls())

# set version
version <- "v2point2" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

participantIDs <- unique(unsorted.data$ID)
nParticipants <- length(participantIDs)

participantComplete <- vector(mode="logical",length=nParticipants)
nTrials <- vector(mode="numeric",length=nParticipants)
gender <- vector(mode="character",length=nParticipants)
language <- vector(mode="character",length=nParticipants)
age <- vector(mode="numeric",length=nParticipants)
country <- vector(mode="character",length=nParticipants)
nInstruct <- vector(mode="numeric",length=nParticipants)


for (i in 1:nParticipants){
  
  tempData <- subset(unsorted.data, unsorted.data$ID == participantIDs[i])
  63890470
  nTrials[i] <- dim(tempData)[1] 
  participantComplete[i] <- nTrials[i] == 90
  
  gender[i] <- tempData[1,]$gender
  language[i] <- tempData[1,]$language
  age[i] <- tempData[1,]$age
  country[i] <- tempData[1,]$country
  nInstruct[i] <- nchar(tempData[1,]$instructionCheckScore[1]) / 3
  
  
}

# make data frame
demographics <- data.frame(cbind(participantComplete,as.numeric(as.character(nTrials)),gender,language,as.numeric(as.character(age)),country,as.numeric(as.character(nInstruct))))
colnames(demographics) <- c("participantComplete","nTrials","gender","language","age","country","nInstruct")
# remove incomplete participants
filtered.demo <- subset(demographics,demographics$participantComplete == TRUE)

# display important demographics
table(filtered.demo$gender)
table(filtered.demo$language)
table(filtered.demo$country)

filtered.demo$age <- as.numeric(as.character(filtered.demo$age))
filtered.demo$nInstruct <- as.numeric(as.character(filtered.demo$nInstruct))

######## Experiment 2 ##########

# clear workspace
rm(list = ls())

# set version
version <- "v3" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

participantIDs <- unique(unsorted.data$ID)
nParticipants <- length(participantIDs)

participantComplete <- vector(mode="logical",length=nParticipants)
nTrials <- vector(mode="numeric",length=nParticipants)
gender <- vector(mode="character",length=nParticipants)
language <- vector(mode="character",length=nParticipants)
age <- vector(mode="numeric",length=nParticipants)
country <- vector(mode="character",length=nParticipants)
nInstruct <- vector(mode="numeric",length=nParticipants)


for (i in 1:nParticipants){
  
  tempData <- subset(unsorted.data, unsorted.data$ID == participantIDs[i])
  63890470
  nTrials[i] <- dim(tempData)[1] 
  participantComplete[i] <- nTrials[i] == 90
  
  gender[i] <- tempData[1,]$gender
  language[i] <- tempData[1,]$language
  age[i] <- tempData[1,]$age
  country[i] <- tempData[1,]$country
  nInstruct[i] <- nchar(tempData[1,]$instructionCheckScore[1]) / 3
  
  
}

# make data frame
demographics <- data.frame(cbind(participantComplete,as.numeric(as.character(nTrials)),gender,language,as.numeric(as.character(age)),country,as.numeric(as.character(nInstruct))))
colnames(demographics) <- c("participantComplete","nTrials","gender","language","age","country","nInstruct")
# remove incomplete participants
filtered.demo <- subset(demographics,demographics$participantComplete == TRUE)

# display important demographics
table(filtered.demo$gender)
table(filtered.demo$language)
table(filtered.demo$country)

filtered.demo$age <- as.numeric(as.character(filtered.demo$age))
filtered.demo$nInstruct <- as.numeric(as.character(filtered.demo$nInstruct))