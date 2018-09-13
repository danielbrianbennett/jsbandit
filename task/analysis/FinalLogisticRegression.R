###### Pre-screen for experiment 1 #######

# clear workspace
rm(list = ls())

# set version
version <- "v2point2" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/task/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)
sorted.data.v2point2 <- sorted.data

# load list of filtered IDs
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

##### DO ANALYSIS #####

# retain only participants with an ID in the white-list
sorted.data.v2point2 <- sorted.data.v2point2[sorted.data.v2point2$ID %in% filtered.IDs,]

# calculate average points per trial
meanPointsWon_v2point2 <- as.vector(by(sorted.data.v2point2$pointsWon, INDICES = sorted.data.v2point2$ID, FUN = mean))
asymptote_v2point2 <- as.vector(by(sorted.data.v2point2[sorted.data.v2point2$trial >= 25,]$pointsWon, INDICES = sorted.data.v2point2[sorted.data.v2point2$trial >= 25,]$ID, FUN = mean))

# restrict ourself to the second trial pre-change and the first trial post-change
eligible.data <- subset(sorted.data.v2point2, (sorted.data.v2point2$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
#eligible.data$fillColour <- as.factor(eligible.data$fillColour)
#eligible.data$tagText <- as.factor(eligible.data$tagText)

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change IN THE FINAL BLOCK
eligible.data <- subset(sorted.data.v2point2, (sorted.data.v2point2$block == 3 & sorted.data.v2point2$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)

# logistic regression
glm.fit <- glm(filledChosen ~changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


# look at the rate of decay of the effect after it has started
eligible.data <- subset(sorted.data.v2point2, (sorted.data.v2point2$changeLag >= 1))

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

###### Pre-screen for experiment 2 #######


# set version
version <- "v3" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/task/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)
sorted.data.v3 <- sorted.data

# load list of filtered IDs
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

##### DO ANALYSIS #####

# retain only participants with an ID in the white-list
sorted.data.v3 <- sorted.data.v3[sorted.data.v3$ID %in% filtered.IDs,]

# calculate average points per trial
meanPointsWon_v3 <- as.vector(by(sorted.data.v3$pointsWon, INDICES = sorted.data.v3$ID, FUN = mean))
asymptote_v3 <- as.vector(by(sorted.data.v3[sorted.data.v3$trial >= 25,]$pointsWon, INDICES = sorted.data.v3[sorted.data.v3$trial >= 25,]$ID, FUN = mean))

# restrict ourself to the second trial pre-change and the first trial post-change
eligible.data <- subset(sorted.data.v3, (sorted.data.v3$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
#eligible.data$fillColour <- as.factor(eligible.data$fillColour)
#eligible.data$tagText <- as.factor(eligible.data$tagText)

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag + C(fillColour, contr = contr.sum, how.many = 1),
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change IN THE FINAL BLOCK
eligible.data <- subset(sorted.data.v3, (sorted.data.v3$block == 3 & sorted.data.v3$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)

# logistic regression
glm.fit <- glm(filledChosen ~changeLag + C(fillColour, contr = contr.sum, how.many = 1),
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


# look at the rate of decay of the effect after it has started
eligible.data <- subset(sorted.data.v3, (sorted.data.v3$changeLag >= 1))

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag + C(fillColour, contr = contr.sum, how.many = 1),
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


###### Combined Exp 1 and Exp 2 #######
sorted.data.v2point2$fillColour <- NA
sorted.data.v2point2$exp <- 1
sorted.data.v3$exp <- 2
all.data <- rbind(sorted.data.v2point2,sorted.data.v3)
all.data$exp <- as.factor(all.data$exp)

eligible.data <- subset(all.data, (all.data$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
#eligible.data$fillColour <- as.factor(eligible.data$fillColour)
#eligible.data$tagText <- as.factor(eligible.data$tagText)

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag*exp,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change IN THE FINAL BLOCK
eligible.data <- subset(all.data, (all.data$block == 3 & all.data$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)

# logistic regression
glm.fit <- glm(filledChosen ~changeLag*exp,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


# look at the rate of decay of the effect after it has started
eligible.data <- subset(all.data, (all.data$changeLag >= 1))

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag*exp,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

###### Pre-screen for experiment 4 #######

# set version
version <- "v4" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/task/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)
sorted.data.v4 <- sorted.data

# load list of filtered IDs
filename <- paste0(fileDir, "filteredIDs_", version, ".RData")
load(filename)

##### DO ANALYSIS #####

# retain only participants with an ID in the white-list
sorted.data.v4 <- sorted.data.v4[sorted.data.v4$ID %in% filtered.IDs,]
sorted.data.v4$tagText <- as.factor(sorted.data.v4$tagText)

# calculate average points per trial
meanPointsWon_v4 <- as.vector(by(sorted.data.v4$pointsWon, INDICES = sorted.data.v4$ID, FUN = mean))
asymptote_v4 <- as.vector(by(sorted.data.v4[sorted.data.v4$trial >= 25,]$pointsWon, INDICES = sorted.data.v4[sorted.data.v4$trial >= 25,]$ID, FUN = mean))

# restrict ourself to the second trial pre-change and the first trial post-change relative to 'bad'
eligible.data <- subset(sorted.data.v4, (sorted.data.v4$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
eligible.data <- within(eligible.data, tagText <- relevel(tagText, ref = 2))

# logistic regression
glm.fit <- glm(filledChosen ~ block + changeLag*tagText,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change relative to '????'
eligible.data <- subset(sorted.data.v4, (sorted.data.v4$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
eligible.data <- within(eligible.data, tagText <- relevel(tagText, ref = 1))

# logistic regression
glm.fit <- glm(filledChosen ~ block + changeLag*tagText,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change IN THE FINAL BLOCK relative to bad
eligible.data <- subset(sorted.data.v4, (sorted.data.v4$block == 3 & sorted.data.v4$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
eligible.data <- within(eligible.data, tagText <- relevel(tagText, ref = 2)) 

# logistic regression
glm.fit <- glm(filledChosen ~changeLag + tagText,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


# look at the rate of decay of the effect after it has started
eligible.data <- subset(sorted.data.v4, (sorted.data.v4$changeLag >= 1))
eligible.data <- within(eligible.data, tagText <- relevel(tagText, ref = 2)) 

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag + tagText,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)
