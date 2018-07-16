# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v3" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/task/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# restrict ourself to the second trial pre-change and the first trial post-change
eligible.data <- subset(sorted.data, (sorted.data$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)
#eligible.data$fillColour <- as.factor(eligible.data$fillColour)
#eligible.data$tagText <- as.factor(eligible.data$tagText)

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)

# restrict ourself to the second trial pre-change and the first trial post-change IN THE FINAL BLOCK
eligible.data <- subset(sorted.data, (sorted.data$block == 1 & sorted.data$changeLag %in% c(-2,1)))
eligible.data$changeLag <- as.factor(eligible.data$changeLag)

# logistic regression
glm.fit <- glm(filledChosen ~changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)


# look at the rate of decay of the effect after it has started
eligible.data <- subset(sorted.data, (sorted.data$changeLag >= 1))

# logistic regression
glm.fit <- glm(filledChosen ~ block*changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)
