# load relevant packages
library(ggplot2)

# clear workspace
rm(list = ls())

# set version
version <- "v3" # either v2point2, v3, or v4
fileDir <- "~/Documents/Git/jsbandit/data/"

# load file
filename <- paste0(fileDir, "banditData_", version, ".RData")
load(filename)

# restrict ourself to the first trial post-change
eligible.data <- subset(sorted.data, (sorted.data$changeLag >= 1))
#eligible.data$fillColour <- as.factor(eligible.data$fillColour)
#eligible.data$tagText <- as.factor(eligible.data$tagText)

# logistic regression
glm.fit <- glm(filledChosen ~ block + changeLag + block:changeLag + fillColour,
               data = eligible.data,
               family = binomial(link = "logit"))

summary(glm.fit)
