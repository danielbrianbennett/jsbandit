# set file name and directory
base.directory <- "/Users/danielbennett/Google Drive/Works in Progress/JSBANDIT/Bandit/data/Bandit project shared data"
directory.separator <- "/"
file.name <- "banditData_v2point2.RData"

# load data
load(paste(base.directory, file.name, sep = directory.separator))

# restrict ourself to cases where participants chose the filled circle on the first trial post-change
eligible.data <- subset(sorted.data, ( sorted.data$changeLag == 1 & sorted.data$filledChosen == 1))

# calculate a reward change score for each of these cases, and record what happened on the subsequent trial
rcs <- vector(mode = "numeric", length = nrow(eligible.data)) #reward change score 
next.trial <- vector(mode = "numeric", length = nrow(eligible.data)) 
for (i in 1:nrow(eligible.data)){
  
  relevant.trials <- subset(sorted.data, (sorted.data$ID == eligible.data[i,]$ID & sorted.data$block == eligible.data[i,]$block & sorted.data$trial >= eligible.data[i,]$trial - 1 & sorted.data$trial <= eligible.data[i,]$trial + 1 ))
  rcs[i] <- relevant.trials[2,]$pointsWon - relevant.trials[1,]$pointsWon
  next.trial[i] <- relevant.trials[3,]$filledChosen 
  
}

# use logistic regression to estimate probability of staying with filled option as a function of RCS
dat <- data.frame(rcs,next.trial) #put the two variables into a dataframe
rcs.fit <- glm(next.trial ~ rcs, data = dat, family = binomial(link = "logit"))
summary(rcs.fit)

# plot the curve and fit
library(ggplot2)
ggplot(dat, aes(x = rcs, y = next.trial)) + geom_point() + stat_smooth(method = "glm", family = "binomial", se=FALSE) + xlab("Reward Change Score") + ylab("Oddball Choice Probability") +
  theme(axis.text = element_text(size = 14), plot.title = element_text(size = 20), axis.title = element_text(size = 16, face = "bold"))
  

