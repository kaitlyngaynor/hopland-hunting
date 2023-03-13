# Explore results of the movement model

library(moveHMM)
library(ggplot2)

# Import model and explore results ----------------------------------------

m <- readRDS("Results/hmm-top-model-2023-03-10.Rds")

# model summary
m 

# proportion of time spent in each state
states <- viterbi(m)
prop.table(table(states)) 

# plot model results
plot(m, plotCI = TRUE)

# Save step length & turn angle plots manually 6"x4"
# step-length-raw.pdf and turn-angle-raw.pdf

# look at CIs of parameters
CI(m)

# plot stationary state probabilities
plotStationary(m, plotCI=TRUE)

# compute the pseudo-residuals
pr <- pseudoRes(m)
hist(pr$stepRes)
ks.test(x=pr$stepRes,y='pnorm',alternative='two.sided')
hist(pr$angleRes)
shapiro.test(pr$angleRes)
ks.test(x=pr$angleRes,y='pnorm',alternative='two.sided')
# From K-S test, residuals are NOT normally distributed, BUT we have many points so it's going to be significantly different from normal. Likely good enough.

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(m)

