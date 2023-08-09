# 01. Select the starting parameters for the 3-state HMM

# Outlined here: https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf

library(dplyr)
library(moveHMM)
library(tidyr)
library(ggplot2)
library(parallel)

# Load cleaned data
igotu_data <- vroom::vroom("igotu_data_3min_covariates.csv")

# Prep data for HMM
data_hmm <- moveHMM::prepData(igotu_data, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# Filter out all NA steps & steps > 15mph
data_hmm <- data_hmm %>% 
    tidyr::drop_na(step) %>% 
    filter(step < 1.207008)

# Scale covariates
data_hmm$Ruggedness_scale <- scale(data_hmm$Ruggedness)
data_hmm$Viewshed_scale <- scale(data_hmm$Viewshed)
data_hmm$Road_Distance_scale <- scale(data_hmm$Road_Distance)
data_hmm$Chaparral_120m_scale <- scale(data_hmm$Chaparral_120m)
data_hmm$Woodland_120m_scale <- scale(data_hmm$Woodland_120m)
data_hmm$Sunrise_Scale <- scale(data_hmm$Elapsed_Time_Sunrise)

# Look at step lengths and turn angles
hist(data_hmm$step)
hist(data_hmm$angle)

# determine proportion of step lengths equal to 0
whichzero <- which(data_hmm$step == 0)
length(whichzero)/nrow(data_hmm)

# Create cluster of size ncores
ncores <- detectCores() - 1
cl <- makeCluster(getOption("cl.cores", ncores))

# Export objects needed in parallelised function to cluster 
clusterExport(cl, list("data_hmm", "fitHMM"))

# Number of tries with different starting values
niter <- 100
# Create list of starting values
allPar0 <- lapply(as.list(1:niter), function(x) { 
    # Step length mean
    stepMean0 <- runif(3, min = c(0.0001, 0.05, 0.3), max = c(0.05, 0.3, 1.4))
    # Step length standard deviation
    stepSD0 <- runif(3, min = c(0.0001, 0.05, 0.3), max = c(0.05, 0.3, 1.4))
    # Step length zero mass
    stepZeromass0 <- runif(3, min = 0, max = 1)
    # Turning angle mean
    angleMean0 <- c(0, 0, 0)
    # Turning angle concentration
    angleCon0 <- runif(3, min = c(0.1, 0.5, 1), max = c(0.5, 1, 5))
    # Return vectors of starting values
    stepPar0 <- c(stepMean0, stepSD0, stepZeromass0)
    anglePar0 <- c(angleMean0, angleCon0)
    
    return(list(step = stepPar0, angle = anglePar0))
})

# Fit the niter models in parallel
allm_parallel <- parLapply(cl = cl, X = allPar0, fun = function(par0) { m <- fitHMM(data = data_hmm, nbStates = 3, stepPar0 = par0$step, anglePar0 = par0$angle, formula = ~Road_Distance_scale + Viewshed_scale + Woodland_120m_scale + Ruggedness_scale + Chaparral_120m_scale + Elapsed_Time_Sunrise)
return(m) })

# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm_parallel, function(m) m$mod$minimum))
allnllk

# The model converged on the same ending parameter values in 82 of the 100 iterations, indicating numerical stability

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
mbest <- allm_parallel[[whichbest]]
mbest
plot(mbest)

# This best model was as follows:
#     
#     Value of the maximum log-likelihood: -74268.87 
# 
# Step length parameters:
#     ----------------------
#     state 1     state 2     state 3
# mean      0.07132109 0.397726187 0.012096380
# sd        0.04713105 0.187850100 0.008903575
# zero-mass 0.04650203 0.002483578 0.502977239
# 
# Turning angle parameters:
#     ------------------------
#     state 1     state 2   state 3
# mean          -0.005414194 -0.01503212 3.1347269
# concentration  1.088494664  1.13892960 0.6481289
# 
# Regression coeffs for the transition probabilities:
#     --------------------------------------------------
#     1 -> 2      1 -> 3      2 -> 1       2 -> 3       3 -> 1       3 -> 2
# intercept            -3.73727460 -2.01175759 -1.48526452 -1.970712949 -2.287434290 -2.913865044
# Road_Distance_scale  -5.53305422 -0.29221567  1.64055197  1.449443518  0.077904367 -1.514322443
# Viewshed_scale       -0.10005749 -0.03964951  0.07089100  0.072791448 -0.067497328 -0.089402038
# Woodland_120m_scale  -0.14717579 -0.11004250 -0.10422348 -0.005319889 -0.029792695 -0.046015022
# Ruggedness_scale     -0.04587131  0.04341439  0.12588883  0.103382671  0.089177872 -0.028920984
# Chaparral_120m_scale -0.07236707 -0.01904208  0.02053091 -0.001728969  0.005371018 -0.089467525
# Elapsed_Time_Sunrise  0.07641253  0.01655358 -0.05040429 -0.058987453 -0.018479451  0.006556806
# 
# Initial distribution:
#     --------------------
#     [1] 0.07229012 0.75694862 0.17076126
# 