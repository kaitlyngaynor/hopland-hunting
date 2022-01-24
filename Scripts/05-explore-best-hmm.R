# Explore the top HMM model

library(dplyr)
library(moveHMM)
library(tidyr)
library(ggplot2)


# Set-up ------------------------------------------------------------------

# Load cleaned data
igotu_data <- read.csv("Data/igotu_data_3min_covariates.csv")

# Select columns of interest
igotu_data_fewer <- igotu_data %>% 
    dplyr::select(ID, Party_ID, Longitude, Latitude, DateTime,
                  rugged49.clean, rugged25.clean, rugged9.clean,
                  hq_dist, vegetation.coarser.clean2, view,
                  veg.edges.dist.clean, road.dist.clean,
                  grass_120m, chap_120m, wood_120m)

# prep data for HMM
data_hmm <- moveHMM::prepData(igotu_data_fewer, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# remove 300 step lengths of 'NA'
data_hmm <- data_hmm %>% 
    drop_na(step) 

# filter out all steps > 15mph
data_hmm <- data_hmm %>% 
    filter(step < 1.207008)

# scale covariates
data_hmm$rugged49_scale <- scale(data_hmm$rugged49.clean)
data_hmm$rugged25_scale <- scale(data_hmm$rugged25.clean)
data_hmm$rugged9_scale <- scale(data_hmm$rugged9.clean)
data_hmm$hq_scale <- scale(data_hmm$hq_dist)
data_hmm$view_scale <- scale(data_hmm$view)
data_hmm$vegedge_scale <- scale(data_hmm$veg.edges.dist.clean)
data_hmm$road_scale <- scale(data_hmm$road.dist.clean)
data_hmm$grass_scale <- scale(data_hmm$grass_120m)
data_hmm$chap_scale <- scale(data_hmm$chap_120m)
data_hmm$wood_scale <- scale(data_hmm$wood_120m)

# set initial parameters (determined by 04b-hmm-3state-parameter-select.Rmd)
mu0_3state <- c(0.01, 0.07, 0.41)
sigma0_3state <- c(0.010, 0.045, 0.204)
zeromass0_3state <- c(0.469, 0.047, 0.0028) 
stepPar0_3state <- c(mu0_3state,sigma0_3state,zeromass0_3state)
angleMean0_3state <- c(pi,0,0)
kappa0_3state <- c(0.61,1.20,1.14)
anglePar0_3state <- c(angleMean0_3state,kappa0_3state)


# Model -------------------------------------------------------------------

# explore top model (which was the full model), as determined in 04-hmm-3state.Rmd
start <- Sys.time()
fit <- fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                   formula = ~road.dist.clean + hq_scale + view_scale + wood_scale + rugged9_scale + chap_scale + vegedge_scale)
end <- Sys.time()
end-start

# model summary
fit 
