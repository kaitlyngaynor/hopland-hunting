# Run and export results of top model

# Set-up ------------------------------------------------------------------

# Load packages
library(dplyr)
library(moveHMM)
library(tidyr)

# Load cleaned data
igotu_data <- read.csv("Data/igotu_data_3min_covariates.csv")

# Select columns of interest
igotu_data_fewer <- igotu_data %>% 
    dplyr::select(ID, Party_ID, Longitude, Latitude, DateTime,
                  rugged49.clean, rugged25.clean, rugged9.clean,
                  vegetation.coarser.clean2, view_for_kg_proj,
                  veg.edges.dist.clean, road.dist.clean,
                  grass_120m, chap_120m, wood_120m,
                  Elapsed_Time_Sunrise, Harvest)

# Prep data for HMM
data_hmm <- moveHMM::prepData(igotu_data_fewer, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# Filter out all NA steps
data_hmm <- data_hmm %>% 
    drop_na(step) 

# Filter out all steps > 15mph
data_hmm <- data_hmm %>% 
    filter(step < 1.207008)

# scale covariates
data_hmm$rugged49_scale <- scale(data_hmm$rugged49.clean)
data_hmm$rugged25_scale <- scale(data_hmm$rugged25.clean)
data_hmm$rugged9_scale <- scale(data_hmm$rugged9.clean)
data_hmm$view_scale <- scale(data_hmm$view_for_kg_proj)
data_hmm$vegedge_scale <- scale(data_hmm$veg.edges.dist.clean)
data_hmm$road_scale <- scale(data_hmm$road.dist.clean)
data_hmm$grass_scale <- scale(data_hmm$grass_120m)
data_hmm$chap_scale <- scale(data_hmm$chap_120m)
data_hmm$wood_scale <- scale(data_hmm$wood_120m)
data_hmm$sunrise_scale <- scale(data_hmm$Elapsed_Time_Sunrise)

# set initial parameters (determined by 04b-hmm-3state-parameter-select.Rmd)
mu0_3state <- c(0.01, 0.07, 0.41)
sigma0_3state <- c(0.010, 0.045, 0.204)
zeromass0_3state <- c(0.469, 0.047, 0.0028) 
stepPar0_3state <- c(mu0_3state,sigma0_3state,zeromass0_3state)
angleMean0_3state <- c(pi,0,0)
kappa0_3state <- c(0.61,1.20,1.14)
anglePar0_3state <- c(angleMean0_3state,kappa0_3state)


# Run & explore model -----------------------------------------------------------------

# see vignette https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf
# run full model
# takes about 2.5 hours to run on my system

m <- fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                   formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)

saveRDS(m, "Results/hmm-top-model-2022-05-09.Rds")
# m <- readRDS("Results/hmm-top-model-2022-05-09.Rds") # to read back in


# Export movement data with predicted states ------------------------------

# Add most likely state to data
data_hmm$state <- factor(viterbi(m))
levels(data_hmm$state) <- c("Stationary", "Walking", "Driving") # change factor level names

# Add state probabilities to data
state_probs <- as.data.frame(stationary(m, data_hmm))
names(state_probs) <- c("Stationary_Prob", "Walking_Prob", "Driving_Prob")
data_hmm <- cbind(data_hmm, state_probs)

# Write file
write.csv(data_hmm, "Results/hmm-data-with-model-predictions.csv", row.names = FALSE)
