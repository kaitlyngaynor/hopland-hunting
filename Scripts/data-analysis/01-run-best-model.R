# Run and export results of top model

# Set-up ------------------------------------------------------------------

# Load packages
library(dplyr)
library(moveHMM)
library(tidyr)
library(vroom)

# Load cleaned data
igotu_data <- vroom::vroom("Data/igotu_data_3min_covariates.csv")

# Prep data for HMM
data_hmm <- moveHMM::prepData(igotu_data, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# Filter out all NA steps & steps > 15mph
data_hmm <- data_hmm %>% 
    tidyr::drop_na(step) %>% 
    filter(step < 1.207008)

# How many removed?
nrow(igotu_data) - nrow(data_hmm) # 2430

# Scale covariates
data_hmm$Ruggedness_scale <- scale(data_hmm$Ruggedness)
data_hmm$Viewshed_scale <- scale(data_hmm$Viewshed)
data_hmm$Road_Distance_scale <- scale(data_hmm$Road_Distance)
data_hmm$Chaparral_120m_scale <- scale(data_hmm$Chaparral_120m)
data_hmm$Woodland_120m_scale <- scale(data_hmm$Woodland_120m)
data_hmm$Sunrise_Scale <- scale(data_hmm$Elapsed_Time_Sunrise)

# Set initial parameters (determined by 05-identify-hmm-parameters.Rmd)
mu0_3state <- c(0.012096380, 0.07132109, 0.397726187)
sigma0_3state <- c(0.008903575, 0.04713105, 0.187850100)
zeromass0_3state <- c(0.502977239, 0.04650203, 0.002483578) 
stepPar0_3state <- c(mu0_3state, sigma0_3state, zeromass0_3state)
angleMean0_3state <- c(pi, 0, 0)
kappa0_3state <- c(0.6481289, 1.088494664, 1.13892960)
anglePar0_3state <- c(angleMean0_3state, kappa0_3state)


# Run & explore model -----------------------------------------------------------------

# see vignette https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf
# run full model
m <- moveHMM::fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                   formula = ~Road_Distance_scale + Viewshed_scale + Woodland_120m_scale + Ruggedness_scale + Chaparral_120m_scale + Sunrise_Scale)

saveRDS(m, "Results/hmm-top-model-2023-03-10.Rds")
# m <- readRDS("Results/hmm-top-model-2023-03-10.Rds") # to read back in


# Export movement data with predicted states ------------------------------

# Add most likely state to data
data_hmm$state <- factor(viterbi(m))
levels(data_hmm$state) <- c("Stationary", "Walking", "Driving") # change factor level names

# Add state probabilities to data
state_probs <- as.data.frame(stationary(m, data_hmm))
names(state_probs) <- c("Stationary_Prob", "Walking_Prob", "Driving_Prob")
data_hmm <- cbind(data_hmm, state_probs)

# Write file
write.csv(data_hmm, "Results/hmm-top-model-2023-03-10.csv", row.names = FALSE)
