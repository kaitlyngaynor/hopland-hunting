# Bayesian RSF

# Load libraries
library(jagsUI) 
library(dplyr)
library(MCMCvis)
library(tidyr)


# A) Prepare data ---------------------------------------------------------

# Bring in all available points in study area
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points
used <- read.csv("Data/igotu_data_30min_covariates.csv")
used$Used <- 1

# Randomly select 100 points for each available point
set.seed(123)
counts <- count(used, ID)
counts$n100 <- counts$n * 100
available_dfs <- list()
for(i in 1:nrow(counts)) {
    available100 <- dplyr::sample_n(available, counts$n100[i]) 
    available100$ID <- counts$ID[i]
    available_dfs[[i]] <- available100
}
available_100 <- dplyr::bind_rows(available_dfs)

# Join into single dataframe and scale covariates
used_available <- dplyr::bind_rows(used, available_100) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m,
                  Used) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m)) %>% 
    drop_na() # remove points with missing covariates

# Bring in clusters
clusters <- read.csv("for-publication/hunter_cluster_success_long_nocov.csv") %>%
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
    dplyr::select(ID, Cluster, Harvest) %>% 
    dplyr::mutate(Cluster_Harvest = paste(Cluster, Harvest, sep = "_"))
used_available <- dplyr::left_join(used_available, clusters)
used_available$ID <- as.character(used_available$ID)

# Create numerical indices for id, period, id_full
used_available$ID_index <- as.numeric(as.factor(used_available$ID))
used_available$Cluster_index <- as.numeric(as.factor(used_available$Cluster_Harvest))

# Create input data for model
jags_data_input <- 
    list(USED = used_available$Used,
         
         # ID index associated with each location
         ID = used_available$ID_index,
         CLUSTER_INDEX = used_available$Cluster_index,

         # Covariate values for each used-available location
         RUGGEDNESS = as.numeric(used_available$Ruggedness_scale),
         VIEWSHED = as.numeric(used_available$Viewshed_scale),
         ROAD = as.numeric(used_available$Road_Distance_scale),
         CHAPARRAL = as.numeric(used_available$Chaparral_120m_scale),
         WOODLAND = as.numeric(used_available$Woodland_120m_scale),
         
         # Sample sizes to use for looping
         N_OBS = nrow(used_available),
         N_IND = max(used_available$ID_index),
         N_CLUSTER = max(used_available$Cluster_index)
         )

# Specify parameters to save
jags_parameters_output <- c("b_intercept",
                            "b_ruggedness", "b_viewshed", "b_road",
                            "b_chaparral", "b_woodland",
                            "mu_ruggedness", "mu_viewshed", "mu_road",
                            "mu_chaparral", "mu_woodland"
                           )


# B) Specify model --------------------------------------------------------

# See separate rsf-model.txt file

# C) Run model ------------------------------------------------------------

model_out <- jagsUI::jags(
    model.file = "for-publication/rsf-model.txt",
    data = jags_data_input,
    parameters.to.save = jags_parameters_output,
    inits = NULL,
    n.chains = 3,
    n.iter = 10000,
    n.burnin = 1000,
    n.thin = 3,
    parallel = T
)

# See if any parameters are failing to converge (R-hat > 1.1)
(Rhat1.1 <- model_out$summary  %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    dplyr::filter(Rhat > 1.1))


# D) Tidy model output ----------------------------------------------------

res_sum <- model_out$summary %>%
	as.data.frame() %>%
	tibble::rownames_to_column() %>%
	tibble::as_tibble()

names(res_sum) <- c("variable", "mean", "sd", "LCI", "Q1", "Q2", "Q3", "UCI", "Rhat", "n.eff", "significant", "f")

# E) Plot posteriors ------------------------------------------------------

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "mu_ruggedness"
    ),
    excl = "deviance")

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "mu_viewshed"
    ),
    excl = "deviance")

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "mu_road"
    ),
    excl = "deviance")

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "mu_chaparral"
    ),
    excl = "deviance")

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "mu_woodland"
    ),
    excl = "deviance")

MCMCvis::MCMCplot(
    model_out, 
    params = c(
        "b_intercept"
    ),
    excl = "deviance")
