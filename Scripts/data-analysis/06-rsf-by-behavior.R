library(dplyr)
library(jtools)

# Bring in datasets
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0
used <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-02-17.csv")
used$Used <- 1

head(available)
head(used)

# Join into single dataframe and scale covariates
used_avail <- dplyr::bind_rows(used, available) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m, HQ_Distance,
                  Used, Harvest, state, state_2stationary) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m),
                  HQ_Distance_scale = scale(HQ_Distance))

# bring in clusters
clusters <- read.csv("Results/hunters_by_cluster_with_success.csv") %>% 
    dplyr::select(ID, Cluster4)
used_avail <- dplyr::left_join(used_avail, clusters)

# Split by cluster
used_avail_drivers <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Waiters") | Used == 0)
used_avail_walkers <- used_avail %>% 
    dplyr::filter((Cluster4 != "Drivers" & Cluster4 != "Waiters") | Used == 0)
used_avail_waiters <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Drivers") | Used == 0)

# Filter by cluster & success
used_avail_drivers_success <- used_avail_drivers %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_walkers_success <- used_avail_walkers %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_waiters_success <- used_avail_waiters %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_drivers_unsuccess <- used_avail_drivers %>% dplyr::filter(Harvest == "N" | Used == 0)
used_avail_walkers_unsuccess <- used_avail_walkers %>% dplyr::filter(Harvest == "N" | Used == 0)
used_avail_waiters_unsuccess <- used_avail_waiters %>% dplyr::filter(Harvest == "N" | Used == 0)

fit_drivers_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_drivers_success,
                   family = binomial) 
fit_walkers_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_walkers_success,
                   family = binomial) 
fit_waiters_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_waiters_success,
                   family = binomial) 
fit_drivers_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_drivers_unsuccess,
                           family = binomial) 
fit_walkers_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_walkers_unsuccess,
                           family = binomial) 
fit_waiters_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_waiters_unsuccess,
                           family = binomial) 

jtools::plot_summs(fit_drivers_success, fit_drivers_unsuccess, 
                   fit_walkers_success, fit_walkers_unsuccess, 
                   fit_waiters_success, fit_waiters_unsuccess,
                   model.names = c("Drivers - Success", "Drivers - Not",
                                   "Walkers - Success", "Walkers - Not",
                                   "Waiters - Success", "Waiters - Not"))


# Export for plotting

# Drivers - success
drivers_success_coef <- fit_drivers_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
drivers_success_ci <- confint(fit_drivers_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
drivers_success_results <- dplyr::left_join(drivers_success_coef, drivers_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Drivers",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Drivers - no success
drivers_unsuccess_coef <- fit_drivers_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
drivers_unsuccess_ci <- confint(fit_drivers_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
drivers_unsuccess_results <- dplyr::left_join(drivers_unsuccess_coef, drivers_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Drivers",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Walkers - success
walkers_success_coef <- fit_walkers_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
walkers_success_ci <- confint(fit_walkers_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
walkers_success_results <- dplyr::left_join(walkers_success_coef, walkers_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Walkers",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Walkers - no success
walkers_unsuccess_coef <- fit_walkers_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
walkers_unsuccess_ci <- confint(fit_walkers_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
walkers_unsuccess_results <- dplyr::left_join(walkers_unsuccess_coef, walkers_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Walkers",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Waiters - success
waiters_success_coef <- fit_waiters_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
waiters_success_ci <- confint(fit_waiters_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
waiters_success_results <- dplyr::left_join(waiters_success_coef, waiters_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Waiters",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Waiters - no success
waiters_unsuccess_coef <- fit_waiters_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
waiters_unsuccess_ci <- confint(fit_waiters_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
waiters_unsuccess_results <- dplyr::left_join(waiters_unsuccess_coef, waiters_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Waiters",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Combine into a single dataframe for exporting
all_rsf_results <- dplyr::bind_rows(drivers_success_results, drivers_unsuccess_results,
                                    walkers_success_results, walkers_unsuccess_results,
                                    waiters_success_results, waiters_unsuccess_results)
write.csv(all_rsf_results, "Results/rsf-results-by-mode-success.csv", row.names = F)



















