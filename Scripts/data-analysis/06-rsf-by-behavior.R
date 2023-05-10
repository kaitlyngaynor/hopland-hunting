library(dplyr)
library(jtools)
library(lme4)

# Bring in datasets
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points (30 min)
used <- read.csv("Data/igotu_data_30min_covariates.csv")
used$Used <- 1

head(available)
head(used)

# Randomly select 100 points for each available point
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
used_avail <- dplyr::bind_rows(used, available_100) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m, HQ_Distance,
                  Used) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m),
                  HQ_Distance_scale = scale(HQ_Distance))

# bring in clusters
clusters <- read.csv("Results/hunter_cluster_success_long.csv") %>%
    tidyr::pivot_wider(names_from = "State_4state", values_from = "Percentage") %>% 
    dplyr::select(ID, Cluster4, Harvest)
used_avail <- dplyr::left_join(used_avail, clusters)
used_avail$ID <- as.character(used_avail$ID)

# Split by cluster
used_avail_coursing <- used_avail %>% 
    dplyr::filter((Cluster4 != "Stalking" & Cluster4 != "Sit-and-wait"))
used_avail_stalking <- used_avail %>% 
    dplyr::filter((Cluster4 != "Coursing" & Cluster4 != "Sit-and-wait"))
used_avail_sitandwait <- used_avail %>% 
    dplyr::filter((Cluster4 != "Stalking" & Cluster4 != "Coursing"))

# Filter by cluster & success
used_avail_coursing_success <- used_avail_coursing %>% dplyr::filter(Harvest == "Y")
used_avail_stalking_success <- used_avail_stalking %>% dplyr::filter(Harvest == "Y")
used_avail_sitandwait_success <- used_avail_sitandwait %>% dplyr::filter(Harvest == "Y")
used_avail_coursing_unsuccess <- used_avail_coursing %>% dplyr::filter(Harvest == "N")
used_avail_stalking_unsuccess <- used_avail_stalking %>% dplyr::filter(Harvest == "N")
used_avail_sitandwait_unsuccess <- used_avail_sitandwait %>% dplyr::filter(Harvest == "N")

fit_coursing_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_coursing_success,
                   family = binomial) 
fit_stalking_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_stalking_success,
                   family = binomial) 
fit_sitandwait_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_sitandwait_success,
                   family = binomial) 
fit_coursing_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_coursing_unsuccess,
                           family = binomial) 
fit_stalking_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_stalking_unsuccess,
                           family = binomial) 
fit_sitandwait_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_sitandwait_unsuccess,
                           family = binomial) 

# Plotting all model output together - takes quite a while to run
# jtools::plot_summs(fit_coursing_success, fit_coursing_unsuccess, 
#                    fit_stalking_success, fit_stalking_unsuccess, 
#                    fit_sitandwait_success, fit_sitandwait_unsuccess,
#                    model.names = c("Coursing - Success", "Coursing - Not",
#                                    "Stalking - Success", "Stalking - Not",
#                                    "Sit-and-wait - Success", "Sit-and-wait - Not"))


# Export for plotting

# Coursing - success
coursing_success_coef <- fit_coursing_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
coursing_success_ci <- confint(fit_coursing_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
coursing_success_results <- dplyr::left_join(coursing_success_coef, coursing_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Coursing",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Coursing - no success
coursing_unsuccess_coef <- fit_coursing_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
coursing_unsuccess_ci <- confint(fit_coursing_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
coursing_unsuccess_results <- dplyr::left_join(coursing_unsuccess_coef, coursing_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Coursing",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Stalking - success
stalking_success_coef <- fit_stalking_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
stalking_success_ci <- confint(fit_stalking_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
stalking_success_results <- dplyr::left_join(stalking_success_coef, stalking_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Stalking",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Stalking - no success
stalking_unsuccess_coef <- fit_stalking_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
stalking_unsuccess_ci <- confint(fit_stalking_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
stalking_unsuccess_results <- dplyr::left_join(stalking_unsuccess_coef, stalking_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Stalking",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Sit-and-wait - success
sitandwait_success_coef <- fit_sitandwait_success$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
sitandwait_success_ci <- confint(fit_sitandwait_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
sitandwait_success_results <- dplyr::left_join(sitandwait_success_coef, sitandwait_success_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Sit-and-wait",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Sit-and-wait - no success
sitandwait_unsuccess_coef <- fit_sitandwait_unsuccess$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
sitandwait_unsuccess_ci <- confint(fit_sitandwait_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
sitandwait_unsuccess_results <- dplyr::left_join(sitandwait_unsuccess_coef, sitandwait_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                  `Hunting Mode` = "Sit-and-wait",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Combine into a single dataframe for exporting
all_rsf_results <- dplyr::bind_rows(coursing_success_results, coursing_unsuccess_results,
                                    stalking_success_results, stalking_unsuccess_results,
                                    sitandwait_success_results, sitandwait_unsuccess_results)
write.csv(all_rsf_results, "Results/rsf-results-by-mode-success-30min.csv", row.names = F)
    