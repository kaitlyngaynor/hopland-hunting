# this was an effort to keep the same ratio of used:available points for all 6 models
# turns out it doesn't matter

# 05. Resource Selection Functions to assess habitat selection for each hunting mode

library(dplyr)
library(jtools)
library(lme4)

# Bring in all available points in study area
available <- read.csv("all-available-point-cov.csv")
available$Used <- 0
available$w <- 5000

# Bring in used points
used <- read.csv("igotu_data_3min_covariates.csv")
used$Used <- 1
used$w <- 1

# Join into single dataframe and scale covariates
used_avail <- dplyr::bind_rows(available, used) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m,
                  Used, w) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m))

# Bring in clusters
clusters <- read.csv("hunter_cluster_success_long.csv") %>%
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
    dplyr::select(ID, Cluster, Harvest)
used_avail <- dplyr::left_join(used_avail, clusters)
used_avail$ID <- as.character(used_avail$ID)

# Split by cluster
used_coursing <- used_avail %>% 
    dplyr::filter((Cluster != "Stalking" & Cluster != "Sit-and-wait"))
used_stalking <- used_avail %>% 
    dplyr::filter((Cluster != "Coursing" & Cluster != "Sit-and-wait"))
used_sitandwait <- used_avail %>% 
    dplyr::filter((Cluster != "Stalking" & Cluster != "Coursing"))

# Filter by cluster & success
used_coursing_success <- used_coursing %>% dplyr::filter(Harvest == "Y")
used_stalking_success <- used_stalking %>% dplyr::filter(Harvest == "Y")
used_sitandwait_success <- used_sitandwait %>% dplyr::filter(Harvest == "Y")
used_coursing_unsuccess <- used_coursing %>% dplyr::filter(Harvest == "N")
used_stalking_unsuccess <- used_stalking %>% dplyr::filter(Harvest == "N")
used_sitandwait_unsuccess <- used_sitandwait %>% dplyr::filter(Harvest == "N")

# 4.5 x available (max w/o repeating)
avail_coursing_success <- dplyr::sample_n(available, 4.5*nrow(used_coursing_success))
avail_stalking_success <- dplyr::sample_n(available, 4.5*nrow(used_stalking_success))
avail_sitandwait_success <- dplyr::sample_n(available, 4.5*nrow(used_sitandwait_success))
avail_coursing_unsuccess <- dplyr::sample_n(available, 4.5*nrow(used_coursing_unsuccess))
avail_stalking_unsuccess <- dplyr::sample_n(available, 4.5*nrow(used_stalking_unsuccess))
avail_sitandwait_unsuccess <- dplyr::sample_n(available, 4.5*nrow(used_sitandwait_unsuccess))

# Combine used and available
used_avail_coursing_success <- dplyr::bind_rows(avail_coursing_success, used_coursing_success)
used_avail_stalking_success <- dplyr::bind_rows(avail_stalking_success, used_stalking_success)
used_avail_sitandwait_success <- dplyr::bind_rows(avail_sitandwait_success, used_sitandwait_success)
used_avail_coursing_unsuccess <- dplyr::bind_rows(avail_coursing_unsuccess, used_coursing_unsuccess)
used_avail_stalking_unsuccess <- dplyr::bind_rows(avail_stalking_unsuccess, used_stalking_unsuccess)
used_avail_sitandwait_unsuccess <- dplyr::bind_rows(avail_sitandwait_unsuccess, used_sitandwait_unsuccess)


fit_coursing_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_coursing_success, weight = w,
                   family = binomial(link = "logit"))

fit_stalking_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_stalking_success, weight = w,
                   family = binomial(link = "logit"))

fit_sitandwait_success <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                   data = used_avail_sitandwait_success, weight = w,
                   family = binomial(link = "logit"))

fit_coursing_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_coursing_unsuccess, weight = w,
                           family = binomial(link = "logit"))

fit_stalking_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_stalking_unsuccess, weight = w,
                           family = binomial(link = "logit"))

fit_sitandwait_unsuccess <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                           data = used_avail_sitandwait_unsuccess, weight = w,
                           family = binomial(link = "logit"))

# Export results

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

# Combine into a single dataframe
all_rsf_results <- dplyr::bind_rows(coursing_success_results, coursing_unsuccess_results,
                                    stalking_success_results, stalking_unsuccess_results,
                                    sitandwait_success_results, sitandwait_unsuccess_results)

write.csv(all_rsf_results, "rsf-results-by-mode-success-weighted-even.csv", row.names = F)
