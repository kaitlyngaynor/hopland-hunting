library(glmmTMB)
library(dplyr)


# Bring in all available points in study area
available <- read.csv("for-publication/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points
used <- read.csv("for-publication/igotu_data_3min_covariates.csv")
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
used_avail <- dplyr::bind_rows(used, available_100) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m,
                  Used) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m))

# Bring in clusters
clusters <- read.csv("for-publication/hunter_cluster_success_long_nocov.csv") %>%
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
    dplyr::select(ID, Cluster, Harvest)
used_avail <- dplyr::left_join(used_avail, clusters)
used_avail$ID <- as.character(used_avail$ID)

# Split by cluster
used_avail_coursing <- used_avail %>% 
    dplyr::filter((Cluster != "Stalking" & Cluster != "Sit-and-wait"))
used_avail_stalking <- used_avail %>% 
    dplyr::filter((Cluster != "Coursing" & Cluster != "Sit-and-wait"))
used_avail_sitandwait <- used_avail %>% 
    dplyr::filter((Cluster != "Stalking" & Cluster != "Coursing"))

# Filter by cluster & success
used_avail_coursing_success <- used_avail_coursing %>% dplyr::filter(Harvest == "Y")
used_avail_stalking_success <- used_avail_stalking %>% dplyr::filter(Harvest == "Y")
used_avail_sitandwait_success <- used_avail_sitandwait %>% dplyr::filter(Harvest == "Y")
used_avail_coursing_unsuccess <- used_avail_coursing %>% dplyr::filter(Harvest == "N")
used_avail_stalking_unsuccess <- used_avail_stalking %>% dplyr::filter(Harvest == "N")
used_avail_sitandwait_unsuccess <- used_avail_sitandwait %>% dplyr::filter(Harvest == "N")

# Muff method 
rsf_stalking_success <- glmmTMB(Used ~ -1 + (1|ID) +
                  Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                  Viewshed_scale + (0 + Viewshed_scale | ID) +
                  Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                  Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                  Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
              family = binomial(),
              data = used_avail_stalking_success)
rsf_coursing_success <- glmmTMB(Used ~ -1 + (1|ID) +
                                    Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                                    Viewshed_scale + (0 + Viewshed_scale | ID) +
                                    Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                                    Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                                    Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
                                family = binomial(),
                                data = used_avail_coursing_success)
rsf_sitandwait_success <- glmmTMB(Used ~ -1 + (1|ID) +
                                    Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                                    Viewshed_scale + (0 + Viewshed_scale | ID) +
                                    Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                                    Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                                    Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
                                family = binomial(),
                                data = used_avail_sitandwait_success)

rsf_stalking_unsuccess <- glmmTMB(Used ~ -1 + (1|ID) +
                                    Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                                    Viewshed_scale + (0 + Viewshed_scale | ID) +
                                    Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                                    Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                                    Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
                                family = binomial(),
                                data = used_avail_stalking_unsuccess)
rsf_coursing_unsuccess <- glmmTMB(Used ~ -1 + (1|ID) +
                                    Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                                    Viewshed_scale + (0 + Viewshed_scale | ID) +
                                    Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                                    Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                                    Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
                                family = binomial(),
                                data = used_avail_coursing_unsuccess)
rsf_sitandwait_unsuccess <- glmmTMB(Used ~ -1 + (1|ID) +
                                      Ruggedness_scale + (0 + Ruggedness_scale | ID) +
                                      Viewshed_scale + (0 + Viewshed_scale | ID) +
                                      Road_Distance_scale + (0 + Road_Distance_scale | ID) +
                                      Chaparral_120m_scale + (0 + Chaparral_120m_scale | ID) +
                                      Woodland_120m_scale + (0 + Woodland_120m_scale | ID),
                                  family = binomial(),
                                  data = used_avail_sitandwait_unsuccess)


# Coursing - success
coursing_success_coef <- glmmTMB::fixef(rsf_coursing_success)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
coursing_success_ci <- confint(rsf_coursing_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
coursing_success_results <- dplyr::left_join(coursing_success_coef, coursing_success_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Coursing",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Stalking - success
stalking_success_coef <- glmmTMB::fixef(rsf_stalking_success)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
stalking_success_ci <- confint(rsf_stalking_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
stalking_success_results <- dplyr::left_join(stalking_success_coef, stalking_success_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Stalking",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Sit and wait - success
sitandwait_success_coef <- glmmTMB::fixef(rsf_sitandwait_success)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
sitandwait_success_ci <- confint(rsf_sitandwait_success) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
sitandwait_success_results <- dplyr::left_join(sitandwait_success_coef, sitandwait_success_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Sit-and-Wait",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))


# Coursing - unsuccess
coursing_unsuccess_coef <- glmmTMB::fixef(rsf_coursing_unsuccess)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
coursing_unsuccess_ci <- confint(rsf_coursing_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
coursing_unsuccess_results <- dplyr::left_join(coursing_unsuccess_coef, coursing_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Coursing",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Stalking - unsuccess
stalking_unsuccess_coef <- glmmTMB::fixef(rsf_stalking_unsuccess)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
stalking_unsuccess_ci <- confint(rsf_stalking_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
stalking_unsuccess_results <- dplyr::left_join(stalking_unsuccess_coef, stalking_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Stalking",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

# Sit and wait - unsuccess
sitandwait_unsuccess_coef <- glmmTMB::fixef(rsf_sitandwait_unsuccess)$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")
sitandwait_unsuccess_ci <- confint(rsf_sitandwait_unsuccess) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
sitandwait_unsuccess_results <- dplyr::left_join(sitandwait_unsuccess_coef, sitandwait_unsuccess_ci) %>% 
    dplyr::mutate(Predictor = c("Ruggedness", "Viewshed", "Road Distance", "Chaparral", "Woodland"),
                  `Hunting Mode` = "Sit-and-Wait",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))


# Combine into a single dataframe
all_rsf_results <- dplyr::bind_rows(coursing_success_results, #coursing_unsuccess_results,
                                    stalking_success_results, #stalking_unsuccess_results,
                                    sitandwait_success_results#, sitandwait_unsuccess_results)
)

all_rsf_results_unsuccess <- dplyr::bind_rows(coursing_unsuccess_results, #coursing_unsuccess_results,
                                    stalking_unsuccess_results, #stalking_unsuccess_results,
                                    sitandwait_unsuccess_results#, sitandwait_unsuccess_results)
)

write.csv(all_rsf_results, "rsf-muff-results.csv", row.names = FALSE)
