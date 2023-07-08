library(dplyr)
library(jtools)
library(lme4)

# Bring in datasets
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points (30 min)
used <- read.csv("Data/igotu_data_30min_covariates.csv")
#used <- read.csv("Data/igotu_data_3min_covariates.csv")
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

# Save scaling parameters
Ruggedness_scale_list <- list(scale = attr(used_avail$Ruggedness_scale, "scaled:scale"),
                              center = attr(used_avail$Ruggedness_scale, "scaled:center"))
Viewshed_scale_list <- list(scale = attr(used_avail$Viewshed_scale, "scaled:scale"),
                              center = attr(used_avail$Viewshed_scale, "scaled:center"))
Road_Distance_scale_list <- list(scale = attr(used_avail$Road_Distance_scale, "scaled:scale"),
                              center = attr(used_avail$Road_Distance_scale, "scaled:center"))
Chaparral_120m_scale_list <- list(scale = attr(used_avail$Chaparral_120m_scale, "scaled:scale"),
                              center = attr(used_avail$Chaparral_120m_scale, "scaled:center"))
Woodland_120m_scale_list <- list(scale = attr(used_avail$Woodland_120m_scale, "scaled:scale"),
                              center = attr(used_avail$Woodland_120m_scale, "scaled:center"))
HQ_Distance_scale_list <- list(scale = attr(used_avail$HQ_Distance_scale, "scaled:scale"),
                              center = attr(used_avail$HQ_Distance_scale, "scaled:center"))

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

# or - versions with random effect for individual (resulting in singular fit, 0 variance among individuals)
fit_coursing_success <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
                              data = used_avail_coursing_success,
                              family = binomial) 
fit_stalking_success <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
                            data = used_avail_stalking_success,
                            family = binomial) 
fit_sitandwait_success <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
                              data = used_avail_sitandwait_success,
                              family = binomial) 
fit_coursing_unsuccess <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
                              data = used_avail_coursing_unsuccess,
                              family = binomial) 
fit_stalking_unsuccess <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
                              data = used_avail_stalking_unsuccess,
                              family = binomial) 
fit_sitandwait_unsuccess <- glmer(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale + (1|ID),
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

library(ggplot2)
library(effects)
library(egg)


# Ruggedness RSF plots ----------------------------------------------------

# ruggedness
coursing_success_rugged <- predictorEffect("Ruggedness_scale", fit_coursing_success) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Successful",
           Mode = "Coursing") 
coursing_unsuccess_rugged <- predictorEffect("Ruggedness_scale", fit_coursing_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Coursing") 
stalking_success_rugged <- predictorEffect("Ruggedness_scale", fit_stalking_success) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Successful",
           Mode = "Stalking") 
stalking_unsuccess_rugged <- predictorEffect("Ruggedness_scale", fit_stalking_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Stalking") 
sitandwait_success_rugged <- predictorEffect("Ruggedness_scale", fit_sitandwait_success) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Successful",
           Mode = "Sit-and-Wait") 
sitandwait_unsuccess_rugged <- predictorEffect("Ruggedness_scale", fit_sitandwait_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Ruggedness_unscaled = Ruggedness_scale * Ruggedness_scale_list$scale + Ruggedness_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Sit-and-Wait") 
rugged_predict <- dplyr::bind_rows(coursing_success_rugged,
                                   coursing_unsuccess_rugged,
                                   stalking_success_rugged,
                                   stalking_unsuccess_rugged,
                                   sitandwait_success_rugged,
                                   sitandwait_unsuccess_rugged)

ggplot(rugged_predict, aes(x = Ruggedness_unscaled, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Ruggedness") +
    #scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    #scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    facet_wrap(~ Mode)

# Road RSF plots ----------------------------------------------------

coursing_success_road <- predictorEffect("Road_Distance_scale", fit_coursing_success) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Successful",
           Mode = "Coursing") 
coursing_unsuccess_road <- predictorEffect("Road_Distance_scale", fit_coursing_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Coursing") 
stalking_success_road <- predictorEffect("Road_Distance_scale", fit_stalking_success) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Successful",
           Mode = "Stalking") 
stalking_unsuccess_road <- predictorEffect("Road_Distance_scale", fit_stalking_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Stalking") 
sitandwait_success_road <- predictorEffect("Road_Distance_scale", fit_sitandwait_success) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Successful",
           Mode = "Sit-and-Wait") 
sitandwait_unsuccess_road <- predictorEffect("Road_Distance_scale", fit_sitandwait_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Road_Distance_unscaled = Road_Distance_scale * Road_Distance_scale_list$scale + Road_Distance_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Sit-and-Wait") 
road_predict <- dplyr::bind_rows(coursing_success_road,
                                   coursing_unsuccess_road,
                                   stalking_success_road,
                                   stalking_unsuccess_road,
                                   sitandwait_success_road,
                                   sitandwait_unsuccess_road)

ggplot(road_predict, aes(x = Road_Distance_unscaled, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Road Distance") +
    #scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    #scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    facet_wrap(~ Mode)


# Chaparral RSF plots ----------------------------------------------------

coursing_success_chaparral <- predictorEffect("Chaparral_120m_scale", fit_coursing_success) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Successful",
           Mode = "Coursing") 
coursing_unsuccess_chaparral <- predictorEffect("Chaparral_120m_scale", fit_coursing_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Coursing") 
stalking_success_chaparral <- predictorEffect("Chaparral_120m_scale", fit_stalking_success) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Successful",
           Mode = "Stalking") 
stalking_unsuccess_chaparral <- predictorEffect("Chaparral_120m_scale", fit_stalking_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Stalking") 
sitandwait_success_chaparral <- predictorEffect("Chaparral_120m_scale", fit_sitandwait_success) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Successful",
           Mode = "Sit-and-Wait") 
sitandwait_unsuccess_chaparral <- predictorEffect("Chaparral_120m_scale", fit_sitandwait_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Chaparral_120m_unscaled = Chaparral_120m_scale * Chaparral_120m_scale_list$scale + Chaparral_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Sit-and-Wait") 
chaparral_predict <- dplyr::bind_rows(coursing_success_chaparral,
                                 coursing_unsuccess_chaparral,
                                 stalking_success_chaparral,
                                 stalking_unsuccess_chaparral,
                                 sitandwait_success_chaparral,
                                 sitandwait_unsuccess_chaparral)

ggplot(chaparral_predict, aes(x = Chaparral_120m_unscaled, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Chaparral Density") +
    #scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    #scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    facet_wrap(~ Mode)

# Woodland RSF plots ----------------------------------------------------

coursing_success_woodland <- predictorEffect("Woodland_120m_scale", fit_coursing_success) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Successful",
           Mode = "Coursing") 
coursing_unsuccess_woodland <- predictorEffect("Woodland_120m_scale", fit_coursing_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Coursing") 
stalking_success_woodland <- predictorEffect("Woodland_120m_scale", fit_stalking_success) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Successful",
           Mode = "Stalking") 
stalking_unsuccess_woodland <- predictorEffect("Woodland_120m_scale", fit_stalking_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Stalking") 
sitandwait_success_woodland <- predictorEffect("Woodland_120m_scale", fit_sitandwait_success) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Successful",
           Mode = "Sit-and-Wait") 
sitandwait_unsuccess_woodland <- predictorEffect("Woodland_120m_scale", fit_sitandwait_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Woodland_120m_unscaled = Woodland_120m_scale * Woodland_120m_scale_list$scale + Woodland_120m_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Sit-and-Wait") 
woodland_predict <- dplyr::bind_rows(coursing_success_woodland,
                                      coursing_unsuccess_woodland,
                                      stalking_success_woodland,
                                      stalking_unsuccess_woodland,
                                      sitandwait_success_woodland,
                                      sitandwait_unsuccess_woodland)

ggplot(woodland_predict, aes(x = Woodland_120m_unscaled, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Woodland Density") +
    #scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    #scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    facet_wrap(~ Mode)


# Viewshed RSF plots ----------------------------------------------------

coursing_success_viewshed <- predictorEffect("Viewshed_scale", fit_coursing_success) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Successful",
           Mode = "Coursing") 
coursing_unsuccess_viewshed <- predictorEffect("Viewshed_scale", fit_coursing_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Coursing") 
stalking_success_viewshed <- predictorEffect("Viewshed_scale", fit_stalking_success) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Successful",
           Mode = "Stalking") 
stalking_unsuccess_viewshed <- predictorEffect("Viewshed_scale", fit_stalking_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Stalking") 
sitandwait_success_viewshed <- predictorEffect("Viewshed_scale", fit_sitandwait_success) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Successful",
           Mode = "Sit-and-Wait") 
sitandwait_unsuccess_viewshed <- predictorEffect("Viewshed_scale", fit_sitandwait_unsuccess) %>% 
    as_tibble() %>% 
    mutate(Viewshed_unscaled = Viewshed_scale * Viewshed_scale_list$scale + Viewshed_scale_list$center,
           Success = "Unsuccessful",
           Mode = "Sit-and-Wait") 
viewshed_predict <- dplyr::bind_rows(coursing_success_viewshed,
                                     coursing_unsuccess_viewshed,
                                     stalking_success_viewshed,
                                     stalking_unsuccess_viewshed,
                                     sitandwait_success_viewshed,
                                     sitandwait_unsuccess_viewshed)

ggplot(viewshed_predict, aes(x = Viewshed_unscaled, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Viewshed") +
    #scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    #scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    facet_wrap(~ Mode, scales = "free")


# ALL ---------------------------------------------------------------------

all_predict <- dplyr::bind_rows(viewshed_predict,
                                chaparral_predict,
                                woodland_predict,
                                road_predict,
                                rugged_predict) %>% 
    dplyr::select(-c(Viewshed_scale,
                     Chaparral_120m_scale,
                     Woodland_120m_scale,
                     Road_Distance_scale,
                     Ruggedness_scale)) %>% 
    tidyr::pivot_longer(cols = c(Viewshed_unscaled,
                                 Chaparral_120m_unscaled,
                                 Woodland_120m_unscaled,
                                 Road_Distance_unscaled,
                                 Ruggedness_unscaled),
                        names_to = "Covariate",
                        values_to = "Covariate_value") %>% 
    tidyr::drop_na()

# scales not working right
ggplot(all_predict, aes(x = Covariate_value, y = fit))+
    geom_line(aes(col = Success)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Success), alpha = 0.2) +
    labs(y = "Probability of Use", x = "Covariate") +
    theme_bw() +
    facet_wrap(Covariate ~ Mode, scales = "free", ncol = 3)
