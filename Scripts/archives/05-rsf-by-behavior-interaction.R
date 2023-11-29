# 05. Resource Selection Functions to assess habitat selection for each hunting mode

library(dplyr)
library(jtools)
library(lme4)

# Bring in all available points in study area
available <- read.csv("all-available-point-cov.csv")
available$Used <- 0

# Bring in used points
used <- read.csv("igotu_data_3min_covariates.csv")
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
clusters <- read.csv("hunter_cluster_success_long_nocov.csv") %>%
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
    dplyr::select(ID, Cluster, Harvest)
used_avail <- dplyr::left_join(used_avail, clusters)
used_avail$ID <- as.character(used_avail$ID)

fit <- glm(Used ~ Ruggedness_scale * Cluster * Harvest + 
                  Viewshed_scale * Cluster * Harvest + 
                  Chaparral_120m_scale * Cluster * Harvest + 
                  Woodland_120m_scale * Cluster * Harvest + 
                  Road_Distance_scale * Cluster * Harvest,
                  data = used_avail,
                  family = binomial) 


# Export results
saveRDS(fit, "fit_interaction.Rds")

# Extract coefficients
coef <- fit$coefficients %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("Coefficient" = ".")

# Extract CI - TAKES AGES!
coef_ci <- confint(fit) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")

coursing_success_results <- dplyr::left_join(coef, coef_ci)

# Predict model
survveg <- data.frame(survveg.2 = c(0,1)) %>% 
    mutate(east = 0,
           north = 0,
           rugged = 0,
           elevation = 0,
           slope = 0,
           water = 0,
           veg = 1)

survveg$veg <- as.factor(survveg$veg)
survveg$survveg.2 <- as.factor(survveg$survveg.2)

survveg$pre <- predict(best_model_pre, survveg, type = "response", re.form=~0, factors = c(veg.levels))

