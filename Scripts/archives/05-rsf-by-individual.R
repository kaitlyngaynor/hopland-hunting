
library(dplyr)
library(jtools)
library(lme4)

# Bring in all available points in study area
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points
used <- read.csv("Data/igotu_data_3min_covariates.csv")
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
used_avail <- dplyr::left_join(used_avail, clusters) %>% 
    dplyr::filter(ID != "080820_39") %>% 
    dplyr::filter(ID != "081620_02") %>% 
    dplyr::filter(ID != "082320_14") %>% 
    dplyr::filter(ID != "082320_40") %>% 
    dplyr::filter(ID != "082022_10") %>% 
    dplyr::filter(ID != "082122_10")
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

models_coursing_success <- list()
for(i in 1:length(unique(used_avail_coursing_success$ID))) {
    print(paste0("Running model for ", unique(used_avail_coursing_success$ID)[i]))
    input_data <- used_avail %>% 
        dplyr::filter(ID == unique(used_avail_coursing_success$ID)[i])
    rsf_model <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                     data = input_data,
                     family = binomial)
    rsf_model_coef <- rsf_model$coefficients %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("Predictor") %>% 
        dplyr::rename("Coefficient" = ".")
    rsf_model_ci <- confint(rsf_model) %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("Predictor") %>% 
        dplyr::rename("LCI" = "2.5 %", "UCI" = "97.5 %")
    # extract standard errors
    stand_err <- sqrt(diag(vcov(rsf_model))) %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("Predictor") %>% 
        dplyr::rename("StandardError" = ".")
    models_coursing_success[[i]] <- dplyr::left_join(rsf_model_coef, rsf_model_ci) %>% 
        dplyr::left_join(stand_err) %>% 
        dplyr::mutate(Predictor = c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road Distance"),
                      ID = unique(used_avail_coursing_success$ID)[i])
}

# you can average them
# The standard error of the average then will be the square root from the average of the squares of the standard errors

models_coursing_success_all <- bind_rows(models_coursing_success) %>% 
    dplyr::select(Predictor, Coefficient, StandardError) %>% 
    group_by(Predictor) %>% 
    mutate(avg = mean(Coefficient),
           se = sqrt(sum(StandardError^2))) %>% 
    dplyr::select(Predictor, avg, se) %>% 
    unique()



models_coursing_success <- list()
for(i in 1:length(unique(used_avail_coursing_success$ID))) {
    print(paste0("Running model for ", unique(used_avail_coursing_success$ID)[i]))
    input_data <- used_avail %>% 
        dplyr::filter(ID == unique(used_avail_coursing_success$ID)[i])
    models_coursing_success[[i]] <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                     data = input_data,
                     family = binomial)
}
MuMIn::model.avg(models_coursing_success)

