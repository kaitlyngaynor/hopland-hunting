
library(dplyr)
library(jtools)
library(lme4)
library(boot)

# Bring in all available points in study area
available <- read.csv("for-publication/all-available-point-cov.csv")
available$Used <- 0

# Bring in used points
used <- read.csv("for-publication/igotu_data_3min_covariates.csv")
used$Used <- 1


# Combine
used_avail <- bind_rows(available, used)

# Weight the available points more
used_avail$w <- ifelse(used_avail$Used, 1, 5000)

# Join into single dataframe and scale covariates
used_avail <- used_avail %>% 
  dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Woodland_120m,
                Used, w) %>% 
  dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                Viewshed_scale = scale(Viewshed),
                Road_Distance_scale = scale(Road_Distance),
                Chaparral_120m_scale = scale(Chaparral_120m),
                Woodland_120m_scale = scale(Woodland_120m))

# Bring in clusters
clusters <- read.csv("for-publication/hunter_cluster_success_long.csv") %>%
  tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
  dplyr::select(ID, Cluster, Harvest)
used_avail <- dplyr::left_join(used_avail, clusters)
used_avail$ID <- as.character(used_avail$ID)

# Split by cluster
used_avail_coursing <- used_avail %>% 
  dplyr::filter((Cluster != "Stalking" & Cluster != "Sit-and-wait") | Used == 0)
used_avail_stalking <- used_avail %>% 
  dplyr::filter((Cluster != "Coursing" & Cluster != "Sit-and-wait") | Used == 0)
used_avail_sitandwait <- used_avail %>% 
  dplyr::filter((Cluster != "Stalking" & Cluster != "Coursing") | Used == 0)

# Filter by cluster & success
used_avail_coursing_success <- used_avail_coursing %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_stalking_success <- used_avail_stalking %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_sitandwait_success <- used_avail_sitandwait %>% dplyr::filter(Harvest == "Y" | Used == 0)
used_avail_coursing_unsuccess <- used_avail_coursing %>% dplyr::filter(Harvest == "N" | Used == 0)
used_avail_stalking_unsuccess <- used_avail_stalking %>% dplyr::filter(Harvest == "N" | Used == 0)
used_avail_sitandwait_unsuccess <- used_avail_sitandwait %>% dplyr::filter(Harvest == "N" | Used == 0)

# Define your model function
model_function <- function(data, indices) {
    # Subset data using the bootstrap indices
    boot_data <- data[indices, ]
    
    # Fit the logistic regression model
    model <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale + Road_Distance_scale,
                 data = boot_data, weight = w,
                 family = binomial(link = "logit"))
    
    # Return the model coefficients
    return(coef(model))
}

# Coursing successful
set.seed(123)
boot_coursing_success <- boot(data = used_avail_coursing_success, 
                              statistic = model_function, R = 1000)
boot_coefs_coursing_success <- boot_coursing_success$t %>% 
    as.data.frame()
names(boot_coefs_coursing_success) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_coursing_success, "Results/rsf-bootstrap/boot_coursing_success2.csv", row.names = FALSE)


# Stalking successful
set.seed(123)
boot_stalking_success <- boot(data = used_avail_stalking_success, 
                              statistic = model_function, R = 1000)
boot_coefs_stalking_success <- boot_stalking_success$t %>% 
    as.data.frame()
names(boot_coefs_stalking_success) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_stalking_success, "Results/rsf-bootstrap/boot_stalking_success2.csv", row.names = FALSE)


# Sit and wait successful
set.seed(123)
boot_sitandwait_success <- boot(data = used_avail_sitandwait_success, 
                              statistic = model_function, R = 1000)
boot_coefs_sitandwait_success <- boot_sitandwait_success$t %>% 
    as.data.frame()
names(boot_coefs_sitandwait_success) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_sitandwait_success, "Results/rsf-bootstrap/boot_sitandwait_success2.csv", row.names = FALSE)


# Coursing unsuccessful
set.seed(123)
boot_coursing_unsuccess <- boot(data = used_avail_coursing_unsuccess, 
                              statistic = model_function, R = 1000)
boot_coefs_coursing_unsuccess <- boot_coursing_unsuccess$t %>% 
    as.data.frame()
names(boot_coefs_coursing_unsuccess) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_coursing_unsuccess, "Results/rsf-bootstrap/boot_coursing_unsuccess2.csv", row.names = FALSE)


# Stalking ununsuccessful
set.seed(123)
boot_stalking_unsuccess <- boot(data = used_avail_stalking_unsuccess, 
                              statistic = model_function, R = 1000)
boot_coefs_stalking_unsuccess <- boot_stalking_unsuccess$t %>% 
    as.data.frame()
names(boot_coefs_stalking_unsuccess) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_stalking_unsuccess, "Results/rsf-bootstrap/boot_stalking_unsuccess2.csv", row.names = FALSE)


# Sit and wait ununsuccessful
set.seed(123)
boot_sitandwait_unsuccess <- boot(data = used_avail_sitandwait_unsuccess, 
                              statistic = model_function, R = 1000)
boot_coefs_sitandwait_unsuccess <- boot_sitandwait_unsuccess$t %>% 
    as.data.frame()
names(boot_coefs_sitandwait_unsuccess) <- c("Intercept", "Ruggedness", "Viewshed", "Chaparral", "Woodland", "Road_Distance")
write.csv(boot_coefs_sitandwait_unsuccess, "Results/rsf-bootstrap/boot_sitandwait_unsuccess2.csv", row.names = FALSE)


# Bring results back in
boot_coefs_coursing_success <- read.csv("Results/rsf-bootstrap/boot_coursing_success2.csv")
boot_coefs_stalking_success <- read.csv("Results/rsf-bootstrap/boot_stalking_success2.csv")
boot_coefs_sitandwait_success <- read.csv("Results/rsf-bootstrap/boot_sitandwait_success2.csv")
boot_coefs_coursing_unsuccess <- read.csv("Results/rsf-bootstrap/boot_coursing_unsuccess2.csv")
boot_coefs_stalking_unsuccess <- read.csv("Results/rsf-bootstrap/boot_stalking_unsuccess2.csv")
boot_coefs_sitandwait_unsuccess <- read.csv("Results/rsf-bootstrap/boot_sitandwait_unsuccess2.csv")

coursing_success_mean <- boot_coefs_coursing_success %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(coursing_success_mean) <- c("Predictor", "Coefficient")
coursing_success_CI <- boot_coefs_coursing_success %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(coursing_success_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Coursing",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_")) 

stalking_success_mean <- boot_coefs_stalking_success %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(stalking_success_mean) <- c("Predictor", "Coefficient")
stalking_success_CI <- boot_coefs_stalking_success %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(stalking_success_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Stalking",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

sitandwait_success_mean <- boot_coefs_sitandwait_success %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(sitandwait_success_mean) <- c("Predictor", "Coefficient")
sitandwait_success_CI <- boot_coefs_sitandwait_success %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(sitandwait_success_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Sit-and-wait",
                  Harvest = "Yes",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))


coursing_unsuccess_mean <- boot_coefs_coursing_unsuccess %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(coursing_unsuccess_mean) <- c("Predictor", "Coefficient")
coursing_unsuccess_CI <- boot_coefs_coursing_unsuccess %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(coursing_unsuccess_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Coursing",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_")) 

stalking_unsuccess_mean <- boot_coefs_stalking_unsuccess %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(stalking_unsuccess_mean) <- c("Predictor", "Coefficient")
stalking_unsuccess_CI <- boot_coefs_stalking_unsuccess %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(stalking_unsuccess_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Stalking",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

sitandwait_unsuccess_mean <- boot_coefs_sitandwait_unsuccess %>% 
    apply(2, mean,  na.rm = TRUE) %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor")
names(sitandwait_unsuccess_mean) <- c("Predictor", "Coefficient")
sitandwait_unsuccess_CI <- boot_coefs_sitandwait_unsuccess %>% 
    apply(2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE) %>% 
    t() %>% 
    as.data.frame %>% 
    tibble::rownames_to_column("Predictor") %>% 
    dplyr::rename(LCI = `2.5%`, UCI = `97.5%`) %>% 
    dplyr::left_join(sitandwait_unsuccess_mean) %>% 
    dplyr::mutate(`Hunting Mode` = "Sit-and-wait",
                  Harvest = "No",
                  Model = paste(`Hunting Mode`, Harvest, sep = "_"))

all_success_CI <- dplyr::bind_rows(coursing_success_CI, 
                                   stalking_success_CI, 
                                   sitandwait_success_CI,
                                   coursing_unsuccess_CI,
                                   stalking_unsuccess_CI,
                                   sitandwait_unsuccess_CI)
write.csv(all_success_CI, "Results/rsf-bootstrapping-results2.csv", row.names = FALSE)
