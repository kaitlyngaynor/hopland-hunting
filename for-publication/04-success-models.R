# 04. Models explaining success of hunters (based on hunting mode, year, and single vs multi-day hunt)

library(dplyr)

# Bring in data
success <- read.csv("hunter_cluster_success_long.csv") %>%
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1)) %>% 
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage")
metadata <- read.csv("igotu_metadata.csv")
success <- dplyr::left_join(success, metadata)

# Run all model combinations

# Null model
fit0 <- glm(Harvest01 ~ 1, data = success, family = binomial) # AIC = 372.23
summary(fit0)

# Hunting mode
fit1 <- glm(Harvest01 ~ Cluster, data = success, family = binomial) # AIC = 370.98
summary(fit1) 

# Year
fit2 <- glm(Harvest01 ~ Year, data = success, family = binomial) # AIC = 370.05
summary(fit2)

# Single vs multiday
fit3 <- glm(Harvest01 ~ Hunt_type, data = success, family = binomial) # AIC = 365.65
summary(fit3)

# Single vs multiday * mode
fit4 <- glm(Harvest01 ~ Hunt_type * Cluster, data = success, family = binomial) # AIC = 369.05
summary(fit4)

# Single vs multiday + mode
fit5 <- glm(Harvest01 ~ Hunt_type + Cluster, data = success, family = binomial) # AIC = 365.64
summary(fit5)

# Year * mode
fit6 <- glm(Harvest01 ~ Year * Cluster, data = success, family = binomial) # AIC = 372.99
summary(fit6)

# Year + mode
fit7 <- glm(Harvest01 ~ Year + Cluster, data = success, family = binomial) # AIC = 369.56
summary(fit7)

# Year + single vs multiday + mode
fit8 <- glm(Harvest01 ~ Hunt_type + Cluster + Year, data = success, family = binomial) # AIC = 366.91
summary(fit8)
