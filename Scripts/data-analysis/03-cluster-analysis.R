# Cluster analysis of behavioral modes of hunters

library(dplyr)
library(ggplot2)
library(tidyr)
library(sjPlot)

# Import data -------------------------------------------------------------

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-2022-10-04.csv")

# Calculate time spent in states across hunters -----------------------------

# For each hunter, calculate number of points in each state
hunter_summary <- data_hmm %>% 
    dplyr::group_by(ID) %>% 
    dplyr::count(state) 

# Calculate percent of time spent in each state for each hunter
hunter_percentages <- hunter_summary %>% 
    tidyr::pivot_wider(names_from = state, values_from = n) %>% 
    tidyr::replace_na(list(Stationary = 0, Walking = 0, Driving = 0)) %>% 
    dplyr::mutate(Total = sum(Stationary, Walking, Driving),
                  Stationary_pct = Stationary/Total,
                  Walking_pct = Walking/Total,
                  Driving_pct = Driving/Total) %>% 
    dplyr::select(-c(Stationary, Walking, Driving, Total)) %>% 
    dplyr::ungroup()

# Create long version of datafrae
hunter_percentages_long <- hunter_percentages %>% 
    tidyr::pivot_longer(cols = c(Stationary_pct, Walking_pct, Driving_pct),
                        names_to = "State",
                        values_to = "Percentage")

# K-means clustering----------------------------------------------

# Prep for K-means cluster analysis
hunter_percentages_noID <- hunter_percentages %>% 
    dplyr::select(-ID) 

# Elbow Method for finding the optimal number of clusters (k=2 to k=15)
# THREE CLUSTERS IS OPTIMAL
set.seed(4321)
k.max <- 15
data <- hunter_percentages_noID
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=25,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Run K-means cluster analysis with k=3 clusters
set.seed(321)
k3 <- kmeans(as.matrix(hunter_percentages_noID), centers = 3, nstart = 25)
k3

# K-means clustering with 3 clusters of sizes 66, 70, 90
# 
# Cluster means:
#     Stationary_pct Walking_pct Driving_pct
# 1      0.2620318   0.4485732   0.2893950
# 2      0.5130728   0.1932013   0.2937259
# 3      0.2175774   0.2019768   0.5804458

# Assign cluster to each point
hunter_percentages$Cluster = factor(k3$cluster)
levels(hunter_percentages$Cluster) <- c("Walkers", "Waiters", "Drivers") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long <- dplyr::left_join(hunter_percentages_long,
                                            dplyr::select(hunter_percentages, ID, Cluster))


# Examine success rates ---------------------------------------------------

# Join cluster with hunting success
hunter_success <- data_hmm %>% 
    dplyr::select(ID, Harvest) %>% 
    unique()
hunter_cluster_success <- dplyr::left_join(hunter_percentages, hunter_success)
hunter_cluster_success_long <- dplyr::left_join(hunter_percentages_long, hunter_success)

# Calculate success rate by cluster
success_rate <- hunter_cluster_success %>%
    dplyr::count(Cluster, Harvest) %>% 
    tidyr::pivot_wider(id_cols = Cluster,
                       names_from = Harvest,
                       values_from = n) %>% 
    dplyr::mutate(Total = N + Y,
                  Success_Rate = Y/Total,
                  Failure_rate = N/Total)

# Model success rate as function of dominant mode
hunter_cluster_success <- hunter_cluster_success %>%
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1))
fit <- glm(Harvest01 ~ Cluster, data = hunter_cluster_success,
           family = binomial)
summary(fit)

sjPlot::plot_model(fit)


# Export data
write.csv(hunter_cluster_success, "Results/hunters_by_cluster_with_success.csv", row.names = FALSE)
write.csv(hunter_percentages_long, "Results/hunter_percentages_long.csv", row.names = FALSE)
write.csv(hunter_cluster_success_long, "Results/hunter_cluster_success_long.csv", row.names = FALSE)
