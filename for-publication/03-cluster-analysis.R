# 03. Cluster analysis of hunting modes based on hunter behavior

library(dplyr)
library(tidyr)

# Import data -------------------------------------------------------------

data_hmm <- read.csv("hmm_data_with_model_predictions_annotated.csv")

# Calculate time spent in states across hunters -----------------------------

# For each hunter, calculate number of points in each state
hunter_summary <- data_hmm %>% 
    dplyr::group_by(ID) %>% 
    dplyr::count(state_2stationary) 

# Calculate percent of time spent in each state for each hunter
hunter_percentages <- hunter_summary %>% 
    tidyr::pivot_wider(names_from = state_2stationary, values_from = n) %>% 
    tidyr::replace_na(list(Stationary_offroad = 0, Stationary_road = 0, Walking = 0, Driving = 0)) %>% 
    dplyr::mutate(Total = sum(Stationary_offroad, Stationary_road, Walking, Driving),
                  Stationary_offroad_pct = Stationary_offroad/Total,
                  Stationary_road_pct = Stationary_road/Total,
                  Walking_pct = Walking/Total,
                  Driving_pct = Driving/Total) %>% 
    dplyr::select(-c(Stationary_offroad, Stationary_road, Walking, Driving, Total)) %>% 
    dplyr::ungroup()

# Create long version of dataframe
hunter_percentages_long <- hunter_percentages %>% 
    tidyr::pivot_longer(cols = c(Stationary_offroad_pct, Stationary_road_pct, Walking_pct, Driving_pct),
                        names_to = "State",
                        values_to = "Percentage")

# K-means clustering----------------------------------------------

# Prep for K-means cluster analysis
hunter_percentages_noID <- hunter_percentages %>% 
    dplyr::select(-ID) 

# Elbow Method for finding the optimal number of clusters
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

# K-means clustering with 3 clusters of sizes 149, 94, 240
# 
# Cluster means:
#     Stationary_offroad_pct Stationary_road_pct Walking_pct Driving_pct
# 1              0.1429111           0.1137739   0.4226213   0.3206938 # STALKING
# 2              0.3919537           0.1629287   0.2093264   0.2357912 # SIT-AND-WAIT
# 3              0.0889380           0.1486706   0.1705093   0.5918820 # COURSING


hunter_percentages$Cluster = factor(k3$cluster)
levels(hunter_percentages$Cluster) <- c("Stalking", "Sit-and-wait", "Coursing") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long <- dplyr::left_join(hunter_percentages_long,
                                            dplyr::select(hunter_percentages, ID, Cluster)) 


# Examine success rates ---------------------------------------------------

# Join cluster with hunting success
hunter_success <- data_hmm %>% 
    dplyr::select(ID, Harvest) %>% 
    unique()
hunter_cluster_success_long <- dplyr::left_join(hunter_percentages_long, hunter_success)

# Calculate success rate by cluster
hunter_cluster_success_long %>%
    dplyr::filter(Harvest != "unknown") %>% 
    dplyr::count(Cluster, Harvest) %>% 
    tidyr::pivot_wider(id_cols = Cluster,
                       names_from = Harvest,
                       values_from = n) %>% 
    dplyr::mutate(Total = N + Y,
                  Success_Rate = Y/Total,
                  Failure_rate = N/Total)

# Export ------------------------------------------------------------------

write.csv(hunter_cluster_success_long, "hunter_cluster_success_long.csv", row.names = FALSE)
