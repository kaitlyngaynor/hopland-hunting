# Cluster analysis of behavioral modes of hunters

library(dplyr)
library(tidyr)
library(sjPlot)
library(factoextra)

# Import data -------------------------------------------------------------

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-03-10.csv")

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

# Create long version of dataframe
hunter_percentages_long <- hunter_percentages %>% 
    tidyr::pivot_longer(cols = c(Stationary_pct, Walking_pct, Driving_pct),
                        names_to = "State",
                        values_to = "Percentage")

# Calculate time spent in states across hunters for FOUR states (2 stationary) -----------------------------

# For each hunter, calculate number of points in each state
hunter_summary_4state <- data_hmm %>% 
    dplyr::group_by(ID) %>% 
    dplyr::count(state_2stationary) 

# Calculate percent of time spent in each state for each hunter
hunter_percentages_4state <- hunter_summary_4state %>% 
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
hunter_percentages_long_4state <- hunter_percentages_4state %>% 
    tidyr::pivot_longer(cols = c(Stationary_offroad_pct, Stationary_road_pct, Walking_pct, Driving_pct),
                        names_to = "State_4state",
                        values_to = "Percentage")

# K-means clustering----------------------------------------------

# Prep for K-means cluster analysis
hunter_percentages_noID <- hunter_percentages %>% 
    dplyr::select(-ID) 
hunter_percentages_noID_4state <- hunter_percentages_4state %>% 
    dplyr::select(-ID) 

# Elbow Method for finding the optimal number of clusters (k=2 to k=15)
# THREE CLUSTERS IS OPTIMAL
set.seed(4321)
k.max <- 15
data <- hunter_percentages_noID_4state
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=25,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Run K-means cluster analysis with k=3 clusters
set.seed(321)
k3_4state <- kmeans(as.matrix(hunter_percentages_noID_4state), centers = 3, nstart = 25)
k3_4state

# K-means clustering with 3 clusters of sizes 149, 94, 240
# 
# Cluster means:
#     Stationary_offroad_pct Stationary_road_pct Walking_pct Driving_pct
# 1              0.1429111           0.1137739   0.4226213   0.3206938 # WALKERS
# 2              0.3919537           0.1629287   0.2093264   0.2357912 # WAITERS
# 3              0.0889380           0.1486706   0.1705093   0.5918820 # DRIVERS


hunter_percentages_4state$Cluster4 = factor(k3_4state$cluster)
levels(hunter_percentages_4state$Cluster4) <- c("Walkers", "Waiters", "Drivers") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long_4state <- dplyr::left_join(hunter_percentages_long_4state,
                                            dplyr::select(hunter_percentages_4state, ID, Cluster4)) 


# Examine success rates ---------------------------------------------------

# Join cluster with hunting success
hunter_success <- data_hmm %>% 
    dplyr::select(ID, Harvest) %>% 
    unique()
hunter_cluster_success_long <- dplyr::left_join(hunter_percentages_long_4state, hunter_success)

# Calculate success rate by cluster
success_rate <- hunter_cluster_success_long %>%
    dplyr::filter(Harvest != "unknown") %>% 
    dplyr::count(Cluster4, Harvest) %>% 
    tidyr::pivot_wider(id_cols = Cluster4,
                       names_from = Harvest,
                       values_from = n) %>% 
    dplyr::mutate(Total = N + Y,
                  Success_Rate = Y/Total,
                  Failure_rate = N/Total)

# Export PCA for plotting -----------------------------------------------------------

# from https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/ 

# Dimension reduction using PCA
percentages.pca <- prcomp(as.matrix(hunter_percentages_noID_4state))
# Coordinates of individuals
ind.coord <- as.data.frame(factoextra::get_pca_ind(percentages.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(k3_4state$cluster)
# Data inspection
head(ind.coord)

hunter_percentages_4state <- cbind(hunter_percentages_4state, ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(percentages.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

# Export ------------------------------------------------------------------

write.csv(hunter_percentages_4state, "Results/hunter_percentages_4state.csv", row.names = FALSE)
write.csv(hunter_cluster_success_long, "Results/hunter_cluster_success_long.csv", row.names = FALSE)


