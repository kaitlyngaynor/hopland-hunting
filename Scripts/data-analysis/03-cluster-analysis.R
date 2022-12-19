# Cluster analysis of behavioral modes of hunters

library(dplyr)
library(ggplot2)
library(tidyr)
library(sjPlot)

# Import data -------------------------------------------------------------

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2022-12-19.csv")

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
data <- hunter_percentages_noID
#data <- hunter_percentages_noID_4state
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

# K-means clustering with 3 clusters of sizes 122, 113, 192
# 
# Cluster means:
#     Stationary_pct Walking_pct Driving_pct
# 1      0.5195945   0.1675685   0.3128369 # WAITERS
# 2      0.2730276   0.4427382   0.2842342 # WALKERS
# 3      0.2004715   0.1955191   0.6040094 # DRIVERS


# Assign cluster to each point
hunter_percentages$Cluster = factor(k3$cluster)
levels(hunter_percentages$Cluster) <- c("Waiters", "Walkers", "Drivers") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long <- dplyr::left_join(hunter_percentages_long,
                                            dplyr::select(hunter_percentages, ID, Cluster)) 


# Do that for the 4-states
k3_4state <- kmeans(as.matrix(hunter_percentages_noID_4state), centers = 3, nstart = 25)
k3_4state
# K-means clustering with 3 clusters of sizes 227, 111, 89
# 
# Cluster means:
#     Stationary_offroad_pct Stationary_road_pct Walking_pct Driving_pct
# 1             0.08984035           0.1472511   0.1854634   0.5774451
# 2             0.15772224           0.1065848   0.4450873   0.2906057
# 3             0.37255626           0.1844699   0.1854782   0.2574955
hunter_percentages_4state$Cluster4 = factor(k3_4state$cluster)
levels(hunter_percentages_4state$Cluster4) <- c("Drivers", "Walkers", "Waiters") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long_4state <- dplyr::left_join(hunter_percentages_long_4state,
                                            dplyr::select(hunter_percentages_4state, ID, Cluster4)) 

# Look at correlations between clusters
hunter_percentages_all <- dplyr::left_join(hunter_percentages, hunter_percentages_4state)
different <- dplyr::filter(hunter_percentages_all, Cluster != Cluster4)
nrow(different) # 49 of the 427 changed cluster
count(different, Cluster, Cluster4)
# Cluster Cluster4     n
# <fct>   <fct>    <int>
#     1 Waiters Drivers     35
#     2 Waiters Walkers      2
#     3 Walkers Drivers      4
#     4 Walkers Waiters      4
#     5 Drivers Walkers      4

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
