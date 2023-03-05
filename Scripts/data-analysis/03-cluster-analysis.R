# Cluster analysis of behavioral modes of hunters

library(dplyr)
library(ggplot2)
library(tidyr)
library(sjPlot)

# Import data -------------------------------------------------------------

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-03-04.csv")

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

# K-means clustering with 3 clusters of sizes 147, 129, 207
# 
# Cluster means:
#     Stationary_offroad_pct Stationary_road_pct Walking_pct Driving_pct
# 1              0.3126273          0.11039971   0.3027784   0.2741946 # WAITERS
# 2              0.1090912          0.06542713   0.3022608   0.5232210 # DRIVERS
# 3              0.1051077          0.08267808   0.6207700   0.1914443 # WALKERS


hunter_percentages_4state$Cluster4 = factor(k3_4state$cluster)
levels(hunter_percentages_4state$Cluster4) <- c("Waiters", "Drivers", "Walkers") # change factor level names

# Make dataframe manually for plotting
cluster_times <- data.frame(Mode = c("Waiters", "Drivers", "Walkers"),
                            Stationary_offroad = c(0.3126273, 0.1090912, 0.1051077),
                            Stationary_road = c(0.11039971, 0.06542713, 0.08267808),
                            Walking_pct = c(0.3027784, 0.3022608, 0.6207700),
                            Driving_pct = c(0.2741946, 0.5232210, 0.1914443)) %>% 
    tidyr::pivot_longer(cols = -Mode, names_to = "State", values_to = "Percent_time")
ggplot(cluster_times, aes(x = Mode, y = Percent_time, fill = State)) +
    geom_bar(stat = "identity") +
    theme_bw()


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

# Export data
write.csv(hunter_percentages_long_4state, "Results/hunter_percentages_long_4state.csv", row.names = FALSE)
write.csv(hunter_cluster_success_long, "Results/hunter_cluster_success_long.csv", row.names = FALSE)
