library(ggplot2)
library(dplyr)

hunter_cluster_success <- read.csv("Results/hunters_by_cluster_with_success.csv")
hunter_percentages_long <- read.csv("Results/hunter_percentages_long.csv")
hunter_cluster_success_long <- read.csv("Results/hunter_cluster_success_long.csv")

# Histogram of percentage of time spent in each state (across all hunters)
ggplot(hunter_percentages_long, aes(x = Percentage)) +
    facet_wrap(~State, nrow = 3) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Make histogram of time spent in each state across clusters
ggplot(hunter_percentages_long, aes(x = Percentage, fill = State)) +
    facet_grid(State~Cluster) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Boxplot of time spent in each state BY Cluster
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = Cluster,
                                        fill = State)) +
    geom_boxplot() +
    theme_bw()

# Boxplot of time spent in each state BY success
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = State,
                                        fill = Harvest)) +
    geom_boxplot() +
    theme_bw()

# Plot success by cluster
ggplot(hunter_cluster_success, aes(x = Cluster, fill = Harvest)) +
    geom_bar(stat = "count") + 
    theme_bw() +
    xlab("Hunting Mode")