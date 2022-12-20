library(ggplot2)
library(dplyr)

hunter_cluster_success <- read.csv("Results/hunters_by_cluster_with_success.csv")
hunter_percentages_long <- read.csv("Results/hunter_percentages_long_4state.csv")
hunter_cluster_success_long <- read.csv("Results/hunter_cluster_success_long.csv")

# Histogram of percentage of time spent in each state (across all hunters)
ggplot(hunter_percentages_long, aes(x = Percentage)) +
    facet_wrap(~State_4state, nrow = 3) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Make histogram of time spent in each state across clusters
ggplot(hunter_percentages_long, aes(x = Percentage, fill = State_4state)) +
    facet_grid(Cluster4 ~ State_4state, scales = "free_y") +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Boxplot of time spent in each state BY Cluster
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = Cluster4,
                                        fill = State_4state)) +
    geom_boxplot() +
    theme_bw()

# Boxplot of time spent in each state BY success
hunter_cluster_success_long %>% 
    dplyr::filter(Harvest != "unknown") %>% 
    ggplot(aes(y = Percentage, x = State_4state, fill = Harvest)) +
    geom_boxplot() +
    theme_bw()

# Plot success by cluster
hunter_cluster_success %>% 
    dplyr::filter(Harvest != "unknown") %>% 
    ggplot(aes(x = Cluster4, fill = Harvest)) +
    geom_bar(stat = "count") + 
    theme_bw() +
    xlab("") +
    ylab("Number of Hunters") +
    scale_fill_manual(values = c("#C0C0C0", "#71797E")) 
ggsave("Figures/cluster-success-count.pdf", width = 3, height = 2)

count(filter(hunter_cluster_success, Harvest != "unknown"), Cluster, Harvest)
count(filter(hunter_cluster_success, Harvest != "unknown"), Cluster)

# 25 of 182 drivers (13.7%)
# 14 of 118 waiters (11.9%)
# 9 of 112 walkers (8.0%)

