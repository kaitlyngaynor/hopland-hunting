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
    ggplot(aes(x = Cluster4, alpha = Harvest, fill = Cluster4)) +
    geom_bar(stat = "count") + 
    geom_text(label = c(2, 4, 9), vjust = -0.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank()) +
    xlab("") +
    ylab("Number of Hunters") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_alpha_manual(values = c(0.4, 1)) +
    guides(fill = "none")
ggsave("Figures/cluster-success-count.pdf", width = 4, height = 3)

count(filter(hunter_cluster_success, Harvest != "unknown"), Cluster4, Harvest)
count(filter(hunter_cluster_success, Harvest != "unknown"), Cluster4)

# 28 of 215 drivers (13.0%)
# 11 of 87 waiters (12.6)
# 9 of 110 walkers (8.2%)

