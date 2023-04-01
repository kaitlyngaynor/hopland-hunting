library(ggplot2)
library(dplyr)

hunter_cluster_success_long <- read.csv("Results/hunter_cluster_success_long.csv")

# Histogram of percentage of time spent in each state (across all hunters)
ggplot(hunter_cluster_success_long, aes(x = Percentage)) +
    facet_wrap(~State_4state, nrow = 3) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Make histogram of time spent in each state across clusters
ggplot(hunter_cluster_success_long, aes(x = Percentage, fill = State_4state)) +
    facet_grid(Cluster4 ~ State_4state, scales = "free_y") +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State") +
    scale_fill_manual(values = c("#614124", "#E8C07D", "gray", "#CC704B"),
                      labels=c("Driving", "Stationary (off-road)", "Stationary (on-road)", "Walking"))

# Boxplot of time spent in each state BY Cluster
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = Cluster4,
                                        fill = State_4state)) +
    geom_boxplot() +
    theme_bw() +
    scale_fill_manual(name = "Behavioral State",
                      values = c("#614124", "#E8C07D", "gray", "#CC704B"),
                      labels=c("Driving", "Stationary (off-road)", "Stationary (on-road)", "Walking")) +
    xlab("Hunting Mode") +
    ylab("Percent Time") 
ggsave("Figures/behavior-state-by-cluster-boxplot.pdf", width = 7, height = 3)

# Also make a plot of time in cluster (data come from model output in 03-cluster-analysis.R)
cluster_times <- data.frame(Mode = c("Walkers", "Waiters", "Drivers"),
                            Stationary_offroad = c(0.1429111, 0.3919537, 0.0889380),
                            Stationary_road = c(0.1137739, 0.1629287, 0.1486706),
                            Walking_pct = c(0.4226213, 0.2093264, 0.1705093),
                            Driving_pct = c(0.3206938, 0.2357912, 0.5918820)) %>% 
    tidyr::pivot_longer(cols = -Mode, names_to = "State", values_to = "Percent_time")
ggplot(cluster_times, aes(x = Mode, y = Percent_time, fill = State)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("#614124", "#E8C07D", "gray", "#CC704B"),
                        labels=c("Driving", "Stationary (off-road)", "Stationary (on-road)", "Walking")) +
    ylab("Average Percent Time") +
    xlab("Hunting Mode")

# Plot success by cluster
hunter_cluster_success_long %>%
    tidyr::pivot_wider(names_from = State_4state, values_from = Percentage) %>% 
    dplyr::filter(Harvest != "unknown") %>% 
    ggplot(aes(x = Cluster4, alpha = Harvest, fill = Cluster4)) +
    geom_bar(stat = "count") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = c(0.8, 0.8)) +
    xlab("") +
    ylab("Number of Hunters") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_alpha_manual(values = c(0.4, 1),
                       labels = c("No", "Yes")) +
    guides(fill = "none") +
    coord_flip()
ggsave("Figures/cluster-success-count.pdf", width = 4, height = 3)

hunter_cluster_success_long %>%
    tidyr::pivot_wider(names_from = State_4state, values_from = Percentage) %>% 
    dplyr::filter(Harvest != "unknown") %>% 
    count(Cluster4, Harvest)

hunter_cluster_success_long %>%
    tidyr::pivot_wider(names_from = State_4state, values_from = Percentage) %>% 
    dplyr::filter(Harvest != "unknown") %>% 
    count(Cluster4)

39/240
# 39 of 240 drivers (16.3%)
10/94
# 10 of 94 waiters (10.6%)
13/149
# 13 of 149 walkers (8.7%)
