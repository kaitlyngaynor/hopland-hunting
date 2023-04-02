library(ggplot2)
library(dplyr)
library(plyr)
library(cowplot)
library(ggfortify)

# from 03-cluster-analysis
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1        0.1             61.4                        61.4
# Dim.2        0.0             26.0                        87.4
# Dim.3        0.0             12.6                       100.0
# Dim.4        0.0              0.0                       100.0

hunter_percentages_4state <- read.csv("Results/hunter_percentages_4state.csv")

#getting the convex hull of each unique point set
find_hull <- function(df) df[chull(df$Dim.1, df$Dim.2), ]
hulls <- plyr::ddply(hunter_percentages_4state, "Cluster4", find_hull)

# Clusters of hunters
ggplot(data = hunter_percentages_4state, 
       aes(x = Dim.1, y = Dim.2, colour = Cluster4, fill = Cluster4)) +
    geom_point() + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Dim.1 (61.4%)", y = "Dim.2 (26.0%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_bw()

# With different shapes for harvest
ggplot(data = hunter_percentages_4state, 
       aes(x = Dim.1, y = Dim.2, colour = Cluster4, fill = Cluster4)) +
    geom_point(aes(shape = Harvest)) + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Dim.1 (61.4%)", y = "Dim.2 (26.0%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_bw()

ggplot(data = hunter_percentages_4state, 
       aes(x = Dim.1, y = Dim.2, colour = Cluster4, fill = Cluster4)) +
    geom_point(aes(shape = Harvest)) + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Dim.1 (61.4%)", y = "Dim.2 (26.0%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_bw()

# two panels
a <- hunter_percentages_4state %>% 
    dplyr::filter(Harvest == "N") %>% 
    ggplot(aes(x = Dim.1, y = Dim.2, colour = Cluster4, fill = Cluster4)) +
    geom_point() + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Dim.1 (61.4%)", y = "Dim.2 (26.0%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_bw()
b <- hunter_percentages_4state %>% 
    dplyr::filter(Harvest == "Y") %>% 
    ggplot(aes(x = Dim.1, y = Dim.2, colour = Cluster4, fill = Cluster4)) +
    geom_point(shape = "triangle", size = 2) + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Dim.1 (61.4%)", y = "Dim.2 (26.0%)") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_bw()

plot_grid(a, b)
