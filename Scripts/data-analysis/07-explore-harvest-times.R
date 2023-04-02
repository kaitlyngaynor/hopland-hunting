library(dplyr)
library(ggplot2)

success <- read.csv("Results/hunter_cluster_success_long.csv") %>%
    tidyr::pivot_wider(names_from = "State_4state", values_from = "Percentage") %>%
    dplyr::filter(Harvest != "unknown") %>% 
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1))

# Bring in metadata
metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv")
metadata <- bind_rows(metadata1, metadata2)
success <- left_join(success, metadata)

# Calculate time from sunrise (06:00) to harvest
success$Sunrise_elapsed_min <- as.numeric(difftime(as.POSIXct(success$Harvest_time, format = "%H:%M"), 
                                                   as.POSIXct("06:00", format = "%H:%M"),
                                                   units = "mins"))
success$Sunrise_elapsed_min_scale <- scale(success$Sunrise_elapsed_min)


set.seed(678)
success %>% 
    dplyr::filter(Harvest == "Y") %>%
    ggplot(aes(x = Sunrise_elapsed_min/60, y = Cluster4, col = Cluster4)) +
    geom_point(size = 3, alpha = 0.75, position = position_jitter(w = 0.15, h = 0.15)) +
    theme_bw() +
    ylab("Hunting Mode") +
    xlab("Time From Sunrise (hours)") +
    theme(legend.position = "none") +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

# Density plot with jittered points
success %>% 
    dplyr::filter(Harvest == "Y") %>%
    ggplot(aes(x = Sunrise_elapsed_min/60, fill = Cluster4)) +
    geom_density(alpha = 0.75) +
    theme_bw() +
    facet_wrap(~Cluster4, ncol = 1) +
    ylab("Density") +
    xlab("Time From Sunrise (hours)") +
    theme(legend.position = "none") +
    geom_jitter(data = success, aes(x = Sunrise_elapsed_min/60, y = -0.0125, col = Cluster4), width = 0.25, height = 0, alpha = 0.5, size = 2) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))
ggsave("Figures/time-of-harvest.pdf", width = 3, height = 5)

# See if time of day varies by hunting mode - Anderson-Darling test

library(kSamples)

coursing <- dplyr::filter(success, Cluster4 == "Coursing", Harvest == "Y")
stalking <- dplyr::filter(success, Cluster4 == "Stalking", Harvest == "Y")
sitandwait <- dplyr::filter(success, Cluster4 == "Sit-and-wait", Harvest == "Y")

set.seed(12)
ad.test(coursing$Sunrise_elapsed_min, stalking$Sunrise_elapsed_min, sitandwait$Sunrise_elapsed_min,
        method = "exact", dist = FALSE, Nsim = 1000)