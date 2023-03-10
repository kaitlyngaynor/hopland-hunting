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

# Bin into three groups (0-5, 5-10, 10-15 hrs from sunrise)
success$Harvest_time_bin <- NA
for(i in 1:nrow(success)){
    if(is.na(success$Sunrise_elapsed_min[i])) {
        success$Harvest_time_bin[i] <- NA
    }
    else if(success$Sunrise_elapsed_min[i] < 300) {
        success$Harvest_time_bin[i] <- "1_Morning"
    } else if(success$Sunrise_elapsed_min[i] >= 300 & success$Sunrise_elapsed_min[i] <= 600) {
        success$Harvest_time_bin[i] <- "2_Midday"
    } else {
        success$Harvest_time_bin[i] <- "3_Evening"
    }
}

harvest_time_bins <- success %>% 
    dplyr::filter(Sunrise_elapsed_min >= 0) %>% 
    dplyr::count(Cluster4, Harvest_time_bin) %>% 
    tidyr::pivot_wider(names_from = "Harvest_time_bin", values_from = "n") %>% 
    tibble::column_to_rownames("Cluster4")

# Plot harvest times by hunting mode
success %>% 
    dplyr::filter(Harvest == "Y") %>%
    ggplot(aes(x = Sunrise_elapsed_min)) +
    geom_histogram() +
    facet_wrap(~Cluster4, nrow = 3) +
    theme_bw() 
success %>% 
    dplyr::filter(Harvest == "Y") %>%
    ggplot(aes(x = Sunrise_elapsed_min, fill = Cluster4)) +
    geom_histogram() +
    theme_bw() +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

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

# See if time of day varies by hunting mode
fit <- aov(Sunrise_elapsed_min_scale ~ Cluster4, data = success)
summary(fit)
TukeyHSD(fit, conf.level=.95)
plot(TukeyHSD(fit, conf.level=.95), las = 2)

# Binned version
fit2 <- chisq.test(as.matrix(harvest_time_bins))
fit2
# Pearson's Chi-squared test
# data:  as.matrix(harvest_time_bins)
# X-squared = 6.5862, df = 4, p-value = 0.1594
