library(dplyr)
library(ggplot2)
library(overlap)
library(kSamples)
library(cowplot)
`%notin%` <- Negate(`%in%`)

success <- read.csv("Results/hunter_cluster_success_long.csv") %>%
    tidyr::pivot_wider(names_from = "State_4state", values_from = "Percentage") %>% 
    dplyr::filter(Harvest == "Y") %>% 
    dplyr::select(ID, Cluster4)

# Bring in metadata
metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv")
metadata <- bind_rows(metadata1, metadata2) %>% 
    dplyr::select(ID, Date, Harvest_time)
success <- left_join(success, metadata)  %>%
    dplyr::filter(ID %notin% c("082419_06", "082419_13")) # missing harvest times

# Scale times
success$Date <- as.POSIXct(success$Date, format = "%m/%d/%y")
success$Time_HM <- lubridate::hm(success$Harvest_time)
success$Time_Decimal <- success$Time_HM$hour + success$Time_HM$minute/60
success$Time_Scaled <- success$Time_Decimal / 24
success$Time_Radians <- success$Time_Scaled * 2 * pi
coords <- matrix(c(-123.079, 39.0013), nrow=1) # note it is c(longitude, latitude)
Coords <- sp::SpatialPoints(coords,
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
success$Time_Sun <- overlap::sunTime(success$Time_Radians, success$Date, Coords)


set.seed(678)
success %>% 
    ggplot(aes(x = Time_Sun, y = Cluster4, col = Cluster4)) +
    geom_point(size = 3, alpha = 0.75, position = position_jitter(w = 0.15, h = 0.15)) +
    theme_bw() +
    ylab("Hunting Mode") +
    xlab("Time of Day") +
    theme(legend.position = "none") +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

# Density plot with jittered points
(a <- success %>% 
    ggplot(aes(x = Time_Sun, fill = Cluster4)) +
    geom_density(alpha = 0.75) +
    theme_bw() +
    facet_wrap(~Cluster4, ncol = 1) +
    ylab("Density") +
    xlab("Time of Day") +
    theme(legend.position = "none") +
          #axis.title.x = element_blank(),
          #axis.ticks.x = element_blank(),
          #axis.text.x = element_blank()) +
    #geom_jitter(data = success, aes(x = Time_Sun, y = -0.0125, col = Cluster4), width = 0.25, height = 0, alpha = 0.5, size = 2) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))  +
    scale_x_continuous(breaks = c(pi/2, pi, (3*pi)/2),
                       labels = c("Sunrise", "Noon", "Sunset"))
    )
#ggsave("Figures/time-of-harvest.pdf", width = 3, height = 5)

# See if time of day varies by hunting mode - Anderson-Darling test

coursing <- dplyr::filter(success, Cluster4 == "Coursing")
stalking <- dplyr::filter(success, Cluster4 == "Stalking")
sitandwait <- dplyr::filter(success, Cluster4 == "Sit-and-wait")

set.seed(12)
ad.test(coursing$Time_Sun, stalking$Time_Sun, sitandwait$Time_Sun,
        method = "exact", dist = FALSE, Nsim = 1000)


# Add buck plot -----------------------------------------------------------

buck_records <- read.csv("Data/legal-buck-during-hunting.csv")

# Plot diel activity
bwA <- getBandWidth(buck_records$Time_Sun, kmax = 3)
diel_data <- data.frame(matrix(ncol = 0, nrow = 128))
diel_data$xxRad <- seq(0, 2 * pi, length = 128)
diel_data$densA <- densityFit(buck_records$Time_Sun, diel_data$xxRad, bwA)/(24/(2 * pi))
diel_data$Species <- "Male Deer"

(b <- ggplot(diel_data, aes(x = xxRad, y = densA)) +
    geom_line() +
    geom_ribbon(aes(x = xxRad, ymax = densA), ymin=0, alpha=0.5) +
    theme_bw() +
    facet_wrap(~Species) +
    scale_x_continuous(limits = c(pi/2, (3*pi)/2),
                       breaks = c(pi/2, pi, (3*pi)/2),
                       labels = c("Sunrise", "Noon", "Sunset")) +
    ylim(c(0, 0.075)) +
    ylab("Density") +
    xlab("Time of Day"))
#ggsave("Figures/time-of-buck-activity.pdf", width = 3, height = 1)

plot_grid(a, b,
          labels = "AUTO",
          rel_heights = c(2.5, 1),
          ncol = 1,
          align = "v")
ggsave("Figures/time-of-harvest-and-buck-activity.pdf", width = 3, height = 6)
