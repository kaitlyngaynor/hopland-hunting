library(viridis)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)

# Import rasters ----------------------------------------------------------

# Unweighted
walking_dens_unweighted <- raster::raster("Results/KDE/walking_dens_unweighted.tif")
driving_dens_unweighted <- raster::raster("Results/KDE/driving_dens_unweighted.tif")
stationary_dens_unweighted <- raster::raster("Results/KDE/stationary_dens_unweighted.tif")

# Weighted
walking_dens_weighted <- raster::raster("Results/KDE/walking_dens_weighted.tif")
driving_dens_weighted <- raster::raster("Results/KDE/driving_dens_weighted.tif")
stationary_dens_weighted <- raster::raster("Results/KDE/stationary_dens_weighted.tif")



# Crop to HREC boundary ---------------------------------------------------


# crop to boundary
hrec_boundary <- readOGR("Data/Spatial data/Raw from Alex", "HREC_boundary") %>% 
    spTransform("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")

walking_dens_unweighted <- mask(crop(walking_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
driving_dens_unweighted <- mask(crop(driving_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
stationary_dens_unweighted <- mask(crop(stationary_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
walking_dens_weighted <- mask(crop(walking_dens_weighted, extent(hrec_boundary)), hrec_boundary)
driving_dens_weighted <- mask(crop(driving_dens_weighted, extent(hrec_boundary)), hrec_boundary)
stationary_dens_weighted <- mask(crop(stationary_dens_weighted, extent(hrec_boundary)), hrec_boundary)


# Comparison plots --------------------------------------------------------

# Weighted vs unweighted for each behavioral state

# Stationary
par(mfrow=c(1,2))
plot(walking_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Walking Unweighted")
plot(walking_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Walking Weighted")

# Walking
par(mfrow=c(1,2))
plot(stationary_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Stationary Unweighted")
plot(stationary_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Stationary Weighted")

# Driving
par(mfrow=c(1,2))
plot(driving_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Driving Unweighted")
plot(driving_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Driving Weighted")

# All 3 unweighted states
par(mfrow=c(1,3))
plot(stationary_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Stationary (Unweighted)")
plot(walking_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Walking (Unweighted)")
plot(driving_dens_unweighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Driving (Unweighted)")

# All 3 weighted states
par(mfrow=c(1,3))
plot(stationary_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Stationary (Weighted)")
plot(walking_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Walking (Weighted)")
plot(driving_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,400),
     main = "Driving (Weighted)")

# All 3 weighted states - different scales

par(mfrow=c(1,3))
plot(stationary_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,350),
     main = "Stationary (Weighted)")
plot(walking_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,300),
     main = "Walking (Weighted)")
plot(driving_dens_weighted, 
     col = viridis(1e3), 
     zlim=c(0,700),
     main = "Driving (Weighted)")



# For exporting -----------------------------------------------------------

weighted_stack <- raster::stack(stationary_dens_weighted,
                                walking_dens_weighted,
                                driving_dens_weighted)
names(weighted_stack) <- c("Stationary", "Walking", "Driving")
weighted_stack_df <- weighted_stack %>% 
    as.data.frame(xy = TRUE) %>% 
    tidyr::pivot_longer(cols = !c(x, y), 
                        names_to = 'variable', 
                        values_to = 'value') %>% 
    dplyr::filter(value != "NA")
ggplot() +
    geom_raster(data = weighted_stack_df, aes(x = x, y = y, fill = value)) + 
    facet_wrap(~ variable) +
    theme_minimal()

# Stationary
#stationary_plot <- 
stationary_dens_weighted %>% 
    as.data.frame(xy = TRUE) %>% 
    tidyr::pivot_longer(cols = !c(x, y), 
                        names_to = 'variable', 
                        values_to = 'value') %>% 
    dplyr::filter(value != "NA") %>% 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = value)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
   # ggtitle("Stationary") +
    scale_fill_viridis()
ggsave("Figures/map-stationary.pdf")

walking_dens_weighted %>% 
    as.data.frame(xy = TRUE) %>% 
    tidyr::pivot_longer(cols = !c(x, y), 
                        names_to = 'variable', 
                        values_to = 'value') %>% 
    dplyr::filter(value != "NA") %>% 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = value)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
   # ggtitle("Walking") +
    scale_fill_viridis()
ggsave("Figures/map-walking.pdf")

driving_dens_weighted %>% 
    as.data.frame(xy = TRUE) %>% 
    tidyr::pivot_longer(cols = !c(x, y), 
                        names_to = 'variable', 
                        values_to = 'value') %>% 
    dplyr::filter(value != "NA") %>% 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = value)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
   # ggtitle("Driving") +
    scale_fill_viridis()

ggsave("Figures/map-driving.pdf")

# bring in roads
roads <- read_sf("Data/Spatial data/Raw from Alex/roads_densified.shp")

# export
