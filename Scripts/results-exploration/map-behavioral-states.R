library(raster)
library(sf)
library(ggplot2)
library(cowplot)

# Import rasters ----------------------------------------------------------

# Unweighted
walking_dens_unweighted <- raster::raster("Results/KDE/walking_dens_unweighted.tif")
driving_dens_unweighted <- raster::raster("Results/KDE/driving_dens_unweighted.tif")
stationary_dens_unweighted <- raster::raster("Results/KDE/stationary_dens_unweighted.tif")
stationary_road_dens_unweighted <- raster::raster("Results/KDE/stationary_road_dens_unweighted.tif")
stationary_offroad_dens_unweighted <- raster::raster("Results/KDE/stationary_offroad_dens_unweighted.tif")

# Weighted
walking_dens_weighted <- raster::raster("Results/KDE/walking_dens_weighted.tif")
driving_dens_weighted <- raster::raster("Results/KDE/driving_dens_weighted.tif")
stationary_dens_weighted <- raster::raster("Results/KDE/stationary_dens_weighted.tif")
stationary_road_dens_weighted <- raster::raster("Results/KDE/stationary_road_dens_weighted.tif")
stationary_offroad_dens_weighted <- raster::raster("Results/KDE/stationary_offroad_dens_weighted.tif")



# Crop to HREC boundary ---------------------------------------------------


# crop to boundary
hrec_boundary <- read_sf("Data/Spatial data/Raw from Alex/HREC_boundary.shp") %>% 
    st_transform(crs = st_crs(walking_dens_unweighted))

walking_dens_unweighted <- mask(crop(walking_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
driving_dens_unweighted <- mask(crop(driving_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
stationary_dens_unweighted <- mask(crop(stationary_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
stationary_road_dens_unweighted <- mask(crop(stationary_road_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
stationary_offroad_dens_unweighted <- mask(crop(stationary_offroad_dens_unweighted, extent(hrec_boundary)), hrec_boundary)
walking_dens_weighted <- mask(crop(walking_dens_weighted, extent(hrec_boundary)), hrec_boundary)
driving_dens_weighted <- mask(crop(driving_dens_weighted, extent(hrec_boundary)), hrec_boundary)
stationary_dens_weighted <- mask(crop(stationary_dens_weighted, extent(hrec_boundary)), hrec_boundary)
stationary_road_dens_weighted <- mask(crop(stationary_road_dens_weighted, extent(hrec_boundary)), hrec_boundary)
stationary_offroad_dens_weighted <- mask(crop(stationary_offroad_dens_weighted, extent(hrec_boundary)), hrec_boundary)

# Create ggplot map theme
theme_for_maps <- theme_minimal() +
    theme(legend.position = "none",
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

# Make plots --------------------------------------------------------

stationary_df <- as.data.frame(stationary_dens_weighted, xy = TRUE)
stationary_plot <- ggplot() + 
    geom_raster(data = stationary_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradient(low = "white", high = "#c28722", na.value = "transparent") +
    coord_equal() +
    geom_sf(data = hrec_boundary, aes(geometry = geometry), fill = "transparent", linewidth = 1) +
    theme_for_maps

walking_df <- as.data.frame(walking_dens_weighted, xy = TRUE)
walking_plot <- ggplot() + 
    geom_raster(data = walking_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradient(low = "white", high = "#ae5113", na.value = "transparent") +
    coord_equal() +
    geom_sf(data = hrec_boundary, aes(geometry = geometry), fill = "transparent", linewidth = 1) +
    theme_for_maps

driving_df <- as.data.frame(driving_dens_weighted, xy = TRUE)
driving_plot <- ggplot() + 
    geom_raster(data = driving_df, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradient(low = "white", high = "#4e341d", na.value = "white") +
    coord_equal() +
    geom_sf(data = hrec_boundary, aes(geometry = geometry), fill = "transparent", linewidth = 1) +
    theme_for_maps

plot_grid(stationary_plot, walking_plot, driving_plot, labels = "AUTO", nrow = 1)
