# Export spatial maps of each state

# Set up --------------------------------------------------------

library(sf)
library(raster)
library(SpatialKDE)
library(dplyr)

# Bring in data with model results
data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-03-04.csv")

# Convert to spatial object & transform to UTM
data_hmm_sf <- sf::st_as_sf(data_hmm,
                            coords = c("x", "y"),
                            crs = "+proj=longlat +datum=WGS84", 
                            remove = FALSE) %>% 
    sf::st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")

# Create 100m resolution reference raster to use for kernel density estimation
sf::sf_use_s2(FALSE)
huntable <- read_sf("Data/Spatial data/huntable.shp") %>% 
    sf::st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
reference <- SpatialKDE::create_raster(huntable, cell_size = 100) 


# Approach 1: KDE for points sorted by most likely state ------------------

# Split observations into the three behavioral states, based on most likely state
walking <- dplyr::filter(data_hmm_sf, state == "Walking") 
stationary <- dplyr::filter(data_hmm_sf, state == "Stationary") 
driving <- dplyr::filter(data_hmm_sf, state == "Driving") 
stationary_road <- dplyr::filter(data_hmm_sf, state_2stationary == "Stationary_road") 
stationary_offroad <- dplyr::filter(data_hmm_sf, state_2stationary == "Stationary_offroad") 

road <- dplyr::filter(data_hmm_sf, Road_Distance < 10)
offroad <- dplyr::filter(data_hmm_sf, Road_Distance >= 10)

# Calculate kernel density for each behavioral state
walking_dens_unweighted <- SpatialKDE::kde(walking, band_width = 400, grid = reference)
driving_dens_unweighted <- SpatialKDE::kde(driving, band_width = 400, grid = reference)
stationary_dens_unweighted <- SpatialKDE::kde(stationary, band_width = 400, grid = reference)
stationary_road_dens_unweighted <- SpatialKDE::kde(stationary_road, band_width = 400, grid = reference)
stationary_offroad_dens_unweighted <- SpatialKDE::kde(stationary_offroad, band_width = 400, grid = reference)


# Approach 2: KDE for all points weighted by probability -----------

walking_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                         weights = data_hmm_sf$Walking_Prob)
stationary_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                         weights = data_hmm_sf$Stationary_Prob)
driving_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                            weights = data_hmm_sf$Driving_Prob)
stationary_road_dens_weighted <- SpatialKDE::kde(road, band_width = 400, grid = reference,
                                            weights = road$Stationary_Prob)
stationary_offroad_dens_weighted <- SpatialKDE::kde(offroad, band_width = 400, grid = reference,
                                                 weights = offroad$Stationary_Prob)

# Export rasters ----------------------------------------------------------

# Unweighted
writeRaster(walking_dens_unweighted, "Results/KDE/walking_dens_unweighted.tif",
            overwrite = TRUE)
writeRaster(driving_dens_unweighted, "Results/KDE/driving_dens_unweighted.tif",
            overwrite = TRUE)
writeRaster(stationary_dens_unweighted, "Results/KDE/stationary_dens_unweighted.tif",
            overwrite = TRUE)
writeRaster(stationary_road_dens_unweighted, "Results/KDE/stationary_road_dens_unweighted.tif",
            overwrite = TRUE)
writeRaster(stationary_offroad_dens_unweighted, "Results/KDE/stationary_offroad_dens_unweighted.tif",
            overwrite = TRUE)

# Weighted
raster::writeRaster(walking_dens_weighted, "Results/KDE/walking_dens_weighted.tif",
                    overwrite = TRUE)
raster::writeRaster(driving_dens_weighted, "Results/KDE/driving_dens_weighted.tif",
                    overwrite = TRUE)
raster::writeRaster(stationary_dens_weighted, "Results/KDE/stationary_dens_weighted.tif",
                    overwrite = TRUE)
raster::writeRaster(stationary_road_dens_weighted, "Results/KDE/stationary_road_dens_weighted.tif",
                    overwrite = TRUE)
raster::writeRaster(stationary_offroad_dens_weighted, "Results/KDE/stationary_offroad_dens_weighted.tif",
                    overwrite = TRUE)



# Approach 3: 10m x 10m KDE for all points weighted_fine by probability -----------
# this is taking ages to run!
reference_fine <- raster("Data/Spatial data/Cleaned rasters/road.dist.clean.tif")

walking_dens_weighted_fine <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference_fine,
                                              weights = data_hmm_sf$Walking_Prob)
raster::writeRaster(walking_dens_weighted_fine, "Results/KDE/walking_dens_weighted_fine.tif")

stationary_dens_weighted_fine <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference_fine,
                                                 weights = data_hmm_sf$Stationary_Prob)
raster::writeRaster(stationary_dens_weighted_fine, "Results/KDE/stationary_dens_weighted_fine.tif")

driving_dens_weighted_fine <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference_fine,
                                              weights = data_hmm_sf$Driving_Prob)
raster::writeRaster(driving_dens_weighted_fine, "Results/KDE/driving_dens_weighted_fine.tif")

stationary_road_dens_weighted_fine <- SpatialKDE::kde(road, band_width = 400, grid = reference_fine,
                                                 weights = road$Stationary_Prob)
raster::writeRaster(stationary_road_dens_weighted_fine, "Results/KDE/stationary_road_dens_weighted_fine.tif")

stationary_offroad_dens_weighted_fine <- SpatialKDE::kde(offroad, band_width = 400, grid = reference_fine,
                                                    weights = offroad$Stationary_Prob)
raster::writeRaster(stationary_offroad_dens_weighted_fine, "Results/KDE/stationary_offroad_dens_weighted_fine.tif")
