# Export spatial maps of each state

# Set up --------------------------------------------------------

library(sf)
library(raster)
library(SpatialKDE)
library(dplyr)

# Bring in data with model results
data_hmm <- read.csv("Results/hmm-data-with-model-predictions.csv")

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
reference <- sf::create_raster(huntable, cell_size = 100) 

# alternatively, use 10m x 10m raster
# reference <- raster("Data/Spatial data/Cleaned rasters/road.dist.clean.tif")


# Approach 1: KDE for points sorted by most likely state ------------------

# Split observations into the three behavioral states, based on most likely state
walking <- dplyr::filter(data_hmm_sf, state == "Walking") 
stationary <- dplyr::filter(data_hmm_sf, state == "Stationary") 
driving <- dplyr::filter(data_hmm_sf, state == "Driving") 

# Calculate kernel density for each behavioral state
walking_dens_unweighted <- SpatialKDE::kde(walking, band_width = 400, grid = reference)
driving_dens_unweighted <- SpatialKDE::kde(driving, band_width = 400, grid = reference)
stationary_dens_unweighted <- SpatialKDE::kde(stationary, band_width = 400, grid = reference)


# Approach 2: KDE for all points weighted by probability -----------

walking_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                         weights = data_hmm_sf$Walking_Prob)
stationary_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                         weights = data_hmm_sf$Stationary_Prob)
driving_dens_weighted <- SpatialKDE::kde(data_hmm_sf, band_width = 400, grid = reference,
                                            weights = data_hmm_sf$Driving_Prob)


# Export rasters ----------------------------------------------------------

# Unweighted
writeRaster(walking_dens_unweighted, "Results/KDE/walking_dens_unweighted.tif")
writeRaster(driving_dens_unweighted, "Results/KDE/driving_dens_unweighted.tif")
writeRaster(stationary_dens_unweighted, "Results/KDE/stationary_dens_unweighted.tif")

# Weighted
writeRaster(walking_dens_weighted, "Results/KDE/walking_dens_weighted.tif")
writeRaster(driving_dens_weighted, "Results/KDE/driving_dens_weighted.tif")
writeRaster(stationary_dens_weighted, "Results/KDE/stationary_dens_weighted.tif")
