# Examine correlation between hunting rasters and environmental covariates

library(raster)

# Import rasters ----------------------------------------------------------

walking <- raster::raster("Results/KDE/walking_dens_weighted.tif")
driving <- raster::raster("Results/KDE/driving_dens_weighted.tif")
stationary <- raster::raster("Results/KDE/stationary_dens_weighted.tif")

# Clip rasters to HREC (so that values are NA outside of boundary)


# Bring in the other environmental rasters


# Resample so that they are at same spatial scale