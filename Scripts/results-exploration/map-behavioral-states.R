library(viridis)
library(raster)

# Import rasters ----------------------------------------------------------

# Unweighted
walking_dens_unweighted <- raster::raster("Results/KDE/walking_dens_unweighted.tif")
driving_dens_unweighted <- raster::raster("Results/KDE/driving_dens_unweighted.tif")
stationary_dens_unweighted <- raster::raster("Results/KDE/stationary_dens_unweighted.tif")

# Weighted
walking_dens_weighted <- raster::raster("Results/KDE/walking_dens_weighted.tif")
driving_dens_weighted <- raster::raster("Results/KDE/driving_dens_weighted.tif")
stationary_dens_weighted <- raster::raster("Results/KDE/stationary_dens_weighted.tif")


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
