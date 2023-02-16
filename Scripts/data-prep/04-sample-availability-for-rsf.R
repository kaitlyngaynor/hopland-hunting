library(raster)
library(sf)

# Bring in raster to use as template
raster_template <- raster::raster("Data/Spatial data/Cleaned rasters/blm_dist.tif") 

# Bring in HREC huntable boundary
huntable <- sf::read_sf("Data/Spatial data/huntable.shp") %>% 
    sf::st_transform(crs = "+proj=utm +zone=10 +ellps=WGS84 +units=m +no_defs")

# Sample availability
sampled_points <- raster_template %>%
    
    # Get x + y from each raster cell and return as tibble
    raster::rasterToPoints() %>%
    dplyr::as_tibble() %>%
    
    # x + y tibble to spatial object
    sf::st_as_sf(coords = c("x", "y"), 
                 crs = st_crs(huntable), 
                 remove = FALSE) %>%
    
    # Intersect with home range
    sf::st_intersection(huntable) %>%
    
    # Return spatial object with systematic points and x + y coords
    dplyr::select(x, y, geometry)

# Bring in raster stack
rastlist <- list.files(path = here::here('Data', 'Spatial data', 'Cleaned rasters'),
                       pattern='.tif$', all.files=TRUE, full.names=FALSE)
raster_stack <- raster::stack(paste0("Data/Spatial data/Cleaned rasters/", rastlist))

# Extract covariates from raster stack
sampled_points_cov <- as.data.frame(raster::extract(raster_stack, sampled_points)) 

# Select and rename covariates
sampled_points_cov <- sampled_points_cov %>% 
    dplyr::select(rugged9.clean, view,
                  vegetation.coarser.clean2, 
                  road.dist.clean,
                  layer.1, layer.2, layer.4,
                  hq_dist) %>% 
    dplyr::rename(Viewshed = view,
                  Ruggedness = rugged9.clean,
                  Habitat = vegetation.coarser.clean2,
                  Road_Distance = road.dist.clean,
                  Chaparral_120m = layer.1,
                  Grassland_120m = layer.2,
                  Woodland_120m = layer.4,
                  HQ_Distance = hq_dist)

# Export for analysis
write.csv(sampled_points_cov, "Data/all-available-point-cov.csv", row.names = FALSE)
