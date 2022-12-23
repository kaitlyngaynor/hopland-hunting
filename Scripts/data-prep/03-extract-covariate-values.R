# Extract covariate values at points

# Load libraries
library(raster)
library(dplyr)
library(sf)
library(rgdal)

# Bring in points
igotu_data <- read.csv("Data/igotu_data_3min.csv")

# convert coordinates to spatial object
igotu_data <- st_as_sf(igotu_data, 
                       coords = c("Longitude", "Latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                       remove = FALSE)

# Clip points to huntable area
hrec_boundary <- readOGR("Data/Spatial data/Raw from Alex", "HREC_boundary") %>% 
    spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
no_hunting <- readOGR("Data/Spatial data/Raw from Alex", "hunt_zone") %>% 
    spTransform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
huntable <- hrec_boundary - no_hunting
writeOGR(obj=huntable, dsn="Data/Spatial data", layer="huntable", driver="ESRI Shapefile", overwrite_layer = TRUE)
sf::sf_use_s2(FALSE) # turn off to avoid errors
huntable_sf <- read_sf("Data/Spatial data/huntable.shp") %>% 
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
igotu_data_cropped <- igotu_data[st_intersects(igotu_data, huntable_sf) %>% lengths > 0,]


# Bring in raster stack
rastlist <- list.files(path = here::here('Data', 'Spatial data', 'Cleaned rasters'),
                       pattern='.tif$', all.files=TRUE, full.names=FALSE)
raster.stack <- raster::stack(paste0("Data/Spatial data/Cleaned rasters/", rastlist))

# Extract covariates from raster stack
igotu.raster <- as.data.frame(raster::extract(raster.stack, igotu_data_cropped)) 

# Combine raster metadata with the other data 
# Note that I use cbind here, so it's important that the rows be in the same order (they should be...)
igotu_data_covariates <- cbind(st_drop_geometry(igotu_data_cropped), igotu.raster) 

# Select columns of interest for final model
igotu_data_fewer <- igotu_data_covariates %>% 
    dplyr::select(ID, Longitude, Latitude, DateTime,
                  rugged9.clean, view,
                  vegetation.coarser.clean2, 
                  road.dist.clean,
                  layer.1, layer.4,
                  hq_dist,
                  Elapsed_Time_Sunrise, Harvest) %>% 
    dplyr::rename(Viewshed = view,
                  Ruggedness = rugged9.clean,
                  Habitat = vegetation.coarser.clean2,
                  Road_Distance = road.dist.clean,
                  Chaparral_120m = layer.1,
                  Woodland_120m = layer.4,
                  HQ_Distance = hq_dist)

# Export
write.csv(igotu_data_fewer, "Data/igotu_data_3min_covariates.csv", row.names = F)
