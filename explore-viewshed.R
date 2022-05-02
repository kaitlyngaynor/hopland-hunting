library(raster)

# Bring in the raw viewshed raster from Alex
viewshed <- raster("Data/Spatial data/viewshed2/view_for_kg.tif")
plot(viewshed)

# Bring in the BLM distance raster as a template
blm <- raster("Data/Spatial data/Cleaned rasters/blm_dist.tif")
plot(blm)

# Reproject the viewshed in the same CRS
viewshed_proj <- raster::projectRaster(from = viewshed,
                                       to = blm)
plot(viewshed_proj)

# Stack rasters just to confirm
stack_test <- raster::stack(viewshed_proj, blm)

# Export the new raster
writeRaster(viewshed_proj, 'Data/Spatial data/Cleaned rasters/view_for_kg_proj.tif', format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

