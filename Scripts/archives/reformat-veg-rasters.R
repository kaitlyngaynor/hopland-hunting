library(raster)

# read in the files that Alex sent
chap <- raster("Data/Spatial data/veg_120/chap_120m")
wood <- raster("Data/Spatial data/veg_120/wood_120m")
grass <- raster("Data/Spatial data/veg_120/grass_120m")
view <- raster("Data/Spatial data/view/view_for_kg.tif")

# get in same resolution
blm_dist <- raster("Data/Spatial data/Cleaned rasters/blm_dist.tif")

# make extent the same
raster::extent(chap) <- blm_dist
raster::extent(wood) <- blm_dist
raster::extent(grass) <- blm_dist
raster::extent(view) <- blm_dist

# make resolution the same
chap <- raster::resample(chap, blm_dist, method = 'bilinear') 
wood <- raster::resample(wood, blm_dist, method = 'bilinear') 
grass <- raster::resample(grass, blm_dist, method = 'bilinear') 
view <- raster::resample(view, blm_dist, method = 'bilinear')

# test stack
test_stack <- raster::stack(blm_dist, chap, wood, grass, view)

# export
raster::writeRaster(chap, 'Data/Spatial data/Cleaned rasters/chap_120m.tif', format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
raster::writeRaster(wood, 'Data/Spatial data/Cleaned rasters/wood_120m.tif', format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
raster::writeRaster(grass, 'Data/Spatial data/Cleaned rasters/grass_120m.tif', format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
raster::writeRaster(view, 'Data/Spatial data/Cleaned rasters/view_for_kg_proj.tif', format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
