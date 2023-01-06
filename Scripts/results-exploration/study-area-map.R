library(ggplot2)
library(sf)
library(vroom)

# Bring in huntable area
hrec_boundary <- sf::st_read("Data/Spatial data/Raw from Alex/HREC_boundary.shp") %>% 
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
no_hunting <- sf::st_read("Data/Spatial data/Raw from Alex/hunt_zone.shp") %>% 
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
huntable <- sf::st_difference(hrec_boundary, st_union(st_combine(no_hunting)))
huntable_line <- sf::st_cast(huntable, "MULTILINESTRING")


# Bring in movement paths
igotu_data <- vroom::vroom("Data/igotu_data_3min_covariates.csv")

# Choose just a few tracks
igotu_few <- igotu_data %>% 
    dplyr::filter(ID %in% c("080815_11", "080815_12", "080815_06"))
igotu_few_sf <- sf::st_as_sf(igotu_few,
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                             remove = FALSE)
igotu_few_sf_lines <- sf::st_cast(igotu_few_sf, "MULTILINESTRING")

# Make a map
ggplot() +
    #geom_sf(data = huntable_line) +
    geom_path(data = igotu_few_sf, aes(Latitude, Longitude, col = ID)) +
    theme_bw()




